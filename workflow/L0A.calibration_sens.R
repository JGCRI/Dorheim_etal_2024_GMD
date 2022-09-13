# Script that does a sensitivity analysis and calibrates Hector 
# historical observations of tas & [CO2]. 


# 0. Set Up -----------------------------------------------------------------------------------------------
# Load the required packages. 
library(assertthat)
library(dplyr)
library(FME)
library(GGally)
library(ggplot2)
library(magrittr)
library(tidyr)
library(readr)

# Load Hector! Make sure it is the correct branch. 
remotes::install_github("JGCRI/hector@v3_dev")
library(hector)

# The location where to save results to. 
BASE_DIR <- here::here()
DATADIR <- file.path(BASE_DIR, "data")

# Figure settings  
theme_set(theme_bw())


# Define helper functions -------------------------------------------------------------------------------

# Calculate gmst based on Hector land and ocean surface temperature
#
# Args
#   data: Hector output dataframe containing results from "Tgav_land" and "Tgav_ocean_ST"
# Return: Hector output data frame containing gmst
get_gmst <- function(data){
  flnd <- 0.29
  
  land_air <- data[data$variable == LAND_TAS(), ]
  tos <- data[data$variable == SST(), ]
  gmst_vals <- land_air$value *  flnd + tos$value * (1 - flnd)
  
  land_air$value <- gmst_vals
  land_air$variable <- "GMST"
  return(land_air)
  
}


# Wrapper function compatible with sensRange & sensFun functions it returns the function 
# that returns another function. 
# Args 
#   pars: vector of the parameters to test 
#   ini: str ini path 
#   vars: str vector of the Hector variables to look at, c(GLOBAL_TAS(), NPP(), CONCENTRATIONS_CO2(), SST())
# Returns dataframe formatted to work with FME
make_runHector <- function(ini, vars){
  
  fx <- function(pars){
    core <- newcore(system.file(ini, package = "hector"))
    
    # Set the variables 
    mapply(function(val, var){
      setvar(core, dates = NA, var = var, values = val, unit = getunits(var))
    },val = pars, var = names(pars), USE.NAMES = FALSE)
    reset(core)
    run(core)
    
    out1 <- fetchvars(core, 1750:2300, vars = vars) 
    out2 <- get_gmst(fetchvars(core, 1750:2300, vars = c(LAND_TAS(), SST())))
    bind_rows(out1, out2) %>% 
      select(time = year, variable, value) %>% 
      spread(variable, value) -> 
      out 
  
    shutdown(core)
    return(out)
    
  }
  
  return(fx)
}


# Make a parameter range table 
# Args 
#   pars: vector of Hector parameter values, this is the central value
#   frac: numeric this is the portion the Hector parameter will be increase/decreased by 
# Returns dataframe of the parameter range to test
make_parRanges <- function(pars, frac){
  
  assert_that(all(is.character(sapply(X = names(pars), FUN = getunits))))
  
  out <- data.frame(min = pars - pars * frac, 
                    max = pars + pars * frac)
  rownames(out) <- names(pars)
  
  return(out)
}


# Format the object returned by the sensRange function 
# Args 
#   obj: obeject created by sensRange
# Returns: a long formatted data frame
format_sensRange <- function(obj){
  
  assert_that(class(obj)[[1]] == "sensRange") 
  
  summary(obj) %>% 
    rename(year = x) -> 
    out 
  
  vars <- gsub(x = rownames(out), pattern = "\\d+", replacement = "")
  
  out$variable <- vars
  rownames(out) <- NULL
  
  rename(out)
  
}


# Format the object returned by the sensFun  function 
# Args 
#   obj: object created by sensFun 
# Returns: a long formatted data frame ready for plotting! 
format_sensFun <- function(obj){
  
  class(obj)[[1]] == "sensFun"
  
  obj %>%  
    rename(year = x, variable = var) %>% 
    gather(parameter, value, -year, -variable, -scenario) -> 
    out 
  
  return(out)
}


# Run FME::senseFun with different ini files 
# Args
#   inis: str path to the ini files 
#   vars: str name of the variables to test 
#   pars: str name of the parameters to runn 
# Returns data frame of sensFun
batch_sensFun <- function(inis, vars, pars) {
  
  out <- data.frame()
  for(ini in inis){
    custom_make_run <- make_runHector(ini, vars)
    x <- sensFun(func = custom_make_run,  parms = pars)  
    x$scenario <- gsub(ini, pattern = "input/hector_|.ini", replacement = "")
    out <- rbind(out, format_sensFun(x))
  }
  
  return(out)
  
}


# Run FME::senseFun with different ini files 
# Args
#   inis: str path to the ini files 
#   vars: str name of the variables to test 
#   pars: df of the parameter range
# Returns data frame of sensFun
batch_sensRange <- function(inis, vars, parRange, n) {
  
  out <- data.frame()
  for(ini in inis){
    custom_make_run <- make_runHector(ini, vars)
    x <- sensRange(func = custom_make_run,  parRange = parRange, num = n)  
    xx <- format_sensRange(x)
    xx$scenario <- gsub(ini, pattern = "input/hector_|.ini", replacement = "")
    out <- rbind(out, xx)
    
  }
  
  return(out)
  
}


# 1. Sensitivity Analysis ------------------------------------------------------------------------------------------
# Of the parameters we are interested in what is the most important parameter to get correctly? 

inis <- c("input/hector_ssp119.ini", "input/hector_ssp126.ini", "input/hector_ssp245.ini", 
          "input/hector_ssp370.ini", "input/hector_ssp434.ini", "input/hector_ssp460.ini", 
          "input/hector_ssp534-over.ini", "input/hector_ssp585.ini")
vars <- c(GLOBAL_TAS(), LAND_TAS(), SST(), NPP(), ATMOSPHERIC_CO2(), PH(), HEAT_FLUX())

# sensFun ---------------------------------------------------------------------------------------------------------
pars <- c("S"=2.7, "diff"=2.4, "alpha"=0.5, "volscl"=1.0, "beta" = 2, "q10_rh" = 2.1, "npp_flux0" = 50)
sensfun_out <- batch_sensFun(inis = inis,vars =  vars, pars = pars)

# Average sensitivity to over the course of the historical period
sensfun_out %>% 
  filter(variable == GLOBAL_TAS()) %>% 
  filter(year %in% 1880:2021) %>% 
  group_by(parameter, variable) %>% 
  summarise(mean = mean(value)) 

sensfun_out %>% 
  filter(variable == ATMOSPHERIC_CO2()) %>% 
  filter(year %in% 1959:2021) %>% 
  group_by(parameter, variable) %>% 
  summarise(mean = mean(value)) 

sensfun_out %>% 
  filter(variable == "GMST") %>% 
  filter(year %in% 1959:2021) %>% 
  group_by(parameter, variable) %>% 
  summarise(mean = mean(value)) 


# Make sure all the parameters can affect historical temperature and [CO2]
sensfun_out %>% 
  filter(variable == GLOBAL_TAS()) %>% 
  filter(year %in% 1880:2021) %>% 
  ggplot(aes(year, value, color = parameter)) + 
  geom_line() + 
  facet_wrap("scenario") + 
  coord_cartesian(ylim = c(-5, 5)) + 
  labs(title = GLOBAL_TAS())

sensfun_out %>% 
  filter(variable == ATMOSPHERIC_CO2()) %>% 
  filter(year %in% 1959:2021) %>% 
  ggplot(aes(year, value, color = parameter)) + 
  geom_line() + 
  facet_wrap("scenario") + 
  labs(title = ATMOSPHERIC_CO2())

sensfun_out %>% 
  filter(variable == HEAT_FLUX()) %>% 
  filter(year %in% 1959:2021) %>% 
  ggplot(aes(year, value, color = parameter)) + 
  geom_line() + 
  facet_wrap("scenario") + 
  labs(title = HEAT_FLUX())

sensfun_out %>% 
  filter(variable == "GMST") %>% 
  filter(year %in% 1959:2021) %>% 
  ggplot(aes(year, value, color = parameter)) + 
  geom_line() + 
  facet_wrap("scenario") + 
  labs(title = "GMST")

sensfun_out %>% 
  filter(variable == GLOBAL_TAS()) %>% 
  filter(year > 2021) %>% 
  ggplot(aes(year, value, color = parameter)) + 
  geom_line() + 
  facet_wrap("scenario") + 
  labs(title = GLOBAL_TAS())

sensfun_out %>% 
  filter(variable == ATMOSPHERIC_CO2()) %>% 
  filter(year > 2021) %>% 
  ggplot(aes(year, value, color = parameter)) + 
  geom_line() + 
  facet_wrap("scenario", scales = "free") + 
  labs(title = ATMOSPHERIC_CO2())

sensfun_out %>% 
  filter(variable == "GMST") %>% 
  filter(year > 2020 ) %>% 
  ggplot(aes(year, value, color = parameter)) + 
  geom_line() + 
  facet_wrap("scenario") + 
  labs(title = "GMST")

sensfun_out %>% 
  filter(variable == HEAT_FLUX()) %>% 
  filter(year > 2020 ) %>% 
  ggplot(aes(year, value, color = parameter)) + 
  geom_line() + 
  facet_wrap("scenario", scales = "free") + 
  labs(title = HEAT_FLUX()) 

# sensRange ---------------------------------------------------------------------------------------------------------

# Standard deviations are based on Leeya's research and code
# https://github.com/JGCRI/trackingC/blob/main/trackingC_setup.Rmd
# The AR6 assessed best estimate is 3°C with a likely range of 2.5°C to 4°C (high confidence)
# The other things came from Leeya's stuff 
parRange <- data.frame(param = c(ECS(), DIFFUSIVITY(), AERO_SCALE(), VOLCANIC_SCALE(), BETA(), Q10_RH(), NPP_FLUX0()), 
                       min = c(2.4, 2.3 - 0.23, 0, 0, 0.36-0.2, 2-0.6, 50.0-14), 
                       max = c(4, 2.3 + 0.23, 1, 1, 0.36+0.2, 2+0.6, 50.0+14))

row.names(parRange) <- parRange$param
parRange$param <- NULL

sensrange_out <- batch_sensRange(inis = inis, vars = vars, parRange = parRange, n = 20)

sensrange_out %>% 
  ggplot() + 
  geom_ribbon(aes(year, ymin = q05, ymax = q95, fill = scenario), alpha = 0.5) + 
  geom_line(aes(year, Mean, color = scenario)) + 
  facet_wrap("variable", scales = "free")

sensrange_out %>% 
  ggplot() + 
  geom_ribbon(aes(year, ymin = Mean - Sd, ymax = Mean + Sd, fill = scenario), alpha = 0.5) + 
  geom_line(aes(year, Mean, color = scenario)) + 
  facet_wrap("variable", scales = "free")

# 2. Calibration -------------------------------------------------------------------------------------------------
# Read in the comparison data 
# Mauna Loa CO2 - downloaded 2022-05-05 from https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.csv
read_csv(file.path(DATADIR, "co2_annmean_mlo.csv"), col_types = "ddd", comment = "#") %>% 
  rename(value = mean) %>% 
  mutate(which = "Mauna Loa") ->
  OBSERVED_CO2
OBSERVED_CO2$unc <- NULL
OBSERVED_CO2$which <- NULL
names(OBSERVED_CO2) <- c("time", CONCENTRATIONS_CO2())


# NASA GISS temperature - downloaded 2020-05-06 from https://climate.nasa.gov/vital-signs/global-temperature/
OBSERVED_TAS <- read_table(file.path(DATADIR, "giss_temperature.txt"), skip = 5,
                           col_names = c("year", "value"),
                           col_types = "dd-")
OBSERVED_TAS$which <- "NASA GISS"
OBSERVED_TAS$which <- NULL

names(OBSERVED_TAS) <- c("time", "GMST")

OBS <- full_join(OBSERVED_CO2, OBSERVED_TAS, on = "time")  %>% 
  as.data.frame


get_fitted_resullts <- function(pars, vars = GLOBAL_TAS()){
  
  core <- newcore(system.file("input/hector_ssp245.ini", package = "hector"))
  setvar(core, dates = NA, var = ECS(), values = 3.0, unit = getunits(ECS()))
  setvar(core, dates = NA, var = Q10_RH(), values = 2.1, unit = getunits(Q10_RH()))
  
  # Set the parameters 
  mapply(function(val, var){
    setvar(core, dates = NA, var = var, values = val, unit = getunits(var))
  },val = pars, var = names(pars), USE.NAMES = FALSE)
  reset(core)
  
  # Run Hector
  run(core, runtodate = 2300)
  
  # Calculate the GMST to compare with NASA GISS results normalize the Hector output 
  # to make the observation reference period. 
  gmst <- get_gmst(fetchvars(core, 1750:2300, vars = c(LAND_TAS(), SST())))
  
  gmst %>% 
    filter(year %in% 1951:1980) %>%  
    pull(value) %>% 
    mean -> 
    ref_value
  
  gmst$value <- gmst$value - ref_value
  
  # Extract the CO2 data
  co2  <- fetchvars(core, 1750:2300, vars = ATMOSPHERIC_CO2())
  
 other_out <- fetchvars(core, 1750:2300, vars = vars)
 
 rbind(co2, other_out, gmst)
  
}






hector_modCost <- function(pars, obs = OBS){
  
  
  hector_rslts <- tryCatch({
    
    core <- newcore(system.file("input/hector_ssp119.ini", package = "hector"))
    setvar(core, dates = NA, var = ECS(), values = 3.0, unit = getunits(ECS()))
    setvar(core, dates = NA, var = Q10_RH(), values = 2.1, unit = getunits(Q10_RH()))
    
    
    # Set the parameters 
    mapply(function(val, var){
      setvar(core, dates = NA, var = var, values = val, unit = getunits(var))
    },val = pars, var = names(pars), USE.NAMES = FALSE)
    reset(core)
    
    # Run Hector
    run(core, runtodate = 2300)
    
    # Calculate the GMST to compare with NASA GISS results normalize the Hector output 
    # to make the observation reference period. 
    gmst <- get_gmst(fetchvars(core, 1750:2300, vars = c(LAND_TAS(), SST())))
    
    gmst %>% 
      filter(year %in% 1951:1980) %>%  
      pull(value) %>% 
      mean -> 
      ref_value
    
    gmst$value <- gmst$value - ref_value
    
    # Extract the CO2 data
    co2  <- fetchvars(core, 1750:2300, vars = CONCENTRATIONS_CO2())
    
    # Format the Hector results to be consistent with the observations.
    rbind(gmst, co2) %>% 
      select(time = year, variable, value) %>% 
      spread(variable, value) %>% 
      filter(time %in% OBS$time) -> 
      x 
    
    x
    
  }, error=function(e){
    x <- obs
    x$atmos_co2 <- 9999
    x$GMST <- 9999
    x
  })
  
  out <- FME::modCost(hector_rslts, obs)
  
  return(out)
  
}




#unconstrained <- modFit(hector_modCost, pars)
pars <- c("S"=2.7, "diff"=2.4, "alpha"=0.5, "volscl"=1.0, "beta" = 2, "q10_rh" = 2.1, "npp_flux0" = 50)

lower_bound <- c("S" = 2.40, "diff" = 2.07, "alpha" = 0.00, "volscl" = 0.00, "beta" = 0, 
                 "q10_rh" = 1.40 , "npp_flux0" = 36.00)
upper_bound <- c("S" = 4.00, "diff" = 2.53, "alpha" = 1.00, "volscl" = 1.00, "beta" = 3, 
                 "q10_rh" = 2.60, "npp_flux0" = 64.00)

fit1 <- modFit(f = hector_modCost, p = c("diff"=2.4, "beta" = 0.36, "alpha"=0.5), 
               lower = c( 0, 0, 0), 
               upper = c( 10, 1, 1))

fit2 <- modFit(f = hector_modCost, p = c("diff"=2.4, "beta" = 0.36, "q10_rh" = 2, "alpha"=0.5), 
               lower = c( 0, 0, 0, 0), 
               upper = c( 10, 1, 3, 1))


out1 <- get_fitted_resullts(fit1$par, vars = c(CONCENTRATIONS_CO2(), SST(), GLOBAL_TAS(), NPP())) %>% 
  filter(year > 1900)

out2 <- get_fitted_resullts(fit1$par, vars = c(CONCENTRATIONS_CO2(), SST(), GLOBAL_TAS(), NPP())) %>% 
  filter(year > 1900)

obs_copy <- OBS
names(obs_copy)[1] <- c("year")
long_obs <- pivot_longer(obs_copy, cols = c(CONCENTRATIONS_CO2(),"GMST"), names_to = "variable")


ggplot() + 
  geom_line(data = out1, aes(year, value, color = "out1")) + 
  geom_line(data = out2, aes(year, value, color = "out2")) + 
  
  geom_line(data = long_obs, aes(year, value, color = "data")) + 
  facet_wrap("variable", scales = "free")
