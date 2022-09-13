# Sensitivity analysis for each of Hector's components. 
#   1. What parameters have the largest global influence? 
#   2. What variable should we be checking?  
#   3. Are some of these interactions scenario dependent? 



# 0. Set Up -----------------------------------------------------------------------------------------------
# Load the required packages. 
library(assertthat)
library(dplyr)
library(FME)
library(GGally)
library(ggplot2)
library(magrittr)
library(tidyr)

# Load Hector! 
#library(hector)
devtools::load_all("/Users/dorh012/Documents/Hector-Versions/Xcode/hector")

# The location where to save results to. 
BASE_DIR <- here::here()
OUTDIR <- file.path(BASE_DIR, "sens_output")
dir.create(OUTDIR, showWarnings = FALSE)

n <- 10
theme_set(theme_bw())

# Define helper functions ---


make_runHector <- function(ini, vars){
  
  fx <- function(pars){
    core <- newcore(system.file(ini, package = "hector"))
    
    # Set the variables 
    mapply(function(val, var){
      setvar(core, dates = NA, var = var, values = val, unit = getunits(var))
    },val = pars, var = names(pars), USE.NAMES = FALSE)
    reset(core)
    run(core)
    
    fetchvars(core, 1750:2300, vars = vars) %>% 
      # Format the data frame into the sensRange function.
      select(time = year, variable, value) %>% 
      spread(variable, value) -> 
      out 
    
    shutdown(core)
    return(out)
    
  }
  
  return(fx)
}



# Wrapper function for running Hector with the FME sensRange & sensFun functions 
# Args 
#   pars: vector of the parameters to test 
#   ini: str ini path 
#   vars: str vector of the Hector variables to look at, c(GLOBAL_TAS(), NPP(), CONCENTRATIONS_CO2(), SST())
# Returns dataframe formatted to work with FME
run_hector <- function(pars, ini = "input/hector_ssp245.ini", 
                       vars = c(GLOBAL_TAS(), SST(), OCEAN_TAS(), HEAT_FLUX(), CONCENTRATIONS_CO2(),  NPP())) {
  
  core <- newcore(system.file(ini, package = "hector"))
  
  # Set the variables 
  mapply(function(val, var){
    setvar(core, dates = NA, var = var, values = val, unit = getunits(var))
  },val = pars, var = names(pars), USE.NAMES = FALSE)
  reset(core)
  run(core)
  
  fetchvars(core, 1750:2300, vars = vars) %>% 
    # Format the data frame into the sensRange function.
    select(time = year, variable, value) %>% 
    spread(variable, value) -> 
    out 
  
  shutdown(core)
  return(out)
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


custom_ribbon_plot <- function(data, var = NULL){
  
  if(is.null(var)){
    var <- unique(data$variable)
  }
  
  data %>% 
    filter(variable %in% var) %>% 
    ggplot() + 
    geom_ribbon(aes(year, ymin = Min, ymax = Max, fill = "min/max"), alpha = 0.5) +
    geom_ribbon(aes(year, ymin = q25, ymax = q75, fill = "q25/q75"), alpha = 0.5) +
    geom_line(aes(year, Mean, color = "mean")) + 
    scale_color_manual(values = c("mean" = "black")) + 
    scale_fill_manual(values = c("min/max" =  "grey", "q25/q75" = "#E69F00")) + 
    theme(legend.title = element_blank()) + 
    labs(y = NULL, x = NULL)
  
}


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


# 1. Ocean (thermal portion only) -----------------------------------------------------------------------------------------------
# No pair wise plots for this I was surprised by how much the CO2 concentrations changed 
# especially compared to the ocean parameters. 

# Because these values are so large will keep the fraction used to adjust the values somewhat small. 
ocean_pars <- c("tt" = 72000000, "tu" = 49000000, "twi" = 12500000, "tid" = 200000000)
ocean_par_range <- make_parRanges(ocean_pars, 0.85)

# The variables to plot and test 
ocean_vars <- c(GLOBAL_TAS(), SST(), OCEAN_TAS(), HEAT_FLUX(), CONCENTRATIONS_CO2())


# Global sensitivity analysis 
ocean_thermal_sens_ssp119 <- format_sensRange(sensRange(func = run_hector, parms = ocean_pars, dist = "latin",
                                                        parRange = ocean_par_range, ini = "input/hector_ssp119.ini",
                                                        num = n, vars = ocean_vars, sensvar = ocean_vars))

ocean_thermal_sens_ssp245 <- format_sensRange(sensRange(func = run_hector, parms = ocean_pars, dist = "latin",
                                                        parRange = ocean_par_range, 
                                                        num = n, vars = ocean_vars, sensvar = ocean_vars))


ocean_thermal_sens_ssp585 <- format_sensRange(sensRange(func = run_hector, parms = ocean_pars, dist = "latin",
                                                        parRange = ocean_par_range, 
                                                        num = n, vars = ocean_vars, sensvar = ocean_vars, 
                                                        ini = "input/hector_ssp585.ini"))
ocean_thermal_sens_ssp245$scenario <- "ssp245"
ocean_thermal_sens_ssp119$scenario <- "ssp119"
ocean_thermal_sens_ssp585$scenario <- "ssp585"

# Local sensitivity analysis 
ocean_sensFun_ssp245 <-sensFun(func = run_hector,  parms = ocean_pars, sensvar = ocean_vars, parscale  = 1)  
ocean_sensFun_ssp119 <-sensFun(func = run_hector, parms = ocean_pars, 
                               ini  = "input/hector_ssp119.ini", sensvar = ocean_vars,  parscale = 1)  
ocean_sensFun_ssp585 <-sensFun(func = run_hector, parms = ocean_pars, 
                               ini  = "input/hector_ssp585.ini", sensvar = ocean_vars,  parscale = 1)  

ocean_sensFun_ssp119 <- format_sensFun(ocean_sensFun_ssp119)
ocean_sensFun_ssp245 <- format_sensFun(ocean_sensFun_ssp245)
ocean_sensFun_ssp585 <- format_sensFun(ocean_sensFun_ssp585)

ocean_sensFun_ssp119$scenario <- "ssp119"
ocean_sensFun_ssp245$scenario <- "ssp245"
ocean_sensFun_ssp585$scenario <- "ssp585"

# Ocean thermal plots 
bind_rows(ocean_sensFun_ssp119, ocean_sensFun_ssp245, ocean_sensFun_ssp585) %>% 
  filter(year > 2000) %>% 
  ggplot(aes(year, value, color = parameter, linetype = scenario)) + 
  geom_line() +  
  facet_wrap("variable", scales = "free") 

bind_rows(ocean_thermal_sens_ssp119, 
          ocean_thermal_sens_ssp245, 
          ocean_thermal_sens_ssp585) %>%  
  ggplot() + 
  geom_line(aes(year, Mean, color = scenario)) + 
  geom_ribbon(aes(year, ymin = Min, ymax = Max, fill = scenario), alpha = 0.5) + 
  facet_wrap("variable", scales = "free")


# 2. simpleNbox  -----------------------------------------------------------------------------------------------
Ccycle_pars <- c("f_nppv" = 0.35, "f_nppd" = 0.60, "f_litterd" = 0.98, "f_lucv" = 0.1,
                 "f_lucd" = 0.01, "beta" = 0.16, "q10_rh" = 2.1, "C0" = 277.15, "veg_c" = 550,
                 "detritus_c" = 55, "soil_c" = 1782, "npp_flux0" = 55.9) 

Ccycle_vars <- c(GLOBAL_TAS(), LAND_TAS(), RH(), NPP(), CONCENTRATIONS_CO2(), EARTH_C(), 
                 VEG_C(), DETRITUS_C(), SOIL_C())

local_Ccycle <- batch_sensFun(inis = c("input/hector_ssp119.ini", 
                                       "input/hector_ssp245.ini",
                                       "input/hector_ssp585.ini"), 
                              vars = Ccycle_vars, pars = Ccycle_pars)

local_Ccycle %>% 
  filter(variable == GLOBAL_TAS()) %>% 
  filter(year > 2000) %>% 
  ggplot(aes(year, value, color = parameter, linetype = scenario)) + 
  geom_line() + 
  facet_wrap("scenario")

local_Ccycle %>% 
  filter(variable == CONCENTRATIONS_CO2()) %>% 
  #filter(year > 2000) %>% 
  ggplot(aes(year, value, color = parameter, linetype = scenario)) + 
  geom_line() + 
  facet_wrap("scenario")

# 3. Forcing ----------------------------------------------------------------------------------------------- 
forcing_pars <- c("delta_co2" = 0.05, "delta_ch4" = -.14, "delta_n2o" = 0.07, "rho_bc" = 0.0508, 
                  "rho_oc"=-.00621, "rho_so2"=-.00000724, "rho_nh3"=-.00208)

forcing_vars <- c(RF_TOTAL(), GLOBAL_TAS(), CONCENTRATIONS_CO2())

local_forcing <- batch_sensFun(inis = c("input/hector_ssp119.ini", 
                                       "input/hector_ssp245.ini",
                                       "input/hector_ssp585.ini"),  
                               vars = forcing_vars, pars = forcing_pars)

local_forcing %>% 
  filter(variable == RF_TOTAL()) %>% 
  filter(year < 2000) %>% 
  ggplot(aes(year, value, color = parameter)) + 
  geom_line() + 
  facet_wrap("scenario") + 
  coord_cartesian(ylim = c(-1, 1))


# 4. Temperature -----------------------------------------------------------------------------------------------
temp_pars <- c("S"=2.7, "diff"=2.4, "alpha"=0.5, "volscl"=1.0, "qco2"=3.75)
temp_vars <- c(GLOBAL_TAS(), CONCENTRATIONS_CO2(), SST(), LAND_TAS(), NPP())   

local_temp <- batch_sensFun(inis = c("input/hector_ssp119.ini", 
                                        "input/hector_ssp245.ini",
                                        "input/hector_ssp585.ini"),  
                               vars = temp_vars, pars = temp_pars)

local_temp %>% 
  filter(parameter != VOLCANIC_SCALE()) %>% 
 # filter(variable == CONCENTRATIONS_CO2()) %>% 
  filter(year > 2000) %>% 
  ggplot(aes(year, value, color = parameter)) + 
  geom_line() + 
  facet_grid(variable~scenario, scales = "free") + 
  #coord_cartesian(ylim = c(-10, 10)) + 
  NULL


# 5. Fit combinations -----------------------------------------------
# Test all the parameters that are used in the calibration fits. 
fit_ps <- c(0.36, 2.1, 3, 55, 1, 1, 2.5)
names(fit_ps) <- c(BETA(), Q10_RH(), ECS(), NPP_FLUX0(), AERO_SCALE(), 
                   VOLCANIC_SCALE(), DIFFUSIVITY())
vars <- c(GLOBAL_TAS(), NPP(), CONCENTRATIONS_CO2())

out <- batch_sensFun(inis = c("input/hector_ssp119.ini", 
                              "input/hector_ssp245.ini",
                              "input/hector_ssp585.ini"), 
                     vars = vars, pars = fit_ps)




# 6. Does ECS affect sensitivity? ------------------------------------

# Wrapper function for running Hector with the FME sensRange & 
# sensFun functions this is the second variation of the function, where the 
# hector core is actually passed in as an argument. This means that users 
# can set a parameter manually. 
# Args 
#   pars: vector of the parameters to test 
#   core: active hcore object 
#   vars: str vector of the Hector variables to look at, c(GLOBAL_TAS(), NPP(), CONCENTRATIONS_CO2(), SST())
# Returns dataframe formatted to work with FME
run_hector2 <- function(pars, core, 
                       vars = c(GLOBAL_TAS(), SST(), OCEAN_TAS(), 
                                HEAT_FLUX(), CONCENTRATIONS_CO2(),  NPP())) {

  # Set the variables 
  mapply(function(val, var){
    setvar(core, dates = NA, var = var, values = val, unit = getunits(var))
  },val = pars, var = names(pars), USE.NAMES = FALSE)
  reset(core)
  run(core)
  
  fetchvars(core, 1750:2300, vars = vars) %>% 
    # Format the data frame into the sensRange function.
    select(time = year, variable, value) %>% 
    spread(variable, value) -> 
    out 
  
  shutdown(core)
  return(out)
}


# Lower end of ECS ranges
ini_path <- system.file("input/hector_ssp245.ini", package =  "hector") 
hcore <- newcore(ini_path)
