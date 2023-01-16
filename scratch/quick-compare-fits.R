# Objective: a quick comparison of different Hector fits, this is a 
# scratch script with helpful functions and figures. It is not part of 
# the reproducible pipeline. 
# 0. Set Up --------------------------------------------------------------------
devtools::install_github("jgcri/hector@luc")
library(hector)
library(dplyr)
library(ggplot2)
library(assertthat)


# Read parameter values into an active Hector core 
# Args 
#   core: active hector core 
#   p: vector of named hector parameter values  
# Return active hector core with the set parameter values 
set_params <- function(core, p){
  
  assert_that(all(is.character(names(p))), msg = "unamed parameter")
  
  mapply(function(pval, pnames, punits){
    setvar(core, dates = NA, values = pval, var = pnames, unit = punits)
  }, pval = p, pnames = names(p), punits = getunits(names(p)))
  
  reset(core)
  
  return(core)
  
}

# Get gmst results from a Hector tun 
# Args 
#   core: active hector core
# Return data frame of the global mean surface temperature 
fetch_gmst <- function(core){
  
  flnd  <- 0.29  
  
  fetchvars(core, dates = 1750:2100, vars = c(LAND_TAS(), SST())) %>% 
    select(year, variable, value) -> 
    output
  
  output <- reshape(output, direction = "wide", idvar = "year", timevar = "variable")
  names(output) <- gsub("value.", "", names(output))
  
  output$value <- with(output, (land_tas * flnd + sst * (1 - flnd)))
  output <- subset(output, select = c(year, value))
  output$variable <- "gmst"
  
  return(output)
}

# Normalize temperature to the historical reference period
# Args 
#   data: data frame of temperature data
# Returns a data frame of temperature data normalized to the 1951:1980 reference period
normalize_to_giss <- function(data){
  
  assert_that(unique(data[["variable"]]) == "gmst")
  
  ref_value <- mean(data[(data$year %in%  1951:1980), ]$value)
  data$value <- data$value - ref_value
  
  return(data)
}


# Set up a Hector core with the correct parameter values, Hector is emission driven
# Arg
#    filename: string name of the ini file 
# Return: a Hector core with the set parameter values alpha = 1, ECS = 3 (IPCC result), volscl = 1, NPP0 = 56.2 (Ito et al)
prep_core <- function(filename){
  # Set up the Hector core, make sure that the ECS is set equal to 3.0 
  
  #ini <- system.file(file.path("input", filename), package = "hector")
  ini <- here::here("input", filename)
  core <- newcore(ini)
  setvar(core, dates = NA, var = ECS(), value = 3.0, unit = getunits(ECS()))
  setvar(core, dates = NA, var = NPP_FLUX0(), value = 56.2, unit = getunits(NPP_FLUX0()))
  setvar(core, dates = NA, var = AERO_SCALE(), value = 1, unit = getunits(AERO_SCALE()))
  setvar(core, dates = NA, var = VOLCANIC_SCALE(), value = 1, unit = getunits(VOLCANIC_SCALE()))
  
  # Reset the core 
  reset(core)
  return(core)
}

# Atmospheric CO2 concentrations from the NOAA Global Monitoring Laboratory
# Dr. Pieter Tans, NOAA/GML (gml.noaa.gov/ccgg/trends/) and Dr. Ralph Keeling, Scripps Institution of Oceanography (scrippsco2.ucsd.edu/).
read.csv(file.path(BASE_DIR, "data", "calibration", "co2_annmean_mlo.csv"), comment.char = "#") %>% 
  select(year, value = mean) %>% 
  mutate(variable = CONCENTRATIONS_CO2()) -> 
  co2_obs_data


fit1 <- c("diff" = 1.3, "q10_rh" = 2.2, "beta"=0.23)
fit2 <- c("diff" = 0.56, "q10_rh" = 1.26, "beta"= 0.24)
fit3 <- c("diff" = 1.42, "q10_rh" = 1.62, "beta"= 0.28)
fit4 <- c("diff" = 0.85, "q10_rh" = 2.7, "beta"= 0.46)
fit5 <- c("diff" = 1.73, "q10_rh" = 0.93, "beta"= 0.29)


core <- prep_core("rcmip_ssp245.ini")
core$name <- "fit1"
set_params(core, fit1)
run(core)
out1 <- fetchvars(core, 1850:2100, vars = c(GLOBAL_TAS(), NBP(), CONCENTRATIONS_CO2()))


core <- prep_core("rcmip_ssp245.ini")
core$name <- "fit2"
set_params(core, fit2)
run(core)
out2 <- fetchvars(core, 1850:2100, vars = c(GLOBAL_TAS(), NBP(), CONCENTRATIONS_CO2()))

core <- prep_core("rcmip_ssp245.ini")
core$name <- "fit3"
set_params(core, fit3)
run(core)
out3 <- fetchvars(core, 1850:2100, vars = c(GLOBAL_TAS(), NBP(), CONCENTRATIONS_CO2()))

core <- prep_core("rcmip_ssp245.ini")
core$name <- "fit4"
set_params(core, fit4)
run(core)
out4 <- fetchvars(core, 1850:2100, vars = c(GLOBAL_TAS(), NBP(), CONCENTRATIONS_CO2()))

core <- prep_core("rcmip_ssp245.ini")
core$name <- "fit5"
set_params(core, fit5)
run(core)
out5 <- fetchvars(core, 1850:2100, vars = c(GLOBAL_TAS(), NBP(), CONCENTRATIONS_CO2()))

out <- rbind(out1, out2, out3, out4, out5) %>% filter(year <= 2020)
ggplot(data = out) + 
  geom_line(aes(year, value, color = scenario)) + 
  facet_wrap("variable", scales = "free")


out %>% 
  filter(variable == CONCENTRATIONS_CO2()) %>% 
  ggplot() + 
  geom_line(aes(year, value, color = scenario)) + 
  geom_line(data = co2_obs_data, aes(year, value))

