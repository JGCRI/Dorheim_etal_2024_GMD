# Objective: Calibrate Hector v3 for the manuscript. We will use historical observations for atmospheric CO2 
# concentrations, global mean surface temperature, and the land sink to find the best fit values for 
# diff, beta, and Q10. ECS, alpha, and volscl are all fixed values. 

# 0. Set Up ---------------------------------------------------------------------------------------
library(assertthat)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
theme_set(theme_bw())

# TODO update this to the v3 branch after the luc branch is merged in 
devtools::install_github("jgcri/hector@luc")
library(hector)

BASE_DIR <- here::here()

# 0A. Helper functions --------------------------------------------------------------------------
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


# 0B. Load & Format Comparison Data  --------------------------------------------------------------------------------
# GISTEMP Analysis (the GISS Surface Temperature Analysis)
# For the GISS analysis, normal always means the average over 
# the 30-year period 1951-1980 for that place and time of year.
# This base period is specific to GISS, not universal. 
# 
# GISTEMP Team, 2022: GISS Surface Temperature Analysis (GISTEMP), version 4.
# NASA Goddard Institute for Space Studies. Dataset accessed 20YY-MM-DD at https://data.giss.nasa.gov/gistemp/.
#
# Lenssen, N., G. Schmidt, J. Hansen, M. Menne, A. Persin, R. Ruedy, and D. Zyss, 2019: 
# Improvements in the GISTEMP uncertainty model. J. Geophys. Res. Atmos., 124, no. 12, 
# 6307-6326, doi:10.1029/2018JD029522.
read.csv(file.path(BASE_DIR, "data", "calibration", "GISTEMP_v4_20221014.csv"), skip = 1) %>% 
  select(year = Year, value = `J.D`) %>% 
  mutate(value = as.numeric(value)) %>% 
  na.omit() %>% 
  mutate(variable = "gmst") -> 
  gmst_obs_data

# Atmospheric CO2 concentrations from the NOAA Global Monitoring Laboratory
# Dr. Pieter Tans, NOAA/GML (gml.noaa.gov/ccgg/trends/) and Dr. Ralph Keeling, Scripps Institution of Oceanography (scrippsco2.ucsd.edu/).
read.csv(file.path(BASE_DIR, "data", "calibration", "co2_annmean_mlo.csv"), comment.char = "#") %>% 
  select(year, value = mean) %>% 
  mutate(variable = CONCENTRATIONS_CO2()) -> 
  co2_obs_data

# Friedlingstein, Pierre, O'Sullivan, Michael, Jones, Matthew W., Andrew, Robbie M., Hauck, Judith, Olsen,
# Are, Peters, Glen P., Peters, Wouter, Pongratz, Julia, Sitch, Stephen, Le Quéré, Corinne. et al. 
# Carbon Budget 2021, Earth Syst. Sci. Data, 2022. https://doi.org/10.5194/essd-14-1917-2022
read_xlsx(file.path(BASE_DIR, "data", "calibration", "Global_Carbon_Budget_2021v1.0.xlsx"), 
          sheet = "Historical Budget", skip = 15) %>% 
  select(year = Year, value = `land sink`) %>% 
  mutate(variable = "land sink") -> 
  land_obs_data


# 1. Calibrate Hector -----------------------------------------------------------------------------
# Calculate the MSE between Hector output and observations, this function will be used by optim to 
# minimze the MSE ()
# Args 
#   p: vector of Hector parameter values to calibrate, must be named 
#   gmst_obs: data frame of the gmst observations from NASA GISS
#   co2_obs: data frame of the CO2 concentrations NOAA 
#   land_obs: data frame of the land sink Global Carbon Project
get_mse <- function(p, gmst_obs, co2_obs, land_obs){
  

  # Prevent an error from crashing the fit, if for some reason (randomly generated parameter value results in an error)
  # return a large MSE value. 
  out <- tryCatch({
    
    # Make sure that the two boundary scenarios run, otherwise there could be issues if the parameter 
    # combination results in error conditions in the future. 
    hc <- prep_core("rcmip_ssp119.ini")
    set_params(hc, p)
    reset(hc)
    run(hc)
    
    hc <- prep_core("rcmip_ssp585.ini")
    set_params(hc, p)
    reset(hc)
    run(hc)
    
    # Fetch the gmst results from the Hector core & calculate the normalized MSE. 
    hector_gmst <- normalize_to_giss(fetch_gmst(hc))
    hector_gmst <- hector_gmst[hector_gmst$year %in% gmst_obs$year, ]
    MSE_gmst <- mean((hector_gmst$value - gmst_obs$value)^2)
    
    # Fetch the co2 results from the Hector core & calculate the normalized MSE. 
    hector_co2 <- fetchvars(hc, dates = co2_obs$year, vars = CONCENTRATIONS_CO2())
    MSE_co2 <-mean((hector_co2$value - co2_obs$value)^2)
    
    # Calculate the size of Hector's land sink & calculate the normalized MSE. 
    fetchvars(hc, dates = land_obs$year, vars = c(NBP(), LUC_EMISSIONS())) %>%  
      spread(variable, value) %>%
      mutate(value = NBP + luc_emissions) ->
      hector_land
    MSE_land <- mean((hector_land$value - land_obs$value)^2)
    
    # The goodness of fit is the mean of the normalized MSEs 
    MSEs <- c(MSE_gmst, MSE_co2, MSE_land)
    
    return(mean(MSEs))
    
  }, error = function(e){
    # If an error was thrown return a large value
    return(9999)
  })
  
  return(out)
  
}

params <- c("diff" = 1, "q10_rh" = 2.2, "beta" = 0.23)
fit <- optim(get_mse, p = params, gmst_obs = gmst_obs_data,
             co2_obs = co2_obs_data, land_obs = land_obs_data)

save(fit, file = file.path(BASE_DIR, "output", paste0("calibration-fit-", gsub(date(), pattern = " ", replacement = "_"), ".rda")))

# # Variations of Hector Calibration  -----------------------------------------------------------------------------
# # No Land Sink Constraint -----------------------------------------------------------------------------
# 
# # Calculate the MSE between Hector output and observations, this function will be used by optim to 
# # minimze the MSE ()
# # Args 
# #   p: vector of Hector parameter values to calibrate, must be named 
# #   gmst_obs: data frame of the gmst observations from NASA GISS
# #   co2_obs: data frame of the CO2 concentrations NOAA 
# get_mse <- function(p, gmst_obs, co2_obs){
#   
#   
#   # Prevent an error from crashing the fit, if for some reason (randomly generated parameter value results in an error)
#   # return a large MSE value. 
#   out <- tryCatch({
#     
#     # Make sure that the two boundary scenarios run, otherwise there could be issues if the parameter 
#     # combination results in error conditions in the future. 
#     hc <- prep_core("rcmip_ssp119.ini")
#     set_params(hc, p)
#     reset(hc)
#     run(hc)
#     
#     hc <- prep_core("rcmip_ssp585.ini")
#     set_params(hc, p)
#     reset(hc)
#     run(hc)
#     
#     # Fetch the gmst results from the Hector core & calculate the normalized MSE. 
#     hector_gmst <- normalize_to_giss(fetch_gmst(hc))
#     hector_gmst <- hector_gmst[hector_gmst$year %in% gmst_obs$year, ]
#     MSE_gmst <- mean((hector_gmst$value - gmst_obs$value)^2/abs(hector_gmst$value))
#     
#     # Fetch the co2 results from the Hector core & calculate the normalized MSE. 
#     hector_co2 <- fetchvars(hc, dates = co2_obs$year, vars = CONCENTRATIONS_CO2())
#     MSE_co2 <-mean((hector_co2$value - co2_obs$value)^2/abs(hector_co2$value))
#     
#     # The goodness of fit is the mean of the normalized MSEs 
#     MSEs <- c(MSE_gmst, MSE_co2)
#     
#     return(mean(MSEs))
#     
#   }, error = function(e){
#     # If an error was thrown return a large value
#     return(9999)
#   })
#   
#   return(out)
#   
# }
# 
# params <- c("diff" = 1, "q10_rh" = 2.2, "beta" = 0.23)
# fit <- optim(get_mse, p = params, gmst_obs = gmst_obs_data, co2_obs = co2_obs_data)
# save(fit, file = file.path(BASE_DIR, "output", paste0("no_land_sink-calibration-fit-", gsub(date(), pattern = " ", replacement = "_"), ".rda")))
# 
# 
# 
# # CO2 Concentrations Double weighted   -----------------------------------------------------------------------------
# 
# # Calculate the MSE between Hector output and observations, this function will be used by optim to 
# # minimze the MSE ()
# # Args 
# #   p: vector of Hector parameter values to calibrate, must be named 
# #   gmst_obs: data frame of the gmst observations from NASA GISS
# #   co2_obs: data frame of the CO2 concentrations NOAA 
# #   land_obs: data frame of the land sink Global Carbon Project
# get_mse <- function(p, gmst_obs, co2_obs, land_obs){
#   
#   
#   # Prevent an error from crashing the fit, if for some reason (randomly generated parameter value results in an error)
#   # return a large MSE value. 
#   out <- tryCatch({
#     
#     # Make sure that the two boundary scenarios run, otherwise there could be issues if the parameter 
#     # combination results in error conditions in the future. 
#     hc <- prep_core("rcmip_ssp119.ini")
#     set_params(hc, p)
#     reset(hc)
#     run(hc)
#     
#     hc <- prep_core("rcmip_ssp585.ini")
#     set_params(hc, p)
#     reset(hc)
#     run(hc)
#     
#     # Fetch the gmst results from the Hector core & calculate the normalized MSE. 
#     hector_gmst <- normalize_to_giss(fetch_gmst(hc))
#     hector_gmst <- hector_gmst[hector_gmst$year %in% gmst_obs$year, ]
#     MSE_gmst <- mean((hector_gmst$value - gmst_obs$value)^2/abs(hector_gmst$value))
#     
#     # Fetch the co2 results from the Hector core & calculate the normalized MSE. 
#     hector_co2 <- fetchvars(hc, dates = co2_obs$year, vars = CONCENTRATIONS_CO2())
#     MSE_co2 <-mean((hector_co2$value - co2_obs$value)^2/abs(hector_co2$value))
#     
#     # Calculate the size of Hector's land sink & calculate the normalized MSE. 
#     fetchvars(hc, dates = land_obs$year, vars = c(NBP(), LUC_EMISSIONS())) %>%  
#       spread(variable, value) %>%
#       mutate(value = NBP + luc_emissions) ->
#       hector_land
#     MSE_land <- mean((hector_land$value - land_obs$value)^2 /abs(hector_land$value))
#     
#     # The goodness of fit is the mean of the normalized MSEs 
#     MSE <- weighted.mean(c(MSE_gmst, MSE_co2, MSE_land), c(1, 2, 1))
#     return(MSE)
#     
#   }, error = function(e){
#     # If an error was thrown return a large value
#     return(9999)
#   })
#   
#   return(out)
#   
# }
# 
# params <- c("diff" = 1, "q10_rh" = 2.2, "beta" = 0.23)
# fit <- optim(get_mse, p = params, gmst_obs = gmst_obs_data,
#              co2_obs = co2_obs_data, land_obs = land_obs_data)
# 
# save(fit, file = file.path(BASE_DIR, "output", paste0("co2_mse_weighted-calibration-fit-", gsub(date(), pattern = " ", replacement = "_"), ".rda")))
# 
# 
# # MSE not percent diff  -----------------------------------------------------------------------------
# 
# # Calculate the MSE between Hector output and observations, this function will be used by optim to 
# # minimze the MSE ()
# # Args 
# #   p: vector of Hector parameter values to calibrate, must be named 
# #   gmst_obs: data frame of the gmst observations from NASA GISS
# #   co2_obs: data frame of the CO2 concentrations NOAA 
# #   land_obs: data frame of the land sink Global Carbon Project
# get_mse <- function(p, gmst_obs, co2_obs, land_obs){
#   
#   
#   # Prevent an error from crashing the fit, if for some reason (randomly generated parameter value results in an error)
#   # return a large MSE value. 
#   out <- tryCatch({
#     
#     # Make sure that the two boundary scenarios run, otherwise there could be issues if the parameter 
#     # combination results in error conditions in the future. 
#     hc <- prep_core("rcmip_ssp119.ini")
#     set_params(hc, p)
#     reset(hc)
#     run(hc)
#     
#     hc <- prep_core("rcmip_ssp585.ini")
#     set_params(hc, p)
#     reset(hc)
#     run(hc)
#     
#     # Fetch the gmst results from the Hector core & calculate the normalized MSE. 
#     hector_gmst <- normalize_to_giss(fetch_gmst(hc))
#     hector_gmst <- hector_gmst[hector_gmst$year %in% gmst_obs$year, ]
#     MSE_gmst <- mean((hector_gmst$value - gmst_obs$value)^2)
#     
#     # Fetch the co2 results from the Hector core & calculate the normalized MSE. 
#     hector_co2 <- fetchvars(hc, dates = co2_obs$year, vars = CONCENTRATIONS_CO2())
#     MSE_co2 <-mean((hector_co2$value - co2_obs$value)^2)
#     
#     # Calculate the size of Hector's land sink & calculate the normalized MSE. 
#     fetchvars(hc, dates = land_obs$year, vars = c(NBP(), LUC_EMISSIONS())) %>%  
#       spread(variable, value) %>%
#       mutate(value = NBP + luc_emissions) ->
#       hector_land
#     MSE_land <- mean((hector_land$value - land_obs$value)^2)
#     
#     # The goodness of fit is the mean of the normalized MSEs 
#     MSE <- weighted.mean(c(MSE_gmst, MSE_co2, MSE_land), c(1, 1, 1))
#     return(MSE)
#     
#   }, error = function(e){
#     # If an error was thrown return a large value
#     return(9999)
#   })
#   
#   return(out)
#   
# }
# 
# params <- c("diff" = 1, "q10_rh" = 2.2, "beta" = 0.23)
# fit <- optim(get_mse, p = params, gmst_obs = gmst_obs_data,
#              co2_obs = co2_obs_data, land_obs = land_obs_data)
# 
# save(fit, file = file.path(BASE_DIR, "output", paste0("not-percent-", gsub(date(), pattern = " ", replacement = "_"), ".rda")))
# 
# 

# 2. Quick Compare Fit Results with Obs -----------------------------------------------------------------------------
# Set up, run, and fectch Hector results. 
hc <- prep_core(filename = "hector_ssp119.ini")
set_params(core = hc, p = fit$par)
run(hc)

normalize_to_giss(fetch_gmst(hc)) %>% 
  filter(year %in% gmst_obs_data$year) -> 
  hector_gmst

fetchvars(hc, dates = 1850:2100, vars = CONCENTRATIONS_CO2()) -> 
  hector_co22 

fetchvars(hc, dates = land_obs_data$year, vars = c(NBP(), LUC_EMISSIONS())) %>%  
  spread(variable, value) %>%
  mutate(value = NBP + luc_emissions) ->
  hector_land

ggplot() + 
  geom_line(data = hector_gmst, aes(year, value, color = "hector v3")) + 
  geom_line(data = gmst_obs_data, aes(year, value, color = "NASA GISS")) + 
  labs(title = "Global Mean Surface Temperature", x = NULL)


file.path(BASE_DIR, "data", "hector_2.2.0_rcp.csv") %>% 
  read.csv() %>% 
  filter(variable == CONCENTRATIONS_CO2() & scenario == "rcp26") -> 
  old_hector

ggplot() + 
  geom_line(data = hector_co2, aes(year, value, color = "hector v3")) + 
  geom_line(data = hector_co22, aes(year, value, color = "hector v3 og")) + 
  geom_line(data = co2_obs_data, aes(year, value, color = "NOAA")) + 
  #geom_line(data = old_hector, aes(year, value, color = "hector v2")) +
  labs(title = "CO2 Concentrations", x = NULL)

ggplot() + 
  geom_line(data = hector_land, aes(year, value, color = "hector v3")) + 
  geom_line(data = land_obs_data, aes(year, value, color = "GCB"), alpha = 0.4) + 
  labs(title = "Land Sink", x = NULL)
