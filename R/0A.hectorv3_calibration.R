# Emission driven Hector was underestimating CH4 & N2O RF related to natural CH4 & N2O emissions.
# Update the natural emissions so that the CH4 RF and N2O RF are consistent with AR6 historical values. 

# 0. Set up -------------------------------------------------------------------------------------------
library(assertthat)
library(data.table)
library(dplyr)
library(ggplot2)
remotes::install_github("JGCRI/hector")
library(hector) 
assert_that(packageVersion("hector") ==  "3.1.1")


# 1. Adjust natural CH4 and N2O emissions -------------------------------------------------------------------------------------------
# Here we will calibrate two Hector parameters, natural CH4 and N2O emissions to AR6 Chapter 7 RF.

# Define the helper functions used to format the data 
# Extract information from the file name 
# Args 
#   path: str to the file path name for the AR6/SSPs/ERF_*.csv
# Returns: data.frame of output type, scenario, time, and the quantile percent
format_data <- function(path){
  
  x <- gsub(pattern = ".csv", replacement = "", x = basename(path))
  xx <- unlist(strsplit(x, split = "_"))
  
  if (length(xx) == 3 ){
    out <- as.data.frame(matrix(xx, nrow = 1, dimnames = list(NULL, c("output", "scn", "time"))))
  }
  
  if (length(xx) == 4 ){
    out <- as.data.frame(matrix(xx, nrow = 1, dimnames = list(NULL, c("output", "scn", "time", "percent"))))
  }
  
  return(out)
  
}

# Import and format the AR6 files 
# Args
#   path: str to the file path name for the AR6/SSPs/ERF_*.csv
# Returns: data.frame, long of the ERF for the different forcing agents 
get_ar6_data <- function(path){
  
  dat <- read.csv(path, stringsAsFactors = FALSE)
  info <- format_data(path)
  
  dat %>% 
    melt(id.vars = c( "year"), 
         variable.name = "variable", value.name = "value") %>% 
    cbind(info) -> 
    out
  
  return(out)
}


# Set up the ar6 comparison data, note that this comes from the repo https://github.com/IPCC-WG1/Chapter-7.git 
list.files(here::here("data", "calibration", "AR6", "SSPs"), pattern = "ERF", full.names = TRUE) %>% 
  lapply(get_ar6_data) %>% 
  bind_rows() %>% 
  select(year, variable, value, scenario = scn, percent) %>% 
  as.data.table() %>% 
  # Select the results for the mean ERF
  filter(is.na(percent)) ->  
  ar6_data_og

# Rename from AR6 to Hector forcing names, this will make it easier 
# to compare with Hector output. 
ar6_names <- c("ch4","n2o")
hector_names <- c(RF_CH4(), RF_N2O()) 

ar6_data_og %>% 
  filter(variable %in% ar6_names) %>% 
  left_join(data.frame(variable = ar6_names, 
                       hvar = hector_names)) %>% 
  select(year, variable = hvar, ar6_val = value, scenario) -> 
  ar6_results 

# Create a list of all the Hector cores to run during the optimization routine. 
ini_files <- list.files(system.file("input", package = "hector"), pattern = "ini", full.names = TRUE)
lapply(ini_files, function(f){
  nm <- gsub(pattern = "hector_|.ini", replacement = "", basename(f))
  newcore(inifile = f, name = nm)
}) -> 
  core_list

# Define the objective function to used in the calibration routine 
# Args
#   comp_data: data frame of the CH4 and N2O RF from AR6
#   par: named vector of nat_n2o and nat_ch4 values to use in a Hector run 
# Returns: MSE bewteen comparison data and Hector CH4 and N2O RF
optim_nat_hector <- function(comp_data, par){
  
  assert_that(has_name(x = par, which = c("nat_n2o", "nat_ch4")))
  
  # Set up the Hector core with the new natural emissions values
  lapply(core_list, setvar, dates = NA, var =  NATURAL_CH4(), value = par[["nat_ch4"]], unit =  "Tg CH4")
  dates <- 1750:2100
  lapply(core_list, setvar, dates = dates, var =  NAT_EMISSIONS_N2O(), value = rep(par[["nat_n2o"]], length(dates)), unit =  "Tg N")
  lapply(core_list, reset)
  
  # Run the Hector cores 
  multi_mean_mse <- tryCatch({
    lapply(core_list, hector::run, runtodate = 2100)
    hector_data <- bind_rows(lapply(core_list, hector::fetchvars, dates = 1740:2100, vars = c(RF_CH4(), RF_N2O())))
    
    # Get 
    hector_data %>% 
      inner_join(comp_data) %>% 
      mutate(SE = (value - ar6_val)^2) %>% 
      group_by(scenario, variable) %>% 
      summarise(MSE = mean(SE, na.rm = TRUE)) %>% 
      ungroup() %>% 
      pull(MSE) %>% 
      mean(na.rm = TRUE)}, error = function(error){99999})
  
  
  return(multi_mean_mse)
  
}

par <- c(5, 300)
names(par) <- c("nat_n2o", "nat_ch4")
natural_emiss_fit <- optim(par = par, fn = optim_nat_hector, comp_data = ar6_results)
natural_emiss_fit$par 
# On May 30 2023 
# nat_n2o    nat_ch4 
# 9.718591 340.902647 

remove("ar6_names", "ar6_results", "core_list", "format_data", "get_ar6_data", "hector_names",
       "ini_files", "optim_nat_hector", "par", "result")


# 2. Calibrate to Historical Observations -------------------------------------------------------------------------------------------
# In this section using the results from part 1 calibrated three Hector parameters ocean heat diffusivity (diff), 
# temperature effects on heterotrophic respiration (q10rh), and the CO2 fertilization factor (beta) be calibrating Hector 
# to the historical observations of CO2 and global mean surface temperature. 

# Set up helper functions 

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
# Return: a Hector core with otherwise V3 parameters defined (ECS, alpha, volsc, natural CH4 & N2O)
prep_core <- function(filename){
  # Set up the Hector core, make sure that the ECS is set equal to 3.0 
  ini <- system.file(file.path("input", filename), package = "hector")
  core <- newcore(ini)
  setvar(core, dates = NA, var = ECS(), values = 3.0, unit = getunits(ECS()))
  setvar(core, dates = NA, var = NPP_FLUX0(), values = 56.2, unit = getunits(NPP_FLUX0()))
  setvar(core, dates = NA, var = AERO_SCALE(), values = 1, unit = getunits(AERO_SCALE()))
  setvar(core, dates = NA, var = VOLCANIC_SCALE(), values = 1, unit = getunits(VOLCANIC_SCALE()))
  setvar(core, dates = NA, var = NATURAL_CH4(), values  = natural_emiss_fit$par[["nat_ch4"]], unit = getunits(NATURAL_CH4()))
  setvar(core, dates = 1750:2100, var = NAT_EMISSIONS_N2O(), values = natural_emiss_fit$par[["nat_n2o"]], unit = getunits(NAT_EMISSIONS_N2O()))
  
  # Reset the core 
  reset(core)
  return(core)
}


# Import and format the comparison data!

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
read.csv(here::here("data", "calibration", "GISTEMP_v4_20221014.csv"), skip = 1) %>% 
  select(year = Year, value = `J.D`) %>% 
  mutate(value = as.numeric(value)) %>% 
  na.omit() %>% 
  mutate(variable = "gmst") -> 
  gmst_obs_data


# Atmospheric CO2 concentrations from the NOAA Global Monitoring Laboratory
# Dr. Pieter Tans, NOAA/GML (gml.noaa.gov/ccgg/trends/) and Dr. Ralph Keeling, Scripps Institution of Oceanography (scrippsco2.ucsd.edu/).
read.csv(here::here("data", "calibration", "co2_annmean_mlo.csv"), comment.char = "#") %>% 
  select(year, value = mean) %>% 
  mutate(variable = CONCENTRATIONS_CO2()) -> 
  co2_obs_data

# Calculate the MSE between Hector output and observations, this function will be used by optim to 
# minimze the MSE ()
# Args 
#   p: vector of Hector parameter values to calibrate, must be named 
#   gmst_obs: data frame of the gmst observations from NASA GISS
#   co2_obs: data frame of the CO2 concentrations NOAA 
get_mse <- function(p, gmst_obs, co2_obs){
  
  
  # Prevent an error from crashing the fit, if for some reason (randomly generated parameter value results in an error)
  # return a large MSE value. 
  out <- tryCatch({
    
    # Make sure that the two boundary scenarios run, otherwise there could be issues if the parameter 
    # combination results in error conditions in the future. 
    hc <- prep_core("hector_ssp119.ini")
    set_params(hc, p)
    reset(hc)
    run(hc)
    
    hc <- prep_core("hector_ssp585.ini")
    set_params(hc, p)
    reset(hc)
    run(hc)
    
    # Fetch the gmst results from the Hector core & calculate the normalized MSE. 
    hector_gmst <- normalize_to_giss(fetchvars(hc, gmst_obs$year, vars = GMST()))
    hector_gmst <- hector_gmst[hector_gmst$year %in% gmst_obs$year, ]
    MSE_gmst <- mean((hector_gmst$value - gmst_obs$value)^2)
    message("gmst: ", MSE_gmst)
    
    # Fetch the co2 results from the Hector core & calculate the normalized MSE. 
    hector_co2 <- fetchvars(hc, dates = co2_obs$year, vars = CONCENTRATIONS_CO2())
    MSE_co2 <-mean((hector_co2$value - co2_obs$value)^2)
    message("co2: ", MSE_co2)
    
    # The goodness of fit is the mean of the normalized MSEs 
    MSEs <- c(MSE_gmst, MSE_co2)
    message("-----")
    return(mean(MSEs))
    
  }, error = function(e){
    # If an error was thrown return a large value
    return(9999)
  })
  
  return(out)
  
}


# An inital guess for the values 
params <- c("diff" = 1, "q10_rh" = 2.2, "beta" = 0.23)

# Test out the get_mse function 
get_mse(params, gmst_obs = gmst_obs_data,  co2_obs = co2_obs_data)

# Calibrate to the historical observations
fit <- optim(get_mse, 
             p = params,
             gmst_obs = gmst_obs_data,
             co2_obs = co2_obs_data)

save(fit, file = file.path(BASE_DIR, "output", paste0("calibration-", gsub(date(), pattern = " ", replacement = "_"), ".rda")))
