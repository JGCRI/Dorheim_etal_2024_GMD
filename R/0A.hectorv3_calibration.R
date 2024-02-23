# Objective: Calibrate the parameter variables for natural CH4 and N20. Then calibrate 
# Hector parameter values for diff (ocean heat diffusitivty), q10_rh (temperature effects 
# on heterotrophic respiration), and beta (the CO2 fertilization factor). This 
# script only needs to be run once in order to determine the new default Hector parameters. 
# The results from this script will be used to update the ini files! Updating the 
# ini files needs to be done manually (TODO figure out a way to update all parameter
# ini files in some reproducible manner). 

# 0. Set up -----------------------------------------------------------------------------------------------------------------------
source("R/0.set_up.R")
source("R/0.functions.R")


# 1. Adjust natural CH4 and N2O emissions -------------------------------------------------------------------------------------------
# Calibrate the natural contributions of CH4 and N2O emissions by calibrating to results from AR6 CH 7.  

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
  
  dat <- as.data.table(read.csv(path, stringsAsFactors = FALSE))
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

save(natural_emiss_fit,
     file = file.path(BASE_DIR, "output", paste0("calibration-natemissions-", gsub(date(), pattern = " ", replacement = "_"), ".rda")))

remove("ar6_names", "ar6_results", "core_list", "format_data", "get_ar6_data", "hector_names",
       "ini_files", "optim_nat_hector", "par")


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

# Normalize temperature to the historical reference period
# Args 
#   data: data frame of temperature data
# Returns a data frame of temperature data normalized to the 1951:1980 reference period
normalize_to_hadcrut <- function(data){
  
  assert_that(unique(data[["variable"]]) == "gmst")
  
  ref_value <- mean(data[(data$year %in%  1961:1990), ]$value)
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

# Hadcrut5
# Global mean surface temperature anomaly
# https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/download.html
# The temperature anomaly is based off of 1961–1990
# surface temperature with the anomaly!
# Morice, C. P., Kennedy, J. J., Rayner, N. A., Winn, J. P., Hogan, E., Killick, R. E., et al. (2021).
# An updated assessment of near-surface temperature change from 1850: the HadCRUT5 data
# set. Journal of Geophysical Research: Atmospheres, 126, e2019JD032361.
# https://doi.org/10.1029/2019JD032361
here::here("data", "HadCRUT5.csv") %>%
  read.csv(stringsAsFactors = FALSE) %>% 
  na.omit ->
  hadcrut_obs_data

names(hadcrut_obs_data) <- c("year", "value", "lower", "upper")
hadcrut_obs_data$variable <- "gmst"
gmst_obs_data <- hadcrut_obs_data


# Meinshausen, M., Vogel, E., Nauels, A., Lorbacher, K., Meinshausen, N., Etheridge, D. M., 
# Fraser, P. J., Montzka, S. A., Rayner, P. J., Trudinger, C. M., Krummel, P. B., Beyerle, 
# U., Canadell, J. G., Daniel, J. S., Enting, I. G., Law, R. M., Lunder, C. R., O'Doherty, 
# S., Prinn, R. G., Reimann, S., Rubino, M., Velders, G. J. M., Vollmer, M. K., Wang, 
# R. H. J., and Weiss, R.: Historical greenhouse gas concentrations for climate modelling 
# (CMIP6), Geosci. Model Dev., 10, 2057–2116, https://doi.org/10.5194/gmd-10-2057-2017, 2017.
here::here("data", "Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017.csv") %>%  
  read.csv(skip = 21) %>% 
  na.omit %>% 
  select(year =  "v.YEARS.GAS..", value = "CO2") %>% 
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
    hector_gmst <- normalize_to_hadcrut(fetchvars(hc, gmst_obs$year, vars = GMST()))
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
params <- c("diff" = 2.3, 
           "q10_rh" = 2.2,
            "beta" = 0.5)

# Set the bounds for the potential parameters based off of Brown et. al 2024
sd <- c(0.1, 0.44, 0.232)
lower <- params - sd
upper <- params + sd

# Test out the get_mse function 
get_mse(params, gmst_obs = gmst_obs_data,  co2_obs = co2_obs_data)

# Calibrate to the historical observations
fit <- optim(get_mse, 
             p = params,
             gmst_obs = gmst_obs_data,
             co2_obs = co2_obs_data, 
             lower = lower, 
             upper = upper, method = "L-BFGS-B")

save(fit, file = file.path(BASE_DIR, "output", paste0("calibration-diff_beta_q10-", gsub(date(), pattern = " ", replacement = "_"), ".rda")))

# 3. Next Step -----------------------------------------------------------------
# The ini files defined in the Hector repository /inst/input will need to be updated 
# to use the new parameter values. This step has not been automated. 
