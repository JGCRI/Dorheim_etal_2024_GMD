# Hector runs to produce the metrics for table 3 in the manuscript. 

# TODO need to run through make sure annocation is all good and then compile 
# into a table that will be saved in the outputs directory. Need to add this to 
# the READ me work through. 
# 0. Set Up --------------------------------------------------------------------
source(here::here("R", "0.set_up.R"))
source(here::here("R", "0.functions.R"))

HECTOR_RSLTS_DIR <- file.path(BASE_DIR, "output", "hector_output")

# Indicator function to determine if using the hector package ini files or if
# we should be using the prep_core function that will use the calibration 
# rda objects created from "R/0A.hectorv3_calibration.R" this is only really 
# helpful when the package ini files have not been updated yet. 
use_pkg_ini <- TRUE

if(use_pkg_ini){
  my_newcore <- newcore
} else {
  my_newcore <- prep_core_v3
}

# Make sure the ini files being used are consistent with the calibration results from 
# part A. This check was only important before the V3 release.
OUTPUT_DIR <- file.path(BASE_DIR, "output", "hector_output")

# 1. Historical Run ------------------------------------------------------------
ini_file <- system.file("input/hector_ssp245.ini", package = "hector") 
hc <- my_newcore(ini_file)
run(hc)

# Methane ERF (W m-2), 2019 relative to 1750
fetchvars(hc, dates = 1750:2020, RF_CH4()) %>% 
  filter(variable == RF_CH4() & year %in% c(1750, 2019)) -> 
  methane_ERF

methane_ERF <- methane_ERF[methane_ERF$year == 2019, ][["value"]]

# Total Aerosol ERF (W m-2), 2005 - 2015 relative to 1750
fetchvars(core = hc, dates = 1750:2020, 
          vars = c(RF_OC(), RF_BC(), RF_NH3(), RF_ACI(), RF_SO2())) %>% 
  group_by(year) %>% 
  summarise(value = sum(value)) %>% 
  pull(value) %>% 
  mean() -> 
  aerosol_ERF

# WMGHG ERF (W m-2) 2019 relative to 1750
# It is unclear what set of GHGs are included here or not... 
fetchvars(hc, dates = 2019, vars = c(RF_TOTAL())) -> 
  total_rf
fetchvars(hc, dates = 2019, vars = c(RF_OC(), RF_BC(), RF_NH3(), RF_ACI(), RF_SO2(), RF_VOL(), RF_ALBEDO())) %>% 
  pull(value) %>% sum() -> 
  not_wmghgs

wmghg_erf <- total_rf[["value"]] - not_wmghgs

# GSAT Warming (°C) 1995-2014 relative to 1850-1900
fetchvars(hc, dates = 1850:1900, vars = GLOBAL_TAS()) %>% 
  pull(value) %>% 
  mean() -> 
  ref_value

fetchvars(hc, dates = 1995:2014, vars = GLOBAL_TAS()) %>% 
  mutate(value = value - ref_value) %>%  
  pull(value) %>% 
  mean -> 
  gsat_warming

# Ocean heat content change (ZJ) 1971-2018
ocean_area <- 5100656e8 * (1 - 0.29) # The total area of the ocean 
unit_conversion <- 3.155693e-14      # Watts to ZJ 
fetchvars(hc, dates = 1971:2018, vars = HEAT_FLUX()) %>%
  pull(value) %>% 
  sum() * ocean_area * unit_conversion ->
  ocean_heat_content


# 2. Idealized Runs ------------------------------------------------------------
# For the ECS need to do the doubling of CO2 experiment.
ini <- here::here("data", "idealized_inputs", "hector_abruptx2CO2.ini") 
hc <- my_newcore(ini)
run(hc)

# Based off of the extrapolation of the global tas! 
fetchvars(hc, 1750:2300, vars = GLOBAL_TAS()) %>% 
  mutate(year = year - 1799) %>% 
  filter(year >= 0) -> 
  full_gsat
ECS <- 3 


# These calculations are based off of the 1pct CO2 run 
ini <- here::here("data", "idealized_inputs", "hector_1pctCO2.ini") 
hc <- my_newcore(ini)
run(hc)

# Extract the pre industrial value & calculate the doubling and quadrupling
preindustrial_co2 <- fetchvars(hc, dates = NA, vars = PREINDUSTRIAL_CO2())
doubling_co2 <- preindustrial_co2$value * 2

# Get the CO2 vs tas data 
fetchvars(hc, dates = 1800:2000, vars = c(GLOBAL_TAS(), CONCENTRATIONS_CO2())) %>%
  select(year, value, variable) %>% 
  spread(variable, value) -> 
  tas_co2 

tas_co2 %>% 
  ggplot() + 
  geom_point(aes(CO2_concentration, global_tas)) + 
  geom_vline(aes(color = "x2CO2", xintercept = doubling_co2)) 

model <- lm(data = tas_co2, global_tas ~ CO2_concentration)
TCR <- predict(model, newdata = data.frame(CO2_concentration = doubling_co2))

# 3. Future Scenarios ----------------------------------------------------------
system.file(package = "hector", "input") %>% 
  list.files(pattern = "ini", full.names = TRUE) %>% 
  lapply(function(ini){
    name <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(ini))
    hc <- prep_core_v3(ini)
    run(hc)
    
    ref_yrs <- 1995:2014
    near_term_yrs <- 2021:2040
    mid_term_yrs <- 2041:2060
    long_term_yrs <- 2081:2100
    
    fetchvars(hc, dates = ref_yrs, vars = GLOBAL_TAS()) %>% 
      pull(value) %>% 
      mean() -> 
      ref_vals
    
    fetchvars(hc, dates = near_term_yrs, vars = GLOBAL_TAS()) %>% 
      pull(value) %>% 
      mean() -> 
      near_term_vals
    
    fetchvars(hc, dates = mid_term_yrs, vars = GLOBAL_TAS()) %>% 
      pull(value) %>% 
      mean() -> 
      mid_term_vals
    
    fetchvars(hc, dates = long_term_yrs, vars = GLOBAL_TAS()) %>% 
      pull(value) %>% 
      mean() -> 
      long_term_vals
    
    data.frame(scenario = name, 
               value = c(near_term_vals, mid_term_vals, long_term_vals), 
               period = c("near term", "mid term", "long term")) %>% 
      mutate(value = value - ref_vals) -> 
      out 
    
    return(out)
    
  }) %>% 
  do.call(what = "rbind") %>% 
  mutate(value = signif(value, 3)) -> 
  gsat_table

# 4. Calculating TCRE ----------------------------------------------------------
# Emission driven Hector run
ini <- system.file(package = "hector", "input/hector_ssp585.ini") 
hc <- newcore(ini)
run(hc)

# Cumulative total CO2 emissions 
fetchvars(hc, 1746:2300, vars = c(LUC_EMISSIONS(), LUC_UPTAKE(), FFI_EMISSIONS(), DACCS_UPTAKE())) %>% 
  mutate(value = if_else(variable %in% c(LUC_UPTAKE(), DACCS_UPTAKE()), value * -1, value)) %>%  
  group_by(year) %>%  
  summarise(value = sum(value)) %>% 
  ungroup -> 
  emissions

cum_emissions <- cumsum(emissions$value)

# Save Hector GSAT and CO2 concentrations
fetchvars(hc, 1746:2300, vars = GLOBAL_TAS()) %>% 
  rename(tas = value) -> 
  tas_out

# We will be looking at (GSAT/cumulative emissions) and
# (CO2 concentration/ cumulative emissions) so format into a wide data set.  
hector_data <- data.frame(emiss = cum_emissions, 
                          gsat = tas_out$tas, 
                          year = tas_out$year)


ggplot(data = hector_data) + 
  geom_line(aes(emiss, gsat)) 

# Calculate the TCRE per 1000 PG of C 
fit <- lm(formula = gsat ~ emiss, data = hector_data)
TCRE <- fit$coefficients[["emiss"]] * 1000

# 5. Format Output --------------------------------------------------------------

data_list <- list("ECS" = ECS,
                  "TCRE" = TCRE,       
                  "TCR" =  TCRE,
                  "gsat_warming" = gsat_warming,
                  "ocean heat content" = ocean_heat_content,
                  "aerosol rf" = aerosol_ERF,
                  "WMGHG ERF" = wmghg_erf,
                  "methane ERF" = methane_ERF)


data.frame(scenario = names(data_list), 
           value =  unlist(data_list), row.names = NULL) %>% 
  bind_rows(gsat_table) %>% 
  rename(metric = scenario) -> 
  table3_data
  
file <- paste0("hector", HECTOR_VERSION, "_table3data.csv")
write.csv(table3_data, file = file.path(OUTPUT_DIR, file), row.names = FALSE)
