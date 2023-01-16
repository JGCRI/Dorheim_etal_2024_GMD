# Objective: Import and format the IPCC results to simplify the manuscript analyses and figures 
# code. 
# 0. Set Up ----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
source("R/0B.functions.R")

devtools::load_all("~/projects/Hector-Versions/v3/hector/")
library(hector)
version <- packageVersion("hector")
assertthat::assert_that(version == "3.0.0")

# The contents of the IPCC_DIR are the supplemental material figure 10 the cumulative CO2 
# emissions vs global temperature change
# https://data.ceda.ac.uk/badc/ar6_wg1/data/spm/spm_10/v20210809
IPCC_DIR <- here::here("data", "IPCC_SPM10")

# Load the IPCC tas vs co2 results 
# 
# Args 
#   f: file name to read in
read_IPCC_rslts <- function(f){
  
  data <- read.csv(file.path(IPCC_DIR, f)) %>% 
    select(-GtCO2)
  
  scn <- tolower(gsub(x = f, pattern = "Top_panel_|.csv|-", replacement = ""))
  if(scn == 'history'){
    scn <- "historical"
  }
  
  co2_vals <- names(data)
  co2_vals <- co2_vals[grepl(x = co2_vals, pattern = "X")]
  co2_vals <- as.numeric(gsub(x = co2_vals, pattern = "X", replacement = ""))
  
  out <- data.frame()
  nrow <- dim(data)[[1]]
  for(i in 1:nrow){
    type <- data[i, 1]
    vals <- as.numeric(data[i, -1])
    df <- data.frame(type = type, 
                     scenario = scn,
                     co2 = co2_vals, 
                     tas = vals)
    out <- rbind(out, df)
  }
  
  
  out %>%  
    mutate(type = if_else(grepl(pattern = "central estimate|central", x = type), "mean", type)) %>% 
    mutate(type = if_else(grepl(pattern = "95th percentile|95th percentile", x = type), "max", type)) %>% 
    mutate(type = if_else(grepl(pattern = "5th percentile estimate|5th percentile", x = type), "min", type)) %>% 
    mutate(type = if_else(grepl(pattern = "Assessed GSAT projection relative to 1850-1900|Assessed global surface temperature relative to 1850-1900", x = type), 
                          "obs", type)) -> 
    out
  
  
  out %>% 
    group_by(type, scenario, co2) %>% 
    summarise(tas = mean(tas)) %>% 
    ungroup %>% 
    spread(type, tas) -> 
    out
  
  return(out)
  
}

# 1. Process the IPCC data ----------------------------------------------------------------------------
IPCC_rslts <- bind_rows(read_IPCC_rslts("Top_panel_HISTORY.csv"), 
                        read_IPCC_rslts("Top_panel_SSP1-19.csv"),
                        read_IPCC_rslts("Top_panel_SSP1-26.csv"),
                        read_IPCC_rslts("Top_panel_SSP1-26.csv"), 
                        read_IPCC_rslts("Top_panel_SSP2-45.csv"),
                        read_IPCC_rslts("Top_panel_SSP3-70.csv"),
                        read_IPCC_rslts("Top_panel_SSP5-85.csv"))

write.csv(IPCC_rslts, file = here::here("data", "IPCC_co2_tas.csv"), row.names = FALSE)

# 2. Update Hector's historical luc emissions ---------------------------------------------------------------------
# By the end of this section we will have the updated luc emissions for IPCC's historical period aka 1850 to 2019.

# Start by loading the hector co2 emissions by year, scenario and emission type. 
here::here("input", "tables") %>% 
  list.files(pattern = "ssp119", full.names = TRUE) %>% 
  lapply(function(f){
    scn <- gsub(basename(f), pattern = "rcmip_|_emiss-constraints_rf.csv", replacement = "")
    data <- read.csv(f, comment.char = ";")
    
    rbind(
      data.frame(year = data$Date, 
                 value = data$luc_emissions, 
                 variable = LUC_EMISSIONS()),
      data.frame(year = data$Date, 
                 value = data$ffi_emissions, 
                 variable = FFI_EMISSIONS()), 
      data.frame(year = data$Date,
                 value = data$daccs_uptake, 
                 variable = DACCS_UPTAKE()),
      data.frame(year = data$Date, 
                 value = data$luc_uptake, 
                 variable = LUC_UPTAKE())) %>% 
      mutate(scenario = scn)
    
  }) %>% 
  do.call(what = "rbind") %>% 
  # One of the challenges with this is that the rcmip scenarios and the IPCC scenarios use 
  # different definitions of current year aka when does history start & future begin? 
  mutate(scenario = if_else(year <= 2019, "historical", scenario)) %>% 
  distinct() %>% 
  dplyr::filter(scenario %in% IPCC_rslts$scenario) ->
  hector_co2_emissions_by_variable

# Resolving the differences between historical emissions. 
# Calculate the total co2 emissions before figuring out the 
# cumlative emissions. These two data frames will be used to 
# compare IPCC and Hector emissions & caculate the difference between them. 
hector_co2_emissions_by_variable %>% 
  filter(scenario == "historical") %>% 
  group_by(year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup -> 
  hector_historical_co2

hector_historical_co2 %>% 
  filter(year >= 1850) %>% 
  mutate(value = cumsum(value)) -> 
  hector_historical_co2_cumlative

# Select the historical CO2 emissions from the IPCC report and convert to Hector units.
IPCC_rslts %>% 
  dplyr::filter(scenario == "historical") %>% 
  select(scenario, value = co2) %>% 
  # Convert to Hector CO2 emission units.
  mutate(value = value * (1/3.67)) %>% 
  mutate(year = 1850:2019) -> 
  IPCC_hist_cumlative

# Here is the difference in the cumlative emissions!
ggplot() + 
  geom_line(data = IPCC_hist_cumlative, aes(year, value, color = "IPCC")) + 
  geom_line(data = hector_historical_co2_cumlative, aes(year, value, color = "hector")) + 
  labs(title = "Differnce in cumlative CO2 emissions between IPCC and Hector historical period")

# Find the per year emissions time series for IPCC 
# Differentiate to get the per year total co2 emissions of the historical IPCC time series.
IPCC_hist_vals<- c(IPCC_hist_cumlative$value[1], diff(IPCC_hist_cumlative$value))
data.frame(value = IPCC_hist_vals, 
           year = seq(from = 1850, by = 1, length.out = length(IPCC_hist_vals))) -> 
  IPCC_hist_annual

# Calculate the difference between the annual IPCC and Hector historical co2 emissions. 
# which will be added to Hector's luc_emissions.
hector_co2_emissions_by_variable %>% 
  filter(variable == LUC_EMISSIONS() & scenario == "historical") -> 
  hector_luc_hist

# Determine the difference between the annual total co2 emissions. 
hector_historical_co2 %>% 
  rename(hector_value = value) %>% 
  full_join(IPCC_hist_annual, by = "year") %>% 
  mutate(difference = value - hector_value) %>% 
  select(year, difference) %>% 
  # If there is no comparison data label the difference as 0 so that 
  # no NAs will appear in the new historical data. 
  mutate(difference = replace_na(difference, 0)) %>% 
  full_join(hector_luc_hist, by = "year") %>% 
  mutate(value = value + difference) %>% 
  select(year, value, variable, scenario) -> 
  new_hector_luc_hist

hector_1745_lucemissions <- 0.0812
hector_1850_luc_emissions <- 0.676

early_history <- seq(from = hector_1745_lucemissions, to = hector_1850_luc_emissions, length.out = length(1745:1850))

new_hector_luc_hist[new_hector_luc_hist$year %in% 1745:1850, ]$value <- early_history


# Compare the old and new LUC emissions inputs for Hector 
ggplot() + 
  geom_line(data = new_hector_luc_hist, aes(year, value, color = "new")) + 
  geom_line(data = hector_luc_hist, aes(year, value, color = "of hector")) + 
  labs(title = "Old v New Hecotr historical luc inputs")

here::here("input") %>% 
  list.files(pattern = "ssp119", full.names = TRUE) %>% 
  lapply(function(f){
    scn <- gsub(x = basename(f), pattern = "rcmip_|.ini", replacement = "")
    core <- newcore(inifile = f, name = "historical")
    setvar(core, dates = new_hector_luc_hist$year, var = LUC_EMISSIONS(), 
           values = new_hector_luc_hist$value, unit = "Pg C/yr")
    reset(core)
    run(core = core)
    out1 <- fetchvars(core = core, dates = 1850:2019, vars = c(FFI_EMISSIONS(), DACCS_UPTAKE(), 
                                                               LUC_EMISSIONS(), GLOBAL_TAS()))
    out2 <- fetch_gmst(core)
    out2$scenario <- "historical"
    out <- dplyr::bind_rows(out1, out2)
    return(out)
  }) %>%  
  do.call(what = "rbind") %>% 
  distinct() -> 
  hector_IPCC_his_rslts


# 3. Future results -----------------------------------------------------------------------------
# By the end of this section we should have the hector results for the future scenarios, it will 
# use the historical + future I think 

# Based on the number of observations in the non historical IPCC data the output is from 2015 to 2050. 
# And it looks like the first year the emissions differ from one another is in the year 2016 which is 
# consistent with Hector's ssp scenarios too. 
IPCC_rslts %>% 
  filter(scenario != "historical") %>%
  select(scenario, co2) %>% 
  cbind(year = 2015:2050) %>% 
  distinct %>% 
  spread(scenario, co2)

IPCC_scns <- paste0(unique(IPCC_rslts$scenario), collapse = "|")

new_hector_luc_hist %>% 
  filter(year <= 2015) -> 
  hist_to_use_with_ssps

here::here("input") %>% 
  list.files(pattern = IPCC_scns, full.names = TRUE) %>% 
  lapply(function(f){
    scn <- gsub(x = basename(f), pattern = "rcmip_|.ini", replacement = "")
    core <- newcore(inifile = f, name = scn)
    setvar(core, dates = hist_to_use_with_ssps$year, var = LUC_EMISSIONS(), 
           values = hist_to_use_with_ssps$value, unit = "Pg C/yr")
    reset(core)
    run(core = core)
    out1 <- fetchvars(core = core, dates = 1850:2050, vars = c(FFI_EMISSIONS(), DACCS_UPTAKE(), 
                                                               LUC_EMISSIONS(), GLOBAL_TAS()))
    out2 <- fetch_gmst(core)
    out2$scenario <- scn
    out <- dplyr::bind_rows(out1, out2)
    return(out)
  }) %>%  
  do.call(what = "rbind") %>% 
  distinct() -> 
  hector_IPCC_spp_rslts


# 4. Format the Hector results ------------------------------------------------------------------------------------------------

hector_rslts <- rbind(hector_IPCC_his_rslts, hector_IPCC_spp_rslts)

# The total co2 emissions per year per Hector scenario. 
hector_rslts %>%  
  filter(variable %in% c(FFI_EMISSIONS(), DACCS_UPTAKE(), LUC_EMISSIONS(), LUC_UPTAKE())) %>% 
  filter(year %in% 1850:2050) %>%  
  group_by(year, scenario) %>% 
  # Get the total co2 emissions per year. 
  summarise(value = sum(value)) %>%  
  # Convert from the Hector units to the IPCC units. 
  mutate(value = value * 3.67) -> 
  hector_annual_co2_emissions

# Find the cumulative co2 emissions. 
split(hector_annual_co2_emissions, hector_annual_co2_emissions$scenario) %>% 
  lapply(function(dat){
    vals <- cumsum(dat$value)
    dat$value <- vals
    return(dat)
  }) %>% 
  do.call(what = "rbind") -> 
  hector_cumsum_co2

# Make sure the historical results are for the period 1850:2020 and that the future 
# results are only for the 2015 to 2050 period. 
hector_cumsum_co2 %>% 
  filter(scenario == "historical") %>% 
  filter(year %in% 1850:2020) -> 
  hist_cumsum_co2

hector_cumsum_co2 %>%  
  filter(scenario != "historical") %>% 
  filter(year %in% 2015:2050) -> 
  future_cumsum_co2

rbind(hist_cumsum_co2, 
      future_cumsum_co2) %>%  
  rename(co2 = value) %>% 
  select(year, scenario, co2) -> 
final_hector_cumsum_co2 


# Normalize the Hector temperature results to the reference period used in the figure of 
# 1850:1900
hector_rslts %>%
  filter(variable %in% c("gmst")) %>% 
  normalize_hector_temp(period = 1850:1900) %>%  
  select(year, scenario, gmst = value) -> 
  gmst_normalized

hector_rslts %>%
  filter(variable == GLOBAL_TAS()) %>% 
  normalize_hector_temp(period = 1850:1900) %>%  
  select(year, scenario, tas = value) -> 
  tas_normalized

final_hector_cumsum_co2 %>% 
  inner_join(gmst_normalized, by = c("year", "scenario")) %>%  
  inner_join(tas_normalized, by = c("year", "scenario")) -> 
  hector_co2_tas

write.csv(hector_co2_tas, file = here::here("output", "hector_output", "hector_3.0.0-IPCCemiss_co2_tas.csv"), row.names = FALSE)
