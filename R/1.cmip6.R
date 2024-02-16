# This script formats the cmip6 output results from 
# https://github.com/JGCRI/hector_cmip6data 
# into ready to plot/analyze data for the manuscript. 
# 0. Set Up ----------------------------------------------------------------
library(dplyr)
library(hector)
library(readr)
library(tidyr)
source("R/0.constants.R")
CMIP_DIR <- file.path(BASE_DIR, "data", "cmip")


# Concatenate the historical and the future scenario results 
# this creates a continuous time series between the historical 
# and future results for a single variable
#
# Args
#   data: data frame of Hector results for a single experiment 
#   period: vector of the years in the reference period
# Return: data frame of normalized values.
paste_historical <- function(data){
  
  assertthat::assert_that(length(unique(data[["variable"]])) == 1)
  
  
  historical_data <- filter(data, scenario == "historical")
  future_data <- filter(data, scenario != "historical")
  
  split(future_data, 
        interaction(future_data$model, future_data$ensemble, future_data$scenario), drop = TRUE) %>% 
    lapply(function(dd){
      mod <- unique(dd[["model"]])
      scn <- unique(dd[["scenario"]])
      en <- unique(dd[["ensemble"]])
      
      historical_data %>% 
        filter(model == mod & ensemble == en) %>% 
        mutate(scenario = scn) -> 
        out 
      return(out)
    }) %>% 
    do.call(what = "rbind") ->
    new_hisorical
  
  out <- rbind(new_hisorical, future_data) %>% 
    filter(!scenario %in% c("historical"))
  return(out)
}

exps <- c("ssp126", "ssp245", "ssp370", "ssp585", "ssp119", 
          "ssp434", "ssp460", "historical")

# 1. Load Data (future scenarios) ----------------------------------------------
list.files(CMIP_DIR, pattern = "cmip6_annual_tas_global", full.names = TRUE) %>% 
  read.csv %>% 
  filter(experiment %in% exps) %>% 
  rename(scenario = experiment) %>% 
  dplyr::filter(variable == "Tgav" & type == "global") %>%  
  mutate(variable = GLOBAL_TAS()) %>% 
  select(model, scenario, ensemble, variable, year, value) %>% 
  filter(year <= 2100) %>% 
  paste_historical %>% 
  select(model, scenario, ensemble, year, global_tas = value) -> 
  cmip6_global_tas

row.names(cmip6_global_tas) <- NULL

list.files(CMIP_DIR, pattern = "tos_global", full.names = TRUE) %>% 
  read_csv(show_col_types = FALSE) %>% 
  filter(experiment %in% exps) %>% 
  rename(scenario = experiment) %>% 
  filter(variable == "tos") %>%
  mutate(variable = SST()) %>% 
  paste_historical %>% 
  na.omit %>% 
  filter(year <= 2100) %>% 
  select(model, scenario, ensemble, year, sst = value) -> 
  cmip6_global_sst

# This is the only cmip variable that needs to be re referenced 
cmip6_global_sst %>% 
  filter(year %in% 1850:1900) %>% 
  group_by(model, scenario, ensemble) %>% 
  summarise(ref = mean(sst)) %>% 
  ungroup -> 
  ref_tos

cmip6_global_sst %>% 
  inner_join(ref_tos, by = join_by(model, scenario, ensemble)) %>% 
  mutate(sst = sst - ref) %>% 
  select(-ref) -> 
  cmip6_global_sst
  

list.files(CMIP_DIR, pattern = "tas_land", full.names = TRUE) %>% 
  read_csv(show_col_types = FALSE) %>% 
  filter(experiment %in% exps) %>% 
  rename(scenario = experiment) %>%  
  # Note that LP uses tgav to refer to the anomaly value
  filter(variable == "Tgav") %>% 
  mutate(variable = LAND_TAS()) %>% 
  paste_historical %>% 
  na.omit %>% 
  filter(year <= 2100) %>% 
  select(model, scenario, ensemble, year, land_tas = value) ->
  cmip6_global_land
  

# 2. Format Data (future scenarios) --------------------------------------------
# Join the data frames together to make wide df to make sure we have full coverage.
cmip6_global_tas %>% 
  inner_join(cmip6_global_sst, by = c("model", "year", "ensemble", "scenario")) %>% 
  inner_join(cmip6_global_land, by = c("model", "year", "ensemble", "scenario")) -> 
  wide_tas_df

ids <- which(!names(wide_tas_df) %in% c("model", "scenario", "ensemble", "year"))

wide_tas_df %>% 
  pivot_longer(ids, 
               names_to = "variable", 
               values_to = "value") -> 
  long_tas_df


split(long_tas_df, long_tas_df$model) %>% 
  lapply(function(d){
    
    ens <- sort(unique(d$ensemble))[[1]]
    out <- filter(d, ensemble == ens)
    return(out)
    
  }) %>% 
  do.call(what = "rbind") -> 
  cmip6_model_rslts
  
write.csv(cmip6_model_rslts, file = here::here("data", "cmip6_model_means.csv"), 
          row.names = FALSE)

  
# 3. Format/Prep Idealized Scenarios -------------------------------------------
list.files(CMIP_DIR, "CMIP6_idealized_tas_global.csv", full.names = TRUE) %>% 
  read.csv -> 
  CMIP6_idealized

split(CMIP6_idealized, CMIP6_idealized$model) %>% 
  lapply(function(d){
    
    ens <- sort(unique(d$ensemble))[[1]]
    out <- filter(d, ensemble == ens)
    return(out)
    
  }) %>% 
  do.call(what = "rbind") -> 
  CMIP6_idealized

write.csv(CMIP6_idealized, file = here::here("data", "cmip6_idealized.csv"), 
          row.names = FALSE)
