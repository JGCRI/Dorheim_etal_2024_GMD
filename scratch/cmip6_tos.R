# Author: Dorheim 
# Date: March 2022 
# Objective: Scratch comparison of CMIP6 temperatures with Hector temperature. 

# 0. Set Up ---------------------------------------------------------------------------------------------
library(dplyr)
library(hector)
library(ggplot2)

theme_set(theme_bw())

BASE_DIR <- here::here()
COMPARISON_DIR <- file.path(BASE_DIR, "data", "comparison")

# 1. CMIP6 Data -----------------------------------------------------------------------------------------

cmip6_tos <- read.csv(file.path(COMPARISON_DIR, "CMIP6", "cmip6_annual_tos.csv"), stringsAsFactors = FALSE) %>% 
  rename(scenario = experiment) %>% 
  filter(year <= 2100)

cmip6_tos %>% 
  group_by(scenario, year) %>% 
  dplyr::summarise(tos_mean = mean(value), 
                   tos_min = min(value), 
                   tos_max = max(value)) %>% 
  ungroup() %>% 
  mutate(scenario = if_else(year <= 2019, "ssp119", scenario)) -> 
  cmip6_tos




# 2. Run Hector -----------------------------------------------------------------------------------------

list.files(system.file("input", package = "hector"), pattern = "ini", full.names = TRUE) %>% 
  lapply(function(ini){
    vars_to_keep <- c(OCEAN_SURFACE_TEMP())
    
    core <- newcore(ini)
    run(core)
    out <- fetchvars(core, dates = 1850:2100, vars_to_keep)
    scenario <- gsub(x = basename(ini), pattern = "hector_|.ini", replacement = "")
    
    out$scenario <- scenario
    out$version <- "3"
    return(out)
    
  }) %>%
  do.call(what = "rbind") ->
  hector_data



# 3. Comparison -----------------------------------------------------------------------------------------

ggplot() + 
  geom_ribbon(data = cmip6_tos, aes(year, ymin = tos_min, ymax = tos_max, group = scenario), 
              alpha = 0.3) +
  geom_line(data = cmip6_tos, aes(year, tos_mean, group = scenario), alpha = 0.3) +
  geom_line(data = hector_data, aes(year, value, color = "Hector", 
                                  group = interaction(scenario))) + 
  facet_wrap("scenario", scales = "free")  + 
  labs(y = NULL, x = NULL, title = "TOS")
