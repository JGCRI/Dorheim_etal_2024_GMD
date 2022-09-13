# Author: Dorheim 
# Date: March 2022 
# Objective: Scratch comparison of CMIP6 temperature with Hector temperature. 
# TODO: 

# 0. Set Up ---------------------------------------------------------------------------------------------
library(dplyr)
library(hector)
library(ggplot2)

theme_set(theme_bw())
BASE <- here::here()

# 1. CMIP6 data ---------------------------------------------------------------------------------------------
tas_global <- read.csv(file.path(BASE, "data", "comparison", "CMIP6", "cmip6_annual_tas_global.csv"), stringsAsFactors = FALSE)

tas_global %>% 
  dplyr::filter(experiment %in% c("ssp126", "ssp245", "ssp370",  
                                "ssp585", "ssp119" , "ssp434", "ssp460")) %>% 
  dplyr::mutate(scenario = experiment) %>% 
  select(model, scenario, ensemble, variable, value = Tgav, year) %>% 
  mutate(variable = GLOBAL_TEMP()) %>% 
  group_by(model, scenario, year) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  filter(year <= 2100) -> 
  cmip6_tas


tas_land <- read.csv(file.path(BASE, "data", "comparison", "CMIP6", "cmip6_annual_tas_over_land.csv"), 
                     stringsAsFactors = FALSE)

tas_land %>% 
  filter(experiment == "historical")



tas_land %>% 
  filter(experiment %in% cmip6_tas$scenario) %>% head()









cmip6_tas %>% 
  filter(year <= 2100) %>% 
  ggplot() + 
  geom_line(aes(year, value, group = model, color = "CMIP6"), alpha = 0.7, color = "grey") + 
  facet_wrap("scenario", scales = "free")


# 2. Hector Results ---------------------------------------------------------------------------------------------

list.files(system.file("input", package = "hector"), pattern = "ini", full.names = TRUE) %>% 
  lapply(function(ini){
    vars_to_keep <- c(GLOBAL_TEMP(), RF_TOTAL(), RH(), HEAT_FLUX(), LAND_AIR_TEMP())
    
    core <- newcore(ini)
    run(core)
    out <- fetchvars(core, dates = 1850:2100, vars_to_keep)
    scenario <- gsub(x = basename(ini), pattern = "hector_|.ini", replacement = "")
    
    out$scenario <- scenario
    out$version <- "3"
    return(out)
    
  }) %>%
  do.call(what = "rbind") ->
  hector_out

# 2. Global tas Comparison ---------------------------------------------------------------------------------------------
hector_tas <- dplyr::filter(hector_out, variable == GLOBAL_TEMP())

ggplot() + 
  geom_line(data = cmip6_tas, aes(year, value, group = model, color = "CMIP6"), alpha = 0.7, color = "grey") + 
  geom_line(data = hector_tas, aes(year, value, color = "Hector v3"), color = "Black") + 
  facet_wrap("scenario", scales = "free") + 
  labs(x = NULL, y = NULL, title = "global tas")
