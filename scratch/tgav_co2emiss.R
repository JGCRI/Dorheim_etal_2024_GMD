# Author: Dorheim 
# Date: March 2022 
# Objective: Cumulative emissions vs temperature 

# 0. Set Up ---------------------------------------------------------------------------------------------
library(dplyr)
library(hector)
library(ggplot2)

theme_set(theme_bw())

BASE_DIR <- here::here()

# 1. Run Hector ---------------------------------------------------------------------------------------------

system.file("input", package = "hector") %>% 
  list.files(pattern = "ssp", full.names = TRUE) %>% 
  lapply(function(f){
    
    name <- gsub(x = basename(f), pattern = "hector_|.ini", replacement = "")
    core <- newcore(f, name = name)
    run(core)
    fetchvars(core, 1850:2050, vars = c(FFI_EMISSIONS(), LUC_EMISSIONS(), GLOBAL_TEMP())) %>% 
      select(year, scenario, variable, value) %>% 
      tidyr::spread(variable, value) -> 
      out
    
    out$total <- out$ffi_emissions + out$luc_emissions
    
    split(out, out$scenario) %>% 
      lapply(function(d){
        cumsum(d[["total"]])
      }) %>% 
      do.call(what = "c") -> 
      cumlativev_emiss
    
    data.frame(year = out$year, scenario = out$scenario, tgav = out$Tgav, cum_co2 = cumlativev_emiss)
    
  }) %>% 
  do.call(what = "rbind") -> 
  out


out %>% 
  mutate(scenario = if_else(year <= 2019, "historical", scenario)) %>%  
  ggplot(aes(cum_co2, tgav, color = scenario)) + 
  geom_line(size = 1.5, alpha = 0.5) + 
  labs(y = "Global Temp Deg C", 
       x = "Cumlative CO2 Emissions")






