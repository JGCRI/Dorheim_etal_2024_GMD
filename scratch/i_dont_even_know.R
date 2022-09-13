remotes::install_github("JGCRI/hector@land_tracking")
library(hector)

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

BASE_DIR <- here::here()

theme_set(theme_bw())


yrs <- 1850:2100 
vars <- c(GLOBAL_TEMP(), ATMOSPHERIC_CO2(), NPP(), HEAT_FLUX(), OCEAN_CFLUX(), NBP())


system.file("input", package = "hector") %>% 
  list.files(full.names = TRUE, pattern = "ini") %>% 
  lapply(function(f){
    name <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(f))
    core <- newcore(f, name = name)
    run(core)
    out <- fetchvars(core, yrs, vars)
    out$type <- "free running"
    return(out)
  }) %>% 
  do.call(what = "rbind") -> 
  free_running_out


system.file("input", package = "hector") %>% 
  list.files(full.names = TRUE, pattern = "ini") %>% 
  lapply(function(f){
    scn_name <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(f))
    #name <- paste0(name, " co2 driven")
    name <- paste0(scn_name)
    
    data_path <- list.files( system.file("input/tables", package = "hector"), pattern = scn_name, full.names = TRUE)
    data <- read.csv(data_path, comment.char = ";")
    
    core <- newcore(f, name = name)
    setvar(core, data$Date, var = CO2_CONSTRAIN(), values = data$CO2_constrain, unit = getunits(CO2_CONSTRAIN()))
    reset(core)
    run(core)
    out <- fetchvars(core, yrs, vars)
    out$type <- "co2 driven"
    return(out)
  }) %>% 
  do.call(what = "rbind") -> 
  co2_driven

var <- ATMOSPHERIC_CO2()
comp_data <- rbind(co2_driven, free_running_out) 


magicc_nbp <- rbind(data.frame(year = c(1960, 1980, 1990, 2000, 2010, 2020, 2030, 2050, 2100),
                               value = c(-0.45, 0.75, 0.64, 0.78, 1.32, 1.43, 2.32, 4.01, 6.01), 
                               variable = c(NBP()), 
                               scenario = "ssp585"), 
                    data.frame(year = c(1960, 1980, 1990, 2000, 2010, 2020, 2030, 2050, 2100),
                               value = c(-0.45, 0.75, 0.64, 0.78, 1.32, 2.2, 2.39, 1.65, 3.12), 
                               variable = c(NBP()), 
                               scenario = "ssp245"), 
                    data.frame(year = c(1960, 1980, 1990, 2000, 2010, 2020, 2030, 2050, 2100),
                               value = c(-0.45, 0.75, 0.64, 0.78, 1.32, 2.21, 2.44, 3.03, -0.22), 
                               variable = c(NBP()), 
                               scenario = "ssp119"))



comp_data %>%  
  filter(year >= 1960) %>% 
  filter(scenario %in% c("ssp119", 'ssp245', "ssp585")) %>%  
  filter(type == "free running") %>% 
  mutate(type = "emiss Hector") -> 
  filter(variable == NBP()) -> 
  hector_NBP
ggplot() + 
  geom_line(data = hector_NBP, aes(year, value, color = type)) + 
  geom_point(data = magicc_nbp, aes(year, value, color = "MAGICC 7")) + 
  geom_line(data = magicc_nbp, aes(year, value)) + 
  facet_wrap("scenario") + 
  labs(title = "NBP")




magicc_ocean <- rbind(data.frame(year = c(1960, 1980, 1990, 2000, 2010, 2020, 2030, 2050, 2100),
                               value = c(0.68, 1.40, 1.56, 1.71, 2.02, 2.49, 3.03, 4.15, 5.40), 
                               variable = OCEAN_CFLUX(), 
                               scenario = "ssp585"), 
                    data.frame(year = c(2020, 2030, 2050, 2100),
                               value = c(2.44, 2.70, 2.88, 1.92), 
                               variable = OCEAN_CFLUX(), 
                               scenario = "ssp245"), 
                    data.frame(year = c(2020, 2030, 2050, 2100),
                               value = c(2.40, 2.09, 1.17, -0.06), 
                               variable = OCEAN_CFLUX(), 
                               scenario = "ssp119"))



comp_data %>%  
  filter(variable == OCEAN_CFLUX()) -> 
  hector_ocean
ggplot() + 
  geom_line(data = hector_ocean, aes(year, value, color = type)) + 
  geom_point(data = magicc_ocean, aes(year, value)) + 
  geom_line(data = magicc_ocean, aes(year, value)) + 
  
  facet_wrap("scenario")




















comp_data %>% 
  filter(variable == var) %>%  
  ggplot(aes(year, value, color = type)) + 
  geom_line() + 
  facet_wrap("scenario", scales = "free") + 
  labs(title = var)

comp_data %>% 
  filter(variable == GLOBAL_TEMP()) %>%  
  ggplot(aes(year, value, color = type)) + 
  geom_line() + 
  facet_wrap("scenario", scales = "free") + 
  labs(title = var)


comp_data %>%  
  filter(scenario == "ssp119") %>% 
  filter(year %in% 1850:2015) %>% 
  select(year, value, type, variable) %>% 
  distinct() %>% 
  spread(type, value) %>% 
  mutate(percent_diff = 100 * (`co2 driven` - `free running`) / `co2 driven`) %>% 
  mutate(scenario = "historical") %>% 
  group_by(scenario, variable) %>% 
  summarise(min = min(percent_diff), 
            mean = mean(percent_diff), 
            max = max(percent_diff))
  pull(percent_diff) %>% 
  summary()
  
  
  comp_data %>%  
    filter(year > 2015) %>% 
   # filter(variable == ATMOSPHERIC_CO2()) %>% 
    filter(variable == GLOBAL_TEMP()) %>% 
    select(year, value, type, variable, scenario) %>% 
    distinct() %>% 
    spread(type, value) %>% 
    mutate(percent_diff = 100 * (`co2 driven` - `free running`) / `co2 driven`) %>% 
    group_by(scenario, variable) %>% 
    summarise(min = min(percent_diff), 
              mean = mean(percent_diff), 
              max = max(percent_diff))
  pull(percent_diff) %>% 
    summary()
