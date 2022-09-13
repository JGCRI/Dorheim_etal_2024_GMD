# Author: Dorheim 
# Date: March 2022 
# Objective: Scratch comparison of CMIP6 temperatures with Hector temperature. 

# 0. Set Up ---------------------------------------------------------------------------------------------
library(dplyr)
remotes::install_github("jgcri/hector@land_tracking")
library(hector)
library(ggplot2)

theme_set(theme_bw())

BASE_DIR <- here::here()
COMPARISON_DIR <- file.path(BASE_DIR, "data", "comparison")

# 1. CMIP6 Data -----------------------------------------------------------------------------------------

cmip6_co2 <- read.csv(file.path(COMPARISON_DIR, "CMIP6", "cmip6_annual_co2.csv"), stringsAsFactors = FALSE)

scns <- c("ssp585", "historical")

to_discard <- c("CESM2", "NorESM2-LM", "NorESM2-MM")

cmip6_co2 %>%
  dplyr::filter(experiment %in% scns) %>% 
  filter(!model %in% to_discard ) %>% 
  dplyr::mutate(experiment = "ssp585") %>% 
  mutate(value = 1e6 * value) -> 
  cmip6_co2_to_plot


# 2. Read & format the co2 obs --------------------------------------------------------------------------

obs <- read.csv(file.path(COMPARISON_DIR, "monthly_in_situ_co2_mlo.csv"), stringsAsFactors = FALSE, skip = 57)
names(obs) <- c("year", "month", "date", "date", "x", "x", "x", "x", "x", "value")
obs %>% 
  select(year, month, value) %>% 
  filter(value != -99.99) %>% 
  filter(year %in% 1959:2021) %>% 
  group_by(year) %>% 
  summarise(value = mean(value)) %>% 
  ungroup -> 
  obs_data




# 2. Run Hector -----------------------------------------------------------------------------------------

hc <- newcore(here::here("input", "rcmip_ssp585_emiss.ini"))
run(hc)
hector_out1 <- fetchvars(hc, 1850:2100, ATMOSPHERIC_CO2())

hc <- newcore(here::here("input", "rcmip_ssp585_conc.ini"))
run(hc)
hector_out2 <- fetchvars(hc, 1850:2100, ATMOSPHERIC_CO2())
fetchvars(hc, dates = 1700:1959, vars = ATMOSPHERIC_CO2())

hc <- newcore(here::here("input", "rcmip_ssp585.ini"))


run(hc)
hector_out3 <- fetchvars(hc, 1850:2100, ATMOSPHERIC_CO2())



# 3. Comparison -----------------------------------------------------------------------------------------

ggplot() + 
  geom_line(data = cmip6_co2_to_plot, aes(year, value, color = "CMIP6", 
                                          group = interaction(model, ensemble)), size = 1) + 
  geom_line(data = hector_out1, aes(year, value, color = "Hector emiss"), size = 1) + 
  geom_line(data = hector_out2, aes(year, value, color = "Hector conc"), size = 1) + 
  geom_line(data = obs_data, aes(year, value, color = "Mauna Loa Obs"), size = 1) + 
  scale_color_manual(values = c("CMIP6" = "grey", "Hector emiss" = "black", "Hector conc" = "blue", 
                                "Mauna Loa Obs" = "green")) + 
  labs(y = "ppm CO2", 
       x = NULL) 

