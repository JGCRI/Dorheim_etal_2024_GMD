# Misc comparisons that may or may not be helpful idk 

library(assertthat)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(readxl)

BASE_DIR <- here::here()
FIGS_DIR  <- here::here("output", "figures")
INPUT_DIR <- here::here("data")
HECTOR_RSLTS_DIR <- here::here("output", "hector_output")

# okay so they are pretty similar to one another espeically when you correct for the refernce period!

# NASA GISS
read.csv(file.path(INPUT_DIR, "calibration", "GISTEMP_v4_20221014.csv"), skip = 1) %>% 
  select(year = Year, value = `J.D`) %>% 
  mutate(value = as.numeric(value)) %>% 
  na.omit() %>% 
  mutate(variable = "gmst") -> 
  giss_obs_data

giss_obs_data %>% 
  filter(year %in% 1961:1990) %>% 
  pull(value) %>% 
  mean -> 
  giss_ref

giss_obs_data %>% 
  mutate(value = value - giss_ref) -> 
  new_giss_obs_data


# Hadcrut5
# Global mean surface temperature anomaly
# https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/download.html
# The temperature anomaly is based off of 1961–1990
# surface temperature with the anomaly!
here::here(INPUT_DIR, "HadCRUT5.csv") %>%
  read.csv(stringsAsFactors = FALSE) ->
  hadcrut_obs_data
names(hadcrut_obs_data) <- c("year", "value", "lower", "upper")


ggplot() + 
  geom_line(data = new_giss_obs_data, aes(year, value, color = "giss new")) + 
  geom_line(data = giss_obs_data, aes(year, value, color = "giss og")) + 
  
  geom_line(data = hadcrut_obs_data, aes(year, value, color = "hadcrut"))


