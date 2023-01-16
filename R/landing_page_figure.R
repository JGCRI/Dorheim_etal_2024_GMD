
library(assertthat)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(readxl)
#remotes::install_github("jgcri/hector@v3_dev")
#library(hector)
devtools::load_all("~/projects/Hector-Versions/v3/hector/")

# Load some custom helpful functions
source(here::here("R", "0B.functions.R"))


BASE_DIR <- here::here()
FIGS_DIR  <- here::here("output", "figures")
INPUT_DIR <- here::here("data")
HECTOR_RSLTS_DIR <- here::here("output", "hector_output")
HECTOR_DATA_DIR <- "~/Documents/2022/hectordata/"

THEME_BASE_SIZE <- 10
theme_set(theme_bw(base_size = THEME_BASE_SIZE))
FULL_FIG <- data.frame(width = 7, height = 7)
MANUSCRIPT_FIG <- data.frame(width = 4, height = 3)


COLOR_SCHEME <- c("grey" = "#999999", "col1" = "#56B4E9", "black" = "#000000",
                  "#009E73", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
SSP_COLORS <- c("ssp119" = "#00a9cf", "ssp126" = "#003466", "ssp245" = "#f69320",
                "ssp370" = "#df0000", "ssp434" = "#2274ae","ssp460" = "#b0724e",
                "ssp585"= "#980002", "historical" = "#000000", "historical"="#92397a")
OBS_SCHEME <- c("obs" = "grey", "Hector v3" = "#E69F00")


list.files(HECTOR_RSLTS_DIR, pattern = "ssp", full.names = TRUE) %>%
  lapply(read.csv) %>%
  do.call(what = "rbind") %>%  
  filter(version == "3.0.0") -> 
  hector_ssp

here::here(INPUT_DIR, "Supplementary_Table_UoM_GHGConcentrations-1-1-0_annualmeans_v23March2017.csv") %>%  
  read.csv(skip = 21) %>% 
  na.omit %>% 
  select(year =  "v.YEARS.GAS..", value = "CO2") %>% 
  mutate(variable = "co2 obs") -> 
  cmip6_co2_obs

hector_ssp %>% 
  filter(!grepl(pattern = "conc", x = scenario)) %>% 
  filter(variable == CONCENTRATIONS_CO2()) %>% 
  mutate(scenario = if_else(year <= 2016, "historical", scenario)) -> 
  hector_co2_conc

ggplot() +
  geom_line(data = hector_co2_conc, aes(year, value, color = scenario), size = 0.75) + 
  geom_line(data = cmip6_co2_obs, aes(year, value,  color = "Observations", linetype = "Observations"), size = 1) + 
  geom_vline(xintercept = 2021, linetype = 2) +
  geom_point(aes(x = 2021, y = 414.72), size = 2) + 
  scale_color_manual(values = c(SSP_COLORS, "Observations" = "green")) + 
  scale_linetype_manual(values = c("Observations" = 2)) + 
  labs(y = expression(CO[2]~" concentrations (ppmv)"), x = NULL) + 
  theme(legend.position = "none")




