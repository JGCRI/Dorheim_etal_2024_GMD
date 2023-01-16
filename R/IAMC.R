# Objective: Produce the figures used in the the IAMC/AGU poster 2022 

# 0. Set Up ----------------------------------------------------------------
library(assertthat)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(readxl)

# Load Hector v3 
devtools::load_all("~/projects/Hector-Versions/v3/hector/")

# Load some custom helpful functions
source(here::here("R", "0B.functions.R"))

BASE_DIR <- here::here()
FIGS_DIR  <- here::here("output", "figures")
INPUT_DIR <- here::here("data")
HECTOR_RSLTS_DIR <- here::here("output", "hector_output")

theme_set(theme_bw(base_size=15) + 
            theme(legend.title = element_blank()))
FULL_FIG <- data.frame(width = 7, height = 7)


COLOR_SCHEME <- c("Hector v 3" = "#56B4E9", 
                  "Hector v 2.5" = "#009E73", 
                  "Observations" = "black")

# 1. Import Data ----------------------------------------------------------------
list.files(HECTOR_RSLTS_DIR, pattern = "rcp", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  do.call(what = "rbind") -> 
  hector_rcp

list.files(HECTOR_RSLTS_DIR, pattern = "hector_3.0.0_ssp-2e1e8ad7", full.names = TRUE) %>%
  lapply(read.csv) %>%
  do.call(what = "rbind") ->
  hector_ssp



# 2. Plots ----------------------------------------------------------------
# Mauna Loa CO2 - downloaded 2022-05-05 from https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.csv
read_csv(file.path(INPUT_DIR, "calibration", "co2_annmean_mlo.csv"),
         col_types = "ddd", comment = "#") %>% 
  rename(value = mean) %>% 
  mutate(which = "Mauna Loa") ->
  co2_obs

hector_rcp %>% 
  filter(version == "2.5.0" & variable == CONCENTRATIONS_CO2()) %>% 
  filter(year %in% 1945:2025) %>% 
  filter(scenario == "rcp45") -> 
  hectorv25_co2

hector_ssp %>% 
  filter(version == "3.0.0" & variable == CONCENTRATIONS_CO2()) %>% 
  filter(scenario == "ssp585") %>% 
  filter(year %in% 1945:2025) -> 
  hectorv3_co2

ggplot() + 
  geom_line(data = hectorv3_co2, aes(year, value, color = "Hector v 3", group = scenario), size = 2) + 
  geom_line(data = hectorv25_co2, aes(year, value, color = "Hector v 2.5"), size = 2) + 
  geom_line(data = co2_obs, aes(year, value, color = "Observations"), size = 2) + 
  scale_color_manual(values = COLOR_SCHEME) + 
  theme_bw(base_size = 40) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(y = NULL, title = NULL, x = NULL) -> 
  co2_plot

ggsave(co2_plot, filename = "./output/IAMC-figs/co2_obs_w_legend.png", width = 10, height = 8)

co2_plot + 
  theme(legend.position = "none") -> 
  co2_plot_plot
ggsave(co2_plot_plot, filename = "./output/IAMC-figs/co2_obs.png", width = 10, height = 8)


# Read in some helper functions that are used.
source(file.path(BASE_DIR, "R", "0B.functions.R"))

# Lenssen, N., G. Schmidt, J. Hansen, M. Menne, A. Persin, R. Ruedy, and D. Zyss, 2019: 
# Improvements in the GISTEMP uncertainty model. J. Geophys. Res. Atmos., 124, no. 12, 
# 6307-6326, doi:10.1029/2018JD029522.
read.csv(file.path(INPUT_DIR, "calibration", "GISTEMP_v4_20221014.csv"), skip = 1) %>% 
  select(year = Year, value = `J.D`) %>% 
  mutate(value = as.numeric(value)) %>% 
  na.omit() %>% 
  mutate(variable = "gmst") -> 
  gmst_obs_data

hector_ssp %>% 
  filter(scenario == "ssp119") %>% 
  filter(version == "3.0.0" & variable == "gmst") %>% 
  normalize_to_giss() %>% 
  filter(year %in% 1880:2021)-> 
  hector_gmst

hector_rcp %>% 
  filter(version == "2.5.0" & variable == "gmst") %>% 
  filter(year %in% 1880:2021) %>% 
  filter(scenario == "rcp45") %>% 
  normalize_to_giss() -> 
  hector_gmst_25

ggplot() + 
  geom_line(data = hector_gmst, aes(year, value, color = "Hector v 3"), size = 2.5) + 
  geom_line(data = hector_gmst_25, aes(year, value, color = "Hector v 2.5"), size = 2.5) + 
  geom_line(data = gmst_obs_data, aes(year, value, color = "Observations"), size = 2.5) + 
  scale_color_manual(values = COLOR_SCHEME) + 
  theme_bw(base_size = 40) +
  labs(y = NULL, title = "GMST", x = NULL) + 
  theme(legend.position = "bottom", legend.title = element_blank()) -> 
  gmst_plot
  
  
  ggsave(gmst_plot, filename = "./output/IAMC-figs/gmst_obs_w_legend.png", width = 10, height = 8)

  gmst_plot + 
  theme(legend.position = "none") -> 
  gmst_plot

ggsave(gmst_plot, filename = "./output/IAMC-figs/gmst_obs.png", width = 10, height = 8)




scns <- c("ssp245", "ssp126", "ssp585")


models_in_very_likey <- c("CAMS-CSM1-0", "NorESM2-MM", "MRI-ESM2-0", "ACCESS-CM2", "CESM2-WACCM")

read.csv(here::here("data", "cmip6_model_means.csv")) %>% 
 # filter(model %in% models_in_very_likey) %>% 
  dplyr::filter(scenario %in% scns) -> 
  cmip6_rslts 

hector_ssp %>% 
  filter(year %in% 1950:2100) %>% 
  filter(scenario %in% scns) %>% 
  filter(version == "3.0.0") %>% 
  filter(variable %in% c(SST(), GLOBAL_TAS(), "land_tas")) %>% 
  mutate(scenario = if_else(year <= 2015, "historical", scenario)) -> 
  hector_temp

ggplot() + 
  geom_line(data = cmip6_rslts, aes(year, value, group = interaction(model, scenario))) +
  geom_line(data = hector_temp, aes(year, value, color = "Hector v3", group = scenario)) + 
  facet_grid(~variable,  scales = "free") + 
  labs(y = NULL, x = NULL)


cmip6_rslts %>% 
  group_by(scenario, year, variable) %>% 
  summarise(ymin = min(value), ymax = max(value)) %>% 
  filter(year %in% 1950:2100) %>% 
  ungroup %>% 
  mutate(scenario = if_else(year <= 2015, "historical", scenario)) -> 
  ccmip6_ribbons


SSP_COLORS <- c("ssp119" = "#00a9cf", "ssp126" = "#003466", "ssp245" = "#f69320",
                "ssp370" = "#df0000", "ssp434" = "#2274ae","ssp460" = "#b0724e",
                "ssp585"= "#980002", "historical" = "#000000", "historical"="#92397a")

ggplot() + 
  geom_ribbon(data = ccmip6_ribbons, aes(year, ymin = ymin, 
                                         ymax = ymax, fill = scenario), alpha = 0.3) + 
  geom_line(data = hector_temp, aes(year, value, color = scenario, linetype = "Hector v 3"), size = 2) + 
  facet_wrap("variable") + 
  scale_fill_manual(values = SSP_COLORS) + 
  labs(y = NULL, x = NULL) + 
  theme(legend.position = "bottom") -> 
  cmip6_plots

ggsave(cmip6_plots, filename = "./output/IAMC-figs/cmip6_w_legend.png", width = 10, height = 8)


cmip6_plots +   
  scale_color_manual(values = SSP_COLORS) + 
  theme_bw(base_size = 40) +
  theme(legend.position = "none", 
           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))->
  cmip6_plot_plot
  
ggsave(cmip6_plot_plot, filename = "./output/IAMC-figs/cmip6.png", width = 24, height = 14)

