# TODO other things to look at 
# concenatrionss from inputs 
# 
# 0. Set Up --------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
remotes::install_github("jgcri/hector@v3_dev")
library(hector)

COMPARISON_DIR <- here::here("data", "comparison")

# Set the color theme 
theme_set(theme_bw())

## Parameters from BBL's calibration. 
##        BETA      Q10_RH   NPP_FLUX0  AERO_SCALE         ECS DIFFUSIVITY 
##    0.160000    1.519490   55.911591    0.500000    2.724023    2.393617

# Set up a hector core with the results from the optim 
# Args 
#   hc: hector core object
# Return: hector core with the variables 
set_optim_params <- function(hc){
  
  vals <- c(0.160000, 1.519490, 55.911591, 0.500000, 2.724023, 2.393617)
  pnames <- c(BETA(), Q10_RH(), NPP_FLUX0(), AERO_SCALE(), ECS(), DIFFUSIVITY())
  units <- c("(unitless)", "(unitless)", "Pg C/yr", "(unitless)", "degC", "cm2/s")
  dates <- rep(NA, length.out = length(vals))
  
  mapply(function(v, p, u){
    setvar(core = hc, dates = NA, var = p, values = v, unit = u)
  }, v = vals, p = pnames, u = units )
  reset(hc)
  return(hc)
  
}

# 1. Run Hector ------------------------------------------------------------------------------
vars_to_keep <- c(GLOBAL_TAS(), NPP(), ATMOSPHERIC_CO2(), RF_TOTAL(),
                  HEAT_FLUX(), RH(), PH(), SST()) 
pnames <- c(BETA(), Q10_RH(), NPP_FLUX0(), AERO_SCALE(), ECS(), DIFFUSIVITY())

system.file("input", package = "hector") %>% 
  list.files(pattern = ".ini", full.names = TRUE) -> 
  inis

# Set up the Hector core list. 
inis %>% 
  lapply(function(p){
    
    scn <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(p))
    hc <- newcore(p, name = scn)
    return(hc)
    
  }) -> 
  hc_list

# Run Hector and fetch results. 
hc_list %>% 
  lapply(function(core){
    
    run(core)
    out <- rbind(fetchvars(core, 1750:2100, vars = vars_to_keep), 
                 fetchvars(core, dates = NA, vars = pnames)) 
    return(out)
    
  }) %>% 
  do.call(what = "rbind") -> 
  hector_old

# Reset Hector params to the optims.  
hc_list %>% 
  lapply(function(core){
    
    core <- set_optim_params(core)
    run(core)
    out <- rbind(fetchvars(core, 1750:2100, vars = vars_to_keep), 
                 fetchvars(core, dates = NA, vars = pnames)) 
    return(out)
    
  }) %>% 
  do.call(what = "rbind") -> 
  hector_new

hector_old$params <- "old default"
hector_new$params <- "optim params"

to_plot <- rbind(hector_old, hector_new)


# 2. Compare Hector Results ------------------------------------------------------------------------------
# How do the Hector results compare with one another? 
plots <- list()

for(v in vars_to_keep){
  to_plot %>%  
    filter(variable == v) %>% 
    ggplot() + 
    geom_line(aes(year, value, color = params, linetype = params)) + 
    facet_wrap("scenario", scales = "free") + 
    labs(title = v) -> 
    plots[[v]]
}

to_plot %>% 
  select(-scenario) %>% 
  distinct %>% 
  filter(variable %in% pnames) %>%
  ggplot(aes(variable, value, color = params)) + 
  geom_point() + 
  facet_wrap("variable", scales = "free")

for(v in vars_to_keep){
  to_plot %>%  
    filter(variable == v) %>% 
    ggplot() + 
    geom_line(aes(year, value, color = params, linetype = params)) + 
    facet_wrap("scenario", scales = "free") + 
    labs(title = v) -> 
    plots[[v]]
}


# 3. Compare with CMIP6 ------------------------------------------------------------------------------
## 3.A CO2 -------------------------------------------------------------------------------------------
# Now lets do some comparisons with CMIP results because we want to take a look at the future stuff 
cmip6_co2 <- read.csv(file.path(COMPARISON_DIR, "CMIP6", "cmip6_annual_co2.csv"), stringsAsFactors = FALSE)

scns <- c("ssp585", "historical")
to_discard <- c("CESM2", "NorESM2-LM", "NorESM2-MM")

cmip6_co2 %>%
  dplyr::filter(experiment %in% scns) %>% 
  filter(!model %in% to_discard ) %>% 
  dplyr::mutate(experiment = "ssp585") %>% 
  mutate(value = 1e6 * value) -> 
  cmip6_co2_to_plot

to_plot %>% 
  filter(scenario == "ssp585" & variable == ATMOSPHERIC_CO2()) -> 
  hector_cmip6_co2

ggplot() + 
  geom_line(data = cmip6_co2_to_plot, aes(year, value, group = interaction(ensemble, model, experiment)), color = "grey") + 
  geom_line(data = hector_cmip6_co2, aes(year, value, color = params), size = 1, alpha = 0.7) + 
  labs(title = "[CO2]", y = NULL, x = NULL) + 
  coord_cartesian(xlim = c(1850, 2100))

## 3.B Temperature -----------------------------------------------------------------------------------
tas_global <- read.csv(file.path(COMPARISON_DIR, "CMIP6", "cmip6_annual_tas_global.csv"), 
                       stringsAsFactors = FALSE)

exps <- setdiff(unique(tas_global$experiment), c("historical", "abrupt-4xCO2", "abrupt-2xCO2", "1pctCO2"))

  lapply(exps, function(exp){
    tas_global %>% 
      filter(experiment == "historical" & variable == "Tgav") %>% 
      mutate(experiment = exp, 
             scenario = exp) -> 
      out 
    return(out)
  }) %>% 
    do.call(what = "rbind") %>% 
    bind_rows(tas_global) -> 
    tas_global

tas_global %>% 
  dplyr::filter(experiment %in% c("ssp126", "ssp245", "ssp370",  
                                  "ssp585", "ssp119" , "ssp434", "ssp460")) %>% 
  dplyr::mutate(scenario = experiment) %>% 
  select(model, scenario, ensemble, variable, value = Tgav, year) %>% 
  mutate(variable = GLOBAL_TEMP()) %>% 
  group_by(model, scenario, year) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  filter(year <= 2100) %>% 
  mutate(experiment = scenario) -> 
  cmip6_tas

to_plot %>% 
  filter(variable == GLOBAL_TEMP()) %>% 
  mutate(experiment = scenario) -> 
  hector_temp

ggplot() + 
  geom_line(data = cmip6_tas, aes(year, value, group = interaction(model, scenario)), color = "grey") + 
  geom_line(data = hector_temp, aes(year, value, color = params)) + 
  facet_wrap("experiment", scales = "free")

cmip6_tas %>% 
  group_by(experiment, year) %>% 
  summarise(min = min(value), 
            max = max(value), 
            mean = mean(value)) %>% 
  ungroup -> 
  cmip6_summary_data


ggplot() + 
  geom_ribbon(data = cmip6_summary_data, aes(x = year, ymin = min, ymax = max), alpha = 0.4) + 
  geom_line(data = cmip6_summary_data, aes(year, mean), size = 0.75) + 
  geom_line(data = hector_temp, aes(year, value, color = params), size = 0.75) + 
  facet_wrap("experiment", scales = "free") + 
  labs(title = "TAS Hector vs Multi Model Mean", y = NULL, x = NULL)
  

## 3.C OCEAN TEMP -----------------------------------------------------------------------------------

cmip6_tos <- read.csv(file.path(COMPARISON_DIR, "CMIP6", "cmip6_annual_tos.csv"), stringsAsFactors = FALSE) %>% 
  rename(scenario = experiment) %>% 
  filter(year <= 2100)

cmip6_tos %>% 
  group_by(scenario, year, model) %>% 
  dplyr::summarise(value = mean(value)) %>% 
  ungroup() %>% 
  mutate(scenario = if_else(year <= 2019, "ssp119", scenario)) %>%  
  mutate(experiment = scenario) -> 
  cmip6_tos

to_plot %>% 
  filter(variable == OCEAN_SURFACE_TEMP()) %>% 
  mutate(experiment = scenario) -> 
  hector_tos

ggplot() + 
  geom_line(data = cmip6_tos, aes(year, value, group = model), color = "grey") + 
  geom_line(data = hector_tos, aes(year, value, color = params)) + 
  facet_wrap("experiment", scales = "free")


cmip6_tos %>% 
  group_by(year, experiment) %>% 
  summarise(min = min(value), 
            max = max(value), 
            mean = mean(value)) %>% 
  ungroup ->
  cmip6_tos_summary

ggplot() + 
  geom_ribbon(data = cmip6_tos_summary, aes(x = year, ymin = min, ymax = max), alpha = 0.4) + 
  geom_line(data = cmip6_tos_summary, aes(year, mean), size = 0.75) + 
  geom_line(data = hector_tos, aes(year, value, color = params), size = 0.75) + 
  facet_wrap("experiment", scales = "free") + 
  labs(title = "TOS Hector vs Multi Model Mean", y = NULL, x = NULL)


# 4. Other comparisons ----------------------------------------------------
## 4.A [CO2] --------------------------------------------------------------
to_plot %>% 
  filter(variable == ATMOSPHERIC_CO2()) -> 
  hector_cmip6_co2


system.file("input/tables", package = "hector") %>% 
  list.files(pattern = "emiss-constraints", full.names = TRUE) %>% 
  lapply(function(f){
    scn <- gsub(pattern = "_emiss-constraints_rf.csv", replacement = "", x = basename(f))
    
    read.csv(f, comment.char =  ";") %>% 
      select(year = Date, CO2 = CO2_constrain) %>%  
      mutate(scenario = scn) -> 
      co2_iiasa
    return(co2_iiasa)
  }) %>% 
  do.call(what = "rbind") %>% 
  filter(year <= 2100) ->
  co2_conc

ggplot() + 
  geom_line(data = co2_conc, aes(year, CO2)) + 
  geom_line(data = hector_cmip6_co2, aes(year, value, color = params)) + 
  facet_wrap("scenario", scales = "free")




