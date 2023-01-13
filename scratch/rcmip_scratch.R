library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
#library(hector)
devtools::load_all("~/projects/Hector-Versions/v3/hector/")

vars_to_keep <- c("Atmospheric Concentrations|CO2", "Surface Air Temperature Change", "Radiative Forcing")

RCMIP_BASE_DIR <- "/Users/dorh012/Documents/2022/rcmip-master"
source("R/0B.functions.R")

# 1. Hector RCMIP submission -------------------------------------------------------------------------------------------
file.path(RCMIP_BASE_DIR, "data", "results", "phase-1", "hector",  "rcmip_phase-1_hector_v3-0-0.xlsx") %>% 
  read_xlsx(sheet="your_data") -> 
  submitted_data

names(submitted_data) <- as.character(names(submitted_data))
yr_cols <- which(grepl(names(submitted_data), pattern = "1|2"))

submitted_data %>% 
  filter(grepl(pattern = "DEFAULT", ClimateModel)) %>% 
  filter(Region == "World") %>% 
  filter(Variable %in% vars_to_keep) %>% 
  pivot_longer(cols = yr_cols, values_transform = list(value = as.double)) %>% 
  select(scenario = Scenario, variable = Variable, unit = Unit, year = name, value) %>% 
  mutate(year = as.numeric(year)) -> 
  rcmip_submission

rcmip_submission$model <- "hector"
rcmip_submission$source <- "rcmip"

hector_rcmip_submission <- rcmip_submission


scns <- unique(hector_rcmip_submission$scenario)


# 2. MAGICC RCMIP submission -------------------------------------------------------------------------------------------
file.path(RCMIP_BASE_DIR, "data", "results", "phase-1", "magicc7",  "rcmip_phase-1_magicc7.1.0.beta_v1-0-0.csv") %>% 
  read.csv -> 
  submitted_data

names(submitted_data) <- as.character(names(submitted_data))
yr_cols <- which(grepl(names(submitted_data), pattern = "X"))

submitted_data %>% 
  filter(Region == "World") %>% 
  filter(Variable %in% vars_to_keep) %>% 
  filter(Scenario %in% scns) %>% 
  pivot_longer(cols = yr_cols, values_transform = list(value = as.double)) %>% 
  select(scenario = Scenario, variable = Variable, unit = Unit, year = name, value) %>% 
  mutate(year = as.numeric(gsub(pattern = "X", replacement = "", year))) -> 
  rcmip_submission

rcmip_submission$model <- "magicc"
rcmip_submission$source <- "rcmip"

magigcc_rcmip_submission <- rcmip_submission


# 3. AR5IR ---------------------------- 

file.path(RCMIP_BASE_DIR, "data", "results", "phase-1", "ar5ir",  "ar5ir-phase-1-results-v2-0-0.csv") %>% 
  read.csv -> 
  submitted_data

submitted_data %>% 
  filter(Region == "World") %>% 
  filter(Variable %in% vars_to_keep) %>% 
  filter(Scenario %in% scns) %>% 
  filter(Climatemodel == "ar5ir2box-ECS-3K") %>% 
  pivot_longer(cols = yr_cols, values_transform = list(value = as.double)) %>% 
  select(scenario = Scenario, variable = Variable, unit = Unit, year = name, value) %>% 
  mutate(year = as.numeric(gsub(pattern = "X", replacement = "", year))) -> 
  rcmip_submission

rcmip_submission$model <- "AR5IR"
rcmip_submission$source <- "rcmip"

ar5_rcmip_submission <- rcmip_submission


# 4. acc2 ---------------------------- 
file.path(RCMIP_BASE_DIR, "data", "results", "phase-1", "acc2",  "rcmip_phase-1_acc2_v2-0-1.xlsx") %>% 
  read_xlsx(sheet="your_data") -> 
  submitted_data

names(submitted_data) <- as.character(names(submitted_data))
yr_cols <- which(grepl(names(submitted_data), pattern = "1|2"))

submitted_data %>% 
  filter(Region == "World") %>% 
  filter(Variable %in% vars_to_keep) %>%   
  filter(Scenario %in% scns) %>% 
  pivot_longer(cols = yr_cols, values_transform = list(value = as.double)) %>% 
  select(scenario = Scenario, variable = Variable, unit = Unit, year = name, value) %>% 
  mutate(year = as.numeric(year)) -> 
  rcmip_submission

rcmip_submission$model <- "ACC2"
rcmip_submission$source <- "rcmip"

ACC2_rcmip_submission <- rcmip_submission


# 5. Fair ------------------------------------

file.path(RCMIP_BASE_DIR, "data", "results", "phase-1", "fair") %>% 
  list.files(pattern = "v1-0-1", full.names = TRUE) -> 
  fair_files
  
to_import <- fair_files[grepl(pattern = "default", x = fair_files)] 

to_import %>% 
  lapply(function(f){
    submitted_data <- read.csv(f)
    
    names(submitted_data) <- as.character(names(submitted_data))
    yr_cols <- which(grepl(names(submitted_data), pattern = "X"))
    
    submitted_data %>% 
      filter(Region == "World") %>% 
      filter(Variable %in% vars_to_keep) %>% 
      filter(Scenario %in% scns) %>% 
      pivot_longer(cols = yr_cols, values_transform = list(value = as.double)) %>% 
      select(scenario = Scenario, variable = Variable, unit = Unit, year = name, value) %>% 
      mutate(year = as.numeric(substr(x = year,start = 2, stop  = 5)))  
  }) %>% 
  do.call(what = "rbind") -> 
  rcmip_submission

rcmip_submission$model <- "fair"
rcmip_submission$source <- "rcmip"

fair_rcmip_submission <- rcmip_submission

# 6. Run Hector v3! -----------------------------------------------------------------------------------------
HECTOR_DATA_DIR <- "~/projects/2023/hectordata/inst/input"
list.files(HECTOR_DATA_DIR, pattern = "abrupt-4xCO2|ssp119|ssp585|1pctCO2", full.names = TRUE) %>% 
  lapply(function(ini){
    scn <- gsub(pattern = "-rcmip|rcmip|.ini|_", replacement = "", x = basename(ini))
    hc <- newcore(name = scn, ini)
    run(hc, runtodate = 2100)
    # looking at how the output was formatted it was in deed tas data
    out <- fetchvars(hc, vars = GLOBAL_TAS(), dates = 1750:2100)
    # out <- fetch_gmst(hc)
    # out$scenario <- scn
    return(out)
  }) %>% 
  do.call(what = "rbind") -> 
  hector_v3


# Plot ------------------------------------------------------------------------------------------------------------
# Plotting! 
hector_v3$model <- "hectorv3"
hector_v3$variable <- "Surface Air Temperature Change"
data <- bind_rows(magigcc_rcmip_submission, hector_rcmip_submission, ar5_rcmip_submission, ACC2_rcmip_submission, 
              fair_rcmip_submission, hector_v3)

data %>% 
  filter(variable == "Surface Air Temperature Change") %>% 
 filter(scenario %in% c("1pctCO2")) %>% 
  filter(year %in% c(1830:2100)) %>% 
  ggplot() + 
  geom_line(aes(year, value, color = model, group = interaction(scenario, model))) + 
  facet_wrap("scenario", scales = "free") 



ini <- list.files(HECTOR_DATA_DIR, pattern = "abrupt-4xCO2", full.names = TRUE) 
hc <- newcore(ini)
pre_indust <- fetchvars(hc, NA, vars = PREINDUSTRIAL_CO2())[["value"]]
run(hc)

out_co2 <- fetchvars(hc, 1745:2100, vars = c(CONCENTRATIONS_CO2(), GLOBAL_TAS()))


ggplot(out_co2) + 
  geom_line(aes(year, value)) +
  facet_wrap("variable", scales = "free")


df1 <- data.frame(year = 1745:1849, 
                 value = pre_indust,
                 variable = CO2_CONSTRAIN(), 
                 units = getunits(CO2_CONSTRAIN()))

df2 <- data.frame(year = 1850:2100, 
                  value = pre_indust * 4, 
                  variable = CO2_CONSTRAIN(), 
                  units = getunits(CO2_CONSTRAIN()))

abruptx4 <- rbind(df2)

setvar(hc, dates = abruptx4$year, values = abruptx4$value,
       var = abruptx4$variable, unit = abruptx4$units)
reset(hc)
run(core = hc, runtodate = 2100)

vars <- c(CONCENTRATIONS_CO2(), GLOBAL_TAS(), RF_TOTAL())
dates <- 1750:2100
out1 <- fetchvars(hc, vars = vars, dates = dates)
out1$scenario <- "Hector PI"


284.317 


ini <- list.files(HECTOR_DATA_DIR, pattern = "abrupt-4xCO2", full.names = TRUE) 
hc <- newcore(ini)

setvar(hc, dates = NA, var = PREINDUSTRIAL_CO2(), values = 284.317,
       unit = getunits(PREINDUSTRIAL_CO2()))


df1 <- data.frame(year = 1745:1849, 
                  value = 284.317,
                  variable = CO2_CONSTRAIN(), 
                  units = getunits(CO2_CONSTRAIN()))

df2 <- data.frame(year = 1850:2100, 
                  value = 284.317 * 4, 
                  variable = CO2_CONSTRAIN(), 
                  units = getunits(CO2_CONSTRAIN()))

abruptx4 <- rbind(df2)

setvar(hc, dates = abruptx4$year, values = abruptx4$value,
       var = abruptx4$variable, unit = abruptx4$units)

run(hc, runtodate = 2100)


out2 <- fetchvars(hc, vars = vars, dates = dates)
out2$scenario <- "rcmip PI"

rbind(out1, out2) %>% 
  ggplot(aes(year, value, color = scenario)) + 
  geom_line() + 
  facet_wrap("variable", scales = "free")

