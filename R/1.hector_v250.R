# Objective: Download the v2.3.0 run Hector and format results so that they have the proper names 
# to plot with the Hector v3 output data. 
# 0. Set Up --------------------------------------------------------------------------------
#devtools::install_github("jgcri/hector@62381e7", force = TRUE)
HECTOR_DIR <- "~/projects/Hector-Versions/v2/hector"
devtools::load_all(HECTOR_DIR)
library(dplyr)
library(hector)
source(file.path("R", "0B.functions.R"))
assertthat::assert_that(packageVersion(pkg = "hector") == "2.5.0")

BASE_DIR <- here::here()

# 1. Run & Save Hector ---------------------------------------------------------------------
# Years & variables to save. 
yrs <- 1750:2100
vars <- c(ATMOSPHERIC_CO2(), GLOBAL_TEMP(), HEAT_FLUX(), RF_TOTAL(), RF_T_ALBEDO(), RF_CO2(),
          RF_SO2(), RF_OC(), RF_BC())
params <- c(ECS(), DIFFUSIVITY(), AERO_SCALE(), VOLCANIC_SCALE(), BETA(), Q10_RH())

# Run Hector, fetch output and format. 
list.files(file.path(HECTOR_DIR, "inst", "input"), pattern = "rcp", full.names = TRUE) %>%
  lapply(function(f){
    name <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(f))
    core <- newcore(f, name = name)
    run(core)
    out1 <- fetchvars(core, dates = yrs, vars = vars)
    out2 <- fetchvars(core, dates = NA, vars = params)
    
    flnd <- 0.29
    fetchvars(core, dates = yrs, vars = c("Tgav_land", "Tgav_ocean_ST")) %>% 
      select(year, variable, value) -> 
      output
    
    output <- reshape(output, direction = "wide", idvar = "year", timevar = "variable")
    names(output) <- gsub("value.", "", names(output))
    
    output$value <- with(output, (Tgav_land * flnd + Tgav_ocean_ST * (1 - flnd)))
    output <- subset(output, select = c(year, value))
    output$variable <- "gmst"
    output$scenario <- unique(out2$scenario)
    output$units <- "deg C"
    out <- rbind(out1, out2, output)
    return(out)
  }) %>%
  do.call(what = "rbind") ->
  rstls


# Format the data to be consistent with the v3 variable names. 
rstls %>% 
  filter(!grepl(pattern = "_constrained", x = scenario)) %>% 
  filter(scenario %in% c("rcp26", "rcp45", "rcp60", "rcp85")) %>% 
  mutate(variable = if_else(variable == ATMOSPHERIC_CO2(), "CO2_concentration", variable)) %>% 
  mutate(variable = if_else(variable == "Tgav", "global_tas", variable)) %>% 
  mutate(variable = if_else(variable == "atm_land_flux", "NBP", variable)) %>% 
  mutate(variable = if_else(variable == "Ftot", "RF_tot", variable)) -> 
  rstls

version <- packageVersion(pkg = "hector")
rstls$version <- version

file <- paste0("hector_", version, "_rcp.csv")
write.csv(rstls, file = file.path(BASE_DIR, "output", "hector_output", file), row.names = FALSE)
