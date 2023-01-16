# Objective: Download the v2.3.0 run Hector and format results so that they have the proper names 
# to plot with the Hector v3 output data. 
# 0. Set Up --------------------------------------------------------------------------------
remove.packages("hector")
devtools::install_github("jgcri/hector@v2.3.0", force = TRUE)
library(dplyr)
library(hector)

assertthat::assert_that(packageVersion(pkg = "hector") == "2.3.0")

BASE_DIR <- here::here()

# Years & variables to save. 
yrs <- 1750:2100
vars <- c(ATMOSPHERIC_CO2(), GLOBAL_TEMP(), HEAT_FLUX(), RF_TOTAL(), RF_T_ALBEDO(), RF_CO2(),
          RF_SO2(), RF_OC(), RF_BC())
params <- c(ECS(), DIFFUSIVITY(), AERO_SCALE(), VOLCANIC_SCALE(), BETA(), Q10_RH())

# 01. Run Hector & format --------------------------------------------------------------------------------
# Run Hector, fetch output and format. 
list.files(system.file("input", package = "hector"), pattern = "rcp", full.names = TRUE) %>%
  lapply(function(f){
    name <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(f))
    core <- newcore(f, name = name)
    run(core)
    out1 <- fetchvars(core, dates = yrs, vars = vars)
    out2 <- fetchvars(core, dates = NA, vars = params)
    out <- rbind(out1, out2)
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

