# Objective: Download the v2.3.0 run Hector and format results so that they have the proper names 
# to plot with the Hector v3 output data. 
# 0. Set Up --------------------------------------------------------------------------------
# Should be the developmental branch
#devtools::install_github("jgcri/hector@luc")
#library(hector)
devtools::load_all("~/Documents/Hector-Versions/luc/hector")
library(dplyr)

BASE_DIR <- here::here()
OUTPUT_DIR <- file.path(BASE_DIR, "output", "hector_output")

version <- packageVersion("hector")
assertthat::assert_that(version == "3.0.0")

source(file.path(BASE_DIR, "R", "0B.functions.R"))

# 1. Run & Save Hector ---------------------------------------------------------------------
# Years & variables to save. 
yrs <- 1750:2100
vars <- c(CONCENTRATIONS_CO2(), GLOBAL_TAS(), HEAT_FLUX(), RF_TOTAL(), RF_ALBEDO(), 
          RF_CO2(), RF_SO2(), RF_OC(), RF_BC(), LAND_TAS(), SST(), FFI_EMISSIONS(), DACCS_UPTAKE(), 
          LUC_EMISSIONS(), LUC_UPTAKE())
params <- c(ECS(), DIFFUSIVITY(), AERO_SCALE(), VOLCANIC_SCALE(), BETA(), Q10_RH())

# Run Hector v3 with the SSP scenarios 
file.path(BASE_DIR, "input") %>%  
  list.files(pattern = "ssp", full.names = TRUE) %>% 
  lapply(function(f){
    name <- gsub(pattern = "rcmip_|.ini", replacement = "", x = basename(f))
    core <- newcore(f, name = name)
    run(core)
    out1 <- fetchvars(core, dates = yrs, vars = vars)
    out2 <- fetchvars(core, dates = NA, vars = params)
    out3 <- fetch_gmst(core)
    out3$scenario <- name
    out3$units <- "degC"
    fetchvars(core, dates = yrs, vars = c(LUC_EMISSIONS(), NBP())) %>% 
      group_by(scenario, year, units) %>% 
      summarise(value = sum(value)) %>% 
      ungroup %>% 
      mutate(variable = "land sink") -> 
      out4
    out <- rbind(out1, out2, out3, out4)
    return(out)
  }) %>%
  do.call(what = "rbind") ->
  rstls

version <- packageVersion(pkg = "hector")
rstls$version <- version
  
file <- paste0("hector_", version, "_ssp.csv")
write.csv(rstls, file = file.path(OUTPUT_DIR, file), row.names = FALSE)

# Run Hector v3 with the rcp scenarios for the RCP scenarios
file.path(BASE_DIR, "input") %>%  
  list.files(pattern = "RCP", full.names = TRUE) %>% 
  lapply(function(f){
    name <- gsub(pattern = "v25_|.ini", replacement = "", x = basename(f))
    core <- newcore(f, name = name)
    run(core)
    out1 <- fetchvars(core, dates = yrs, vars = vars)
    out2 <- fetchvars(core, dates = NA, vars = params)
    out <- rbind(out1, out2)
    return(out)
  }) %>%
  do.call(what = "rbind") ->
  rstls

version <- packageVersion(pkg = "hector")
rstls$version <- version

file <- paste0("hector_", version, "_rcp.csv")
write.csv(rstls, file = file.path(OUTPUT_DIR, file), row.names = FALSE)
