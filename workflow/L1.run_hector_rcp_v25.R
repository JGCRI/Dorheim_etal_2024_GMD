# Run Hector v 2.5 with the RCPs to get the comparison data for the different Hector versions.
# Note the user will have to provide a direct path to the location of the v25 root directory. 

# 0. Set Up ---------------------------------------------------------------------------------
library(dplyr)

V25_DIR <- "/Users/dorh012/Documents/Hector-Versions/v2/hector"
devtools::load_all(V25_DIR)
assertthat::assert_that(packageVersion("hector") == "2.5.0")

BASE_DIR <- here::here()

# Years to save 
yrs <- 1750:2100

# Variables to save
vars <- c(ATMOSPHERIC_CO2(), GLOBAL_TEMP(), NPP(), OCEAN_CFLUX(), NBP(),
          HEAT_FLUX(), RF_TOTAL(), RF_T_ALBEDO(), RF_CO2(),
          RF_SO2(), RF_OC(), RF_BC())

# 1. Run Hector -------------------------------------------------------------------------------
file.path(V25_DIR, "inst", "input") %>%
    list.files(pattern = "rcp", full.names = TRUE) %>%
    lapply(function(f){
        name <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(f))
        core <- newcore(f, name = name)
        run(core)
        out <- fetchvars(core, dates = yrs, vars = vars)
        return(out)
    }) %>%
    do.call(what = "rbind") ->
    rstls

# Format the data to be consistent with the v3 variable names. 
rstls %>% 
  filter(scenario %in% c("rcp26", "rcp45", "rcp60", "rcp85")) %>% 
  mutate(variable = if_else(variable == ATMOSPHERIC_CO2(), "CO2_concentration", variable)) %>% 
  mutate(variable = if_else(variable == "Tgav", "global_tas", variable)) %>% 
  mutate(variable = if_else(variable == "atm_land_flux", "NBP", variable)) %>% 
  mutate(variable = if_else(variable == "Ftot", "RF_tot", variable)) -> 
  rstls

version <- packageVersion(pkg = "hector")
rstls$version <- version

write.csv(rstls, file = file.path(BASE_DIR, "data", "hector_v25_rcp.csv"), row.names = FALSE)
