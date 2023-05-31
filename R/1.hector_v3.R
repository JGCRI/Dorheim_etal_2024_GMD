# Objective: Run Hector for the various ssp scenarios/experiments that will be used in 
# the manuscript. 

# 0. Set Up --------------------------------------------------------------------------------
library(assertthat)
library(dplyr)
library(tidyr)
library(hector)
version <- packageVersion("hector")
assertthat::assert_that(version == "3.1.1")

# TODO
# Check the calibration values 
# load("output/calibration-Mon_Jan_16_15:33:01_2023.rda")

source(file.path(BASE_DIR, "R", "0B.functions.R"))

BASE_DIR <- here::here()
OUTPUT_DIR <- file.path(BASE_DIR, "output", "hector_output")



# 1. Emission Driven Hector ---------------------------------------------------------------------
# Years & variables to save. 
yrs <- 1750:2100
vars <- c(CONCENTRATIONS_CO2(), GLOBAL_TAS(), HEAT_FLUX(), RF_TOTAL(), RF_ALBEDO(), 
          RF_CO2(), RF_SO2(), RF_OC(), RF_BC(), LAND_TAS(), SST(), FFI_EMISSIONS(),
          DACCS_UPTAKE(), LUC_EMISSIONS(), LUC_UPTAKE(), NBP(), OCEAN_UPTAKE(), NPP(), PH(), GMST())
params <- c(ECS(), DIFFUSIVITY(), AERO_SCALE(), VOLCANIC_SCALE(), BETA(), Q10_RH())

# Run Hector v3 with the SSP scenarios 
system.file("input", package = "hector") %>% 
  list.files(pattern = "ssp", full.names = TRUE) %>% 
  lapply(function(f){
    name <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(f))
    core <- newcore(f, name = name)
    run(core)
    out1 <- fetchvars(core, dates = yrs, vars = vars)
    out2 <- fetchvars(core, dates = NA, vars = params)
    fetchvars(core, dates = yrs, vars = c(LUC_EMISSIONS(), NBP())) %>% 
      group_by(scenario, year, units) %>% 
      summarise(value = sum(value)) %>% 
      ungroup %>% 
      mutate(variable = "land sink") -> 
      out4
    out <- rbind(out1, out2, out4)
    return(out)
  }) %>%
  do.call(what = "rbind") ->
  rstls

version <- packageVersion(pkg = "hector")
rstls$version <- version

file <- paste0("hector_", version, "_ssp.csv")
write.csv(rstls, file = file.path(OUTPUT_DIR, file), row.names = FALSE)


# 3. Run Concentration Driven RCP Hector --------------------------------------------------------
# Years & variables to save. 
yrs <- 1750:2100
vars <- c(CONCENTRATIONS_CO2(), GLOBAL_TAS(), HEAT_FLUX(), RF_TOTAL(), RF_ALBEDO(), 
          RF_CO2(), RF_SO2(), RF_OC(), RF_BC(), LAND_TAS(), SST(), FFI_EMISSIONS(),
          DACCS_UPTAKE(), LUC_EMISSIONS(), LUC_UPTAKE(), NBP(), GMST())
params <- c(ECS(), DIFFUSIVITY(), AERO_SCALE(), VOLCANIC_SCALE(), BETA(), Q10_RH())


system.file("input", package = "hector") %>% 
  list.files(pattern = "ssp", full.names = TRUE)  %>%
  lapply(function(ini_file){
    scn <- gsub(x = basename(ini_file), pattern = "hector_|.ini", replacement = "" )
    hc <- newcore(ini_file, name = scn)
    csv_file <- list.files(file.path(dirname(ini_file), "tables"), pattern = scn, full.names = TRUE)
    data <- read.csv(csv_file, comment.char = ";")
    
    conc_constraints <- c("C2F6_constrain", "CCl4_constrain", "CF4_constrain", "CFC113_constrain",   
                          "CFC114_constrain", "CFC115_constrain", "CFC11_constrain", "CFC12_constrain", 
                          "CH3Br_constrain", "CH3Cl_constrain", "CH4_constrain", "CO2_constrain", 
                          "HCFC141b_constrain", "HCFC142b_constrain", "HCFC22_constrain", "HFC125_constrain", 
                          "HFC134a_constrain", "HFC143a_constrain", "HFC227ea_constrain", "HFC23_constrain",
                          "HFC32_constrain", "HFC4310_constrain", 
                          "N2O_constrain", "SF6_constrain", "halon1211_constrain", "halon1301_constrain", 
                          "halon2402_constrain")
    
    data[, c("Date", conc_constraints)] %>% 
      pivot_longer(cols = conc_constraints) %>%  
      mutate(units = getunits(name)) ->
      conc_inputs_hc
    
    name <- paste0(gsub(pattern = "hector_|.ini", replacement = "", x = basename(ini_file)), " conc")
    hc <- newcore(inifile = ini_file, name = name)
    setvar(core = hc, dates = conc_inputs_hc$Date, var = conc_inputs_hc$name, 
           values = conc_inputs_hc$value, unit = conc_inputs_hc$units)
    run(hc)
    out <- fetchvars(hc, dates = yrs, vars = vars)
    return(out)
  }) %>%  
  do.call(what = "rbind") -> 
  rstls

version <- packageVersion(pkg = "hector")
rstls$version <- version

file <- paste0("hector_", version, "_ssp-conc.csv")
write.csv(rstls, file = file.path(OUTPUT_DIR, file), row.names = FALSE)
