# Objective: Run Hector for the various ssp scenarios/experiments that will be used in 
# the manuscript. 

# 0. Set Up --------------------------------------------------------------------------------
# Set up environment. 
source(here::here("R", "0.set_up.R"))
source(file.path(BASE_DIR, "R", "0.functions.R"))

# Indicator function to determine if using the hector package ini files or if
# we should be using the prep_core function that will use the calibration 
# rda objects created from "R/0A.hectorv3_calibration.R"
use_pkg_ini <- TRUE

if(use_pkg_ini){
  my_newcore <- newcore
} else {
  my_newcore <- prep_core_v3
}

# Make sure the ini files being used are consistent with the calibration results from 
# part A. This check was only important before the V3 release.
#source(file.path(BASE_DIR, "R", "0C.check_ini_values.R"))
OUTPUT_DIR <- file.path(BASE_DIR, "output", "hector_output")


# 1. Emission Driven Hector ---------------------------------------------------------------------
# Years & variables to save. 
yrs <- 1750:2300
vars <- c(CONCENTRATIONS_CO2(), GLOBAL_TAS(), HEAT_FLUX(), RF_TOTAL(), RF_ALBEDO(), 
          RF_CO2(), RF_SO2(), RF_OC(), RF_BC(), LAND_TAS(), SST(), FFI_EMISSIONS(),
          DACCS_UPTAKE(), LUC_EMISSIONS(), LUC_UPTAKE(), NBP(), OCEAN_UPTAKE(), NPP(), PH(), GMST())
params <- c(ECS(), DIFFUSIVITY(), AERO_SCALE(), VOLCANIC_SCALE(), BETA(), Q10_RH())

# Run Hector v3 with the SSP scenarios 
system.file("input", package = "hector") %>% 
  list.files(pattern = "ssp", full.names = TRUE) %>% 
  lapply(function(f){
    name <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(f))
    core <- my_newcore(f, name = name)
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


# Save post spin up values & pre industrial values! 

# Years & variables to save. 
yrs <- 1746
vars <- c(CONCENTRATIONS_CO2(), GLOBAL_TAS(), HEAT_FLUX(), RF_TOTAL(), RF_ALBEDO(), 
          RF_CO2(), RF_SO2(), RF_OC(), RF_BC(), LAND_TAS(), SST(), FFI_EMISSIONS(),
          DACCS_UPTAKE(), LUC_EMISSIONS(), LUC_UPTAKE(), NBP(), OCEAN_UPTAKE(), NPP(), 
          PH(), GMST(), VEG_C(), DETRITUS_C(), SOIL_C(), PERMAFROST_C(), EARTH_C(), 
          OCEAN_C(), OCEAN_C_DO(), OCEAN_C_HL(), OCEAN_C_IO(), OCEAN_C_LL(), OCEAN_C_ML(), OCEAN_C_IO())
params <- c(ECS(), DIFFUSIVITY(), AERO_SCALE(), VOLCANIC_SCALE(), BETA(), Q10_RH())

# Run Hector v3 with the SSP scenarios 
system.file("input", package = "hector") %>% 
  list.files(pattern = "hector_ssp245", full.names = TRUE) -> 
  f 
name <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(f))
core <- my_newcore(f, name = name)
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

# 2. Run Concentration Driven RCP Hector --------------------------------------------------------
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
    hc <- my_newcore(ini_file, name = scn)
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
    hc <- my_newcore(ini_file, name = scn)
    
    split(conc_inputs_hc, conc_inputs_hc$name) %>% 
      lapply(function(d){
        var <- unique(d[["name"]])
        setvar(core = hc, 
               dates = d[["Date"]], 
               var = var, 
               values = d[["value"]], 
               unit = getunits(var))
        reset(hc)
      })
    
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


# 3. Idealized Runs -----------------------------------------------------------

# Years & variables to save. 
yrs <- 1750:2300
vars <- c(CONCENTRATIONS_CO2(), GLOBAL_TAS(), HEAT_FLUX(), RF_TOTAL(), RF_ALBEDO(), 
          RF_CO2(), RF_SO2(), RF_OC(), RF_BC(), LAND_TAS(), SST(), FFI_EMISSIONS(),
          DACCS_UPTAKE(), LUC_EMISSIONS(), LUC_UPTAKE(), NBP(), OCEAN_UPTAKE(), NPP(), PH(), GMST())
params <- c(ECS(), DIFFUSIVITY(), AERO_SCALE(), VOLCANIC_SCALE(), BETA(), Q10_RH())

# Run Hector v3 for the 1pct CO2 run! 
here::here("data",  "idealized_inputs", "hector_1pctCO2.ini") %>%  
  lapply(function(f){
    name <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(f))
    core <- my_newcore(f, name = name)
    run(core)
    out1 <- fetchvars(core, dates = 1750:2070, vars = vars)
    out2 <- fetchvars(core, dates = NA, vars = params)
    fetchvars(core, dates = 1750:2070, vars = c(LUC_EMISSIONS(), NBP())) %>% 
      group_by(scenario, year, units) %>% 
      summarise(value = sum(value)) %>% 
      ungroup %>% 
      mutate(variable = "land sink") -> 
      out4
    out <- rbind(out1, out2, out4)
    return(out)
  }) %>%
  do.call(what = "rbind") ->
  rstls_1pcCO2

rstls_1pcCO2$year <- rstls_1pcCO2$year - 1800

# Run Hector v3 for the abrupt 4 run 
here::here("data",  "idealized_inputs", "hector_abruptx4CO2.ini") %>%  
  lapply(function(f){
    name <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(f))
    core <- my_newcore(f, name = name)
    run(core)
    out1 <- fetchvars(core, dates = 1750:2070, vars = vars)
    out2 <- fetchvars(core, dates = NA, vars = params)
    fetchvars(core, dates = 1750:2070, vars = c(LUC_EMISSIONS(), NBP())) %>% 
      group_by(scenario, year, units) %>% 
      summarise(value = sum(value)) %>% 
      ungroup %>% 
      mutate(variable = "land sink") -> 
      out4
    out <- rbind(out1, out2, out4)
    return(out)
  }) %>%
  do.call(what = "rbind") ->
  rstls_x4

rstls_x4$year <- rstls_x4$year - 1799

version <- packageVersion(pkg = "hector")
rstls <- rbind(rstls_x4, rstls_1pcCO2) %>% 
  filter(year >= 0)
rstls$version <- version

file <- paste0("hector_", version, "_idealized.csv")
write.csv(rstls, file = file.path(OUTPUT_DIR, file), row.names = FALSE)


# 4. Spin Up Information -------------------------------------------------------
# Run Hector with the settings to write the logs out to disk which will then 
# be used to determine the post spin up information. Note that the log files 
# will be written out to the current working directory. 

setwd(file.path(BASE_DIR, "output", "hector_output"))
inifile <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- my_newcore(inifile = inifile, 
                 loglevel = LL_DEBUG(), 
                 suppresslogging = FALSE)
run(hc)
setwd(BASE_DIR)
