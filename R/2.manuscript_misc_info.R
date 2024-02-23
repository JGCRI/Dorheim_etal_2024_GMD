# Objective: Scrape the Hector C++ code base & the input tables for information to include in the manuscript

# 0. Set Up -----------------------------------------------------------------------------------------------------
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


# 1. Emissions -----------------------------------------------------------------------------------------------------
# Figure out how many emissions time series are used as Hector inputs. 
# Read in an input table and save a copy of all the column names that contain emiss. 
system.file("input/tables/ssp245_emiss-constraints_rf.csv", package = "hector") %>%  
  read.csv(comment.char = ";") -> 
  data

xx <- names(data)

# subtract the one because the ffi_emissions and luc_emissions both refer to CO2 emissions 
emissions <- xx[grepl(pattern = "emiss", x = xx)]
emission_count <- xx[grepl(pattern = "emiss", x = xx)] %>% length() - 1 # the minus 1 accounts for the fact that there are two co2 emission categories

# Figure out the ghg emissions vs the aerosols 
# The emissions that are immediately used to calculate their respective forcing effect on Hector's energy budget. 
# NH3, BC, SO2, OC  
immediate <- 4 

# The emissions that accumulate or effect ghg gasses in the atmosphere as gases. The non CO2 gases  
emission_count - immediate

# 2. Hector RF -----------------------------------------------------------------------------------------------------
# List of all the forcing agents in Hector 
all_rf <- c(RF_ALBEDO(), RF_CO2(), RF_H2O_STRAT(), RF_O3_TROP(), RF_BC(), RF_OC(), RF_NH3(),
            RF_SO2(), RF_ACI(), RF_VOL(), RF_MISC(), RF_CH4()
            , RF_CF4(), RF_C2F6(), RF_HFC23(), RF_HFC32(), RF_HFC4310(), RF_HFC125(), RF_HFC134A(), RF_HFC143A(), 
            RF_HFC227EA(), RF_HFC245FA(), RF_SF6(), RF_CFC11(), RF_CFC12(), RF_CFC113(), RF_CFC114(), RF_CFC115(), 
            RF_CCL4(), RF_CH3CCL3(), RF_HCFC22(), RF_HCFC141B(), RF_HCFC142B(), RF_HALON1211(), RF_HALON1301(), 
            RF_HALON2402(), RF_CH3CL(), RF_CH3BR())

# Read in the component_data hpp file and scrape for all the RF time series that Hector internally calculates. 
lines <- readLines(file.path(system.file("include", package = "hector"), "component_data.hpp"))

# Parse ouot all the lines containing foring info. 
rf <- lines[grepl(pattern = "D_RF_", x = lines)]
exlucde <- "D_RF_BASEYEAR|#define D_RF_PREFIX |D_FTOT_CONSTRAIN|D_RF_TOTAL|D_RF_halocarbons"
indvidual_rf <- rf[!grepl(pattern = exlucde, x = rf)]
length(indvidual_rf)

# Exclude the RF that are read in as inputs
inputs <- "D_RF_T_ALBEDO|D_RF_MISC|D_RF_VOL"
modeled_by_hector <- indvidual_rf[!(grepl(pattern = inputs, x = indvidual_rf))]
modeled_by_hector
length(modeled_by_hector)
