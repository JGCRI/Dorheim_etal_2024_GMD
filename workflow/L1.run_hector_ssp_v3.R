# Run Hector v3 and save output. 

# 0. Set Up ---------------------------------------------------------------------------------------------------------
library(magrittr)
library(dplyr)

remotes::install_github("JGCRI/hector@v3_dev")
library(hector) 
if(packageVersion("hector") != '3.0.0') stop("unexpected verrsion of hector")

# Define the paths to directories that will be used in this script 
INPUTDIR <- here::here("input")
DATADIR <- here::here("data")


# Calculate gmst based on Hector land and ocean surface temperature
#
# Args
#   data: Hector output dataframe containing results from "Tgav_land" and "Tgav_ocean_ST"
# Return: Hector output data frame containing gmst
get_gmst <- function(data){
  flnd <- 0.29
  
  land_air <- data[data$variable == LAND_TAS(), ]
  tos <- data[data$variable == SST(), ]
  gmst_vals <- land_air$value *  flnd + tos$value * (1 - flnd)
  
  land_air$value <- gmst_vals
  land_air$variable <- "GMST"
  return(land_air)
  
}



# Run Hector and fetch the results 
# Args
#   path: string path to the ini file 
#   vars: vector of the output variables
#   dates: vector of the dates to save 
# Return: a data frame of Hector results
custom_run_hector <- function(path, vars = c(GLOBAL_TAS(), OCEAN_TAS(), SST(), SST_HL(), SST_LL(), CONCENTRATIONS_CO2(), PH(), FFI_EMISSIONS(), 
                                             LUC_EMISSIONS(), NPP(), RH(), NBP(), RF_TOTAL(), LAND_TAS()), dates = 1750:2300){
  
  scn <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(path))
  core <- newcore(path, name = scn)
  run(core)
  out <- fetchvars(core, dates, vars) %>%  
    distinct()
  
  # Add the gsmt data to the results returned 
  gmst_data <- get_gmst(out)
  out <- rbind(out, gmst_data)
  
  return(out)
  
}

# 1. Run Hector & save output ---------------------------------------------------------------------------------------------------------
list.files(INPUTDIR, pattern = "ssp", full.names = TRUE) %>% 
  lapply(custom_run_hector) %>%  
  do.call(what = "rbind") -> 
  results

results$version <- packageVersion("hector")

write.csv(results, file = file.path(DATADIR, "hector_v3_results.csv"), row.names = FALSE)
# End