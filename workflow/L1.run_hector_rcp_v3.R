# Run Hector v3 with RCP emissions (consistent with the v 2.5 inputs)
# 0. Set Up ---------------------------------------------------------------------------------------------------------
library(dplyr)
library(hector)
if(packageVersion("hector") != '3.0.0') stop("unexpected version of hector")
library(magrittr)

# Define the paths to directories that will be used in this script 
INPUTDIR <- here::here("input")
DATADIR <- here::here("data")

# Run Hector and fetch the results 
# Args
#   path: string path to the ini file 
#   vars: vector of the output variables
#   dates: vector of the dates to save 
# Return: a data frame of Hector results
custom_run_hector <- function(path, vars = c(GLOBAL_TAS(), OCEAN_TAS(), SST(), SST_HL(), SST_LL(), CONCENTRATIONS_CO2(), PH(), FFI_EMISSIONS(), 
                                             LUC_EMISSIONS(), NPP(), RH(), NBP(), RF_TOTAL(), LAND_TAS()), dates = 1750:2300){
  
  scn <- gsub(pattern = "hector_|.ini", replacement = "", x = basename(path))
  print(scn)
  core <- newcore(path, name = scn)
  run(core, 2100)
  out <- fetchvars(core, dates, vars) %>%  
    distinct()
  
  return(out)
  
}


# 1. Run Hector & save output ---------------------------------------------------------------------------------------------------------
list.files(INPUTDIR, pattern = "RCP", full.names = TRUE) %>% 
  lapply(custom_run_hector) %>%  
  do.call(what = "rbind") %>%  
  mutate(scenario = tolower(gsub(x = scenario, pattern = "_emissions", replacement = ""))) -> 
  results

results$version <- packageVersion("hector")

write.csv(results, file = file.path(DATADIR, "hector_v3_rcp_results.csv"), row.names = FALSE)