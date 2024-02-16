# Define the project constants used by multiple scripts. 

# The root directory for the project. 
BASE_DIR <- here::here()

# Define which version of Hector should be used here! 
HECTOR_VERSION <-  "3.1.1"


load(here::here("output", "calibration-diff_beta_q10-Tue_Feb_13_09:57:15_2024.rda"))
load(here::here("output", "calibration-natemissions-Tue_Feb_13_09:53:00_2024.rda"))

# Helper function that prepares a new Hector core, good to use before 
# updating all of the ini files. 
prep_core_v3 <- function(ini, ...){
  
  core <- newcore(ini, ...)
  
  setvar(core, dates = NA, var = ECS(), values = 3.0, unit = getunits(ECS()))
  setvar(core, dates = NA, var = NPP_FLUX0(), values = 56.2, unit = getunits(NPP_FLUX0()))
  setvar(core, dates = NA, var = AERO_SCALE(), values = 1, unit = getunits(AERO_SCALE()))
  setvar(core, dates = NA, var = VOLCANIC_SCALE(), values = 1, unit = getunits(VOLCANIC_SCALE()))
  
  setvar(core, dates = NA, var = Q10_RH(), values = fit$par[["q10_rh"]], unit = getunits(Q10_RH()))
  setvar(core, dates = NA, var = DIFFUSIVITY(), values = fit$par[["diff"]], unit = getunits(DIFFUSIVITY()))
  setvar(core, dates = NA, var = BETA(), values = fit$par[["beta"]], unit = getunits(BETA()))
  
  setvar(core, dates = NA, var = NATURAL_CH4(), values = natural_emiss_fit$par[["nat_ch4"]], unit = getunits(NATURAL_CH4()))
  setvar(core, dates = 1750:2100, var = NAT_EMISSIONS_N2O(), values = natural_emiss_fit$par[["nat_n2o"]], unit = getunits(NAT_EMISSIONS_N2O()))
  
  # Reset the core 
  reset(core)
  return(core)
  
}
