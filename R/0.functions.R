# Normalize  Hector temperature
#
# Args
#   data: data frame of Hector results for multiple experiments
#   period: vector of the years in the reference period
# Return: data frame of normalized values.
normalize_hector_temp <- function(data, period){
  
  assertthat::assert_that(length(unique(data[["variable"]])) == 1)
  
  split(data, data$scenario) %>% 
    lapply(function(dd){
      ref_value <- mean(dd[dd[["year"]] %in% period , ][["value"]])
      dd[["value"]] <- dd[["value"]] - ref_value
      return(dd)
    }) %>% 
    do.call(what = "rbind") -> 
    data
  
  return(data)
  
}

# Read parameter values into an active Hector core 
# Args 
#   core: active hector core 
#   p: vector of named hector parameter values  
# Return active hector core with the set parameter values 
set_params <- function(core, p){
  
  assert_that(all(is.character(names(p))), msg = "unamed parameter")
  
  mapply(function(pval, pnames, punits){
    setvar(core, dates = NA, values = pval, var = pnames, unit = punits)
  }, pval = p, pnames = names(p), punits = getunits(names(p)))
  
  reset(core)
  
  return(core)
  
}



# Normalize temperature to the historical reference period
# Args 
#   data: data frame of temperature data
# Returns a data frame of temperature data normalized to the 1951:1980 reference period
normalize_to_hadcrut <- function(data){
  
  assert_that(unique(data[["variable"]]) == "gmst")
  
  ref_value <- mean(data[(data$year %in%  1961:1990), ]$value)
  data$value <- data$value - ref_value
  
  return(data)
}


# TODO  need some sort of way to tell to use the ini files or use this function 
# to set up the ini 

# Helper function that prepares a new Hector core, good to use before 
# updating all of the ini files. 
prep_core_v3 <- function(inifile, ...){
  
  core <- newcore(inifile, ...)
  
  setvar(core, dates = NA, var = Q10_RH(), values = fit$par[["q10_rh"]], unit = getunits(Q10_RH()))
  setvar(core, dates = NA, var = DIFFUSIVITY(), values = fit$par[["diff"]], unit = getunits(DIFFUSIVITY()))
  setvar(core, dates = NA, var = BETA(), values = fit$par[["beta"]], unit = getunits(BETA()))
  
  setvar(core, dates = NA, var = NATURAL_CH4(), values = natural_emiss_fit$par[["nat_ch4"]], unit = getunits(NATURAL_CH4()))
  setvar(core, dates = 1750:2100, var = NAT_EMISSIONS_N2O(), values = natural_emiss_fit$par[["nat_n2o"]], unit = getunits(NAT_EMISSIONS_N2O()))
  
  # Reset the core 
  reset(core)
  return(core)
  
}






