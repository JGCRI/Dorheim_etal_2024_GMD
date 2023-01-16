# Get gmst results from a Hector tun 
# Args 
#   core: active hector core
# Return data frame of the global mean surface temperature 
fetch_gmst <- function(core){
  
  flnd  <- 0.29  
  
  fetchvars(core, dates = 1750:2100, vars = c(LAND_TAS(), SST())) %>% 
    select(year, variable, value) -> 
    output
  
  output <- reshape(output, direction = "wide", idvar = "year", timevar = "variable")
  names(output) <- gsub("value.", "", names(output))
  
  output$value <- with(output, (land_tas * flnd + sst * (1 - flnd)))
  output <- subset(output, select = c(year, value))
  output$variable <- "gmst"
  
  return(output)
}

# Normalize temperature to the historical reference period
# Args 
#   data: data frame of temperature data
# Returns a data frame of temperature data normalized to the 1951:1980 reference period
normalize_to_giss <- function(data){
  
  assert_that(unique(data[["variable"]]) == "gmst")
  
  ref_value <- mean(data[(data$year %in%  1951:1980), ]$value)
  data$value <- data$value - ref_value
  
  return(data)
}

# Normalize  Hector temperature
#
# Args
#   data: data frame of Hector results for mulitple experiments
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