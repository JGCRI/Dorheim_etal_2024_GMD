# Author: Dorheim 
# Date: March 2022 
# Objective: Scratch comparison of Hadcrut temperature with Hector temperature. 
# TODO: what is going on during early pre industrial time, Hector is pretty warm, what is going on? This is 
# surprising that this does so much worse than Hector v1... why is that fml could is be a parameter thing? 

# 0. Set Up ---------------------------------------------------------------------------------------------
library(dplyr)

HECTOR_DIR <- "/Users/dorh012/Documents/Hector-Versions/Xcode/hector"
devtools::load_all(HECTOR_DIR)
#library(hector) #unclear why I am running into issues when I try to do a library load 
library(ggplot2)

theme_set(theme_bw())

  
# Hadcrut5 ---------------------------------------------------------------------------------------------
# Global mean surface temperature anomoly 
# https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/download.html
# The temperature anomaly is based off of 1961–1990 
# surface temperature with the anomaly! 

# Import and format data. 
here::here("data", "comparison", "HadCRUT.5", "HadCRUT.5.0.1.0.analysis.summary_series.global.annual.csv") %>% 
  read.csv(stringsAsFactors = FALSE) -> 
  data
names(data) <- c("year", "value", "lower", "upper")

# Hector ---------------------------------------------------------------------------------------------
# Get historical global mean surface temperature, will need to calculate the surface temperature 
# from Hector output and calculate the anomaly based off of a reference value. 

# Import and format data. 
#ini <- system.file("input/hector_ssp434.ini", package = "hector")
ini <- file.path(HECTOR_DIR, "inst", "input", "hector_ssp434.ini")
hc <- newcore(ini, name = "emission driven")
run(hc)
output  <- fetchvars(hc, 1850:2020, vars = c(LAND_AIR_TEMP(), OCEAN_SURFACE_TEMP()))
output$params <- "old"
  
set_optim_params <- function(hc){
  
  vals <- c(0.160000, 1.519490, 55.911591, 0.500000, 2.724023, 2.393617)
  pnames <- c(BETA(), Q10_RH(), NPP_FLUX0(), AERO_SCALE(), ECS(), DIFFUSIVITY())
  units <- c("(unitless)", "(unitless)", "Pg C/yr", "(unitless)", "degC", "cm2/s")
  dates <- rep(NA, length.out = length(vals))
  
  mapply(function(v, p, u){
    setvar(core = hc, dates = NA, var = p, values = v, unit = u)
  }, v = vals, p = pnames, u = units )
  reset(hc)
  return(hc)
  
}

hc <- set_optim_params(hc)
run(hc)
output_optim  <- fetchvars(hc, 1850:2020, vars = c(LAND_AIR_TEMP(), OCEAN_SURFACE_TEMP()))
output_optim$params <- "optim"



input <- read.csv(here::here("input", "tables", "rcmip_ssp119_emiss-constraints_rf.csv"), skip = 5)
input %>% 
  dplyr::filter(Date %in% 1850:1960) 








# Calculate gmst based on Hector land and ocean surface temperature 
# 
# Args 
#   data: Hector output dataframe containing results from "Tgav_land" and "Tgav_ocean_ST"
# Return: Hector output data frame containing gmst
get_gmst <- function(data){
  flnd <- 0.29
  
  land_air <- data[data$variable == LAND_AIR_TEMP(), ]
  tos <- data[data$variable == OCEAN_SURFACE_TEMP(), ]
  gmst_vals <- land_air$value *  flnd + tos$value * (1 - flnd)
  
  data$value <- gmst_vals
  data$variable <- "GMST"
  return(data)

}

# Get the gmst data. 
rslt <- get_gmst(output)
rslt2 <- get_gmst(output_optim)

rslt$params <- "deafult"
rslt2$params <- "optim"

# Calcualte the reference value. 
ref_value <- mean(rslt[rslt$year %in% 1961:1990, ][["value"]])
rslt$value <- rslt$value - ref_value

# Calcualte the reference value. 
ref_value <- mean(rslt2[rslt$year %in% 1961:1990, ][["value"]])
rslt2$value <- rslt2$value - ref_value

ggplot() + 
  geom_ribbon(data = data, aes(year, ymin = lower, ymax = upper), alpha = 0.5) + 
  geom_line(data = data, aes(year, value, color = "HadCrut 5"), color = "black") + 
  #geom_line(data = rslt, aes(year, value, color = "Hector v3"), size = 1.5,) + 
  geom_line(data = rslt2, aes(year, value, color = "Hector v3 new p"), size = 1.5,) + 
  labs(title = "GMST Hector vs Obs", y = "deg C", y = "Year")














