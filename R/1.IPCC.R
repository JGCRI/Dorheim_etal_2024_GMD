# Objective: Import and format the IPCC results to simplify the manuscript analyses and figures 
# code. 
# 0. Set Up ----------------------------------------------------------------------------
library(dplyr)
library(tidyr)

# The contents of the IPCC_DIR are the supplemental material figure 10 the cumulative CO2 
# emissions vs global temperature change
# https://data.ceda.ac.uk/badc/ar6_wg1/data/spm/spm_10/v20210809
IPCC_DIR <- here::here("data", "IPCC_SPM10")

# Load the IPCC tas vs co2 results 
# 
# Args 
#   f: file name to read in
read_IPCC_rslts <- function(f){
  
  data <- read.csv(file.path(IPCC_DIR, f)) %>% 
    select(-GtCO2)
  
  scn <- tolower(gsub(x = f, pattern = "Top_panel_|.csv|-", replacement = ""))
  if(scn == 'history'){
    scn <- "historical"
  }
  
  co2_vals <- names(data)
  co2_vals <- co2_vals[grepl(x = co2_vals, pattern = "X")]
  co2_vals <- as.numeric(gsub(x = co2_vals, pattern = "X", replacement = ""))
  
  out <- data.frame()
  nrow <- dim(data)[[1]]
  for(i in 1:nrow){
    type <- data[i, 1]
    vals <- as.numeric(data[i, -1])
    df <- data.frame(type = type, 
                     scenario = scn,
                     co2 = co2_vals, 
                     tas = vals)
    out <- rbind(out, df)
  }
  
  
  out %>%  
    mutate(type = if_else(grepl(pattern = "central estimate|central", x = type), "mean", type)) %>% 
    mutate(type = if_else(grepl(pattern = "95th percentile|95th percentile", x = type), "max", type)) %>% 
    mutate(type = if_else(grepl(pattern = "5th percentile estimate|5th percentile", x = type), "min", type)) %>% 
    mutate(type = if_else(grepl(pattern = "Assessed GSAT projection relative to 1850-1900|Assessed global surface temperature relative to 1850-1900", x = type), 
                          "obs", type)) -> 
    out
  
  
  out %>% 
    group_by(type, scenario, co2) %>% 
    summarise(tas = mean(tas)) %>% 
    ungroup %>% 
    spread(type, tas) -> 
    out
  
  return(out)
  
}

# 1. Process the IPCC data ----------------------------------------------------------------------------
IPCC_rslts <- bind_rows(read_IPCC_rslts("Top_panel_HISTORY.csv"), 
                        read_IPCC_rslts("Top_panel_SSP1-19.csv"),
                        read_IPCC_rslts("Top_panel_SSP1-26.csv"),
                        read_IPCC_rslts("Top_panel_SSP1-26.csv"), 
                        read_IPCC_rslts("Top_panel_SSP2-45.csv"),
                        read_IPCC_rslts("Top_panel_SSP3-70.csv"),
                        read_IPCC_rslts("Top_panel_SSP5-85.csv"))

write.csv(IPCC_rslts, file = here::here("data", "IPCC_co2_tas.csv"), row.names = FALSE)
