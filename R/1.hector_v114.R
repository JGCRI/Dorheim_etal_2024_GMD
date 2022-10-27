# Objective: Download the v114 results and format them so that they have the proper names 
# to plot with the Hector v3 output data. 
# 0. Set Up --------------------------------------------------------------------------------
library(dplyr)

BASE_DIR <- here::here()
SCRATCH_DIR <- file.path(BASE_DIR, "scratch")
dir.create(SCRATCH_DIR)

# 1. Get & load data -----------------------------------------------------------------------
url <- "https://zenodo.org/record/821646/files/JGCRI/hector-v1.1.4.zip?download=1"

version <- gsub(x = basename(url), pattern = "hector-v|", replacement = "")
version <- substr(x = version, start = 1, stop = 5)

assertthat::assert_that(version == "1.1.4")

dest_file <- file.path(SCRATCH_DIR, "hector114.zip")
download.file(url, dest_file)
unzip(zipfile = dest_file, exdir = SCRATCH_DIR)

list.files(file.path(dirname(dest_file), "JGCRI-hector-ec9af49", "output"),
           full.names = TRUE) %>% 
  lapply(function(f){
    d <- read.csv(f, skip = 1)
    d$version <- version
    return(d)
  }) %>% 
  do.call(what = "rbind") -> 
  rstls 

# 2. Get data --------------------------------------------------------------------------------
# Format the data to be consistent with the v3 variable names. 
rstls %>% 
  filter(run_name %in% c("rcp26", "rcp45", "rcp60", "rcp85")) %>% 
  mutate(variable = if_else(variable == "Ca", "CO2_concentration", variable)) %>% 
  mutate(variable = if_else(variable == "Tgav", "global_tas", variable)) %>% 
  mutate(variable = if_else(variable == "atm_land_flux", "NBP", variable)) %>% 
  mutate(variable = if_else(variable == "Ftot", "RF_tot", variable)) %>% 
  filter(spinup == 0) -> 
  rstls

rstls %>% 
  select(year, scenario = run_name, variable, value, units, version) -> 
  output

list.files(file.path(dirname(dest_file), "JGCRI-hector-ec9af49", "input"), pattern = "hector_rcp", 
           full.names = TRUE, recursive = TRUE) %>%
  lapply(function(f){
    lines <- readLines(f)
    pattern <- "S=|diff=|apha=|volscl=|beta=|q10_rh="
    param_vals <- lines[grepl(pattern = pattern, x = lines)]
    
    lapply(param_vals, function(p){
      p_list <- unlist(strsplit(p, "="))
      name <- p_list[[1]]
      
      val <- gsub(x = p_list[[2]], pattern = " ", replacement = "")
      val <- as.numeric(unlist(strsplit(val, "\t| "))[1])
      data.frame(variable = name, value = val)
    }) %>% 
      do.call(what = "rbind") ->
      param_table
    
    name <- gsub(x = lines[grepl(x = lines, pattern = "run_name")], pattern = "run_name=", replacement = "")
    param_table$scenario <- name
    param_table$year <- NA 
    param_table$units <- NA 
    param_table$version <- version
    
    return(param_table)
    
  }) %>% 
  do.call(what = "rbind") -> 
  params_table


rstls <- rbind(output, params_table)
file <- paste0("hector_", version, "_rcp.csv")
write.csv(rstls, file = file.path(BASE_DIR, "hector_output", file), row.names = FALSE)  
