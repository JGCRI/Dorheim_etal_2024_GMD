# Script that formats the RCP tables shipped with v2 of Hector, update so that 
# they are compatible with Hector v3. 

# 0. Set up ------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(hector)

# Directory
out_dir <- here::here("input", "v2tables")
dir.create(out_dir,showWarnings = FALSE)


# Function that updates the tables 
# Args 
#   name: string path of the RCP emissions file to up date 
# return: the hector csv table and the ini file 
update_rcpfiles <- function(name){
  
  old_csv <- read.csv(here::here("input", "v2tables_unformated", name), comment.char = ";")
  vol_csv <- read.csv(here::here("input", "v2tables_unformated", "volcanic_RF.csv"), comment.char = ";")
  
  # Format the CO2 emissions & uptake values
  ffi_values <- old_csv$ffi_emissions
  old_csv$daccs_uptake <- if_else(ffi_values > 0, 0, -1 * ffi_values)
  old_csv$ffi_emissions <- if_else(ffi_values < 0, 0, ffi_values)
  
  luc_values <- old_csv$luc_emissions
  old_csv$luc_uptake <- if_else(luc_values > 0, 0, -1 * luc_values)
  old_csv$luc_emissions <- if_else(luc_values < 0, 0, luc_values)
  
  # Add the new RF/emission species to Hector
  old_csv$RF_misc <- 0 
  old_csv$SV <- vol_csv$SV
  old_csv$NH3_emissions <- 0
  
  # Write as a csv file once. 
  names(old_csv) <- gsub(names(old_csv), replacement = "", pattern = " ")
  csv_output <- file.path(out_dir, name)
  write.csv(old_csv, file = csv_output, row.names = FALSE)
  
  # Formatting the Hector data table! 
  header <- readLines(here::here("input", "tables", "ssp245_emiss-constraints_rf.csv"))[1:3]
  lines <- readLines(csv_output)
  flines <- append(header, lines)
  writeLines(flines, csv_output)
  
  
  # Formatting the Hector ini file!
  ini_lines <- readLines(here::here("input", "hector_ssp119.ini"))
  new_ini_lines <- gsub(x = ini_lines, pattern = "tables/ssp119_emiss-constraints_rf.csv", replacement = paste0("v2tables/", name))
  
  # Insert in the v2 albedo values into the ini file
  albedo_index <- which(grepl(pattern = "albedo", x = new_ini_lines))
  final_lines <- c(new_ini_lines[1:(albedo_index-1)], "RF_albedo[1750]=0.0", "RF_albedo[1950]=-0.2", new_ini_lines[(albedo_index+1):length(new_ini_lines)]) 
  
  writeLines(as.character(final_lines), here::here("input", gsub(x = name, pattern = "csv", replacement = "ini")))
  
}

# 1. Make the RCP materials ---------------------------------------------------------------------
# Sometimes there are issues with the headers of the files, they may need to be replace with 
# the csv table header. 
update_rcpfiles("RCP26_emissions.csv")
update_rcpfiles("RCP45_emissions.csv")
update_rcpfiles("RCP60_emissions.csv")
update_rcpfiles("RCP85_emissions.csv")
