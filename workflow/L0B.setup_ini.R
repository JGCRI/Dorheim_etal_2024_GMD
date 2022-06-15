#  Update Hector ini files with the values from the calibration process. 
# 
# The location of where to write the hector ini files out to, eventually these ini files will be included in the
# hector package but for now this prevents needing to do a clean rebuild every time a change is made to an ini file. 

# 0. Set Up ---------------------------------------------------------------------------------------------------------
library(magrittr)

# Define the paths to directories that will be used in this script 
DATADIR <- here::here("data")
INPUTDIR <- here::here("input")

# Load the parameter values found from the calibration process. 
params <- read.csv(file.path(DATADIR, "calibration_param_fits.csv"))

# Replace parameter values in Hector ini files with calibration parameter values. 
# Args 
#   path: path to the inifile 
#   params: data frame of parameter values 
# Return: write ini files out to the project location. 
update_ini <- function(path, params){
  
  inifile_contents <- readLines(path)
  inifile_name <- basename(path)
  
  for (pname in names(params)) {
    hector_recognized_param_string <- match.fun(pname)()
    inifile_contents[grepl(pattern = paste0(hector_recognized_param_string, "="), x = inifile_contents)] <- paste0(hector_recognized_param_string, "=", params[[pname]])
  }
  
  writeLines(inifile_contents, con = file.path(INPUTDIR, inifile_name))
  
  return(inifile_contents)
}

# 1. Update the inifiles ---------------------------------------------------------------------------------------------------------
system.file("input", package = "hector") %>% 
  list.files(pattern = "ini", full.names = TRUE) %>%  
  lapply(update_ini, params = params) %>%  
  unlist -> 
  new_inis

# 2. Copy all of the contents of the input/tables to the project input location --------------------------------------------------
system.file("input/tables", package = "hector") %>% 
  list.files(pattern = "csv", full.names = TRUE) %>% 
  lapply(function(x){
    fname <- basename(x)
    dir <- file.path(INPUTDIR, "tables")
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    out <- file.path(dir, fname)
    file.copy(from = x, to = out)
    return(out)
  }) %>% 
  unlist() ->
  files_moved

# End 