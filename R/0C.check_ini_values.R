# Check to make sure that all of the Hector ini files are set up with calibration parameter values. 

# 0. Set Up  -------------------------------------------------------------------------------------------
library(assertthat)
library(data.table)
library(dplyr)
library(ggplot2)

remotes::install_github("JGCRI/hector")
library(hector) 
assert_that(packageVersion("hector") ==  "3.1.1")

BASE_DIR <- here::here()

# 1. Load the calibration results -------------------------------------------------------------------------------------------
nat_emiss <- get(load(list.files(file.path(BASE_DIR, "output"), pattern = "natemissions", full.names = TRUE)))[["par"]]
diff_beta_q10 <- get(load(list.files(file.path(BASE_DIR, "output"), pattern = "diff_beta_q10", full.names = TRUE)))[["par"]]



# 2. Check all the default Hector ini files --------------------------------------------------------------------------------

list.files(system.file("input", package = "hector"), pattern = "ini",
           full.names = TRUE) %>% 
  sapply(function(ini){
    
    #print(basename(ini))
    
    hc <- newcore(inifile = ini)
    run(hc, runtodate = 1748)
    
    hec_diff_beta_q10 <- fetchvars(hc, vars = names(diff_beta_q10), dates = NA)
    hec_diff_beta_q10_values <- hec_diff_beta_q10$value
    names(hec_diff_beta_q10_values) <- hec_diff_beta_q10$variable
    
    hec_natural_ch4 <- fetchvars(hc, vars = NATURAL_CH4(), dates = NA)
    hec_natural_n2o <- fetchvars(hc, vars = NAT_EMISSIONS_N2O (), dates = 1745)
    
    hec_nat_emiss_vals <- c(hec_natural_n2o$value, hec_natural_ch4$value)
    names(hec_nat_emiss_vals) <- names(nat_emiss)
    
    assert_that(all(round(hec_diff_beta_q10_values, digits = 2) - round(diff_beta_q10, digits = 2) == 0), msg = "beta, diff, q10 are different")
    assert_that(all(signif(hec_nat_emiss_vals, digits = 2) - signif(nat_emiss, digits = 2) == 0), msg = "natural emiss are different")
    
   #print("----")
    TRUE
    
  }, USE.NAMES = FALSE) -> 
  checked

checked






