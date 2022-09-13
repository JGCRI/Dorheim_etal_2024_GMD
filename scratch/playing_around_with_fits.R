
# 0. Set Up -----------------------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
theme_set(theme_bw())

# Read in ini file, initiate new core
library(hector)
if(packageVersion("hector") != '3.0.0') stop("unexpected verrsion of hector")

LOGDIR <- here::here("workflow", "L0.logs")
dir.create(LOGDIR, showWarnings = FALSE, recursive = TRUE)

DATADIR <- here::here("data")
LOGFILE <- file.path(LOGDIR, "optim_logfile.csv"); unlink(LOGFILE)
CO2LOGFILE <- file.path(LOGDIR, "optim_logfile_co2.csv"); unlink(CO2LOGFILE)
TASLOGFILE <- file.path(LOGDIR, "optim_logfile_tas.csv"); unlink(CO2LOGFILE)

ssp245 <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ssp245)

# Mauna Loa CO2 - downloaded 2022-05-05 from https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.csv
# R actually has this as a built-in dataset but it stops at 1997! Too bad
read_csv(file.path(DATADIR, "co2_annmean_mlo.csv"), col_types = "ddd", comment = "#") %>% 
  rename(value = mean) %>% 
  mutate(which = "Mauna Loa") ->
  OBSERVED_CO2

# NASA GISS temperature - downloaded 2020-05-06 from https://climate.nasa.gov/vital-signs/global-temperature/
# 1951-1980 baseline 
OBSERVED_TAS <- read_table(file.path(DATADIR, "giss_temperature.txt"), skip = 5,
                           col_names = c("year", "value"),
                           col_types = "dd-")
OBSERVED_TAS$which <- "NASA GISS"


params <- c(BETA(), Q10_RH(), ECS(), NPP_FLUX0(), AERO_SCALE(), VOLCANIC_SCALE(), DIFFUSIVITY())
punits <- sapply(params, getunits, USE.NAMES = FALSE)
# inital_vals <- sapply(X = params, 
#                       FUN = function(p){
#                         fetchvars(core, dates = NA, vars = p)[["value"]]
#                         }, USE.NAMES = FALSE)
# Hard code the old defaul values.... although these should probably be from Leeya's disturbtuion
inital_vals <- c(0.36, 2.0, 3, 50.0, 1, 1, 2.3)
p_df <- data.frame(params = params, units = punits, values = inital_vals)

# # Standard deviations are based on Leeya's research and code
# # https://github.com/JGCRI/trackingC/blob/main/trackingC_setup.Rmd
 sd_df <- data.frame(sd = c(0.2, 0.6, 14, 0.5, 1.0, 0.23, 1), 
                   params = c(BETA(), Q10_RH(), NPP_FLUX0(), AERO_SCALE(), ECS(), DIFFUSIVITY(), VOLCANIC_SCALE()))
# Define the SD as the
#param_table <- inner_join(p_df, sd_df, by = "params")
 param_table <- p_df
param_table[["sd"]] <- p_df$values * 1.5
param_table[["params_lower"]] <- 0
param_table[["params_upper"]] <- param_table$values + param_table$sd

# param_table %>%  
#   dplyr::filter(!params %in% c(VOLCANIC_SCALE(), AERO_SCALE(), Q10_RH(), ECS())) -> 
#   param_table

setvar(core, NA, var = ECS(), values = 3, unit = getunits(ECS()))


# okay so this might not work at all... FML why did I not do a closer llook 
set_params <- function(params, core) {
  if(all(!step %% 10, !is.null(step))) message(step, " ", Sys.time())
  
  mapply(function(var, val){
    setvar(core, dates = NA, var = var, values = val, unit = getunits(var))
  }, var = names(params), val = params, USE.NAMES = FALSE)
  
  invisible()
}

drop_inf <- function(x){
 out <- x[which(is.finite(x))]
 return(out)
}

run_hector <- function(params, core, log = TRUE) {
  # params is a named vector of parameters
  # core is a Hector core
  step <<- step + 1
  
  nss <- tryCatch({
    set_params(params, core)
    reset(core)
    run(core, runtodate = max(OBSERVED_CO2$year, OBSERVED_TAS$year))
    
    # Get atmospheric [CO2] and compare to observational record
    # We use normalized sum of squares so entire time series is weighted equally
    co2 <- fetchvars(core, OBSERVED_CO2$year, CONCENTRATIONS_CO2())
    co2_nss <- sum(((OBSERVED_CO2$value - co2$value) ^ 2) / OBSERVED_CO2$value)
    
    # NASA GISS temperature uses a 1951-1980 baseline, need to normalize the Hector 
    # data before calculating the errors.  
    tas <- fetchvars(core, OBSERVED_TAS$year, GLOBAL_TAS())
    hector_ref <- mean(tas[tas$year %in% OBSERVED_TAS$year, ][["value"]])
    tas$value <- tas$value - hector_ref
    tas_nss <- sum(drop_inf(((OBSERVED_TAS$value - tas$value) ^ 2) / OBSERVED_TAS$value))
    
    # Log
    if(log) {
      co2$step <- step
      write_csv(co2, CO2LOGFILE, append = TRUE, col_names = !file.exists(CO2LOGFILE))
      tas$step <- step
      write_csv(tas, TASLOGFILE, append = TRUE, col_names = !file.exists(CO2LOGFILE))
      
      params_df <- as.data.frame(t(as.data.frame(params)))
      params_df$NSS <- out
      params_df$step <- step
      write_csv(params_df, LOGFILE, append = TRUE, col_names = !file.exists(LOGFILE))
    }
    
    # One potential problem is going to be the relative size of the different variables... 
    mean(co2_nss, tas_nss)
  }, error = function(e){9999})

  
  # Return normalized sum of squares to optimizer
  return(nss)
}




p <- param_table$values
names(p) <- param_table$params

step <- 0
start <- Sys.time()
result <- optim(p, 
                run_hector,
                gr = NULL, core,
                method = "L-BFGS-B",
                lower = param_table$params_lower,
                upper = param_table$params_upper)
end <- Sys.time()
shutdown(core)
time_taken <- difftime(end, start, units = "mins")



param_results <- readr::read_csv(LOGFILE, col_types = "dddddddd")
param_results %>%
  pivot_longer(-step) %>%
  ggplot(aes(step, value)) + geom_line() +
  facet_wrap(~name, scales = "free_y")

params_tab <- tibble(Parameter = names(params),
                     `Start value` = params,
                     `Low bound` = params_lower,
                     `High bound` = params_upper,
                     `Final value` = result$par)
knitr::kable(params_tab)


write_csv(params_tab, file = file.path(LOGDIR, "params_table.csv"))


param_fits <- t(data.frame(params_tab$`Final value`))
write.csv(param_fits, file = file.path(DATADIR, "calibration_param_fits.csv"), row.names = FALSE)

# Brand new core, run default and optimized
core <- newcore(ssp245)
suppressMessages(run(core))
out1 <- fetchvars(core, core$strtdate:core$enddate)
out1$which <- "Default"
set_params(result$par, core)
reset(core)
suppressMessages(run(core))
out2 <- fetchvars(core, core$strtdate:core$enddate)
out2$which <- "Optimized"
out <- rbind(out1, out2)

# Compare to observed CO2
out %>%
  filter(variable == CONCENTRATIONS_CO2()) %>% 
  select(year, value, which) %>% 
  bind_rows(OBSERVED_CO2)  %>%
  filter(year %in% OBSERVED_CO2$year)  %>% 
  ggplot(aes(year, value, color = which)) +
  geom_line() + 
  ggtitle("CO2 (optimized against)") + scale_color_discrete("")

# Compare to observed temperature
tas <- filter(out, variable == GLOBAL_TAS())
tas %>%
  select(year, value, which) %>% 
  # renormalize to 1951-1980 mean, to match GISS
  filter(year %in% 1951:1980) %>%
  group_by(which) %>%
  summarise(mean19511980 = mean(value)) %>%
  right_join(tas, by = "which") %>%
  mutate(value = value - mean19511980) %>%
  select(-mean19511980) %>%
  bind_rows(OBSERVED_TAS)  %>%
  filter(year %in% OBSERVED_TAS$year) ->
  tas_combined

ggplot(tas_combined, aes(year, value, color = which)) +
  geom_line() + 
  ggtitle("Air temperature (not optimized against)") + scale_color_discrete("")

# Plot all data for four main outputs
ggplot(out, aes(year, value, color = which)) +
  geom_line() +
  facet_wrap(~variable, scales = "free") +
  scale_color_discrete("")

shutdown(core)
sessionInfo()

