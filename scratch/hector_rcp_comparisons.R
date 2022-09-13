library(data.table)
library(ggplot2)
library(hector)
library(magrittr)

BASE_DIR <- here::here()
theme_set(theme_bw())

file.path(BASE_DIR, "data", "comparison", "hector-1.1") %>% 
  list.files(full.names = TRUE) %>%  
  lapply(function(f){
    dt <- as.data.table(read.csv(f, comment.char = "#"))
    cols <- c("year", "run_name", "variable", "value", "units")
    out <- dt[spinup == 0, ..cols]
    names(out) <- c("year", "scenario", "variable", "value", "units")
    out$version <- "1.1"
    return(out)
  }) %>% 
  do.call(what = "rbind") -> 
  out1


file.path(BASE_DIR, "data", "comparison", "hector-2.0.1") %>% 
  list.files(full.names = TRUE) %>%  
  lapply(function(f){
    dt <- as.data.table(read.csv(f, comment.char = "#"))
    cols <- c("year", "run_name", "variable", "value", "units")
    out <- dt[spinup == 0, ..cols]
    names(out) <- c("year", "scenario", "variable", "value", "units")
    out$version <- "2.0.1"
    return(out)
  }) %>% 
  do.call(what = "rbind") -> 
  out2


file.path(BASE_DIR, "data", "comparison", "hector-2.5") %>% 
  list.files(full.names = TRUE) %>%
  lapply(function(f){
    dt <- as.data.table(read.csv(f, comment.char = "#"))
    cols <- c("year", "run_name", "variable", "value", "units")
    out <- dt[spinup == 0, ..cols]
    names(out) <- c("year", "scenario", "variable", "value", "units")
    out$version <- "2.5"
    return(out)
  }) %>% 
  do.call(what = "rbind") -> 
  out3

hector_results <- rbind(out1, out2, out3)

unique(hector_results$year) %>% range()

hector_results$year


hector_results[variable == GLOBAL_TEMP(), ] %>% 
  ggplot(aes(year, value, color = version)) + 
  geom_line() + 
  facet_wrap("scenario", scales = "free")



file.path(BASE_DIR, "input") %>% 
  list.files(pattern = "rcmip", full.names = TRUE) %>% 
  #list.files(pattern = "hectorv1_", full.names = TRUE) %>% 
  lapply(function(ini){
    vars_to_keep <- c(GLOBAL_TEMP(), RF_TOTAL(), RH(), RF_N2O(), RF_CO2(), RF_CH4(), RF_OC(), RF_BC(),RF_SO2(), "Ftalbedo")
    
    core <- newcore(ini)
    run(core)
    out <- fetchvars(core, dates = 1750:2100, vars_to_keep)
    #scenario <- gsub(x = basename(ini), pattern = "hectorv1_|.ini", replacement = "")
    scenario <- gsub(x = basename(ini), pattern = "rcmip_|.ini", replacement = "")
    
    out$scenario <- scenario
    out$version <- "3"
    return(out)

  }) %>%
  dplyr::bind_rows() ->
  out


hector_results %>% 
  rbind(out) -> 
  hector_results

hector_results[variable == GLOBAL_TEMP(), ] %>% 
  .[year <= 2100] %>% 
  ggplot(aes(year, value, color = version)) + 
  geom_line() + 
  facet_wrap("scenario", scales = "free") + 
  labs(title = "Global Mean Temp") + 
  labs(y = NULL, x = NULL)

  
  
hector_results[variable == GLOBAL_TEMP(), ] %>% 
  .[year == 2100] %>% 
  distinct() %>% 
  dcast.data.table(year + scenario + variable + units ~ version) %>% 
  knitr::kable(digits = 3)
  
