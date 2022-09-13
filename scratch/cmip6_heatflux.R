# UGH this is really annoying okay so there so a lot going on well need to have Leeya clean up 
# the and global mean temp UGH this is annnoying I wish th

list.files(system.file("input", package = "hector"), pattern = "ini", full.names = TRUE) %>% 
  lapply(function(ini){
    vars_to_keep <- c(GLOBAL_TEMP(), RF_TOTAL(), RH(), HEAT_FLUX())
    
    core <- newcore(ini)
    run(core)
    out <- fetchvars(core, dates = 1850:2100, vars_to_keep)
    scenario <- gsub(x = basename(ini), pattern = "hector_|.ini", replacement = "")
    
    out$scenario <- scenario
    out$version <- "3"
    return(out)
    
  }) %>%
  do.call(what = "rbind") ->
  out

here::here("data", "comparison", "CMIP6", "CMIP5_heat_flux_final.csv") %>% 
  read.csv(stringsAsFactors = FALSE) -> 
  cmi5_hf
cmi5_hf %>% head()

out %>% 
  filter(variable == HEAT_FLUX()) -> 
  hector_hf

ggplot() + 
  geom_point(data = cmi5_hf, aes(year, value, color = "CMIP5")) + 
  geom_line(data = hector_hf, aes(year, value, color = "Hector v3")) + 
  facet_wrap("scenario", scales = "free")


here::here("data", "comparison", "CMIP6", "Global_tas_data.csv") %>% 
  read.csv() %>% 
  filter(type == "global") %>% 
  select(year, value = Tgav, ensemble, model, scenario = experiment) %>%  
  filter(scenario %in%  hector_hf$scenario) %>%  
  filter(year <= 2100) -> 
  cmip6



out %>% 
  filter(variable == GLOBAL_TEMP()) -> 
  hector_tgav

hector_tgav$scenario %>% unique(
  
)
cmip6 %>%  
  group_by(year, model, scenario) %>%  
  summarise(value = mean(value)) %>% 
  ungroup -> 
  cmip6

ggplot() + 
  geom_line(data = cmip6, aes(year, value, color = "cmip6", group_by = interaction(model)), alpha = 0.4 , color = "grey") + 
  geom_line(data = hector_tgav, aes(year, value, color = "Hector v3"), color = "black") + 
  facet_wrap("scenario", scales = "free") + 
  labs(y = NULL, x = NULL, title = "Ocean Heat flux")
  





