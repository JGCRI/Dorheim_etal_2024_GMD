# PUlling out basic information from Hector.... 

library(dplyr)
library(hector)

system.file("input/tables/ssp245_emiss-constraints_rf.csv", package = "hector") %>%  
  read.csv(comment.char = ";") -> 
  data

xx <- names(data)
# subtract the one because the ffi_emissions and luc_emissions both refer to CO2 emissions 
emissions <- xx[grepl(pattern = "emiss", x = xx)]
emission_count <- xx[grepl(pattern = "emiss", x = xx)] %>% length() - 1 # the minus 1 accounts for the fact that there are two co2 emission categories


# The emissions that are immeidately used to calcaulte their repsective forcing effect on Hector's energy budget. 
# NH3, BC, SO2, OC  
immediate <- 4 

# The emissions that accumulate or effect ghg gasses in the atmosphere as gases. The non CO2 gases  
emission_count - immediate


CONCENTRATIONS_CH4()
CONCENTRATIONS_CO2()
CONCENTRATIONS_N2O()
CONCENTRATIONS_O3()
help(RF_ACI)


all_rf <- c(RF_ALBEDO(), RF_CO2() ,RF_H2O_STRAT(), RF_O3_TROP(), RF_BC(), RF_OC(), RF_NH3(),
            RF_SO2(), RF_ACI(), RF_VOL(), RF_MISC(), RF_CH4()
            , RF_CF4(), RF_C2F6(), RF_HFC23(), RF_HFC32(), RF_HFC4310(), RF_HFC125(), RF_HFC134A(), RF_HFC143A(), 
            RF_HFC227EA(), RF_HFC245FA(), RF_SF6(), RF_CFC11(), RF_CFC12(), RF_CFC113(), RF_CFC114(), RF_CFC115(), 
            RF_CCL4(), RF_CH3CCL3(), RF_HCFC22(), RF_HCFC141B(), RF_HCFC142B(), RF_HALON1211(), RF_HALON1301(), 
            RF_HALON2402(), RF_CH3CL(), RF_CH3BR())


lines <- readLines(file.path(system.file("include", package = "hector"), "component_data.hpp"))
rf <- lines[grepl(pattern = "D_RF_", x = lines)]
exlucde <- "D_RF_BASEYEAR|#define D_RF_PREFIX |D_FTOT_CONSTRAIN|D_RF_TOTAL|D_RF_halocarbons"
indvidual_rf <- rf[!grepl(pattern = exlucde, x = rf)]
length(indvidual_rf)

