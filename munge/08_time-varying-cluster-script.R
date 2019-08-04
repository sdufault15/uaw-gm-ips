###############################
# Creating time-varying dataset
###############################
library(date)
library(dplyr)
library(tidyr)
library(foreach)
library(doSNOW)
library(here)

source(here("lib", "2019-07-15_time-varying-function.R"))
load(here("data", "2019-07-10_cohort_work-history_long.RData"))
dta <- dta_end_of_employment

cl <- makeCluster(24, 
                  outfile="") # number of cores. Notice 'outfile'
registerDoSNOW(cl)

identifiers <- unique(dta$STUDYNO)
tv_out <- time_varying_function_par(dta, identifiers)

save(tv_out,
     file = here("output",
                 "2019-07-15_time-varying-data.RData"))