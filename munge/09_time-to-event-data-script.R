############################
# Suzanne Dufault
# TTE Data Wrangling
# August 9, 2019
############################
# Need to RUN THIS - may take 45 minutes
library(tidyr)
library(dplyr)
library(here)
library(date)

load(here("data", "2019-08-09_time-to-event-cohort_needs-work-hist-person-years.RData"))
source(here("lib", "2019-07-15_time-varying-function_nonpar.R"))

ids <- unique(work_hist$STUDYNO)

# Build person-year dataset
dta_person_year <- time_varying_function_nonpar(work_hist, 
                                                ids)

dta_person_year <- dta_person_year %>%
  distinct()
save(dta_person_year, file = here("data","2019-08-09_person-year-time-to-event-1970s.RData"))