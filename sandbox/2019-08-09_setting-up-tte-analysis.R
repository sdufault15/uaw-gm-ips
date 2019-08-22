###################
# Time-to-event analysis
# August 09, 2019
###################

library(boxr)
box_auth()

cohort <- box_read(485768272681) # need the long dataset for mean age ...
cohort_long <- box_read(488427532080) # the long dataset
load(here("data","2019-07-10_cohort_work-history_long.RData")) # end of employment dataset (work history)
source(here("lib", "2019-07-15_time-varying-function_nonpar.R"))


# Identify those who were observed for at least 1 year after 1970
fixed_covariates <- cohort %>%
  filter(floor(YOUT16) > 1970)

###############################################
# THESE ARE THE INDIVIDUALS IN THE COHORT
ids <- fixed_covariates$STUDYNO
###############################################

# Extract their long data entries
dta_ips_long <- cohort_long %>%
  filter(STUDYNO %in% ids)

# Extract their work histories
work_hist <- dta_end_of_employment %>%
  filter(STUDYNO %in% ids)

# Identify work histories beginning in 1970s
work_hist <- work_hist %>%
  filter(date.mdy(DATEIN)$year >= 1970 | date.mdy(DATEOUT)$year >= 1970)

save(work_hist, file = "data/2019-08-09_time-to-event-cohort_needs-work-hist-person-years.RData")

# BUILD PERSON-YEAR DATASET ON CLUSTER

# Build person-year dataset
# COMMENTED TO SAVE TIME - FEEL FREE TO RERUN IF THE COHORT CHANGES
# dta_person_year <- time_varying_function_nonpar(work_hist, ids)
# 
# dta_person_year <- dta_person_year %>%
#   distinct()
# save(dta_person_year, file = here("data","2019-08-06_person-year-data-1970s.RData"))

# Drop person-time before 1970
load(here("data","2019-08-06_person-year-data-1970s.RData"))
jj <- julian(as.POSIXct("1970-01-01"), origin = as.POSIXct("1960-01-01"))[[1]]
dta_person_year <- dta_person_year %>%
  filter(origin >= jj)

cohort <- cohort %>%
  mutate(cohort_tenure = YOUT16 - yrin16)
