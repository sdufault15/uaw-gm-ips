library(tidyverse)
library(boxr)
library(date)
box_auth()

#library(foreach)
#library(doSNOW)
library(here)

source(here("lib", "2019-07-15_time-varying-function_nonpar.R"))
load(here("data","2019-07-10_cohort_work-history_long.RData")) # end of employment dataset (work history)

cohort <- box_read(485768272681) # need the long dataset for mean age ...
cohort_long <- box_read(488427532080) # the long dataset


# Identify those who were in the cohort by 1970
fixed_covariates <- cohort %>%
  filter(yrin16 < 1970 & YOUT16 > 1970)

# Identify those who were at least 30 and no greater than 45 in 1970
fixed_covariates <- fixed_covariates %>%
  filter(1970 - YOB <= 45 & 1970 - YOB >= 30)

###############################################
# THESE ARE THE INDIVIDUALS IN THE COHORT
ids <- fixed_covariates$STUDYNO
###############################################

# Extract their long data entries
dta_ips_long <- cohort_long %>%
  filter(STUDYNO %in% ids)

  n_distinct(dta_ips_long$STUDYNO)
  length(ids)

# Extract their work histories
work_hist <- dta_end_of_employment %>%
  filter(STUDYNO %in% ids)

  n_distinct(work_hist$STUDYNO)
  length(ids)

# Identify work histories beginning in 1970s
work_hist <- work_hist %>%
  filter(date.mdy(DATEIN)$year >= 1970 | date.mdy(DATEOUT)$year >= 1970)

  n_distinct(work_hist$STUDYNO)
  length(ids)

# Build person-year dataset
# COMMENTED TO SAVE TIME - FEEL FREE TO RERUN IF THE COHORT CHANGES
dta_person_year_full_1970s <- time_varying_function_nonpar(work_hist, ids)

dta_person_year_full_1970s <- dta_person_year_full_1970s %>%
  distinct()
save(dta_person_year_full_1970s, file = here("data","2019-08-22_full-person-year-data-1970s.RData"))

# Drop person-time before 1970
load(here("data","2019-08-22_full-person-year-data-1970s.RData"))
jj <- julian(as.POSIXct("1970-01-01"), origin = as.POSIXct("1960-01-01"))[[1]]
dta_person_year <- dta_person_year %>%
  filter(origin >= jj)

###############################################
# Combine time-varying and fixed data
###############################################

n_distinct(dta_person_year$STUDYNO)
n_distinct(dta_ips_long$STUDYNO)

dta_person_year <- dta_person_year %>%
  mutate(cal_obs = date.mdy(origin)$year)
dta_ips <- left_join(dta_person_year, dta_ips_long, by = c("STUDYNO", "cal_obs"))
save(dta_ips, file = here("data","2019-08-09_ips-data-with-work.RData"))

###############################################
# Combine time-varying and fixed data
###############################################

dta_ips <- dta_ips %>%
  rowwise() %>%
  # Identify the proportion of year spent at each plant
  mutate(prop.days.gan = ndays.GAN/sum(ndays.GAN, ndays.HAN, ndays.SAN),
         prop.days.han = ndays.HAN/sum(ndays.GAN, ndays.HAN, ndays.SAN),
         prop.days.san = ndays.SAN/sum(ndays.GAN, ndays.HAN, ndays.SAN)) %>%
  # Identify the proportion of year spent in each job type
  mutate(prop.days.mach = ndays.mach/sum(ndays.mach, ndays.assembly, ndays.off),
         prop.days.assembly = ndays.assembly/sum(ndays.mach, ndays.assembly, ndays.off),
         prop.days.off = ndays.off/sum(ndays.mach, ndays.assembly, ndays.off)) %>%
  # Identify pension eligibility - primarily based off of the 30-and-out rule
  mutate(pension.eligibility = case_when(yearWork >= 30 ~ "eligible",
                                         TRUE ~ "ineligible"))

###############################################
# Add Zero Rows
###############################################

dta_ips <- dta_ips %>%
  group_by(STUDYNO) %>%
  mutate(rownumber = row_number())

lessthan26 <- dta_ips %>%
  filter(max(row_number()) != 26) %>%
  mutate(max = max(rownumber)) %>%
  filter(rownumber == max)

library(splitstackshape)

lessthan26 <- lessthan26 %>%
  ungroup() %>%
  mutate(need = (26 - max)) %>%
  expandRows("need", count.is.col = TRUE, drop = TRUE)

lessthan26 <- lessthan26 %>%
  dplyr::select(-rownumber, -max) %>%
  mutate(prop.days.assembly = 0,
         prop.days.mach = 0,
         prop.days.off = 0,
         prop.days.gan = 0,
         prop.days.han = 0,
         prop.days.san = 0,
         ndays.mach = 0,
         ndays.assembly = 0, 
         ndays.off = 0,
         ndays.SAN = 0,
         ndays.GAN = 0,
         ndays.HAN = 0)

lessthan26 <- lessthan26 %>%
  group_by(STUDYNO) %>%
  mutate(year_obs = min(year_obs) + row_number())
