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
dta_person_year_full_1970s <- dta_person_year_full_1970s %>%
  filter(origin >= jj)

###############################################
# Combine time-varying and fixed data
###############################################

n_distinct(dta_person_year_full_1970s$STUDYNO)
n_distinct(dta_ips_long$STUDYNO)

dta_person_year_full_1970s <- dta_person_year_full_1970s %>%
  mutate(cal_obs = date.mdy(origin)$year)
dta_ips <- left_join(dta_person_year_full_1970s, dta_ips_long, by = c("STUDYNO", "cal_obs"))
save(dta_ips, file = here("data","2019-08-22_ips-data-with-work.RData"))

###############################################
# Combine time-varying and fixed data
###############################################
load(here("data","2019-08-22_ips-data-with-work.RData"))

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
  mutate(pension.eligibility = case_when(yearWork >= 30 ~ 1,
                                         TRUE ~ 0))

dta_ips <- dta_ips %>%
  group_by(STUDYNO) %>%
  mutate(cumulative_days_off = cumsum(ndays.off))

###############################################
# Duplicate last entries for those without followup
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
  group_by(STUDYNO) %>%
  mutate(cal_obs = min(cal_obs) + row_number())

lessthan26 <- lessthan26 %>%
  select(-max)

# Merge back together
dta_ips <- bind_rows(dta_ips, lessthan26) %>%
  arrange(STUDYNO, cal_obs)

dta_ips <- dta_ips %>%
  group_by(STUDYNO) %>%
  mutate(rownumber = row_number())

# Confirm there are 26 rows for all people

dta_ips %>%
  #filter(rownumber == max(rownumber)) %>%
  group_by(rownumber) %>%
  summarize(n = n_distinct(STUDYNO)) %>%
  View()

dta_ips <- dta_ips %>%
  mutate(A = ifelse(cal_obs >= floor(YOUT16) & YOUT16 != 1995, 1, 0))

save(dta_ips, file = here("data","2019-08-22_ips-data-with-work-final.RData"))

load(here("data","2019-08-22_ips-data-with-work-final.RData"))

###############
# IPS
###############

lackingrecords <- dta_ips %>%
  filter(is.na(A)) %>%
  select(STUDYNO) %>% 
  distinct() %>%
  unlist()

dta_ips <- dta_ips %>% 
  filter(!STUDYNO %in% lackingrecords)


x.trt <- dta_ips %>% select(STUDYNO, YOB, YIN16, race, sex, 
                            year_obs, age_obs, 
                            prop.days.gan, prop.days.han, prop.days.san,
                            prop.days.mach, prop.days.assembly, prop.days.off,
                            pension.eligibility,
                            cumulative_days_off) %>% data.matrix()
x.out <- dta_ips %>% select(STUDYNO, YOB, race, sex, age_obs) %>% data.matrix()
time <- dta_ips %>% ungroup() %>% select(cal_obs) %>% unlist()
a <- dta_ips %>% ungroup() %>% select(A) %>%  unlist()
#y <- dta.ips %>% select(STUDYNO, suicide) %>% distinct() %>% select(suicide) %>% unlist() # just suicides
y <- dta_ips %>% mutate(SIM = ifelse(suicide == 1 | poison == 1, 1, 0)) %>% select(STUDYNO, SIM) %>% distinct() %>% ungroup() %>% select(SIM) %>% unlist()
id <- dta_ips %>% select(STUDYNO) %>% unlist()

dim(x.trt); dim(x.out)
length(time); length(a); length(y) ; length(id)

d.seq <- c(seq(0.1,0.9, length.out = 9), seq(1,5, length.out = 5))
ipsi.res <- ipsi(y,a, x.trt,x.out, time, id, d.seq)

