library(tidyverse)
library(boxr)
library(date)
box_auth()

#library(foreach)
#library(doSNOW)
library(here)

# Suzanne's script for getting time-varying covariates (tidyverse implementation)
# source(here("lib", "2019-07-15_time-varying-function_nonpar.R"))
# Kevin's script for getting time-varing covariates (data.table implementation)
source(here("lib", "2020-04-21_time-varying-function_nonpar.R"))

cohort <- box_read(485768272681) # need the long dataset for mean age ...
cohort_long <- box_read(488427532080) # the long dataset
dta_end_of_employment <- box_read(656270359800) # Job history

# Merge end of employment
cohort <- full_join(
  cohort,
  (dta_end_of_employment %>% select(STUDYNO, year_left_work) %>% distinct),
  by = "STUDYNO")

cohort_long <- full_join(
  cohort_long,
  (dta_end_of_employment %>% select(STUDYNO, year_left_work) %>% distinct),
  by = "STUDYNO")

# Identify those who were in the cohort by 1970
fixed_covariates <- cohort %>%
  filter(#yrin16 < 1970 & 
    year_left_work > 1970 &
    yrin16 < year_left_work) # filter by new end of employment date, not YOUT16


# Comment out, as we decided not to limit by age
# # Identify those who were at least 30 and no greater than 45 in 1970
# fixed_covariates <- fixed_covariates %>%
#   filter(1970 - YOB <= 45 & 1970 - YOB >= 30)

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

n_distinct(work_hist$STUDYNO)
length(ids)

# Build person-year dataset
# COMMENTED TO SAVE TIME - FEEL FREE TO RERUN IF THE COHORT CHANGES
dta_person_year_full_1970s <- time_varying_function_nonpar(work_hist)
dta_person_year_full_1970s <- dta_person_year_full_1970s %>%
  distinct()
box_save(dta_person_year_full_1970s,
         dir_id = 80875764240,
         file_name = "2020-04-21_full-person-year-data-1970s.RData",
         description = "Long form version of the cohort dataset with time-varying covariates.")

# Drop person-time before 1970
# dta_person_year_full_1970s <- box_read(656269683749)
dta_person_year_full_1970s <- dta_person_year_full_1970s %>%
  filter(year >= 1970)

###############################################
# Combine time-varying and fixed data
###############################################

n_distinct(dta_person_year_full_1970s$STUDYNO)
n_distinct(dta_ips_long$STUDYNO)

dta_person_year_full_1970s <- dta_person_year_full_1970s %>%
  mutate(cal_obs = year)
dta_ips <- left_join(dta_person_year_full_1970s, dta_ips_long, by = c("STUDYNO", "cal_obs"))
dta_ips <- left_join(select(dta_ips, -YOB, -YIN16, -race, -sex, - yod09),
                     select(cohort, STUDYNO, YOB, YIN16, race, sex, yod09), by= c("STUDYNO"))

# Get rid of rows after death, rows before yrin16, rows after 1994
dta_ips <- dta_ips %>% filter(
  cal_obs <= floor(year_left_work),
  cal_obs <= floor(yod09),
  cal_obs >= floor(yrin16),
  cal_obs >= 1970,
  cal_obs <= 1994)

library(data.table)
setDT(dta_ips)
# Fill missing covariates
dta_ips[,`:=`(
  yearWork = zoo::na.locf(yearWork),
  ndays.GAN = zoo::na.locf(ndays.GAN, na.rm = F),
  ndays.HAN = zoo::na.locf(ndays.HAN, na.rm = F),
  ndays.SAN = zoo::na.locf(ndays.SAN, na.rm = F),
  prop.days.GAN = zoo::na.locf(prop.days.GAN, na.rm = F),
  prop.days.HAN = zoo::na.locf(prop.days.HAN, na.rm = F),
  prop.days.SAN = zoo::na.locf(prop.days.SAN, na.rm = F)
), by = .(STUDYNO)]

# There are gaps in the work history data
dta_ips[,.(n.missing = sum(is.na(ndays.mach))),
        by = .(STUDYNO)][n.missing > 0]$n.missing %>% table

# Fill them in, for now
dta_ips[,`:=`(
  ndays.mach = zoo::na.locf(ndays.mach),
  ndays.assembly = zoo::na.locf(ndays.assembly),
  ndays.off = zoo::na.locf(ndays.off),
  prop.days.mach = zoo::na.locf(prop.days.mach),
  prop.days.assembly = zoo::na.locf(prop.days.assembly),
  prop.days.off = zoo::na.locf(prop.days.off)
)]

# Fill in time-varying covariates for time after leaving work
dta_ips[cal_obs > year_left_work,`:=`(
  ndays.mach = 0,
  ndays.assembly = 0,
  ndays.off = 0,
  prop.days.mach = 0,
  prop.days.assembly = 0,
  prop.days.off = 0
)]

setorder(dta_ips, STUDYNO, cal_obs)
dta_ips <- as.data.frame(dta_ips)
detach(package:data.table)

box_save(dta_ips,
         dir_id = 80875764240,
         file_name = "2020-04-21_ips-data-with-work.RData",
         description = "Long form version of the cohort dataset with baseline and time-varying covariates.")

###############################################
# Transform data
###############################################
# dta_ips <- box_read(656275470844)

# Identify pension eligibility - primarily based off of the 30-and-out rule
dta_ips <- dta_ips %>% mutate(pension.eligibility = case_when(yearWork >= 30 ~ 1,
                                       TRUE ~ 0))

dta_ips <- dta_ips %>%
  group_by(STUDYNO) %>%
  mutate(cumulative_days_off = cumsum(ndays.off)) %>% mutate(
    cumulative_days_off = zoo::na.locf(cumulative_days_off, na.rm = F)
  )

###############################################
# Add leading and lagging entries for those without followup
###############################################

# Remove un-needed rows
dta_ips <- dta_ips %>% filter(cal_obs <= 1994 &
                                cal_obs >= 1970)

# Duplicate `cal_obs`
dta_ips <- dta_ips %>% mutate(calendar_year = cal_obs)

# Those who need leading rows
dta_ips %>% group_by(STUDYNO) %>% summarise(
  need = min(cal_obs) > 1970
) %>% filter(need == T) %>% select(STUDYNO) %>% unlist -> need.lead.who

# Those who need trailing rows
dta_ips %>% group_by(STUDYNO) %>% summarise(
  need = max(cal_obs) < 1994
) %>% filter(need == T) %>% select(STUDYNO) %>% unlist -> need.trail.who

library(data.table)
# Make leading rows
leaders <- as.data.table(dta_ips)[STUDYNO %in% need.lead.who,]
leaders[,`:=`(cal_obs.min = min(cal_obs)), by = .(STUDYNO)]
leaders <- leaders[cal_obs == cal_obs.min]
leaders <- merge(leaders[,.(cal_obs = ((cal_obs[1] - 1):1970)), by = .(STUDYNO)],
                 leaders[,-"cal_obs"],
                 on = "STUDYNO",
                 all = T)[,-"cal_obs.min", with = F]

# Make trailing rows
trailers <- as.data.table(dta_ips)[STUDYNO %in% need.trail.who,]
trailers[,`:=`(cal_obs.max = max(cal_obs)), by = .(STUDYNO)]
trailers <- trailers[cal_obs == cal_obs.max]
trailers <- merge(trailers[,.(cal_obs = ((cal_obs[1] + 1):1994)), by = .(STUDYNO)],
                  trailers[,-"cal_obs"],
                  on = "STUDYNO",
                  all = T)[,-"cal_obs.max", with = F]

# Bind newly created rows
dta_ips <- rbindlist(list(leaders, dta_ips, trailers), use.names = T)
setorder(dta_ips, STUDYNO, cal_obs)
dta_ips <- as.data.frame(dta_ips)
detach(package:data.table)

# Confirm there are 25 rows for all people
dta_ips %>%
  group_by(cal_obs) %>%
  summarize(n = n_distinct(STUDYNO)) %>% select(n) %>% table

dta_ips <- dta_ips %>%
  mutate(A = ifelse(cal_obs >= floor(year_left_work) & year_left_work != 1995, 1, 0))

box_save(dta_ips,
         dir_id = 80875764240,
         file_name = "2019-08-22_ips-data-with-work-final.RData",
         description = "Final analytic dataset.")

# dta_ips <- box_read(656285655983)

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

# Verify order
dta_ips <- ungroup(arrange(dta_ips, STUDYNO, cal_obs))
x.trt <- dta_ips %>% select(
  yearWork,
  prop.days.mach, prop.days.assembly, prop.days.off,
  PLANT, calendar_year, age_obs, pension.eligibility, cumulative_days_off
) %>% data.matrix()
x.out <- dta_ips %>% select(
  YIN16,
  race,
  sex,
  pension.eligibility
) %>% data.matrix()
time <- dta_ips %>% ungroup() %>% select(cal_obs) %>% unlist()
a <- dta_ips %>% ungroup() %>% select(A) %>%  unlist()
#y <- dta.ips %>% select(STUDYNO, suicide) %>% distinct() %>% select(suicide) %>% unlist() # just suicides
y <- dta_ips %>% mutate(SIM = ifelse(suicide == 1 | poison == 1, 1, 0)) %>% select(STUDYNO, SIM) %>% distinct() %>% ungroup() %>% select(SIM) %>% unlist()
id <- dta_ips$STUDYNO

dim(x.trt); dim(x.out)
length(time); length(a); length(y) ; length(id)

delta.seq <- unique(c(seq(0.1,1.5, by = 0.025), seq(1.5, 5, by = 0.5)))
# ipsi.res <- npcausal::ipsi(y, a, x.trt, x.out, time, id, delta.seq, nsplits = 3)
# box_save(ipsi.res,
#          dir_id = 80875764240,
#          file_name = "ipsi.RData")
