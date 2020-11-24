# devtools::install_github("ehkennedy/npcausal")

library(tidyverse)
library(boxr)
library(date)
box_auth()

#library(foreach)
#library(doSNOW)
library(here)

# Which variable to use as year of employment end
yout.which <- "year_left_work"
# yout.which <- "YOUT16"

# Include baseline covariates YIN16, race, and sex?
full_ps <- F

# Which Box directory
box.dir <- ifelse(yout.which == "YOUT16",
                  111331236161,
                  111328660604)

# Suzanne's script for getting time-varying covariates (tidyverse implementation)
# source(here("lib", "2019-07-15_time-varying-function_nonpar.R"))
# Kevin's script for getting time-varing covariates (data.table implementation)
source(here("lib", "2020-04-21_time-varying-function_nonpar.R"))

cohort <- box_read(485768272681) # need the long dataset for mean age ...
cohort_long <- box_read(488427532080) # the long dataset
dta_end_of_employment <- box_read(656270359800) # Job history

# Merge end of employment
cohort <- full_join(
  (if (yout.which == "YOUT16") {cohort %>% select(-yout.which)} else {cohort}),
  (dta_end_of_employment %>% select(STUDYNO, yout.which,
                                    if (yout.which == "year_left_work") {"month_left_work"},
                                    if (yout.which == "year_left_work") {"day_left_work"}) %>% distinct),
  by = "STUDYNO")

cohort_long <- full_join(
  (if (yout.which == "YOUT16") {cohort_long %>% select(-yout.which)} else {cohort_long}),
  (dta_end_of_employment[,c("STUDYNO", yout.which,
                            if (yout.which == "year_left_work") {"month_left_work"},
                            if (yout.which == "year_left_work") {"day_left_work"})] %>% distinct),
  by = "STUDYNO")

# Remove people who have been employed for less than 3 years
if (yout.which == "year_left_work") {
  # Make decimal year of leaving work corresponding to the new leave work date
  date.to.gm <- function(x = "2013-01-01") {
    require(lubridate)
    as.numeric(
      year(x) +
        time_length(difftime(x, as.Date(paste0(year(x), "-01-01"))), "year") / (
          time_length(difftime(as.Date(paste0(year(x), "-12-31")),
                               as.Date(paste0(year(x), "-01-01"))), "year"))
    )}

  cohort$year_left_work.gm <- 1995
  cohort[cohort$month_left_work != 1995, "year_left_work.gm"] <- mutate(cohort[cohort$month_left_work != 1995,],
                                                                        year_left_work.gm = paste(year_left_work, month_left_work, day_left_work, sep = "/")
  ) %>% mutate(year_left_work.gm = date.to.gm(as.Date(year_left_work.gm))) %>%
    select(year_left_work.gm) %>% unlist

  cohort_long <- full_join(cohort_long,
                           cohort[,c("STUDYNO", "year_left_work.gm")],
                           by = "STUDYNO")

  # Filter
  cohort %>% filter(round(year_left_work.gm - YIN16, 1) >= 3) -> cohort
  cohort_long %>% filter(round(year_left_work.gm - YIN16, 1) >= 3) -> cohort_long
} else {
  cohort %>% filter(round(YOUT16 - YIN16, 1) >= 3) -> cohort
  cohort_long %>% filter(round(YOUT16 - YIN16, 1) >= 3) -> cohort_long
}

# Build person-year dataset
dta_person_year_full <- time_varying_function_nonpar(dta_end_of_employment)
dta_person_year_full <- dta_person_year_full %>%
  distinct()
box_save(dta_person_year_full,
         dir_id = box.dir,
         file_name = "2020-04-21_full-person-year-data.RData",
         description = "Long form version of the cohort dataset with time-varying covariates.")

# dta_person_year_full <- box_read(ifelse(yout.which == "YOUT16", 661842322105, 661838165913))

# Drop person-time before 1970
# Identify those who were in the cohort by 1970
fixed_covariates <- cohort %>%
  filter(
    yout.which > 1970,
    yrin16 < yout.which) # filter by new end of employment date, not YOUT16

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
dta_person_year_full <- dta_person_year_full %>%
  filter(STUDYNO %in% ids)

n_distinct(dta_person_year_full$STUDYNO)
length(ids)

###############################################
# Combine time-varying and fixed data
###############################################

n_distinct(dta_person_year_full$STUDYNO)
n_distinct(dta_ips_long$STUDYNO)

dta_person_year_full <- dta_person_year_full %>%
  mutate(cal_obs = year)
dta_ips <- left_join(dta_person_year_full, dta_ips_long, by = c("STUDYNO", "cal_obs"))
dta_ips <- left_join(select(dta_ips, -YOB, -YIN16, -yrin16, -race, -sex, - yod15, -yout.which),
                     select(cohort, STUDYNO, YOB, YIN16, yrin16, race, sex, yod15, yout.which),
                     by= c("STUDYNO"))

# Get rid of rows after death, rows before yrin16, rows after 1994
dta_ips <- dta_ips %>% filter(
  cal_obs <= apply(data.frame(floor(get(yout.which)), 1994), 1, min),
  cal_obs <= apply(data.frame(floor(yod15), 2015), 1, min),
  cal_obs >= floor(yrin16))

library(data.table)
setDT(dta_ips)
# Fill missing covariates
dta_ips[, `:=`(
  yearWork = zoo::na.locf(yearWork),
  ndays.GAN = zoo::na.locf(ndays.GAN, na.rm = F),
  ndays.HAN = zoo::na.locf(ndays.HAN, na.rm = F),
  ndays.SAN = zoo::na.locf(ndays.SAN, na.rm = F),
  prop.days.GAN = zoo::na.locf(prop.days.GAN, na.rm = F),
  prop.days.HAN = zoo::na.locf(prop.days.HAN, na.rm = F),
  prop.days.SAN = zoo::na.locf(prop.days.SAN, na.rm = F)
), by = .(STUDYNO)]

# There are gaps in the work history data
dta_ips[, .(
  n.missing = sum(is.na(ndays.mach))),
        by = .(STUDYNO)][n.missing > 0]$n.missing %>% table

# Fill them in, for now
dta_ips[, `:=`(
  ndays.mach = zoo::na.locf(ndays.mach),
  ndays.assembly = zoo::na.locf(ndays.assembly),
  ndays.off = zoo::na.locf(ndays.off),
  prop.days.mach = zoo::na.locf(prop.days.mach),
  prop.days.assembly = zoo::na.locf(prop.days.assembly),
  prop.days.off = zoo::na.locf(prop.days.off)
)]

# Fill in time-varying covariates for time after leaving work
dta_ips[cal_obs > get(yout.which), `:=`(
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
         dir_id = box.dir,
         file_name = "2020-04-21_ips-data-with-work.RData",
         description = "Long form version of the cohort dataset with baseline and time-varying covariates.")

###############################################
# Transform data
###############################################
# dta_ips <- box_read(ifelse(yout.which == "YOUT16", 657147487614, 656275470844))

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
dta_ips <- dta_ips %>% filter(cal_obs <= 1994, cal_obs >= 1970)

# Make indicator for whether "filler" rows were added
dta_ips$filler <- 0

# Make indicator for whether "filler" rows were added
dta_ips$filler <- 0

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
leaders$filler <- 1

# Make trailing rows
trailers <- as.data.table(dta_ips)[STUDYNO %in% need.trail.who,]
trailers[,`:=`(cal_obs.max = max(cal_obs)), by = .(STUDYNO)]
trailers <- trailers[cal_obs == cal_obs.max]
trailers <- merge(trailers[,.(cal_obs = ((cal_obs[1] + 1):1994)), by = .(STUDYNO)],
                  trailers[,-"cal_obs"],
                  on = "STUDYNO",
                  all = T)[,-"cal_obs.max", with = F]
trailers$filler <- 1

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
  mutate(A = ifelse(cal_obs >= floor(get(yout.which)) & yout.which != 1995, 1, 0))

box_save(dta_ips,
         dir_id = box.dir,
         file_name = "2020-04-23_ips-data-with-work-final.RData",
         description = "Final analytic dataset.")

# dta_ips <- box_read(ifelse(yout.which == "YOUT16", 657149129798, 656285655983))

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
  PLANT, calendar_year, age_obs, pension.eligibility, cumulative_days_off,
  if (full_ps) {"YIN16"},
  if (full_ps) {"sex"},
  if (full_ps) {"race"}
) %>% data.matrix()
x.out <- dta_ips %>% select(
  YIN16,
  race,
  sex,
  pension.eligibility
) %>% data.matrix()
time <- dta_ips %>% ungroup() %>% select(cal_obs) %>% unlist()
a <- dta_ips %>% ungroup() %>% select(A) %>%  unlist()
# y <- dta.ips %>% select(STUDYNO, suicide) %>% distinct() %>% select(suicide) %>% unlist() # just suicides
id <- dta_ips$STUDYNO

# Outcome vector for different end of FU cutoffs ####
FU_cutoffs <- c(2015, 2010, 2005, 2000, 1995)
y <- lapply(FU_cutoffs, function(x) {
           dta_ips %>% mutate(SIM = ifelse((suicide == 1 | poison == 1) & yod15 < x + 1, 1, 0)) %>%
             select(STUDYNO, SIM) %>% distinct() %>% ungroup() %>% select(SIM) %>% unlist()
         })
names(y) <- FU_cutoffs

for (k in FU_cutoffs) {
  y_k <- unlist(y[names(y) == k])
  dim(x.trt); dim(x.out)
  length(y_k); length(table(id))
  table(c(length(time), length(a), length(y_k) * length(table(time)) , length(id)))

  # delta.seq <- unique(c(seq(0.1,1.5, by = 0.025), seq(1.5, 5, by = 0.5)))
  delta.seq <- unique(c(seq(0.75,1.25, by = 0.025)))

  # File name indicating delta range
  file.name <- paste0(
    "ipsi_", round(delta.seq[1], digits = 2), "-",
    round(delta.seq[length(delta.seq)], digits = 2),
    ifelse(full_ps, "_full-PS", ""),
    "_FU-through-", k,
    ".RData")

  ipsi.res <- npcausal::ipsi(y_k, a, x.trt, x.out, time, id, delta.seq, nsplits = 3)
  box_save(ipsi.res,
           dir_id = box.dir,
           file_name = file.name,
           description = paste0(
             "IPS results for delta spanning ",
             delta.seq[1], " to ", delta.seq[length(delta.seq)],
             ", using `", yout.which, "` for year of leaving work.",
             ifelse(full_ps, " PS model includes year of hire, race, and sex.", ""),
             " FU through ", k, "."))
}

####################################
# Calendar year and outcome status #
####################################
# library(data.table); library(pander)
# dta_ips_long.dt <- as.data.table(dta_ips_long)
# dta_ips_long.dt[,`:=`(
#     suicide = {if (suicide[1] == 1) {
#         suicide <- rep(0, .N)
#         suicide[cal_obs == floor(yod15)] <- 1
#         suicide
#     } else {rep(0, .N)}},
#     poisoning = {if (poison[1] == 1) {
#         poison <- rep(0, .N)
#         poison[.N] <- 1
#         poison
#     } else {rep(0, .N)}}
# ), by = .(STUDYNO)]
# box_write(dta_ips_long.dt,
#          "dta_ips_long.dt.rds",
#          box.dir)
# dta_ips_long.dt <- box_read(741123550410)
#
# rbindlist(lapply(c(2015, 2010, 2005, 2000, 1995), function(x) {
#   dta_ips_long.dt[
#     get(yout.which) >= 1970 & cal_obs >= 1970 & cal_obs <= x, .(
#     `Follow-up` = x,
#     Suicide = sum(suicide),
#     Poisoning = sum(poisoning),
#     SIM = sum(suicide ==1 | poisoning == 1)
# )]})) %>% pander
