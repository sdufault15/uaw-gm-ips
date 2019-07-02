# Final Cleaning


# Race
cohort <- cohort %>% 
  mutate(race = ifelse(FINRACE != 2, 1, 0)) #%>% select(-FINRACE) # changing race to binary

# Sex
cohort <- cohort %>%
  mutate(sex = ifelse(SEX == 2, 0, 1)) %>% select(-SEX)

# Negative employment time?
cohort <- cohort %>% 
  filter(!(YOUT16 < YIN16)) # removing individuals who were employed a negative amount of time (?)

# Changing age of hire to 13 for an 8 year old hire
cohort$YOB[cohort$STUDYNO == 118137] <- 
  cohort$YOB[cohort$STUDYNO == 118137] - 5  # changing age of 8 year old employee to 13

# Creating a variable for age of hire
cohort <- cohort %>% 
  mutate(ageHire = YIN16 - YOB) # age of hire

# Replacing end of employment date with yod09 if yod09 < YOUT16
cohort <- cohort %>% 
  mutate(YOUT16 = ifelse(is.na(yod09), YOUT16, ifelse(YOUT16 > yod09, yod09, YOUT16))) # replacing end of employment with date of death when date of death comes first
#cohort <- cohort %>% mutate(yod09 = replace(yod09, is.na(yod09), 110)) # adding time of administrative censoring


# Changing the years
cohort <- cohort %>%
  mutate(YOB = 1900 + YOB,
         YIN16 = 1900 + YIN16,
         YOUT16 = 1900 + YOUT16,
         yod09 = ifelse(is.na(yod09), NA, 1900 + yod09),
         yrin16 = 1900 + yrin16)

