# Final Cleaning


# Race
cohort <- cohort %>% 
  mutate(race = ifelse(FINRACE != 2, 1, 0)) #%>% select(-FINRACE) # changing race to binary

# Sex
cohort <- cohort %>%
  mutate(sex = ifelse(SEX == 2, 0, 1)) %>% select(-SEX)

# Changing age of hire to 13 for an 8 year old hire
cohort$YOB[cohort$STUDYNO == 118137] <- 
  cohort$YOB[cohort$STUDYNO == 118137] - 5  # changing age of 8 year old employee to 13

# Creating a variable for age of hire
cohort <- cohort %>% 
  mutate(ageHire = YIN16 - YOB) # age of hire

# Replacing end of employment date with yod15 if yod15 < YOUT16
cohort <- cohort %>% 
  mutate(YOUT16 = ifelse(is.na(yod15), YOUT16, ifelse(YOUT16 > yod15, yod15, YOUT16))) # replacing end of employment with date of death when date of death comes first
#cohort <- cohort %>% mutate(yod15 = replace(yod15, is.na(yod15), 110)) # adding time of administrative censoring


# Changing the years
cohort <- cohort %>%
  mutate(YOB = 1900 + YOB,
         YIN16 = 1900 + YIN16,
         YOUT16 = 1900 + YOUT16,
         yod15 = ifelse(is.na(yod15), NA, 1900 + yod15),
         yrin16 = 1900 + yrin16)

