# Final Cleaning

cohort <- cohort %>% mutate(race = ifelse(FINRACE != 2, 1, 0)) #%>% select(-FINRACE) # changing race to binary
cohort <- cohort %>% filter(!(YOUT16 < YIN16)) # removing individuals who were employed a negative amount of time (?)
cohort$YOB[cohort$STUDYNO == 118137] <- cohort$YOB[cohort$STUDYNO == 118137] - 5  # changing age of 8 year old employee to 13
cohort <- cohort %>% mutate(ageHire = YIN16 - YOB) # age of hire
cohort <- cohort %>% mutate(YOUT16 = ifelse(is.na(yod09), YOUT16, ifelse(YOUT16 > yod09, yod09, YOUT16))) # replacing end of employment with date of death when date of death comes first
cohort <- cohort %>% mutate(yod09 = replace(yod09, is.na(yod09), 110)) # adding time of administrative censoring

box_save(cohort, dir_id = 51069117810, file_name = "cohort8282018.RData")
