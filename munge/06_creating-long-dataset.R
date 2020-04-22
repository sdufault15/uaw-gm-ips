# Generating long person-year dataset
# This identifies the person-years that an individual is under
# observation in the cohort

cohort_long <- cohort %>%
    mutate(tenure = floor(yod09) - floor(yrin16) + 1) %>%
    expandRows("tenure", count.is.col = TRUE, drop = FALSE) %>%
    group_by(STUDYNO) %>%
    mutate(year_obs = row_number())

cohort_long <- cohort_long %>%
    mutate(age_obs = floor(yrin16 - YOB) + (year_obs - 1),
           cal_obs = floor(yrin16 + year_obs) - 1)

box_save(cohort_long, 
         dir_id = 80875764240, 
         file_name = "2019-07-09_cohort-demographics_long.RData", 
         description = "Long form version of the cohort dataset. One row per person-year of enrollment in cohort.")
