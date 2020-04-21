# Calling in new NDI data and merging with existing

new_ndi <- box_search("cod10_15_noid.RData") %>%
  box_read()

ndi <- new_ndi %>% 
  select(STUDYNO = studyno,
         V_ICD = v_icd,
         yod09 = yod15,
         cod_09 = ICD) %>%
  mutate(yod09 = 1900 + yod09)

# Identifying those that did and did not have 
# a yod09 in the old cohort data
missing_death_old <- cohort %>%
  filter(is.na(yod09))

not_missing_death_old <- cohort %>%
  filter(!is.na(yod09))


# Combining new NDI data with those missing records
new_dta <- missing_death_old %>%
  select(-cod_09, -yod09, -V_ICD)
new_dta <- left_join(new_dta, ndi, by = "STUDYNO") %>%
  mutate(V_ICD = as.numeric(V_ICD))

# Combining new data with old data
cohort <- bind_rows(new_dta, not_missing_death_old)

# One person has two separate NDI entries - removing for now
cohort <- cohort %>%
  filter(STUDYNO != 141368)

# Saving
box_save(cohort, 
         dir_id = 80875764240, 
         file_name = "2019-07-01_cohort-covariates.RData",
         description = "Just the subset from the original auto_vs_09_v5 file.")
