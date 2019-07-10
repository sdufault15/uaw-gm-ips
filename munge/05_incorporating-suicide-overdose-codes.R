#############
# Suzanne Dufault
# Fixing the coding of several variables for the working dataset
# July 2, 2019
#############

library(boxr)
library(tidyverse)

box_auth() # "signing in"
cohort <- box_read(484669026313) # reading in the data file "[last-modified-date]_cohort-covariates.RData"

###########################################
# Data Processing
###########################################
cohort <- cohort %>% 
  mutate(death.indicator = ifelse(!is.na(cod_09), 1, 0),
         yod09 = ifelse(is.na(yod09), 2016, yod09))
sum(cohort$yod09 - cohort$YOB > 100) # There are 218 individuals who are over the age of 100 in the dataset
sum(cohort$yod09[cohort$death.indicator == 0] - cohort$YOB[cohort$death.indicator == 0] > 100) # 201 of whom have not yet been recorded as dead

# Censor the individuals who die after the age of 100. As such, I am going to modify their death.indicators to all be 2 and 
# set their age at yod09 to be 90
# Hence, death.indicator: 0/alive, 1/dead, 2/censored
cohort$death.indicator[cohort$yod09 - cohort$YOB > 100] <- 2
table(cohort$death.indicator) # there are now 219 censored individuals in the dataset
cohort$yod09[cohort$death.indicator == 2] <- cohort$YOB[cohort$death.indicator == 2] + 100 # they are now all censored at age 100

# Matching the Descriptions by Case & Deaton
source(here("lib","2019-07-01_suicide-overdose-helper-function.R"))
sim <- self_injury_function()

cohort <- cohort %>% 
  mutate(suicide = ifelse(cod_09 %in% sim$suicide_codes & death.indicator != 2, 1, 0),
         poison = ifelse(cod_09 %in% sim$overdose_codes & death.indicator !=2, 1, 0))
                      

box_save(cohort, dir_id = 80875764240, file_name = "2019-07-03_cohort-demographics.RData", 
         description = "Includes indicator of death or censoring and Rockett et al. suicide/poisoning indicators as suggested by Sidra.")
