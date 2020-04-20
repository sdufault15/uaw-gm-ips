# munge

library(tidyverse)
library(boxr)
library(here)
library(splitstackshape)
library(date)

box_auth()
dta <- box_read(655863812176) # "raw" cohort file


source(here("munge", "01_administrative.R"))
source(here("munge", "02_subsetting-cohort.R"))
source(here("munge", "03_demographic-covariate-cleaning.R"))
source(here("munge", "04_incorporating-new-ndi.R"))
source(here("munge", "05_incorporating-suicide-overdose-codes.R"))
source(here("munge", "06_creating-long-dataset.R")) # just demographics, no work history

source(here("munge", "07_adding-work-history.R"))
source(here("munge", "08_ips-dataset.R"))

# source(here("munge", "04_data-cleaning.R"))
# source(here("munge", "data-cleaning-end-of-work.R"))
# source(here("munge", "data-cleaning-immortal-times-summary.R"))