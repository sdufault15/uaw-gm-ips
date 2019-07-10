# munge

library(tidyverse)
library(boxr)
library(here)
library(splitstackshape)

box_auth()
dta <- box_read(291022061422) # "raw" cohort file

source(here("munge", "01_administrative.R"))
source(here("munge", "02_subsetting-cohort.R"))
source(here("munge", "03_demographic-covariate-cleaning.R"))
source(here("munge", "04_incorporating-new-ndi.R"))
source(here("munge", "05_incorporating-suicide-overdose-codes.R"))

# source(here("munge", "04_data-cleaning.R"))
# source(here("munge", "data-cleaning-end-of-work.R"))
# source(here("munge", "data-cleaning-immortal-times-summary.R"))