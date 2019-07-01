# munge

library(dplyr)
library(boxr)
library(here)

box_auth()
dta <- box_read(291022061422)

source(here("munge", "01-Administrative.R"))
source(here("munge", "03-Cohort-Subset.R"))
source(here("munge", "10-Final-Cleaning.R"))

source(here("munge", "data-cleaning.R"))
source(here("munge", "data-cleaning-end-of-work.R"))
source(here("munge", "data-cleaning-immortal-times-summary.R"))