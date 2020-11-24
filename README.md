
# uaw-gm-ips
Repository to hold code for UAW GM IPS manuscript

## Change log

### November 23, 2020

- `munge/08_ips-data-70s.R`
 	- Lines 1-2: Script for installing the `npcausal` package
	- Lines 300-336: Loop IPS analysis over different ends of follow-up
		- Ending FU in 2010, 2010, 2005, 2000, or 1995
	- Lines 338-367: Verify case counts

### July 13, 2020

- `reports/2020-05-06_tables.Rmd`
	- Lines 79-86: Exclude those still at work in 1995 for rows "Year of worker exit" and "Age at worker exit" in the tables of population characteristics

### July 7, 2020

- `munge/08_ips-data-70s.R`
	- Changed name of flag `augmented_ps` to `full_ps`
- `reports/2020-04-23_ips-curve.R`
	- Script specifying `ggplot2` theme elements and `tikz` options moved to new file
	- Line 120: the theme for Figure 4 is now based on `theme_bw()` and not `theme_classic()`
- `reports/2020-05-19_descriptive_figures.R`
	- Figures now follow consistent aesthetic themes
	- Script specifying `ggplot2` theme elements and `tikz` options moved to new file
	- Lines 124-152: Greatly simplified data manipulation steps for plotting
	- Lines 148-50: New helper functions `get.center()` and `get.widths()` to return summary measures of quantile cutpoints
	- Lines 232-240: New helper function `get.sim.tab()` for getting counts and rates from a grouped long dataset
	- Lines 301-381: New Figure 3: self-injury mortality rates by plant over the period of downsizing: years 1970--2015
- `reports/ggplot-theme.R`
	- New file for `ggplot2` theme elements and `tikz` options

### May 21, 2020

- `munge/02_subsetting-cohort.R`
	- Line 6: Use mortality follow-up through 2015, not through 2009
- `munge/03_demographic-covariate-cleaning.R`
	- Lines 12-15: Move cohort subsetting using end of employment year to later in the workflow
	- Lines 20, 22, 23, 31: Use mortality follow-up through 2015, not through 2009
- `munge/05_incoporating-suicide-overdose-codes.R`
	- Lines 12-15, 18, 20-22, 29, 30: Use mortality follow-up through 2015, not through 2009
- `munge/06_creating-long-dataset.R`
	- Line 6: Use mortality follow-up through 2015, not through 2009
- `munge/07_adding-work-history.R`
	- Lines 187-186: Add variables `month_left_work` and `day_left_work` to go with `year_left_work`
- `munge/08_ips-data-70s.R`
	- Lines 14-16: Add switch `augmented_ps` to allow user to run IPS with or without year of hire, race, and sex in the PS model
	- Lines 34-26, 41-43: Incorporate month and day of leaving work
	- Lines 46-75: Remove people who have been employed for less than 3 years using either `YOUT16` or the new date of leaving work variable (depends on `year_left_work`, `month_left_work`, and `day_left_work`)
		- Note that a new helper function `date.to.gm` is defined, which maps objects of class `Date` to `numeric` objects indicating calendar year in decimal form
	- Lines 77-85: By default, run `time_varying_function_nonpar()`
	- Lines 123, 124, 129, 130: Use mortality follow-up through 2015, not through 2009
	- Lines 282-285: Depending on the value of `augmented_ps`, include (or not) `YIN16`, `sex`, and `race` in the tratment model
	- Lines 309-311, 320, 321: Give new file name and description to IPS run with `augmented_ps = T`
- Deleted un-used script in `munge`
- `reports/2020-04-23_ips-curve.R`
	- Lines 7-10: Unify `ggplot` theme across the 4 figures
	- Lines 15-17, 38, 39: Add switch `augmented_ps` to allow user to load IPS results with or without year of hire, race, and sex in the PS model
	- Lines 41-121: Minor changes in `ggplot` appearance
	- Lines 161-162, 320, 321: Give new file name and description to Figure 4 with `augmented_ps = T`
	- Lines 174, 175, 152, 183: Include `magick` in system commands to produce image files from the compiled TeX
- `reports/2020-05-06_tables.Rmd`
	- Lines 20-29, 230, 232, 234: Change working file for the "full cohort" to a version from earlier in the workflow; merge end of employment variable
	- Line 53, 69-71: Use `cal_obs`, which was created earlier in the workflow, not `year`, which was created using `time_varying_function_nonpar()` (and is therefore NA for years not represented in the work history files)
	- Line 75, 81-84, 187: Use mortality follow-up through 2015, not through 2009
- `reports/2020-05-19_descriptive-figures.R`
	- New script, adapted from `2019-07-10_figures.Rnw`, for making Figures 1, 2, and 3

### May 8, 2020

- `munge/plot_ips.R` renamed to `reports/2020-04-23_ips-curve.R`
- `munge/08_ips-data-70s.R`
	- Lines 166-172: Filtering by year of leaving work moved downstream in the workflow, to facilitate construction of Tables 1 and 2 (summaries of population characteristics)
- `reports/2020-05-06_tables.Rmd`
	- New code for generating Tables 1 and 2
	- Question: Should Table 2 (summary of population characteristics of the analytic cohort) summarize time after 1994?

### April 27, 2020

- `munge/plot_ips.R`
	- Lines 23-27, 44-45, 47, 63-64: Update IPS effect curve to include both point-wise and uniform confidence band estimates

### April 24, 2020

- `munge/08_ips-data-70s.R`
	- Lines 30 and 35: Avoid merge conflict when using `YOUT16` for year of leaving work
	- Line 269: Defining a narrowing range for $\delta$
	- Line 271: Creates a file name for the output of `npcausal::ipsi()` that includes information on the range of $\delta$
	- Line 277: Added file description when saving output of `npcausal::ipsi()` to Box

### April 23, 2020

- `munge/08_ips-data-70s.R`
	- Line 10: Create a "switch" `yout.which` for picking variable to use as year of employment end: `YOUT16` _or_ `year_left_work`
	- Line 14: Upload intermediate data steps to different Box directories, depending on which end of employment variable was selected
	- Script sourced again, and all `box_read()` statements updated

### April 22, 2020

- `munge/2020-07-21_time-varying-function-nonpar.R`
	- Re-naming variables created by the function to explicitly reflect number of days or proportion of year
- `munge/08_ips-data-70s.R`
	- Line 32: Now allowing people to enter cohort after 1970
	- Line 56: Run `time_varying_function_nonpar()` for entire work history (for selected `STUDYNO`s)
	- Line 93: Get rid of rows after death/year of leaving work, rows before entry into cohort, and rows after 1994
	- Line 100: Carry forward job history data (imputation ends on year of leaving work, thanks to previous subsetting step)
	- Line 127: Set machining to 0 after leaving work (should not affect any rows)
	- Line 150: Removed lines re-creating `prop.days`, as proportions are already given as output of `time_varying_function_nonpar()`
	- Line 156: Avoid `NA` days off from affecting cumulative sums
	- Line 164: Remove un-needed rows
	- Lines 171-207: Pre-pend and post-pend as needed, such that each individual has 1994 - 1970 + 1 = 25 rows
	- Line 238: Change covariate list to match that given by Suzanne
			- Exposure (propensity to leave work)
				- `yearWork` cumulative years of employment
				- `prop.days.mach` proportion of year spent in machining
				- `prop.days.assembly` proportion of year spent in assembly
				- `prop.days.off` proportion of time spent off work
				- `PLANT` plant
				- `calendar_year` calendar year
				- `age_obs` age
				- `pension.eligibility` pension eligibility
				- `cumultaive_days_off` cumulative time off work
			- Outcome
				- Propensities (from first model)
				- `YIN16` year of hire
				- `race` race
				- `sex` sex
				- `pension.eligibility` pension eligibility

### April 21, 2020

- `munge/07_adding-work-history.R`
	- Line 41: Collapsed duplicated entries
	- Line 89: Get `STUDYNO`s for those whose final and penultimate entries are both non-numeric with `DATEIN==DATEOUT`
	- Line 120: Ignore last record if both the final and penultimate entries are non-numeric with `DATEIN==DATEOUT`
	- Line 175: Create `year_left_work`, taking into account all that was discussed
	- Changed file name passed to `box_save()`
- `munge/08_ips-data-70s.R`
	- Line 15: Load `dta_end_of_employment`
	- Line 17: Merge `year_left_work` much earlier in the workflow so that we can filter `cohort` by the newly-created end of employment year instead of `YOUT16`
	- Line 27: Comment out filtering step by age
	- Line 64, 85, 158: Save to `Box`, not locally
	- Line 90: Make sure important time-invariant variables are inherited for all folks
	- Line 156: using `year_left_work` instead of `YOUT16`
- `munge/2019-07-15_time-varying-function-nonpar.R`
	- Added default arguments for easier troubleshooting
- `munge/2020-07-21_time-varying-function-nonpar.R`
	- Fast implementation with `data.table`
	- Leap years accounted for, to avoid issue where certain years were duplicated
		- See `STUDYNO` 100017
- `06_creating-long-dataset.R`
	- Line 5: Make sure all calendar years are represented
