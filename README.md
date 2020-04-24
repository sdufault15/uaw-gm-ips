
# uaw-gm-ips
Repository to hold code for UAW GM IPS manuscript

## Change log

### April 23, 2020

- `08_ips-data-70s.R`
	- Line 10: Create a "switch" `yout.which` for picking variable to use as year of employment end: `YOUT16` _or_ `year_left_work`
	- Line 14: Upload intermediate data steps to different Box directories, depending on which end of employment variable was selected
	- Script sourced again, and all `box_read()` statements updated

### April 22, 2020

- `2020-07-21_time-varying-function-nonpar.R`
	- Re-naming variables created by the function to explicitly reflect number of days or proportion of year
- `08_ips-data-70s.R`
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

- `./munge/07_adding-work-history.R`
	- Line 41: Collapsed duplicated entries
	- Line 89: Get `STUDYNO`s for those whose final and penultimate entries are both non-numeric with `DATEIN==DATEOUT`
	- Line 120: Ignore last record if both the final and penultimate entries are non-numeric with `DATEIN==DATEOUT`
	- Line 175: Create `year_left_work`, taking into account all that was discussed
	- Changed file name passed to `box_save()`
- `08_ips-data-70s.R`
	- Line 15: Load `dta_end_of_employment`
	- Line 17: Merge `year_left_work` much earlier in the workflow so that we can filter `cohort` by the newly-created end of employment year instead of `YOUT16`
	- Line 27: Comment out filtering step by age
	- Line 64, 85, 158: Save to `Box`, not locally
	- Line 90: Make sure important time-invariant variables are inherited for all folks
	- Line 156: using `year_left_work` instead of `YOUT16`
- `2019-07-15_time-varying-function-nonpar.R`
	- Added default arguments for easier troubleshooting
- `2020-07-21_time-varying-function-nonpar.R`
	- Fast implementation with `data.table`
	- Leap years accounted for, to avoid issue where certain years were duplicated
		- See `STUDYNO` 100017
- `06_creating-long-dataset.R`
	- Line 5: Make sure all calendar years are represented
