
# uaw-gm-ips
Repository to hold code for UAW GM IPS manuscript

## Change log

- April 21, 2020
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
