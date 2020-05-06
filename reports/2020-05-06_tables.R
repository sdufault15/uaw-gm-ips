# 2020-05-06_tables.R
# Kevin Chen
# May 6, 2020
# Summary of study population characteristics

library(here); library(boxr); box_auth(); library(tidyverse); library(tikzDevice)
# Pick variable to use for year of employment end
yout.which <- "year_left_work"
# yout.which <- "YOUT16"

# Load analytic data based on which variable used as year of employment end
dta_ips <- box_read(ifelse(yout.which == "YOUT16", 657149129798, 656285655983))

lackingrecords <- dta_ips %>%
	filter(is.na(A)) %>%
	select(STUDYNO) %>%
	distinct() %>%
	unlist()

dta_ips <- dta_ips %>%
	filter(!STUDYNO %in% lackingrecords)

<<<<<<< HEAD
<<<<<<< HEAD
=======
=======
<<<<<<< HEAD
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
# Helper function to format dates
gm.to.date <- function(x) {
	x <- x + 1900
	as.Date(paste0(floor(x), '/01/01')) +
		floor((x - floor(x)) * time_length(difftime(
			as.Date(paste0(floor(x), "-12-31")),
			as.Date(paste0(floor(x), "-01-01"))), 'day'
		))}

# Helper function for table
get.tab1 <- function(
	df = dta_ips,
	table_engine = "xtable",
	use_finrace = T,
	mathmode = T) {
	
	require(data.table); require(lubridate)
	
	df <- as.data.table(as.data.frame(df))
	
	setorder(df, STUDYNO, year)
	
	df[,`:=`(
		YIN16 = as.Date(gm.to.date(YIN16), origin = '1970-01-01'),
		YOB = as.Date(gm.to.date(YOB), origin = '1970-01-01'),
		yod09 = as.Date(gm.to.date(yod09), origin = '1970-01-01'),
		year_left_work = as.Date(gm.to.date(year_left_work), origin = '1970-01-01')
	)]
	
=======
<<<<<<< HEAD
=======
>>>>>>> ae5fc54e6c774bf4ea15880e614992740c5ad0d5
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
# Helper function for table
get.tab1 <- function(
	df,
	table_engine = "xtable",
	use_finrace = T,
	mathmode = T,
	col.name = "stat") {
	
	require(data.table); require(lubridate)
	
	df <- as.data.table(as.data.frame(as.data.table(df)[filler == 0]))
	
	setorder(df, STUDYNO, year)
	
<<<<<<< HEAD
>>>>>>> e273474c83be8a5bba658d1bc54222febec0b125
=======
<<<<<<< HEAD
=======
>>>>>>> e273474c83be8a5bba658d1bc54222febec0b125
>>>>>>> ae5fc54e6c774bf4ea15880e614992740c5ad0d5
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
	# Individual-level summary
	tab1.sum <- df[,.(
		
		'\\textbf{Race}, $n$ (\\%)' = NA,
		'\\hspace{10pt}White' = if (use_finrace) {
			ifelse(FINRACE[1] == 1, 1, 0)} else {
				ifelse(FINRACE[1] == 1 | FINRACE[1] == 9, 1, 0)
			},
		'\\hspace{10pt}Black' = ifelse(FINRACE[1] == 2, 1, 0),
		'\\hspace{10pt}Unknown' = if (use_finrace) {
			ifelse(FINRACE[1] == 9, 1, 0)},
		'\\textbf{Sex}, $n$ (\\%)' = NA,
		'\\hspace{10pt}Male' = ifelse(sex[1] == 'M', 1, 0),
		'\\hspace{10pt}Female' = ifelse(sex[1] == 'F', 1, 0),
<<<<<<< HEAD
<<<<<<< HEAD
=======
=======
<<<<<<< HEAD
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
		"\\textbf{Plant}$^\\natural$, $n$ (\\%)" = NA,
		"\\hspace{10pt}Plant 1" = as.numeric(names(sort(table(PLANT[year < 1995]), decreasing = T)[1])) == 1,
		"\\hspace{10pt}Plant 2" = as.numeric(names(sort(table(PLANT[year < 1995]), decreasing = T)[1])) == 2,
		"\\hspace{10pt}Plant 3" = as.numeric(names(sort(table(PLANT[year < 1995]), decreasing = T)[1])) == 3,
		'\\textbf{Complete work records}' = as.numeric(
			year(year_left_work[1]) < 1995),
		
		# Years since follow-up
		"\\textbf{Years of follow-up}" = {
			{
						time_length(
							difftime(min(yod09[1], yoc[1], as.Date("2017-01-01"), na.rm = T), YIN16[1]), 'years') - 3
					}
		},
		'\\textbf{Year of hire}' = as.numeric(year(YIN16[1])),
		'\\textbf{Age at hire}' = time_length(difftime(YIN16[1], YOB[1]), 'years'),
		'\\textbf{Year of birth}' = as.numeric(year(YOB[1])),
		'\\textbf{Year of worker exit}' = year(year_left_work[1]),
		'\\textbf{Age at worker exit}' = time_length(difftime(year_left_work[1],	YOB[1]), 'years'),
		'\\textbf{Age at death among deceased}' = time_length(difftime(
			yod09[1], YOB[1]), 'years'),
		'\\textbf{Year of death among deceased}' =
			year(yod09[1])
=======
<<<<<<< HEAD
=======
>>>>>>> ae5fc54e6c774bf4ea15880e614992740c5ad0d5
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
		"\\textbf{Plant}$^a$, $n$ (\\%)" = NA,
		"\\hspace{10pt}Plant 1" = as.numeric(names(sort(table(PLANT[year < 1995]), decreasing = T)[1])) == 1,
		"\\hspace{10pt}Plant 2" = as.numeric(names(sort(table(PLANT[year < 1995]), decreasing = T)[1])) == 2,
		"\\hspace{10pt}Plant 3" = as.numeric(names(sort(table(PLANT[year < 1995]), decreasing = T)[1])) == 3,
		'\\textbf{Complete work records}' = as.numeric(year_left_work[1] < 1995),
		
		# Years since follow-up
		"\\textbf{Years of follow-up}" = min(2016, yod09[1], na.rm = T) - YIN16[1] + 3,
		'\\textbf{Year of hire}' = floor(YIN16[1]),
		'\\textbf{Age at hire}' = YIN16[1] - YOB[1],
		'\\textbf{Year of birth}' = floor(YOB[1]),
		'\\textbf{Year of leaving work}' = floor(year_left_work[1]),
		'\\textbf{Age at leaving work}' = year_left_work[1] -	YOB[1],
		'\\textbf{Age at death among deceased}' = yod09[1] - YOB[1],
		'\\textbf{Year of death among deceased}' = floor(yod09[1])
<<<<<<< HEAD
>>>>>>> e273474c83be8a5bba658d1bc54222febec0b125
=======
<<<<<<< HEAD
=======
>>>>>>> e273474c83be8a5bba658d1bc54222febec0b125
>>>>>>> ae5fc54e6c774bf4ea15880e614992740c5ad0d5
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
	),
	by = .(STUDYNO)][,-'STUDYNO']
	
	# Population-level summary
	table.break <- "Years of follow-"
	tab1 <- rbind(
		t(apply(tab1.sum[,1:(grep(table.break, names(tab1.sum)) - 1)], 2, function(x) {
			return(
				c(mean(x, na.rm = T) * sum(!is.na(x)),
					mean(x, na.rm = T) * 100,
					NA
					# NA,
					# NA
				)
			)
		})),
		t(apply(tab1.sum[,grep(table.break, names(tab1.sum)):ncol(tab1.sum)], 2,
						function(x) {
							return(
								c(
									# mean(x, na.rm = T),
									# sd(x, na.rm = T)
									# min(x, na.rm = T),
									median(as.numeric(x), na.rm = T),
									quantile(as.numeric(x), 0.25, na.rm = T),
									quantile(as.numeric(x), 0.75, na.rm = T)
									# max(x, na.rm = T)
								)
							)
						}))
	)
	
	tab1[!is.finite(as.matrix(tab1))] <- NA
	
	# Table names
	colnames(tab1) <- c('n', '%', "temp")#, 'Minimum', 'Median', 'Maximum')
	
	# Digits
	tab1.digits <- matrix(2, ncol = 3,#5
												nrow = nrow(tab1))
	tab1.digits[grep("Age", rownames(tab1), ignore.case = T), ] <- 0
	tab1.digits[grep("year ", rownames(tab1), ignore.case = T), ] <- 0
	tab1.digits[1:(grep(table.break, rownames(tab1), ignore.case = T) - 1), -1] <- 0
	
	tab1 <- matrix(
		sapply(1:length(tab1), function(i) {
			as.character(round(as.vector(tab1)[i], as.vector(tab1.digits)[i]))
		}),
		ncol = ncol(tab1),
		nrow = nrow(tab1),
		dimnames = dimnames(tab1)
	)
	
	# Pretty counts
	tab1[1:grep("Years of", rownames(tab1)), 1] <- sapply(
		tab1[1:grep("Years of", rownames(tab1)), 1],
		function (i) {
			if (!is.na(i)) {
				prettyNum(as.numeric(i), big.mark = '\\\\,')
			} else {NA}
		})
	
	# Pretty percents
	tab1[1:(grep("Years of fo", rownames(tab1)) - 1), 2] <- sapply(
		tab1[1:(grep("Years of fo", rownames(tab1)) - 1), 2],
		function (i) {
			if (!is.na(i)) {
				paste(as.numeric(i), '\\%')
			} else {NA}
		})
	
	# Quartiles one column
	tab1[!is.na(tab1[,3]),2] <- paste0(
		tab1[!is.na(tab1[,3]),2],
		",\\,",
		tab1[!is.na(tab1[,3]),3]
	)
	
	tab1 <- tab1[,-3]
	
	# Second column gets parentheses
	tab1[!is.na(tab1[,1]), 2] <- paste0(
		"(", tab1[!is.na(tab1[,1]), 2], ")"
	)
	
	# Math mode
	if (mathmode) {
		tab1 <- matrix(
			sapply(1:length(tab1), function(i) {
				if (!is.na(as.vector(tab1)[i])) {
					paste0('$', as.vector(tab1)[i], '$')
				} else {NA}
			}),
			ncol = ncol(tab1),
			nrow = nrow(tab1),
			dimnames = dimnames(tab1))
		
		# Counts
		tab1 <- rbind(
			c(paste0('$',
							 prettyNum(n_distinct(df$STUDYNO), big.mark = '\\\\,'),
							 '$'),
				paste0('$(',
<<<<<<< HEAD
<<<<<<< HEAD
=======
							 prettyNum(sum(df[,.(py = min(yod09[1], 2016) - YIN16[1] + 3), by = .(STUDYNO)]$py), big.mark = '\\\\,'),
=======
<<<<<<< HEAD
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
							 prettyNum(nrow(df[(is.na(yod09) | year <= year(yod09)) |
							 										(is.na(yoc) | year <= year(yoc))]), big.mark = '\\\\,'),
=======
							 prettyNum(sum(df[,.(py = min(yod09[1], 2016) - YIN16[1] + 3), by = .(STUDYNO)]$py), big.mark = '\\\\,'),
>>>>>>> e273474c83be8a5bba658d1bc54222febec0b125
<<<<<<< HEAD
=======
>>>>>>> ae5fc54e6c774bf4ea15880e614992740c5ad0d5
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
							 ')$')
			),
			tab1
		)
		
		rownames(tab1)[1] <- "$N$ (person$\\cdot$years)"
		
<<<<<<< HEAD
<<<<<<< HEAD
=======
=======
<<<<<<< HEAD
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
		# # Collapse columns
		# tab1[!is.na(tab1[,1]), 1] <- paste(
		# 	tab1[!is.na(tab1[,1]), 1],
		# 	tab1[!is.na(tab1[,1]), 2]
		# )
		# tab1 <- tab1[,-2]
		
		tab1.rownames <- c(rownames(tab1),
											 "\\textbf{Suicide cases}",
											 "\\textbf{Fatal overdose cases}")
		tab1 <- rbindlist(list(
			as.data.frame(tab1),
			data.frame(
				n = c(paste0("$", sum(df$Suicide), "$"),
							paste0("$", sum(df$Overdose), "$")),
				'%' = c("", ""),
				check.names = F
			)
		))
=======
<<<<<<< HEAD
=======
>>>>>>> ae5fc54e6c774bf4ea15880e614992740c5ad0d5
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
		# Collapse columns
		tab1[!is.na(tab1[,1]), 1] <- paste(
			tab1[!is.na(tab1[,1]), 1],
			tab1[!is.na(tab1[,1]), 2]
		)
		tab1 <- tab1[,-2]
<<<<<<< HEAD
>>>>>>> e273474c83be8a5bba658d1bc54222febec0b125
=======
<<<<<<< HEAD
=======
>>>>>>> e273474c83be8a5bba658d1bc54222febec0b125
>>>>>>> ae5fc54e6c774bf4ea15880e614992740c5ad0d5
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
	}
	
	tab1 <- as.data.frame(tab1)
	
<<<<<<< HEAD
<<<<<<< HEAD
=======
=======
<<<<<<< HEAD
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
	rownames(tab1) <- tab1.rownames
	
	colnames(tab1) <- c('V1', 'V2')
	
	return(tab1)
}
=======
<<<<<<< HEAD
=======
>>>>>>> ae5fc54e6c774bf4ea15880e614992740c5ad0d5
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
	if (table_engine != "xtable") {
		rownames(tab1) <- gsub("\\\\.*\\{|10pt\\}|\\}", "", rownames(tab1))
	}
	
	colnames(tab1) <- col.name
	
	return(tab1)
}

all.tab1 <- get.tab1(dta_ips, col.name = "Full cohort")
suicide.tab1 <- get.tab1(filter(dta_ips, suicide == 1), col.name = "Suicide cases")
poison.tab1 <- get.tab1(filter(dta_ips, poison == 1), col.name = "Fatal overdose cases")

tab1 <- cbind(all.tab1, suicide.tab1, poison.tab1)
<<<<<<< HEAD
>>>>>>> e273474c83be8a5bba658d1bc54222febec0b125
=======
<<<<<<< HEAD
=======
>>>>>>> e273474c83be8a5bba658d1bc54222febec0b125
>>>>>>> ae5fc54e6c774bf4ea15880e614992740c5ad0d5
>>>>>>> 797e2e1dedd05fa6d4b2dc81273cf6b497dc8fe3
