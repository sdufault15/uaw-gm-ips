# 2020-05-19_descriptive-figures.R
# Suzanne Dufault, adpated by Kevin Chen
# May 19, 2020
# Descriptive plots

library(tidyverse)
library(here)
library(tikzDevice)
library(boxr)
box_auth()
library(ggridges)
library(ggfittext)

yout.which <- "YOUT16"

source(here::here("reports", "ggplot-theme.R"))

# Some helper functions for ggplotting
get.center <- function(quants) {
	sapply(2:(length(quants)), function(i) {
		mean(c(quants[i - 1], quants[i]))
	})
}

get.widths <- function(quants) {
	sapply(2:(length(quants)), function(i) {
		quants[i] - quants[i - 1] + (i == 2)
	})
}

get.sim.tab <- function(tib) {
	sim.tab <- dplyr::summarize(
		tib,
		n = length(table(STUDYNO)),
		n_py = length(STUDYNO),
		Suicide = sum(suicide)/length(STUDYNO),
		`Fatal overdose` = sum(poison)/length(STUDYNO),
		`Suicide and fatal overdose` = sum(poison + suicide)/length(STUDYNO),
		Plant = PLANT[1],
		Race = ifelse(RACE[1] == 0, "Black", "White"))

	sim.pivot <- pivot_longer(
		sim.tab,
		c("Suicide", "Fatal overdose", "Suicide and fatal overdose"),
		names_to = "outcome",
		values_to = "rate")

	return(sim.pivot)
}

# Render plot in TeX
directory.name <- here::here("graphs")

theme.tmp <- theme_bw() + mytheme + theme(
	legend.key.size = unit(10, "pt"),
	plot.margin = margin(5, 5, 5, 5)
)

# Helper function for pretty cut labels
pretty.cut <- function(x = c("(35, 45]",
														 "(45, 60]"),
											 right = T) {
	if (right) {
		upper.delim <- "\\("
		lower.delim <- "\\]"
	} else {
		upper.delim <- "\\["
		lower.delim <- "\\)"
	}

	lower <-
		gsub(" ", "", substr(x, unlist(gregexpr(upper.delim, x)) + 1,
												 unlist(gregexpr(",", x)) - 1))
	upper <- gsub(" ", "", substr(x, unlist(gregexpr(",", x)) + 1,
																unlist(gregexpr(lower.delim, x)) - 1))

	lower <- as.numeric(lower)
	upper <- as.numeric(upper)

	if (right) {
		lower <- lower + 1
	}

	return(paste0(lower, " to ", upper))
}

# Load analytic data based on which variable used as year of employment end
cohort_long <- box_read(488427532080) # the long dataset
names(cohort_long)[names(cohort_long) == "race"] <- "RACE"

dta_end_of_employment <- box_read(656270359800) # Job history
# Load cohort data
cohort <- box_read(485768272681)
cohort <- filter(cohort, STUDYNO %in% cohort_long$STUDYNO)

# Merge end of employment
dta.selected <- distinct(dta_end_of_employment[, c(
	"STUDYNO",
	"year_left_work",
	"month_left_work",
	"day_left_work")])

yout.select <- function(tibble) {
	if ("year_left_work" %in% colnames(tibble)) {
		return(select(tibble,-"year_left_work"))
	} else {
		return(tibble)
	}
}

# Merge end of employment
sapply(c("cohort", "cohort_long"), function(x) {
	assign(x,
				 full_join(yout.select(get(x, envir = .GlobalEnv)),
				 					dta.selected, by = "STUDYNO"),
				 envir = .GlobalEnv)
})

cohort_long <- arrange(cohort_long, STUDYNO, cal_obs)

# Remove people who have been employed for less than 3 years
# Make decimal year of leaving work corresponding to the new leave work date
date.to.gm <- function(x = "2013-01-01") {
	require(lubridate)
	as.numeric(year(x) +
						 	time_length(difftime(x, as.Date(
						 		paste0(year(x), "-01-01")
						 	)), "year") / (time_length(difftime(
						 		as.Date(paste0(year(x), "-12-31")),
						 		as.Date(paste0(year(x), "-01-01"))
						 	), "year")))
}

cohort$year_left_work.gm <- 1995
cohort[cohort$month_left_work != 1995, "year_left_work.gm"] <-
	mutate(
		cohort[cohort$month_left_work != 1995, ],
		year_left_work.gm = paste(year_left_work, month_left_work, day_left_work, sep = "/")
	) %>% mutate(year_left_work.gm = date.to.gm(as.Date(year_left_work.gm))) %>%
	select(year_left_work.gm) %>% unlist

cohort_long <- full_join(cohort_long,
												 cohort[, c("STUDYNO", "year_left_work.gm")],
												 by = "STUDYNO")

# Filter
cohort %>% filter(round(year_left_work.gm - YIN16, 1) >= 3) -> cohort
cohort_long %>% filter(round(year_left_work.gm - YIN16, 1) >= 3) -> cohort_long

# Make suicide and overdose time-varying
cohort_long %>% filter(suicide == 1) %>% select(STUDYNO) %>% n_distinct()
cohort_long %>% filter(poison == 1) %>% select(STUDYNO) %>% n_distinct()

cohort_long %>% mutate(suicide = ifelse(cal_obs == floor(yod15) & suicide == 1, 1, 0)) -> cohort_long
cohort_long %>% mutate(poison = ifelse(cal_obs == floor(yod15) & poison == 1, 1, 0)) -> cohort_long

# Pick year of leaving work variable
if (yout.which == "YOUT16") {
	cohort_long$year_left_work <- floor(cohort_long$YOUT16)
	cohort_long$year_left_work.gm <- cohort_long$YOUT16
}

# Figure 0 ####
# Trends in worker exit for the UAW-GM cohort. The observed annual rates of leaving work among actively employed workers are denoted by the solid blue line. A smooth loess line fit to the observed annual rates is plotted in gold with an error bar to denote 95% confidence intervals. The rate of leaving work hovered around 4% from the 1960s until the early 1980s. The sharp upward trend of worker exit after 1980 is the result of forced separation from mass layoffs as well as an aging cohort that is not replenishing after 1981.
employment <- cohort_long %>%
	ungroup() %>%
	group_by(STUDYNO) %>%
	filter(floor(cal_obs) <= year_left_work) %>%
	mutate(leaving_work = ifelse(
		floor(cal_obs) == floor(year_left_work) &
			year_left_work != 1995, 1, 0))

employment_plot.tab <- employment %>%
	ungroup() %>%
	#filter(PLANT != 9) %>%
	group_by(cal_obs) %>%
	dplyr::summarize(n = length(table(STUDYNO)),
									 n_leave = sum(leaving_work)) %>%
	mutate(rate = n_leave / n) %>%
	filter(cal_obs < 1995)

# Lets group the cal_obs by quantiles of n
cal_obs.quantiles <- quantile(unlist(apply(employment_plot.tab, 1, function(x) {
	as.numeric(rep(x[1], x[2]))
})), seq(0, 1, 0.05))

cal_obs.means <- get.center(cal_obs.quantiles)
cal_obs.years <- get.widths(cal_obs.quantiles)

employmenet_plot_binned.tab <- dplyr::summarize(
	group_by(mutate(
		employment_plot.tab,
		cal_obs = cut(cal_obs, cal_obs.quantiles, include.lowest = T)
	), cal_obs),
	n = sum(n),
	n_leave = sum(n_leave))

employmenet_plot_binned.tab$means <- cal_obs.means
employmenet_plot_binned.tab$years <- cal_obs.years

employmenet_plot_binned.tab <- mutate(
	employmenet_plot_binned.tab, rate = n_leave / n)

ggplot(data.frame()) +
	xlab("Calendar Year") +
	ylab("UAW-GM Employees\n Leaving Work, \\%") +
	#facet_wrap(~PLANT, ncol = 1) +
	geom_smooth(
		data = employment_plot.tab,
		aes(x = cal_obs,
				y = rate * 100),
		fill = "grey",
		alpha = 0.3,
		size = 0.5,
		color = "black",
	) +
	geom_point(data = employmenet_plot_binned.tab,
						 aes(x = means,
						 		y = rate * 100),
						 size = 1) +
	geom_rug(
		data = dplyr::summarize(group_by(
			filter(employment, year_left_work != 1995), STUDYNO),
			year_left_work.gm = year_left_work.gm[1])[sample(1:27599, 1000), ],
		aes(x = year_left_work.gm)) +
	scale_x_continuous(
		breaks = seq(1940, 1995, by = 5),
		labels = as.vector(sapply(seq(1940, 1990, by = 10), function(x) {return(c(x, ""))}))) +
	geom_vline(
		xintercept = 1981, lty = 3, alpha = 0.5) +
	coord_cartesian(
		xlim = c(1942, 1993)) +
	annotate(
		geom = "text",
		x = 1980, y = 13, label = "End of Enrollment",
		size = 2.5, col = "black", hjust = "right", alpha = 0.5) +
	theme.tmp -> fig0

# quartz(width = 4, height = 3)
# fig0
# dev.off()

# tikz(
# 	paste0(directory.name, "/Figure 0.tex"),
# 	standAlone = T,
# 	width = 4,
# 	height = 3
# )
# print(fig0)
# dev.off()

# Figure 1 ####
# SIM over time
sim.tab <- cohort_long %>%
	group_by(cal_obs) %>%
	get.sim.tab()

sim.quantiles <- quantile(unlist(filter(cohort_long, suicide == 1 | poison == 1)[, "cal_obs"]), seq(0, 1, 0.05))
sim.center <- get.center(sim.quantiles)
sim.quantiles[c(1, length(sim.quantiles))] <- c(min(cohort_long$cal_obs), max(cohort_long$cal_obs))

sim.binned.tab <- cohort_long %>%
	group_by(calyear = cut(
		cal_obs,
		breaks = sim.quantiles,
		include.lowest = T)) %>%
	get.sim.tab()

sim.binned.tab$means <- rep(sim.center, each = 3)

ggplot(data.frame()) +
	xlab("Calendar Year") +
	ylab("Suicide and Fatal Overdose Count") +
	#facet_wrap(~PLANT, ncol = 1) +
	geom_line(
		data = employment_plot.tab ,
		aes(x = cal_obs,
				y = n / 6e2,
				linetype = "Employees at work"),
	) +
	geom_bar(data = mutate(filter(sim.tab, outcome == "Suicide and fatal overdose"),
												 # levels(calyear) <= 9),
												 n = rate * n_py) %>% filter(n > 0) %>%
					 	group_by(cal_obs) %>%
					 	# group_by(means) %>%
					 	summarize(n = 1:n),
					 aes(
					 	# x = means,
					 	x = cal_obs,
					 	# y = rate * n_py,
					 	fill = "Suicide and Fatal Overdose"
					 )) +
	scale_fill_manual(values = "grey") +
	# geom_rug(
	# 	data = dplyr::summarize(group_by(
	# 		filter(employment, year_left_work != 1995), STUDYNO),
	# 		year_left_work.gm = year_left_work.gm[1])[sample(1:27599, 1000), ],
	# 	aes(x = year_left_work.gm)) +
	scale_y_continuous(
		sec.axis = sec_axis( ~ . * 6e2, name = "Employees At Work")) +
	scale_x_continuous(
		breaks = seq(1940, 2015, by = 5),
		labels = as.vector(sapply(seq(1940, 2010, by = 10), function(x) {return(c(x, ""))}))) +
	geom_text(aes(
		x = 1995,
		y = filter(employment_plot.tab, cal_obs == max(cal_obs))$n / 6e2
		),
		label = "1995 - End of\nemployment records",
		size = 2,
		hjust = 0) +
	coord_cartesian(
		xlim = c(1937, 2017),
		ylim = c(0, 46)) +
	theme.tmp  +
	theme(legend.position = c(0.001, 0.92),
				legend.margin = margin(-10, 5, 0, 5, "pt"),
				legend.box.margin = margin(10, 1, 5, 1, "pt"),
				axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5)),
				axis.title.y.right = element_text(margin = margin(t = 0, r = 2, b = 0, l = 8)),
				legend.key.size = unit(6, "pt")
	) -> fig1

# quartz(width = 4, height = 3)
fig1
# dev.off()

tikz(
	paste0(directory.name, "/Figure 1.tex"),
	standAlone = T,
	width = 4,
	height = 3
)
print(fig1)
dev.off()

# Compile using lualatex
lualatex("Figure 1.*\\.tex", directory.name)

# Age at leaving work and calendar year ####
cohort_long %>%
	ungroup() %>% filter(
		YOUT16 != 1995,
		cal_obs == floor(YOUT16),
		yod15 > YOUT16
	) %>% select(STUDYNO, YOUT16, YOB) -> yout_age

cohort_long %>%
	ungroup() %>% filter(
		year_left_work.gm != 1995,
		cal_obs == floor(year_left_work.gm),
		yod15 > year_left_work.gm
	) %>% select(
		STUDYNO, year_left_work.gm, YOB,
		YOUT16) -> year_left_work_age

tikz(
	paste0(directory.name, "/yout16_age.tex"),
	standAlone = T,
	width = 4,
	height = 3
)
yout_age %>% ggplot(
	aes(x = YOUT16, y = YOUT16 - YOB)
) + geom_point(alpha = 0.2, cex = 0.2) +
	geom_smooth(se = F, cex = 0.75) +
	geom_hline(yintercept = 65, color = "red", alpha = 0.5) +
	labs(x = "Year of leaving work (YOUT16)",
			 y = "Age at leaving work") +
	theme.tmp
dev.off()
lualatex("yout16_age\\.tex", directory.name, break_after = 60*3)

tikz(
	paste0(directory.name, "/year-left-work_age.tex"),
	standAlone = T,
	width = 4,
	height = 3
)
year_left_work_age %>% ggplot(
	aes(x = year_left_work.gm, y = year_left_work.gm - YOB)
) + geom_point(alpha = 0.2, cex = 0.2) +
	geom_smooth(se = F) +
	geom_hline(yintercept = 65, color = "red", alpha = 0.5) +
	labs(x = "Year of leaving work",
			 y = "Age at leaving work") +
	theme.tmp
dev.off()
lualatex("year-left-work_age\\.tex", directory.name, break_after = 60*3)
