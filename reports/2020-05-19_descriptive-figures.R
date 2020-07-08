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

source(here::here("reports", "ggplot-theme.R"))

# Render plot in TeX
directory.name <- here::here("graphs")

theme.tmp <- theme_bw() + mytheme + theme(
	panel.grid = element_blank(),
	legend.key.size = unit(10, "pt"),
	legend.title = element_text(size = 8),
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
dta_end_of_employment <- box_read(656270359800) # Job history
# Load cohort data
cohort <- box_read(485768272681)
cohort <- filter(cohort, STUDYNO %in% cohort_long$STUDYNO)

# Merge end of employment
dta.selected <- distinct(dta_end_of_employment[, c("STUDYNO",
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

# Figure 1 ####
# Trends in worker exit for the UAW-GM cohort. The observed annual rates of leaving work among actively employed workers are denoted by the solid blue line. A smooth loess line fit to the observed annual rates is plotted in gold with an error bar to denote 95% confidence intervals. The rate of leaving work hovered around 4% from the 1960s until the early 1980s. The sharp upward trend of worker exit after 1980 is the result of forced separation from mass layoffs as well as an aging cohort that is not replenishing after 1981.
employment <- cohort_long %>%
	ungroup() %>%
	group_by(STUDYNO) %>%
	filter(floor(cal_obs) <= year_left_work) %>%
	mutate(leaving_work = ifelse(
		floor(cal_obs) == floor(year_left_work) &
			year_left_work != 1995,
		1,
		0
	))

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

cal_obs.means <- get.center(cal_obs.quantiles)
cal_obs.years <- get.widths(cal_obs.quantiles)

employmenet_plot_binned.tab <- dplyr::summarize(group_by(mutate(
	employment_plot.tab,
	cal_obs = cut(cal_obs, cal_obs.quantiles, include.lowest = T)
), cal_obs),
n = sum(n),
n_leave = sum(n_leave))

employmenet_plot_binned.tab$means <- cal_obs.means
employmenet_plot_binned.tab$years <- cal_obs.years

employmenet_plot_binned.tab <- mutate(employmenet_plot_binned.tab,
																			rate = n_leave / n)

ggplot(data.frame()) +
	xlab("Calendar Year") +
	ylab("UAW-GM Employees\n Leaving Work, \\%") +
	#facet_wrap(~PLANT, ncol = 1) +
	geom_smooth(
		data = employment_plot.tab,
		aes(x = cal_obs,
				y = rate * 100),
		alpha = 0.2,
		size = 0.5
	) +
	geom_point(data = employmenet_plot_binned.tab,
						 aes(x = means,
						 		y = rate * 100),
						 size = 1) +
	geom_rug(
		data = dplyr::summarize(group_by(
			filter(employment, year_left_work != 1995), STUDYNO
		),
		year_left_work.gm = year_left_work.gm[1])[sample(1:27599, 1000), ],
		aes(x = year_left_work.gm)
	) +
	scale_x_continuous(breaks = seq(1940, 1995, by = 5),
										 labels = as.vector(
										 	sapply(seq(1940, 1990, by = 10), function(x) {
										 		return(c(x, ""))}))
	) +
	geom_vline(xintercept = 1981,
						 lty = 3,
						 col = 'darkgray') +
	coord_cartesian(xlim = c(1942, 1993)) +
	annotate(
		geom = "text",
		# family = "serif",
		x = 1980,
		y = 13,
		label = "End of Enrollment",
		size = 2.5,
		col = "darkgray",
		hjust = "right"
	) +
	theme.tmp -> fig1

fig1

tikz(
	paste0(directory.name, "/Figure 1.tex"),
	standAlone = T,
	width = 4,
	height = 3
)
print(fig1)
dev.off()

# Figure 2 ####
# The estimated incidence rates of overdose and suicide by calendar time in the full UAW-GM cohort.
get.sim.tab <- function(tibble) {
	sim.tab <- dplyr::summarize(
		tibble,
		n = length(table(STUDYNO)),
		n_py = length(STUDYNO),
		Suicide = sum(suicide)/length(STUDYNO),
		`Fatal overdose` = sum(poison)/length(STUDYNO),
		Plant = PLANT[1])
	
	sim.pivot <- pivot_longer(
		sim.tab,
		c("Suicide", "Fatal overdose"),
		names_to = "outcome",
		values_to = "rate")
	
	return(sim.pivot)
}

sim.tab <- cohort_long %>%
	group_by(cal_obs) %>%
	get.sim.tab()

sim.quantiles <- quantile(unlist(filter(cohort_long, suicide == 1 | poison == 1)[, "cal_obs"]), seq(0, 1, 1/15))
sim.center <- get.center(sim.quantiles)
sim.quantiles[c(1, length(sim.quantiles))] <- c(min(cohort_long$cal_obs), max(cohort_long$cal_obs))

sim.binned.tab <- cohort_long %>%
	group_by(calyear = cut(cal_obs,
												 breaks = sim.quantiles,
												 include.lowest = T)) %>%
	get.sim.tab()

sim.binned.tab$means <- rep(sim.center, each = 2)

ggplot(data.frame()) +
	geom_point(
		data = sim.binned.tab,
		aes(x = means,
				y = rate * 100000,
				shape = outcome), size = 1) +
	geom_smooth(
		data = sim.tab,
		aes(x = cal_obs,
				y = rate * 100000,
				lty = outcome), size = 0.5, se = F,
		method = "loess",
		span = 0.7) +
	geom_rug(
		data = filter(cohort_long, poison == 1 | suicide == 1),
		aes(x = yod15)
	) +
	xlab("Calendar Year") +
	ylab("Suicide and Fatal Overdose Rate\nper 100,000 person-years") +
	coord_cartesian(ylim = c(0, 37)) +
	theme.tmp + theme(legend.title = element_blank(),
										legend.position = c(0.291, 0.9165)) -> fig2

fig2

tikz(
	paste0(directory.name, "/Figure 2.tex"),
	standAlone = T,
	width = 4,
	height = 3
)
print(fig2)
dev.off()

# Figure 3 ####
# Examines unadjusted self-injury mortality rates by plant over the period of downsizing
sim_by_plant.tab <- data.table::rbindlist(
	lapply(1:3, function(plant) {
		cohort_long %>%
			filter(PLANT == plant) %>%
			group_by(cal_obs) %>%
			get.sim.tab()
	}))

sim_by_plant.quantiles <- sapply(1:3, function(plant) {
	quantile(unlist(filter(cohort_long, (suicide == 1 | poison == 1) & PLANT == plant)[, "cal_obs"]), seq(0, 1, 1/15))
})
sim.center <- apply(sim_by_plant.quantiles, 2, get.center)

sim_by_plant.quantiles[c(1, nrow(sim_by_plant.quantiles)),] <- sapply(
	1:3, function(plant) {
		cohort_long <- filter(cohort_long, PLANT == plant)
		c(min(cohort_long$cal_obs), max(cohort_long$cal_obs))
	})

sim_by_plant.binned.tab <- data.table::rbindlist(
	lapply(1:3, function(plant) {
		cohort_long %>%
			filter(PLANT == plant) %>%
			group_by(calyear = cut(cal_obs,
														 breaks = sim_by_plant.quantiles[, plant],
														 include.lowest = T)) %>%
			get.sim.tab()
	}))


sim_by_plant.binned.tab$means <- as.vector(apply(sim.center, 2, rep, each = 2))

closures.tab <- data.frame(
	Plant = 1:3,
	year = c(2012, 2010, 2014)
)

ggplot(data.frame()) +
	geom_point(
		data = sim_by_plant.binned.tab,
		aes(x = means,
				y = rate * 100000,
				shape = outcome), size = 1.1) +
	geom_smooth(
		data = sim_by_plant.tab,
		aes(x = cal_obs,
				y = rate * 100000,
				lty = outcome), size = 0.75, se = F,
		method = "loess",
		span = 0.7) +
	geom_rug(
		data = mutate(filter(cohort_long, (poison == 1 | suicide == 1) & PLANT %in% 1:3), Plant = PLANT),
		aes(x = yod15)
	) +
	geom_vline(data = closures.tab, aes(
		xintercept = year,
		color = "Plant closure"
	), size = 0.75, lty = 4) +
	facet_wrap(. ~ factor(Plant, 1:3, paste("Plant", 1:3)), ncol = 3) +
	xlab("Calendar Year") +
	ylab("Suicide and Fatal Overdose Rate\nper 100,000 person-years") +
	coord_cartesian(
		xlim = c(1970, 2015),
		ylim = c(0, 40)) +
	theme.tmp + theme(
		legend.spacing.x = unit(10, "pt"),
		legend.title = element_blank(),
		legend.position = "bottom",
		legend.box.background = element_blank(),
		legend.box.margin = margin(0, 10, 0, 0, "pt"),
		legend.text = element_text(size = 8)) +
	guides(shape = guide_legend(override.aes = list(size = 1.1))) -> fig3


fig3

tikz(
	paste0(directory.name, "/Figure 3.tex"),
	standAlone = T,
	width = 0.8 + 3.2 * 3,
	height = 3.25
)
print(fig3)
dev.off()

# Compile using lualatex
if (!grepl("Darwin", Sys.info()['sysname'], ignore.case = T)) {
	# Windows
	system(
		paste(
			paste0("cd \"", directory.name, "\"\n"),
			paste0("for %i in (Figure.*\\.tex) do"),
			"lualatex $i; del %~ni.aux; del %~ni.log",
			paste0("magick -density ", 800, " \"%~ni.pdf\" \"%~ni.png\""),
			sep = " "
		),
		timeout = 20
	)
} else {
	# *nix
	sapply(list.files(directory.name, "Figure.*\\.tex"), function(x) {
		system(
			paste(
				paste0("cd \"", directory.name, "\";"),
				paste0("for i in \"", x, "\"; do {"),
				# "ls $i",
				"lualatex \"$i\"; rm \"${i%.tex}.aux\"; rm \"${i%.tex}.log\"",
				paste0(
					"magick -density ",
					800,
					" \"${i%.tex}.pdf\" \"${i%.tex}.png\";"
				),
				"} done",
				sep = "\n"
			),
			timeout = 20
		)
	})
}
