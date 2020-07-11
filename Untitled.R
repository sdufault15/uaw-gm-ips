names(cohort_long)[names(cohort_long) == "race"] <- "RACE"
# Figure 3 ####
# Examines unadjusted self-injury mortality rates by race over the period of downsizing
sim_by_race.tab <- data.table::rbindlist(
	lapply(0:1, function(race) {
		cohort_long %>%
			filter(RACE == race) %>%
			group_by(cal_obs) %>%
			get.sim.tab()
	}))

sim_by_race.quantiles <- sapply(0:1, function(race) {
	quantile(unlist(filter(cohort_long, (suicide == 1 | poison == 1) & RACE == race)[, "cal_obs"]), seq(0, 1, 1/7))
})

sim.center <- apply(sim_by_race.quantiles, 2, get.center)

sim_by_race.quantiles[c(1, nrow(sim_by_race.quantiles)),] <- sapply(
	0:1, function(race) {
		cohort_long <- filter(cohort_long, RACE == race)
		c(min(cohort_long$cal_obs), max(cohort_long$cal_obs))
	})

sim_by_race.binned.tab <- data.table::rbindlist(
	lapply(0:1, function(race) {
		cohort_long %>%
			filter(RACE == race) %>%
			group_by(calyear = cut(cal_obs,
														 breaks = sim_by_race.quantiles[, race  + 1],
														 include.lowest = T)) %>%
			get.sim.tab()
	}))


setDT(sim_by_race.binned.tab)
sim_by_race.binned.tab[,
	means := 1/2*(as.numeric(substr(calyear, 7, 10)) + as.numeric(substr(calyear, 2, 5)))]


# closures.tab <- data.frame(
# 	Race = 0:1,
# 	year = c(2012, 2010, 2014)
# )

ggplot(data.frame()) +
	geom_point(
		data = sim_by_race.binned.tab,
		aes(x = means,
				y = rate * 100000,
				shape = outcome,
				col = outcome), size = 2) +
	geom_smooth(
		data = sim_by_race.tab,
		aes(x = cal_obs,
				y = rate * 100000,
				lty = outcome), size = 0.75, se = F,
		method = "loess",
		span = 0.7) +
	geom_rug(
		data = mutate(filter(cohort_long, (poison == 1 | suicide == 1) & RACE %in% 0:1), Race = factor(
			RACE, 0:1, c("Black", "White"))),
		aes(x = yod15)
	) +
	# geom_vline(data = closures.tab, aes(
	# 	xintercept = year,
	# 	color = "Race closure"
	# ), size = 0.75, lty = 4) +
	facet_wrap(. ~ Race, ncol = 3) +
	xlab("Calendar Year") +
	ylab("Suicide and Fatal Overdose Rate\nper 100,000 person-years") +
	coord_cartesian(
		xlim = c(1940, 2015),
		ylim = c(0, 40)) +
	mytheme.web + theme(
		legend.spacing.x = unit(10, "pt"),
		legend.title = element_blank(),
		legend.position = "bottom",
		legend.box.background = element_blank(),
		legend.box.margin = margin(0, 10, 0, 0, "pt"),
		legend.text = element_text(size = 8)) +
	guides(shape = guide_legend(override.aes = list(size = 1.1))) -> fig3


fig3

