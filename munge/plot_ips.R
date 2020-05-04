library(boxr); box_auth(); library(tidyverse)
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

# Load IPS results
ipsi.res <- box_read(
	ifelse(yout.which == "YOUT16", 657188392651, 657672760394))

# plot results
rbind(
	mutate(ipsi.res$res.ptwise,
				 `Inference:` = "Pointwise"),
	mutate(ipsi.res$res,
				 `Inference:` = "Uniform")) %>% ggplot(
	aes(x = increment,
			y = est * length(table(dta_ips$STUDYNO))
	)) +
	# geom_point() +
	geom_line() +
	geom_vline(aes(
		xintercept = 1
	), linetype = 2, color = 'salmon') +
	geom_hline(aes(
		yintercept = ipsi.res$res.ptwise[
			ipsi.res$res.ptwise$increment == 1, 2] * length(
				table(dta_ips$STUDYNO))),
		linetype = 2,
		color = 'salmon') +
	geom_ribbon(aes(
		ymin = ci.ll * length(table(dta_ips$STUDYNO)),
		ymax = ci.ul * length(table(dta_ips$STUDYNO)),
		fill = `Inference:`
	),
	# fill = 'grey',
	alpha = 0.2) +
	labs(
		x = "Shift in OR of the PS for job loss",
		y = "Suicide and overdose deaths"
	) +
	coord_cartesian(xlim = c(0.77, 1.23),
									ylim = c(150, 500)) +
	theme_bw() +
	theme(
		# text=element_text(family="serif"),
		axis.text = element_text(size = 6),
		axis.title = element_text(size = 9),
		plot.title = element_text(size = 9),
		legend.title = element_text(size = 9),
		legend.text = element_text(size = 9),
		strip.text = element_text(size = 9),
		legend.position = "bottom"
	)

ipsi.res$res.ptwise %>% mutate(
	`Number of deaths` = prettyNum(est * length(table(dta_ips$STUDYNO))),
	`Lower bound` = prettyNum(ci.ll * length(table(dta_ips$STUDYNO))),
	`Upper bound` = prettyNum(ci.ul * length(table(dta_ips$STUDYNO)))
) %>% select(
	increment,
	`Number of deaths`,
	`Lower bound`,
	`Upper bound`
)
