# 2020-04-23_ips-curve.R
# Kevin Chen
# April 4, 2020
# Plot IPS results

library(here); library(boxr); box_auth(); library(tidyverse); library(tikzDevice)
source(here::here("reports", "ggplot-theme.R"))
# library(ggthemr)
# ggthemr(palette = "fresh",
#         layout = "clean")

# Load analytic data based on which variable used as year of employment end
dta_ips <- box_read(656285655983)

lackingrecords <- dta_ips %>%
	filter(is.na(A)) %>%
	select(STUDYNO) %>%
	distinct() %>%
	unlist()

dta_ips <- dta_ips %>%
	filter(!STUDYNO %in% lackingrecords)

# Study population size
N <- length(table(dta_ips$STUDYNO))

# Number of cases
y <- dta_ips %>% mutate(SIM = ifelse(suicide == 1 | poison == 1, 1, 0)) %>% select(STUDYNO, SIM) %>% distinct() %>% ungroup() %>% select(SIM) %>% unlist()

# Load IPS results
ipsi.res <- box_read(667528945939)

# Make plot-ready table
ips.ggtab <- rbind(
	mutate(ipsi.res$res.ptwise,
				 `Inference:` = "Pointwise 95\\% CI"),
	mutate(ipsi.res$res,
				 `Inference:` = "Uniform 95\\% CI"))

# Plot results ####
ips.ggplot <- ggplot(
	ips.ggtab,
	aes(x = increment,
			y = est * N
	)) +
	# geom_point() +
	geom_line() +
	# Draw dotted line at OR shift = 1.0
	geom_vline(aes(
		xintercept = 1
	), linetype = "dashed", alpha = 0.25) +
	# Draw dotted line at observed cumulative incidence
	geom_hline(aes(
		yintercept = ipsi.res$res.ptwise[
			ipsi.res$res.ptwise$increment == 1, 2] * length(
				table(dta_ips$STUDYNO))),
		linetype = "dashed", alpha = 0.25) +
	# Uniform CI: Lower bound
	geom_ribbon(data = {
		ips.ggtab %>% filter(`Inference:` == unique(`Inference:`)[1])},
		aes(ymin = ci.ll * N,
				ymax = ci.ul * N,
				fill = unique(`Inference:`)[1]), alpha = 0.35) +
	scale_fill_manual(values = "grey") +
	# Uniform CI: Lower bound
	geom_line(data = {
		ips.ggtab %>% filter(`Inference:` == unique(`Inference:`)[2])},
		aes(y = ci.ll * N,
				linetype = `Inference:`)) +
	# Uniform CI: Upper bound
	geom_line(data = {
		ips.ggtab %>% filter(`Inference:` == unique(`Inference:`)[2])},
		aes(y = ci.ul * N,
				linetype = `Inference:`)) +
	scale_linetype_manual(values = c("dotted")) +
	# Arrows point from annotation to curve
	geom_segment(data = {
		ips.ggtab %>% filter(`Inference:` == "Pointwise 95\\% CI") %>%
			slice(seq(1, nrow(.), 2)) %>% filter(increment != 1)},
		aes(x = increment - 0.025,
				xend = increment,
				y = est * N + 40,
				yend = est * N),
		size = 0.5, arrow = arrow(length = unit(0.01, 'npc'), type = 'closed')) +
	# Annotation with counts (want this to cover arrows)
	geom_label(data = {
		ips.ggtab %>% filter(`Inference:` == "Pointwise 95\\% CI") %>%
			slice(seq(1, nrow(.), 2)) %>% filter(increment != 1)},
		aes(y = est * N + 40,
				x = increment - 0.025,
				label = round(est * N)
		), size = 2) +
	# Arrows point from annotation to curve: Observed
	geom_segment(data = {
		ips.ggtab %>% filter(increment == 1)},
		aes(x = increment - 0.025 * 1.4,
				xend = increment,
				y = est * N + 40 * 1.85,
				yend = est * N),
		size = 0.5, arrow = arrow(length = unit(0.01, 'npc'), type = 'closed')) +
	# Annotation with counts (want this to cover arrows): observed
	geom_label(data = {
		ips.ggtab %>% filter(increment == 1)},
		aes(x = increment - 0.025 * 1.4,
				y = est * N + 40 * 1.85,
				label = round(est * N)
		), size = 2) +
	# Window
	coord_cartesian(xlim = c(0.775, 1.225),
									ylim = c(150, 550)) +
	labs(
		x = "Leaving work odds ratio, $\\delta$",
		y = "Suicide and overdose mortality, $\\Psi(\\delta)$"
	) +
	guides(fill = guide_legend(override.aes = list(alpha = 0.6))) +
	theme_bw() + mytheme +
	theme(
		legend.position = c(0.0005, 0.9015),
		legend.margin = margin(2.35, 5, 5, 5, "pt"),
		panel.grid = element_blank(),
		legend.spacing.y = unit(2.1, "pt"),
		legend.key = element_rect(size = 20))

# quartz(width = 4, height = 3)
ips.ggplot
# dev.off()

# Render plot in TeX
directory.name <- here::here("graphs")
file.name <- paste0("Figure 4.tex")
tikz(paste0(directory.name, "/", file.name),
		 standAlone = T, width = 4, height = 3)
print(ips.ggplot)
dev.off()

# Compile using lualatex
if (!grepl("Darwin", Sys.info()['sysname'], ignore.case = T)) {
	# Windows
	system(paste(
		paste0("cd \"", directory.name, "\"\n"),
		paste0("for %i in (", file.name, ") do"),
		"lualatex $i; del %~ni.aux; del %~ni.log;",
		paste0("magick -density ", 800, " \"%~ni.pdf\" \".\\%~ni.png\""),
		sep = " "), timeout = 20)
} else {
	# *nix
	system((paste(
		paste0("cd \"", directory.name, "\";"),
		paste0("for i in \"", file.name,"\"; do {"),
		"lualatex \"$i\"; rm \"${i%.tex}.aux\"; rm \"${i%.tex}.log\";",
		paste0("magick -density ", 800, " \"${i%.tex}.pdf\" \"${i%.tex}.png\";"),
		"} done", sep = "\n")), timeout = 20)
}

# # Print results
# ipsi.res$res.ptwise %>% mutate(
# 	`Number of deaths` = prettyNum(est * N),
# 	`Lower bound` = prettyNum(ci.ll * N),
# 	`Upper bound` = prettyNum(ci.ul * N)
# ) %>% select(
# 	increment,
# 	`Number of deaths`,
# 	`Lower bound`,
# 	`Upper bound`
# )
