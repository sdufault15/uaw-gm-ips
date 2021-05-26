# 2020-04-23_ips-curve.R
# Kevin Chen
# April 4, 2020
# Plot IPS results

library(here); library(boxr); box_auth(); library(tidyverse); library(tikzDevice)
source(here::here("reports", "ggplot-theme.R"))
# library(ggthemr)
# ggthemr(palette = "fresh",
#         layout = "clean")

# yout.which <- "year_left_work"
yout.which <- "YOUT16"

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

# # Load IPS results
# ipsi.res <- box_read(667528945939)
#
# # Make plot-ready table
# ips.ggtab <- rbind(
# 	mutate(ipsi.res$res.ptwise,
# 				 `Inference:` = "Pointwise 95\\% CI"),
# 	mutate(ipsi.res$res,
# 				 `Inference:` = "Uniform 95\\% CI"))

# Load IPS results for different lengths of FU
library(data.table)
ips.ggtab <- rbindlist(lapply(c(
	"2015" = ifelse(yout.which == "year_left_work", 667528945939, 814811972496),
	# "2010" = 745324315714,
	# "2005" = 745340077468,
	# "2000" = 745355315482,
	"1995" = ifelse(yout.which == "year_left_work", 745367896672, 814875681954)
), function(file_id) {
	ipsi.res <- box_read(file_id)
	ips.ggtab <- rbind(
		mutate(ipsi.res$res.ptwise,
					 `Inference:` = "Pointwise 95\\% CI"),
		mutate(ipsi.res$res,
					 `Inference:` = "Uniform 95\\% CI"))
	ips.ggtab$calpha <- ipsi.res$calpha
	return(ips.ggtab)
}), idcol = T)
ips.ggtab$FU <- paste0("Follow-up through ", ips.ggtab$.id)

# Get CIs
constant_scaling <- unique(ips.ggtab[increment == 1 & .id == 1995, est])
ips.ggtab[
	(increment == 0.9 | increment == 1.1) & .id == 1995
][order(increment),.(
	# `FU through` = .id,
	delta = increment,
	`Point estimate` = est / constant_scaling,
	`(95% CI)` = paste0(
		"(", signif(ci.ll / constant_scaling, 4),
		", ",
		signif(ci.ul / constant_scaling, 4),
		")"),
	`CI method` = gsub(" .*", "", `Inference:`)
)]

# Within group indices
ips.ggtab[,`:=`(
	I = 1:.N,
	even = as.numeric(.I %% 2 == 0)
), by = .(.id, `Inference:`)]

ips.ggtab_2 <- ips.ggtab[.id == 2015]
ips.ggtab_3 <- ips.ggtab[.id %in% c(1995, 2015)]
ips.ggtab_3b <- ips.ggtab[.id %in% c(1995, 2005, 2015)]
# ips.ggtab_3b[,`:=`(
# 	est = est/est[increment == 1],
# 	ci.ll = ci.ll/est[increment == 1],
# 	ci.ul = ci.ul/est[increment == 1]
# ), by = .(.id, `Inference:`)]

# Plot results ####
ips.ggplot_2 <- ggplot(
	ips.ggtab_2,
	aes(x = increment,
			y = est
	)) +
	# geom_point() +
	geom_line() +
	# Draw dotted line at OR shift = 1.0
	geom_vline(aes(
		xintercept = 1
	), linetype = "dashed", alpha = 0.25) +
	# Draw dotted line at observed cumulative incidence
	geom_hline(data = ips.ggtab_2[
		increment == 1, c("FU", "est")], aes(
			yintercept = est ),
		linetype = "dashed", alpha = 0.25) +
	# Pointwise CI
	geom_ribbon(data = {
		ips.ggtab_2[`Inference:` == unique(`Inference:`)[1]]},
		aes(ymin = ci.ll ,
				ymax = ci.ul ,
				fill = unique(`Inference:`)[1]), alpha = 0.35) +
	scale_fill_manual(values = "grey") +
	# Uniform CI: Lower bound
	geom_line(data = {
		ips.ggtab_2[`Inference:` == unique(`Inference:`)[2]]},
		aes(y = ci.ll ,
				linetype = `Inference:`)) +
	# Uniform CI: Upper bound
	geom_line(data = {
		ips.ggtab_2[`Inference:` == unique(`Inference:`)[2]]},
		aes(y = ci.ul ,
				linetype = `Inference:`)) +
	scale_linetype_manual(values = c("dotted")) +
	# Arrows point from annotation to curve
	geom_segment(
		data = ips.ggtab_2[`Inference:` == "Pointwise 95\\% CI" & even == 0 & increment != 1],
		aes(x = increment - 0.025,
				xend = increment,
				y = est  + 0.008,
				yend = est ),
		size = 0.5, arrow = arrow(length = unit(0.01, 'npc'), type = 'closed')) +
	# Annotation with counts (want this to cover arrows)
	geom_label(
		data = ips.ggtab_2[`Inference:` == "Pointwise 95\\% CI" & even == 0 & increment != 1],
		aes(y = est  + 0.008,
				x = increment - 0.025,
				label = round(est * N)
		), size = 2) +
	# Arrows point from annotation to curve: Observed
	geom_segment(
		data = ips.ggtab_2[increment == 1],
		aes(x = increment - 0.025 * 1.4,
				xend = increment,
				y = est  + 0.008 * 1.85,
				yend = est),
		size = 0.5, arrow = arrow(length = unit(0.01, 'npc'), type = 'closed')) +
	# Annotation with counts (want this to cover arrows): observed
	geom_label(
		data = ips.ggtab_2[increment == 1],
		aes(x = increment - 0.025 * 1.4,
				y = est  + 0.008 * 1.85,
				label = round(est * N)
		), size = 2) +
	# Window
	scale_x_continuous(breaks = unique(
		c(seq(0.8, 1.2, 0.05))
		),
		trans = "log") +
	coord_cartesian(xlim = c(0.775, 1.225)
									# ylim = c(90, 550)
									) +
	labs(
		x = "Leaving work odds ratio, $\\delta$",
		y = "Suicide and overdose mortality, $\\Psi(\\delta)$"
	) +
	guides(fill = guide_legend(override.aes = list(alpha = 0.6))) +
	theme_bw() + mytheme +
	theme(
		legend.position = c(0.001, 0.918),
		legend.spacing = unit(2.5, "pt"),
		legend.margin = margin(-2, 5, 2, 5, "pt"),
		legend.box.margin = margin(2, 2, 2, 2, "pt"),
		legend.background = element_rect(fill = alpha("white", 0)),
		axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5)),
		axis.title.y.right = element_text(margin = margin(t = 0, r = 2, b = 0, l = 8))
	)

# quartz(width = 4, height = (3 - 1) * 3 + 1)
ips.ggplot_2
# dev.off()

ips.ggplot_3b <- ggplot(
	ips.ggtab_3b,
	aes(x = increment,
			y = est
	)) +
	# geom_point() +
	geom_line() +
	# Draw dotted line at OR shift = 1.0
	geom_vline(aes(
		xintercept = 1
	), linetype = "dashed", alpha = 0.25) +
	# Draw dotted line at observed cumulative incidence
	geom_hline(data = ips.ggtab_3a[
		increment == 1, c("FU", "est")], aes(
			yintercept = est ),
		linetype = "dashed", alpha = 0.25) +
	# Pointwise CI
	geom_ribbon(data = {
		ips.ggtab_3a[`Inference:` == unique(`Inference:`)[1]]},
		aes(ymin = ci.ll ,
				ymax = ci.ul ,
				fill = unique(`Inference:`)[1]), alpha = 0.35) +
	scale_fill_manual(values = "grey") +
	# Uniform CI: Lower bound
	geom_line(data = {
		ips.ggtab_3a[`Inference:` == unique(`Inference:`)[2]]},
		aes(y = ci.ll ,
				linetype = `Inference:`)) +
	# Uniform CI: Upper bound
	geom_line(data = {
		ips.ggtab_3a[`Inference:` == unique(`Inference:`)[2]]},
		aes(y = ci.ul ,
				linetype = `Inference:`)) +
	scale_linetype_manual(values = c("dotted")) +
	# Arrows point from annotation to curve
	geom_segment(
		data = ips.ggtab_3a[`Inference:` == "Pointwise 95\\% CI" & even == 0 & increment != 1],
		aes(x = increment - 0.025,
				xend = increment,
				y = est  + 0.008,
				yend = est ),
		size = 0.5, arrow = arrow(length = unit(0.01, 'npc'), type = 'closed')) +
	# Annotation with counts (want this to cover arrows)
	geom_label(
		data = ips.ggtab_3a[`Inference:` == "Pointwise 95\\% CI" & even == 0 & increment != 1],
		aes(y = est  + 0.008,
				x = increment - 0.025,
				label = round(est * N)
		), size = 2) +
	# Arrows point from annotation to curve: Observed
	geom_segment(
		data = ips.ggtab_3a[increment == 1],
		aes(x = increment - 0.025 * 1.4,
				xend = increment,
				y = est  + 0.008 * 1.85,
				yend = est ),
		size = 0.5, arrow = arrow(length = unit(0.01, 'npc'), type = 'closed')) +
	# Annotation with counts (want this to cover arrows): observed
	geom_label(
		data = ips.ggtab_3a[increment == 1],
		aes(x = increment - 0.025 * 1.4,
				y = est  + 0.008 * 1.85,
				label = round(est * N)
		), size = 2) +
	# Window
	scale_x_continuous(breaks = unique(
		c(seq(0.8, 1.2, 0.05))
		),
		trans = "log") +
	coord_cartesian(xlim = c(0.775, 1.225)
									# ylim = c(90, 550)
									) +
	# Facet by FU length
	facet_wrap(. ~ FU, ncol = 1) +
	labs(
		x = "Leaving work odds ratio, $\\delta$",
		y = "Suicide and overdose mortality, $\\Psi(\\delta)$"
	) +
	guides(fill = guide_legend(override.aes = list(alpha = 0.6))) +
	theme_bw() + mytheme +
	theme(
		panel.grid = element_blank(),
		legend.position = "bottom",
		legend.justification = "center",
		legend.box.background = element_blank()
	)

# quartz(width = 4, height = (3 - 1) * 3 + 1)
ips.ggplot_3b
# dev.off()

ips.ggplot_3 <- ggplot(
	ips.ggtab_3,
	aes(x = increment,
			y = est)) +
	# geom_point() +
	# Uniform CI
	geom_ribbon(
		data = ips.ggtab_3b[`Inference:` == unique(`Inference:`)[2]],
		aes(ymin = ci.ll,
				ymax = ci.ul,
				fill = FU,
				alpha = FU
		)) +
	scale_fill_manual(values = c("grey", "blue")) +
	scale_alpha_manual(values = c(0.5, 0.25)) +
	geom_line(aes(linetype = FU)) +
	# Window
	scale_x_continuous(breaks = unique(
		c(seq(0.8, 1.2, 0.05))
		),
		trans = "log") +
	coord_cartesian(xlim = c(0.775, 1.225)
									# ylim = c(0, 4)
									) +
	# Draw dotted line at OR shift = 1.0
	geom_vline(aes(
		xintercept = 1
	), linetype = "dashed", alpha = 0.25) +
	# Draw dotted line at observed cumulative incidence
	geom_hline(data = ips.ggtab_3b[
		increment == 1, c("FU", "est")], aes(
			yintercept = est),
		linetype = "dashed", alpha = 0.25) +
	labs(
		x = "Leaving work odds ratio, $\\delta$",
		y = "Suicide and overdose mortality, $\\Psi(\\delta)$"
	) +
	theme_bw() + mytheme +
	theme(legend.position = c(0.001, 0.922),
				legend.margin = margin(-5, 5, 0, 5, "pt"),
				legend.box.margin = margin(2, 2, 2, 2, "pt"),
				legend.background = element_rect(fill = alpha("white", 0)),
				axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5)),
				axis.title.y.right = element_text(margin = margin(t = 0, r = 2, b = 0, l = 8)),
				legend.key.height = unit(12, "pt"),
				legend.key.width = unit(6, "pt")
				# legend.text = element_text(margin = margin(2, 0, 5, 0, unit = "pt"))
	)

ips.ggplot_3b

# Render plot in TeX
directory.name <- here::here("graphs")

apply(data.frame(
	file.name = c("Figure 2.tex", "Figure 3 (all FU).tex", "Figure 3.tex"),
	height = c(3, 6.75, 3),
	width = 4,
	plot.name = paste0("ips.ggplot_", c("2", "3b", "3"))
), 1, function(x) {
	file.name <- unlist(x[1])
	height    <- unlist(x[2])
	width     <- unlist(x[3])
	plot.name <- unlist(x[4])

	tikz(paste0(directory.name, "/", file.name), standAlone = T,
			 width = width, height = height)
	print(get(plot.name, envir = .GlobalEnv))
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
})

# # Print results
# ipsi.res$res.ptwise %>% mutate(
# 	`Number of deaths` = prettyNum(est ),
# 	`Lower bound` = prettyNum(ci.ll ),
# 	`Upper bound` = prettyNum(ci.ul )
# ) %>% select(
# 	increment,
# 	`Number of deaths`,
# 	`Lower bound`,
# 	`Upper bound`
# )