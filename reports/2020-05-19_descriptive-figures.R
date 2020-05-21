# 2020-05-19_descriptive-figures.R
# Suzanne Dufault, adpated by Kevin Chen
# May 19, 2020
# Descriptive plots

library(tidyverse)
library(here); library(tikzDevice)
library(boxr); box_auth()
library(ggridges)
library(ggfittext)
library(ggthemr)
ggthemr(palette = "fresh",
        layout = "clean")

# Tikz/lualatex options
options(
	tikzDefaultEngine = 'luatex',
	tikzLualatexPackages = c(
		"\\usepackage{amssymb}",
		"\\usepackage[no-math]{fontspec}\n",
		paste0(
			"\\setmainfont{Arial}",
			ifelse(Sys.info()["sysname"] == "Darwin",
						 "\n",
						 "[Extension = .ttf,
			UprightFont = *,
			BoldFont = *bd,
			talicFont = *i,
			BoldItalicFont = *bi]\n")),
		"\\usepackage[italic]{mathastext}",
		"\\usepackage{tikz}\n",
		"\\usepackage[active,tightpage,psfixbb]{preview}\n",
		"\\PreviewEnvironment{pgfpicture}\n",
		"\\setlength\\PreviewBorder{0pt}\n"
	)
)

# Render plot in TeX
directory.name <- here::here("graphs")

theme.tmp <- theme_classic() +
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.justification = "right",
    # Legend position is relative to plot window:
    #   c(0,0) is bottom left, c(1,1) is top right
    legend.margin = margin(0, 5, 5, 5, "pt"),
    legend.box.margin = margin(0, 0, 0, 0, "pt"),
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
    lower.delim <- "\\)"}
  
  lower <- gsub(" ", "", substr(x, unlist(gregexpr(upper.delim, x)) + 1,
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

yout.which <- "year_left_work"

# Load analytic data based on which variable used as year of employment end
cohort_long <- box_read(488427532080) # the long dataset
	# box_read(ifelse(yout.which == "YOUT16", 657147487614, 656275470844))
dta_end_of_employment <- box_read(656270359800) # Job history

# Merge end of employment
cohort_long <- full_join(
  (if (yout.which == "YOUT16") {cohort_long %>% select(-yout.which)} else {cohort_long}),
  (dta_end_of_employment[,c("STUDYNO", yout.which)] %>% distinct),
  by = "STUDYNO")

cohort_long %>% arrange(STUDYNO, cal_obs) -> cohort_long

# Load cohort data
cohort <- box_read(485768272681)
cohort <- filter(cohort, STUDYNO %in% cohort_long$STUDYNO)

# Merge end of employment
cohort <- full_join(
  (if (yout.which == "YOUT16") {cohort %>% select(-yout.which)} else {cohort}),
  (dta_end_of_employment %>% select(STUDYNO, yout.which,
                                    if (yout.which == "year_left_work") {"month_left_work"},
                                    if (yout.which == "year_left_work") {"day_left_work"}) %>% distinct),
  by = "STUDYNO")

cohort_long <- full_join(
  (if (yout.which == "YOUT16") {cohort_long %>% select(-yout.which)} else {cohort_long}),
  (dta_end_of_employment[,c("STUDYNO", yout.which,
                            if (yout.which == "year_left_work") {"month_left_work"},
                            if (yout.which == "year_left_work") {"day_left_work"})] %>% distinct),
  by = "STUDYNO")

# Remove people who have been employed for less than 3 years
if (yout.which == "year_left_work") {
  # Make decimal year of leaving work corresponding to the new leave work date
  date.to.gm <- function(x = "2013-01-01") {
    require(lubridate)
    as.numeric(
      year(x) +
        time_length(difftime(x, as.Date(paste0(year(x), "-01-01"))), "year") / (
          time_length(difftime(as.Date(paste0(year(x), "-12-31")),
                               as.Date(paste0(year(x), "-01-01"))), "year"))
    )}
  
  cohort$year_left_work.gm <- 1995
  cohort[cohort$month_left_work != 1995, "year_left_work.gm"] <- mutate(cohort[cohort$month_left_work != 1995,],
                                                                        year_left_work.gm = paste(year_left_work, month_left_work, day_left_work, sep = "/")
  ) %>% mutate(year_left_work.gm = date.to.gm(as.Date(year_left_work.gm))) %>%
    select(year_left_work.gm) %>% unlist
  
  cohort_long <- full_join(cohort_long,
                           cohort[,c("STUDYNO", "year_left_work.gm")],
                           by = "STUDYNO")
  
  # Filter
  cohort %>% filter(round(year_left_work.gm - YIN16, 1) >= 3) -> cohort
  cohort_long %>% filter(round(year_left_work.gm - YIN16, 1) >= 3) -> cohort_long
} else {
  cohort %>% filter(round(YOUT16 - YIN16, 1) >= 3) -> cohort
  cohort_long %>% filter(round(YOUT16 - YIN16, 1) >= 3) -> cohort_long
}

# Figure 1 ####
# Trends in worker exit for the UAW-GM cohort. The observed annual rates of leaving work among actively employed workers are denoted by the solid blue line. A smooth loess line fit to the observed annual rates is plotted in gold with an error bar to denote 95% confidence intervals. The rate of leaving work hovered around 4% from the 1960s until the early 1980s. The sharp upward trend of worker exit after 1980 is the result of forced separation from mass layoffs as well as an aging cohort that is not replenishing after 1981.
employment <- cohort_long %>%
  ungroup() %>%
  group_by(STUDYNO) %>%
  filter(floor(cal_obs) <= YOUT16) %>%
  mutate(leaving_work = ifelse(floor(cal_obs) == floor(YOUT16) & YOUT16 != 1995, 1, 0))

employment_plot_dta <- employment %>%
  ungroup() %>%
  #filter(PLANT != 9) %>%
  group_by(cal_obs) %>%
  dplyr::summarize(n = length(table(STUDYNO)),
            n_leave = sum(leaving_work)) %>%
  mutate(rate = n_leave/n) %>%
  filter(cal_obs < 1995) 

employment_plot_dta %>% 
  ggplot(aes(x = cal_obs)) +
  xlab("Calendar Year") +
  ylab("UAW-GM Employees\n Leaving Work, \\%") +
  #facet_wrap(~PLANT, ncol = 1) + 
  geom_smooth(aes(y = rate*100), 
              alpha = 0.2, 
              size = 0.5, 
              col = swatch()[5],
              fill = swatch()[5],
              lty = 2) + 
  geom_line(aes(y = rate*100), 
            size = 0.5) + 
  # geom_point(aes(y = n_py,
  #                color = "At Work (Count)")) + 
  # geom_point(aes(y = n_leave,
  #                color = "Leaving Work (Count)",
  #                shape = "Leaving Work (Count)")) +
  # scale_y_continuous(
  #   sec.axis = sec_axis(~./20000, name = "Proportion Leaving Work")) +
  scale_x_continuous(name = waiver(),
                     breaks = seq(1940,2000,by = 10),
                     labels = seq(1940,2000,by = 10)) +
  labs(color = "", shape = "") + 
  geom_vline(xintercept = 1981,
             lty = 3,
             col = 'darkgray') + 
  ylim(0,15) +
  annotate(geom = "text",
           # family = "serif",
           x = 1980,
           y = 13,
           label = "End of Enrollment",
           size = 3, col = "darkgray",
           hjust = "right") +
  theme.tmp -> fig1

fig1

# ggsave(here::here("graphs","Figure 1.png"),
#        device = "png",
#        width = 4,
#        height = 2.5,
#        units = "in")

tikz(paste0(directory.name, "/Figure 1.tex"),
		 standAlone = T, width = 4, height = 3)
print(fig1)
dev.off()

# Figure 2 ####
# The estimated incidence rates of overdose and suicide by calendar time in the full UAW-GM cohort.
denom <- cohort_long %>%
  group_by(calyear = cut(floor(cal_obs), breaks = c(1940, seq(1950,2015,by = 5)))) %>%
  dplyr::summarize(n_py = length(STUDYNO),
  								 n = length(table(STUDYNO)))

suicides <- cohort %>%
  filter(suicide == 1) %>%
  group_by(calyear = cut(floor(yod15), breaks = c(1940, seq(1950,2015,by = 5)))) %>%
  dplyr::summarize(n_suicide = n_distinct(STUDYNO))

overdoses <- cohort %>%
  filter(poison == 1) %>%
  group_by(calyear = cut(floor(yod15), breaks = c(1940, seq(1950,2015,by = 5)))) %>%
  dplyr::summarize(n_od = n_distinct(STUDYNO))

temp <- full_join(full_join(denom, suicides, by = "calyear"), overdoses, by = "calyear") %>%
  replace(is.na(.), 0) 

temp_long <- temp %>%
  gather("type", "count", (ncol(temp) - 1):ncol(temp)) %>%
  mutate(rate = count/n_py)

temp_long %>% mutate(
    calyear = factor(calyear,
                     labels = pretty.cut(levels(calyear)))) %>%
  filter(calyear != levels(calyear)[1]) %>% 
  ggplot(aes(x = calyear, y = rate*100000, col = type, shape = type)) + 
  geom_point(size = 2) + 
  # geom_line(aes(group = type), lty = 2, alpha = 0.5, lwd = 1) + 
  scale_color_manual(NULL, 
                     values = c("n_suicide" = swatch()[2], 
                                "n_od" = swatch()[4]),
                     labels = c("Overdose", "Suicide")) +
  scale_shape_manual(NULL, 
                     values = c("n_suicide" = 19, 
                                "n_od" = 17),
                     labels = c("Overdose", "Suicide")) +
  xlab("Calendar Year") + 
  ylab("Suicide and Overdose Rate\nper 100,000 person-years") + 
  # scale_y_continuous(name = waiver(),
  #                    breaks = seq(0,35, by = 2.5),
  #                    labels = seq(0,35, by = 2.5)) + 
  #geom_segment(aes(x = calyear, xend = calyear, y = lb10000, yend = ub10000)) + 
  theme.tmp +
	theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 0)) -> fig2

fig2

# ggsave(here::here("graphs","Figure 2.png"),
#        device = "png",
#        width = 4.5,
#        height = 3,
#        units = "in")

tikz(paste0(directory.name, "/Figure 2.tex"),
		 standAlone = T, width = 4.5, height = 3.25)
print(fig2)
dev.off()

# Figure 3 ####
# The estimated annual self-injury mortality rate by age category. Self-injury is comprised of suicide and fatal drug overdose events. The rates for those aged 45-55 is highlighted in blue.
denom <- cohort_long %>%
  filter(age_obs > 20, age_obs <= 75) %>%
  group_by(calyear = cut(floor(cal_obs), breaks = c(1940, seq(1950,2015,by = 5))),
           agecat = cut(floor(age_obs), breaks = c(20,35,45,55,65,75)),
           .drop = FALSE) %>%
  dplyr::summarize(n_py = length(STUDYNO),
  								 n = length(table(STUDYNO)))

sim <- cohort %>%
  filter(suicide == 1 | poison == 1) %>%
  filter(floor(yod15 - YOB) > 20, floor(yod15 - YOB) <= 75) %>%
  group_by(calyear = cut(floor(yod15), breaks = c(1940, seq(1950,2015,by = 5))),
           agecat = cut(floor(yod15 - YOB), breaks = c(20,35,45,55,65,75)),
           .drop = FALSE) %>%
  dplyr::summarize(n_sim = n_distinct(STUDYNO))

temp <- full_join(denom, sim, by = c("calyear", "agecat")) %>%
  replace(is.na(.), 0) %>%
  mutate(rate = n_sim/n_py) %>%
  replace(is.na(.), 0)

temp %>% filter(n_py > 0) %>%
  ungroup() %>%
  filter(calyear != "(1940,1950]",
         agecat != "(65,75]",
         agecat != "(20,35]") %>% mutate(
           calyear = factor(calyear),
           agecat = factor(agecat)
         ) %>% mutate(
           calyear = factor(calyear, labels = pretty.cut(levels(calyear))),
           agecat = factor(agecat, labels = pretty.cut(levels(agecat)))
         ) %>%
  ggplot(aes(x = calyear, y = rate*100000, linetype = agecat, group = agecat)) + 
  #geom_bar(stat = "identity", position = position_dodge(0.65), width = 0.5) +
  geom_line(aes(col = agecat), size = 0.5) + 
  scale_color_manual("Age Category",
                     values = c(swatch()[5] , swatch()[2], swatch()[5])) +
  scale_linetype("Age Category") + 
  xlab("Calendar Year") + 
  ylab("Self-Injury Mortality Rate\nper 100,000 person-years")  +
  theme.tmp +
  theme(axis.text.x = element_text(angle = -45,hjust = 0, vjust = 0.3)) -> fig3

fig3

# ggsave(here::here("graphs","Figure 3.png"),
#        device = "png",
#        width = 4.5,
#        height = 3,
#        units = "in")

tikz(paste0(directory.name, "/Figure 3.tex"),
		 standAlone = T, width = 4.5, height = 3.25)
print(fig3)
dev.off()

# Compile using lualatex
if (!grepl("Darwin", Sys.info()['sysname'], ignore.case = T)) {
  # Windows
  system(paste(
    paste0("cd \"", directory.name, "\"\n"),
    paste0("for %i in (Figure.*\\.tex) do"),
    "lualatex $i; del %~ni.aux; del %~ni.log",
    paste0("magick -density ", 800, " \"%~ni.pdf\" \"%~ni.png\""),
    sep = " "), timeout = 20)
} else {
  # *nix
  sapply(list.files(directory.name, "Figure.*\\.tex"), function(x) {
    system(paste(
      paste0("cd \"", directory.name, "\";"),
      paste0("for i in \"", x, "\"; do {"),
      # "ls $i",
      "lualatex \"$i\"; rm \"${i%.tex}.aux\"; rm \"${i%.tex}.log\"",
      paste0("magick -density ", 800, " \"${i%.tex}.pdf\" \"${i%.tex}.png\";"),
      "} done", sep = "\n"), timeout = 20)
  })
}
