# Demographic Plots
library(tidyverse)
library(boxr)

library(ggthemr)
ggthemr(palette = 'pale')

box_auth()
cohort <- box_read(file_id = 485768272681) # reading in the data file "2019-07-03_cohort-demographics.RData"

################################################
# Histogram of Deaths by Time since leaving work
################################################
p1 <- cohort %>%
  filter(suicide == 1 | poison == 1) %>%
  filter(YOUT16 < 1995) %>%
  ggplot(aes(x = yod09 - YOUT16)) + 
  geom_histogram(color = 'white') + 
  scale_x_continuous(breaks = seq(0,50, by = 10),
                     labels = seq(0,50, by = 10)) +
  xlab("Years Since Leaving Work\n among individuals who left work before 1995") + 
  ylab("Suicide and Overdose Count") 
p1 + 
  annotate(geom = "text", x = 42, y = 80, label = ("No. Suicides = 242\n No. Overdoses = 46"))
ggsave(filename = here("graphs", "hist_suicide-overdose_years-since-leaving.png"), 
       device = "png",
       width = 6,
       height = 4,
       units = "in")

p1
ggsave(filename = here("graphs", "hist_suicide-overdose_years-since-leaving_v2.png"), 
       device = "png",
       width = 6,
       height = 4,
       units = "in")

cohort %>%
  filter(YOUT16 < 1995) %>%
  group_by(suicide == 1 , poison == 1) %>%
  summarize(n_distinct(STUDYNO))



################################################
# Rates by Age Category
################################################

dim(cohort)

library(splitstackshape)
# dtalong <- dta %>%
#   mutate(tenure = ceiling(yod09 - YIN16)) %>%
#   expandRows("tenure", count.is.col = TRUE, drop = FALSE) %>%
#   group_by(STUDYNO) %>%
#   mutate(year_obs = row_number()) 
# 
# dtalong <- dtalong %>%
#   mutate(age_obs = (YIN16 - YOB) + (year_obs - 1),
#          cal_obs = YIN16 + year_obs) 
# 
# 
# denom_py <- dtalong %>%
#   ungroup() %>%
#   group_by(age = cut(age_obs, breaks = c(0,seq(20,90,by = 5)))) %>%
#   summarize(n_py = n_distinct(STUDYNO))
# 
# num_event <- dta %>%
#   filter(suicide == 1 | poison == 1) %>%
#   group_by(age = cut(yod09 - YOB, breaks = c(0,seq(20,90,by = 5)))) %>%
#   summarize(n_event = n_distinct(STUDYNO))
# 
# ggthemr(palette = "fresh", layout = "clear")
# new <- full_join(denom_py, num_event, by = "age") %>%
#   replace(is.na(.), 0) %>%
#   filter(age != "(85,90]") %>%
#   mutate(rate = n_event/n_py) 
# 
# ggthemr(palette = "fresh",
#         layout = "clean")
# new %>%
#   ggplot(aes(x = age, y = rate*10000)) +
#   geom_segment(aes(x = age, xend = age, y = 0, yend = rate*10000 - 0.25), alpha = 0.2) +  
#   geom_point(aes(size = n_py)) +
#   ylab("Suicide and Overdose Rate\nper 10,000 person-years") +
#   xlab("Age Category") + 
#   ylim(0,25) + 
#   theme(axis.text.x = element_text(angle = 315, hjust = 0)) + 
#   scale_size("Number of Person-Years\nObserved", breaks = waiver(), labels = waiver()) + 
#   geom_text(aes(label = n_event), vjust = -1.25, size = 3)#paste0(n_event, "/", n_py)), vjust = -1)
# ggsave(filename = here("graphs", "point_suicide-overdose-rate_by-age_v2.png"),
#        device = "png",
#        width = 8,
#        height = 4.5,
#        units = "in")
# 
# #+ 
# # annotate(geom = "text", x = age, y = rate*10000 + .25, labels = n_event, data = new)+
# # geom_point(aes(x = age, y = rate*10000),  size = 3) + 
# # geom_segment(aes(x = age, xend = age, y = 0, yend = rate*10000 - 0.25), alpha = 0.5)  
# 
# 
# #### By Cal Year
# 
# denom_py <- dtalong %>%
#   ungroup() %>%
#   group_by(calyear = cut(floor(1900 + cal_obs), breaks = c(1900,seq(1950,2010,by = 10)), dig.lab = 4)) %>%
#   summarize(n_py = n_distinct(STUDYNO))
# 
# num_event_suic <- dta %>%
#   filter(suicide == 1) %>%
#   group_by(calyear = cut(1900 + yod09, breaks = c(1900,seq(1950,2010,by = 10)), dig.lab = 4)) %>%
#   summarize(suicides = n_distinct(STUDYNO)) 
# 
# num_event_pois <- dta %>%
#   filter(poison == 1) %>%
#   group_by(calyear = cut(1900 + yod09, breaks = c(1900,seq(1950,2010,by = 10)), dig.lab = 4)) %>%
#   summarize(overdoses = n_distinct(STUDYNO))  
# 
# ggthemr(palette = "fresh", layout = "clear")
# new <- full_join(full_join(denom_py, num_event_suic, by = "calyear"), num_event_pois, by = "calyear") %>%
#   replace(is.na(.), 0) %>%
#   gather("event", "number", 3:4) %>%
#   mutate(rate = number/n_py) 
# 
# ggthemr(palette = "fresh",
#         layout = "clean")
# new %>%
#   ggplot(aes(x = calyear, y = rate*10000, col = event)) +
#   geom_point(aes(size = n_py)) + 
#   geom_line(aes(group = event), alpha = 0.6, lty = 2)  +
#   scale_color_manual(NULL, 
#                      values = c("suicides" = swatch()[2], 
#                                 "overdoses" = swatch()[4])) + 
#   ylab("Suicide and Overdose Rate\nper 10,000 person-years") +
#   xlab("Calendar Year") + 
#   # ylim(0,25) + 
#   theme(axis.text.x = element_text(angle = 315, hjust = 0)) + 
#   scale_size("Number of Person-Years\nObserved", breaks = waiver(), labels = waiver()) + 
#   geom_text(aes(label = number), vjust = -1.25, size = 3)#paste0(n_event, "/", n_py)), vjust = -1)
# ggsave(filename = here("graphs", "point_suicide-overdose-rate_by-cal.png"),
#        device = "png",
#        width = 9,
#        height = 5.5,
#        units = "in")
# 
# new %>%
#   ggplot(aes(x = calyear, y = rate*10000, col = event)) +
#   geom_point(aes(size = n_py)) + 
#   geom_line(aes(group = event), alpha = 0.6, lty = 2)  +
#   scale_color_manual(NULL, 
#                      values = c("suicides" = swatch()[2], 
#                                 "overdoses" = swatch()[4])) + 
#   guides(col = FALSE) + 
#   ylab("Suicide and Overdose Rate\nper 10,000 person-years") +
#   xlab("Calendar Year") + 
#   # ylim(0,25) + 
#   theme(axis.text.x = element_text(angle = 315, hjust = 0)) + 
#   scale_size("Number of Person-Years\nObserved", breaks = waiver(), labels = waiver()) + 
#   annotate(geom = "text", x = 5.5, y = 6.25, label = "Overdoses & Poisonings", col = swatch()[4], size = 3) + 
#   annotate(geom = "text", x = 4, y = 15.5, label = "Suicides", col = swatch()[2], size = 3) + 
#   geom_text(aes(label = number), vjust = -1.25, size = 3)
# ggsave(filename = here("graphs", "point_suicide-overdose-rate_by-cal_v2.png"),
#        device = "png",
#        width = 8,
#        height = 5.5,
#        units = "in")
# 
# 
# 
# 
# #### Age Distribution in 2000
# 
# d2000 <- dta %>% 
#   filter(1900 + yod09 >= 2000) %>% 
#   ggplot(aes(x = 2000 - (1900 + YOB))) + 
#   geom_histogram() + 
#   xlab("Age (year)") + 
#   xlim(38,90)  +
#   ylim(0,2000) + 
#   ylab("Number of Workers") +
#   annotate(geom = "text", x = 80, y = 150, label = "2000", col = "white", size = 8)
# 
# d2010 <- dta %>% 
#   filter(1900 + yod09 >= 2010) %>% 
#   ggplot(aes(x = 2010 - (1900 + YOB))) + 
#   geom_histogram() + 
#   xlab("Age (year)") + 
#   xlim(38,90)  +
#   ylim(0,2000) + 
#   ylab("Number of Workers") +
#   annotate(geom = "text", x = 80, y = 150, label = "2010", col = "white", size = 8)
# d2010
# 
# 
# library(gridExtra)
# 
# p1 <- grid.arrange(d2000, d2010, ncol = 1)
# ggsave(plot = p1, 
#        filename = here("graphs", "hist_age-dist-2000s.png"),
#        device = "png",
#        width = 8,
#        height = 5.5,
#        units = "in")
# 
# dta %>% 
#   filter(suicide == 1 | poison == 1, yod09 - YOUT16 <= 1, YOUT16 < 95) %>% 
#   ggplot(aes(x  = floor((yod09 - YOUT16)*365))) +
#   geom_histogram() + 
#   xlab("Days since leaving work") + 
#   xlim(0,180)
# 
# dta %>% 
#   filter(suicide == 1 | poison == 1, yod09 - YOUT16 <= 1, YOUT16 < 95) %>% 
#   ggplot(aes(x = floor((yod09 - YOUT16)*365))) + 
#   geom_bar() + 
#   xlim(0,180) + 
#   ylim(0,15)
# 
# dta %>% 
#   filter(suicide == 1 | poison == 1, yod09 - YOUT16 <= 1, YOUT16 < 95) %>% 
#   group_by(days = floor((yod09 - YOUT16)*365)) %>%
#   summarize(n = n_distinct(STUDYNO))
# 
# 
