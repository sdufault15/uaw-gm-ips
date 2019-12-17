load("/Users/sdufault/Downloads/suicide.ipsi.Rdata")
library(tidyverse)
library(latex2exp)
library(ggrepel)
library(here)

# This file explores the "odd" behavior in this new sample compared to the 
# previous results presented at SER. My intuition is that these results are actually
# very similar, but the range of incremental effects may be too drastically large for 
# our limited sample. When we look over a more reasonable range, the plot very much echoes
# the results presented at SER.

# Suggested next steps: narrow the range of incremental effects to something like 0.75 to 1.5, by
# 0.5 or so, which will give us a very fine grid over a more reasonable range. This would generate 
# a nice looking curve over which inference may be reasonable. 

# Extracting results and generating (approximate) suicide counts
res <- suicide.ipsi$res
res.points <- res %>% 
  filter(increment > 0.6, increment < 1.5) %>%  # for stability, only extracting moderate incremental effects
  mutate(no.sim.events = est*24941.02) %>%  # approximated the full sample size (135/0.00541277)
  distinct() # for some reason the 1.00 increment is duplicated, so removing

res %>%
  filter(increment > 0.6, increment < 1.5) %>%
  ggplot(aes(x = increment, y = est)) + 
  # estimates
  geom_line() + 
  # confidence band
  geom_ribbon(aes(x = increment, ymin = ci.ll, ymax = ci.ul), 
              alpha = 0.3, 
              fill = "lightblue") + 
  xlab(TeX("Incremental propensity shift, $\\delta")) + 
  # setting the observed line
  geom_vline(xintercept = 1, lty = 2, col = "darkgray") + 
  # Labeling the incidence counts and rates along the plotted line
  geom_point(data = res.points, col = 'darkblue', pch = 21, fill = 'white') +
  geom_label_repel(data = res.points, label= round(res.points$est, 4), box.padding = 0.5, label.padding = 0.3, point.padding = 0.1, segment.color = 'darkblue', nudge_y = -0.005, arrow = arrow(length = unit(0.075, "inches"), type = "open")) + 
  geom_label_repel(label = ceiling(res.points$no.sim.events), data = res.points, box.padding = 0.5, label.padding = 0.3, point.padding = 0.1, segment.color = 'darkred', nudge_y = 0.005, arrow = arrow(length = unit(0.075, "inches"), type = "open")) + 
  ggtitle("Delta Curve") +
  ylab("Estimated Suicide Incidence") + 
  theme_classic()

ggsave(filename = here("graphs", "2019-12-17_exploratory-suicide-plot.png"),
       units = "in",
       height = 6, 
       width = 10,
       device = "png")  
