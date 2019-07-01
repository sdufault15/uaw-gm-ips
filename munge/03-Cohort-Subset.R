# Separating out those individuals who are hired before the start of followup (1941)
# and worked at multiple plants

cohort <- dta %>% 
  filter(NOHIST == 0 & WH == 1 & YIN16 >= 38) %>% 
  select(STUDYNO, FINRACE, SEX, YOB, PLANT, YIN16, YOUT16, yod09, cod_09, V_ICD, yrin16)

#dta %>% filter(NOHIST == 0 & WH == 1) %>% group_by(YIN16 >= 38) %>% summarize(n = n_distinct(STUDYNO))
