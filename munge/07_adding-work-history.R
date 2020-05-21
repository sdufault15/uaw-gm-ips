#############
# Suzanne Dufault
# Taking the information from data-cleaning-flowchart and implementing a series of rules for cleaning the data
# Original: August 6, 2018
# Rerun: July 10, 2019
#############

gan <- box_search("gpp84_3.Rdata") %>% box_read()
han <- box_search("hpp84_3.Rdata") %>% box_read()
san <- box_search("spp84_3.Rdata") %>% box_read()

# Removing individuals hired before 1938
gan38 <- gan %>% mutate(remove = ifelse(date.mdy(DATEIN)$year < 1938,1,0))
han38 <- han %>% mutate(remove = ifelse(date.mdy(DATEIN)$year < 1938,1,0))
san38 <- san %>% mutate(remove = ifelse(date.mdy(DATEIN)$year < 1938,1,0))

rem <- c(gan38$STUDYNO[gan38$remove == 1], han38$STUDYNO[han38$remove == 1], san38$STUDYNO[san38$remove == 1])
length(unique(rem)) # individuals hired before 1938

gan38 <- gan38 %>% filter(!STUDYNO %in% unique(rem))
han38 <- han38 %>% filter(!STUDYNO %in% unique(rem)) 
san38 <- san38 %>% filter(!STUDYNO %in% unique(rem))

# Dropping individuals from the work files who are not in the main cohort
s1 <- unique(cohort$STUDYNO)
gan38 <- gan38 %>% filter(STUDYNO %in% s1) %>% select(STUDYNO, DATEIN, DATEOUT, HISTCODE, MACH) # %>% filter(DATEIN != DATEOUT)
han38 <- han38 %>% filter(STUDYNO %in% s1) %>% select(STUDYNO, DATEIN, DATEOUT, HISTCODE, MACH) # %>% filter(DATEIN != DATEOUT)
san38 <- san38 %>% filter(STUDYNO %in% s1) %>% select(STUDYNO, DATEIN, DATEOUT, HISTCODE, MACH) # %>% filter(DATEIN != DATEOUT)
length(unique(gan38$STUDYNO)) # individuals who are also in the cohort data
length(unique(han38$STUDYNO))
length(unique(san38$STUDYNO))

allplants <- bind_rows(gan = gan38, han = han38, san = san38, .id = "Plant") %>%
  mutate(monthIN = date.mdy(DATEIN)$month, 
         dayIN = date.mdy(DATEIN)$day,
         yearIN = date.mdy(DATEIN)$year,
         monthOUT = date.mdy(DATEOUT)$month,
         dayOUT = date.mdy(DATEOUT)$day,
         yearOUT = date.mdy(DATEOUT)$year)

# Collapse duplicated rows
allplants <- distinct(allplants)

allplants <- full_join(allplants, cohort, by = "STUDYNO") %>% 
  arrange(STUDYNO, DATEOUT) 
allplants <- allplants %>% group_by(STUDYNO) %>% 
  mutate(t = row_number(), maxT = max(t)) %>% 
  mutate(finalyear = ifelse(t == maxT, 1, 0))
allplants %>% filter(t == maxT) %>% 
  group_by(YOUT16 == 1995) %>% 
  summarize(n = n_distinct(STUDYNO)) # number of individuals administratively censored

allplants.nocens <- allplants %>% filter(YOUT16 != 1995)
allplants.cens <- allplants %>% filter(YOUT16 == 1995)

# FOR ASSIGNING END OF EMPLOYMENT AND END OF ACTIVE JOB HISTORY
censored <- allplants.cens %>% select(STUDYNO) %>% distinct()
###############################################################

# Examining the end dates from the job histories
allplants.nocens %>% 
  filter(t == maxT) %>% 
  group_by(HISTCODE == "OFF", HISTCODE == "MSS", HISTCODE == "MD", HISTCODE == "SR") %>% 
  summarize(n = n_distinct(STUDYNO)) # how many have OFF, MSS, MD, SR or Numeric final records
allplants.nocens %>% filter(t == maxT, HISTCODE %in% c("OFF", "MSS", "MD", "SR")) %>% group_by(DATEIN == DATEOUT) %>% 
  summarize(n = n_distinct(STUDYNO)) # how many have DATEIN == DATEOUT as final record

allplants.nocens %>% 
  filter(t == maxT, DATEOUT != DATEIN, HISTCODE %in% c("OFF", "MSS", "MD", "SR")) %>% 
  group_by(cut(yearOUT - yearIN, breaks = c(-1,1,2,5,10,20,50,100))) %>% 
  summarize(n = n_distinct(STUDYNO))

# FOR ASSIGNING END OF EMPLOYMENT ############################
final.record.off <- allplants.nocens %>% 
  filter(t == maxT, 
         #DATEOUT != DATEIN, 
         HISTCODE %in% c("OFF", "MSS", "MD", "SR"), 
         yearOUT - yearIN < 11) %>% 
  select(STUDYNO) %>% 
  distinct()

final.record.off.censored <- allplants.nocens %>% 
  filter(t == maxT, 
         #DATEOUT != DATEIN, 
         HISTCODE %in% c("OFF", "MSS", "MD", "SR"), 
         yearOUT - yearIN >= 11) %>% 
  select(STUDYNO) %>% 
  distinct()

last.record.off.datein.equals.dateout <- allplants.nocens %>% 
  filter(t == maxT, 
         #DATEOUT != DATEIN, 
         HISTCODE %in% c("OFF", "MSS", "MD", "SR"), 
         DATEIN == DATEOUT) %>% 
  select(STUDYNO) %>% 
  distinct()

penultimate.record.off.datein.equals.dateout <- allplants.nocens %>% 
  filter(t == maxT - 1, 
         #DATEOUT != DATEIN, 
         HISTCODE %in% c("OFF", "MSS", "MD", "SR"), 
         DATEIN == DATEOUT) %>% 
  select(STUDYNO) %>% 
  distinct()

final.penultimate.record.off <- intersect(last.record.off.datein.equals.dateout,
                                          penultimate.record.off.datein.equals.dateout)

###############################################################

allplants.cens %>% 
  filter(t == maxT) %>% 
  group_by(cut(yearOUT - yearIN, breaks = c(-1,1,2,5,10,20,50,100))) %>% 
  summarize(n = n_distinct(STUDYNO)) 
# since YOUT was constructed by assigning all job history end dates within 3 years of 1995 as 95, we will call all of these
# censored.

# End of Active Employment
new2 <- allplants.nocens %>% 
  filter(t == maxT,
         # Exclude folks whose penultimate record is also non-numeric, with DATEIN == DATEOUT
         !STUDYNO %in% unlist(final.penultimate.record.off),
         HISTCODE %in% c("OFF", "MSS", "MD", "SR"), DATEIN == DATEOUT) %>% 
  select(STUDYNO, finalyearIN = yearIN) 
new2 <- allplants.nocens %>% 
  filter(t == maxT - 1, # Use penultimate record
         STUDYNO %in% unlist(final.penultimate.record.off)) %>% 
  select(STUDYNO, finalyearIN = yearIN) %>% rbind(new2)

# Last numeric entry for these folks
new2b <- allplants.nocens %>% 
  filter(STUDYNO %in% new2$STUDYNO, !HISTCODE %in% c("OFF", "MSS", "MD", "SR"), DATEIN != DATEOUT) %>% 
  group_by(STUDYNO) %>% mutate(n.maxT = max(t))
t <- new2b %>% filter(t == n.maxT) %>% select(STUDYNO, yearOUT, YOUT16)

t2 <- full_join(t, new2, by = "STUDYNO")
t2 %>% 
  group_by(finalyearIN - yearOUT <= 1, YOUT16 - finalyearIN <= 2) %>% 
  summarize(n = n_distinct(STUDYNO)) # number of records within 1 year of the final

# FOR ASSIGNING END OF ACTIVE JOB HISTORY ############################
final.record.off.1year <- t2 %>% filter(finalyearIN - yearOUT <= 1) %>% select(STUDYNO) %>% distinct()

final.record.off.g1year <- t2 %>% filter(finalyearIN - yearOUT > 1) %>% select(STUDYNO) %>% distinct()
###############################################################

allplants.nocens %>% filter(t == maxT, HISTCODE %in% c("OFF", "MSS", "MD", "SR"), DATEIN != DATEOUT) %>% 
  group_by(yearOUT - yearIN <= 1) %>% summarize(n = n_distinct(STUDYNO)) # number of final records whose DATEIN != DATEOUT and last less than a year

# FOR ASSIGNING END OF ACTIVE JOB HISTORY ############################
final.record.long <- allplants.nocens %>% filter(t == maxT, HISTCODE %in% c("OFF", "MSS", "MD", "SR"), DATEIN != DATEOUT, yearOUT - yearIN > 1) %>% select(STUDYNO) %>% distinct()
######################################################################

new3 <- allplants.nocens %>% filter(t == maxT, HISTCODE %in% c("OFF", "MSS", "MD", "SR"), DATEIN != DATEOUT, yearOUT - yearIN <= 1) %>% select(STUDYNO, finalyearIN = yearIN)
new3b <- allplants.nocens %>% filter(STUDYNO %in% new3$STUDYNO, !HISTCODE %in% c("OFF", "MSS", "MD", "SR")) %>% group_by(STUDYNO) %>% mutate(n.maxT = max(t))

t3 <- new3b %>% filter(t==n.maxT) %>% select(STUDYNO, yearOUT, YOUT16)
t3b <- full_join(t3, new3, by = "STUDYNO")
t3b %>% group_by(finalyearIN - yearOUT <= 1, YOUT16 - finalyearIN <= 2) %>% summarize(n = n_distinct(STUDYNO)) # number of final records whose DATEIN != DATEOUT within 1 year of final numeric record

# FOR ASSIGNING END OF ACTIVE JOB HISTORY ############################
final.record.dnd.1year <- t3b %>% filter(finalyearIN - yearOUT <= 1) %>% select(STUDYNO) %>% distinct()

final.record.dnd.g1year <- t3b %>% filter(finalyearIN - yearOUT > 1) %>% select(STUDYNO) %>% distinct()
###############################################################

################################################################################################
# FINALIZING DATASETS
################################################################################################

# End of Employment
dta_end_of_employment <- allplants
dta_end_of_employment <- dta_end_of_employment %>%
  group_by(STUDYNO) %>% 
  mutate(year_left_work = ifelse(STUDYNO %in% final.record.off$STUDYNO,
                                 ifelse(!STUDYNO %in% unlist(final.penultimate.record.off), yearOUT[t == maxT], yearOUT[t == (maxT - 1)]),
                                 ifelse(STUDYNO %in% final.record.off.censored$STUDYNO, yearIN[t == maxT] + 10,
                                        ifelse(STUDYNO %in% censored$STUDYNO, 1995, yearOUT[t == maxT]))),
         month_left_work = ifelse(STUDYNO %in% final.record.off$STUDYNO,
                                 ifelse(!STUDYNO %in% unlist(final.penultimate.record.off), monthOUT[t == maxT], monthOUT[t == (maxT - 1)]),
                                 ifelse(STUDYNO %in% final.record.off.censored$STUDYNO, monthIN[t == maxT] + 10,
                                        ifelse(STUDYNO %in% censored$STUDYNO, 1995, monthOUT[t == maxT]))),
         day_left_work = ifelse(STUDYNO %in% final.record.off$STUDYNO,
                                 ifelse(!STUDYNO %in% unlist(final.penultimate.record.off), dayOUT[t == maxT], dayOUT[t == (maxT - 1)]),
                                 ifelse(STUDYNO %in% final.record.off.censored$STUDYNO, dayIN[t == maxT] + 10,
                                        ifelse(STUDYNO %in% censored$STUDYNO, 1995, dayOUT[t == maxT])))) %>% 
  ungroup()

box_save(dta_end_of_employment, 
         dir_id = 80875764240, 
         file_name = "2020-04-21_cohort_work-history_long.RData", 
         description = "Merging the cohort demographic data with the job histories. Reconciling differences in YOUT (cohort) and DATEOUT (job hist).")


# # End of Active Job History
# dta_end_of_active_job <- allplants
# dta_end_of_active_job_1 <- dta_end_of_active_job %>% filter(STUDYNO %in% c(final.record.dnd.1year$STUDYNO, final.record.off.1year$STUDYNO))
# dta_end_of_active_job_2 <- dta_end_of_active_job %>% 
#   filter(STUDYNO %in% c(final.record.dnd.g1year$STUDYNO, final.record.off.g1year$STUDYNO, final.record.long$STUDYNO))
# dta_end_of_active_job_cens <- dta_end_of_active_job %>% filter(STUDYNO %in% censored$STUDYNO)
# dta_end_of_active_job_num <- dta_end_of_active_job %>% filter(!STUDYNO %in% c(final.record.dnd.1year$STUDYNO, final.record.off.1year$STUDYNO,
#                                                                               final.record.dnd.g1year$STUDYNO, final.record.off.g1year$STUDYNO, 
#                                                                               final.record.long$STUDYNO, censored$STUDYNO))
# 
# dta_end_of_active_job_1 <- bind_rows(dta_end_of_active_job_1, dta_end_of_active_job_num) %>% group_by(STUDYNO) %>%
#   filter(t == max(t)) %>% mutate(leaveWORK = yearOUT) %>% select(STUDYNO, leaveWORK) 
# dta_end_of_active_job_2 <- dta_end_of_active_job_2 %>% filter(!HISTCODE %in% c("OFF", "MSS", "MD", "SR")) %>% arrange(STUDYNO, DATEIN)
# dta_end_of_active_job_2 <- dta_end_of_active_job_2 %>% group_by(STUDYNO) %>% filter(t == max(t)) %>% ungroup() %>% select(STUDYNO, HISTCODE, yearOUT)
# dta_end_of_active_job_2 <- dta_end_of_active_job_2 %>% mutate(leaveWORK = yearOUT) %>% select(STUDYNO, leaveWORK)
# 
# dta_end_of_active_job_cens <- dta_end_of_active_job_cens %>% mutate(leaveWORK = 1995) %>% select(STUDYNO, leaveWORK) %>% distinct()
# 
# out.dta <- bind_rows(dta_end_of_active_job_1, dta_end_of_active_job_2, dta_end_of_active_job_cens)
# 
# dta_end_of_active_job <- full_join(allplants, out.dta, by = "STUDYNO")
# 
# box_save(dta_end_of_active_job, dir_id = 51069117810, file_name = "endofactivejob942018.RData")
# 
# 
