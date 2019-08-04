time_varying_function <- function(dta){
  # This function produces the time-varying dataset for use in the IPS analysis
  
  dta_tv <- data.frame(STUDYNO = NULL, ndays.Plant1 = NULL, ndays.Plant2 = NULL, ndays.Plant3 = NULL, ndays.mach = NULL, ndays.assembly = NULL, ndays.off = NULL, yearWork = NULL, origin = NULL)
  studynos <- dta %>% select(STUDYNO) %>% distinct()
  
  #out <- foreach(id = els, .packages = c("dplyr", "date"), .combine = 'rbind') %dopar% {
  iter0 <- 1
  for (id in studynos){
    temp <- dta %>% filter(STUDYNO == id)
    dates <- temp %>% mutate(origin = min(DATEIN), 
                             origout = max(DATEOUT)) %>% select(STUDYNO, origin, origout, year_left_work) %>% distinct()
    dates <- dates %>% group_by(STUDYNO) %>% mutate(origout = ifelse(date.mdy(origout)$year != year_left_work, 
                                                                     julian(as.POSIXct(paste0(year_left_work,"-01-01")), origin = as.POSIXct("1960-01-01")), 
                                                                     origout)) %>% select(-year_left_work)
    iter1 <- 0
    
    if (dates$origin <= dates$origout){
      for (yr in seq(dates$origin, dates$origout, by = 365)){
        temp2 <- temp %>% filter(DATEIN < yr + 365 & DATEOUT > yr) # identify all entries for individual who was still at work or entered work within (yr, yr + 365)
        temp2 <- temp2 %>% mutate(DATEOUT = ifelse(DATEOUT > yr + 365, yr + 365, DATEOUT),  # if individual doesn't leave work in the interval, cut the number of days to yr + 365
                                DATEIN = ifelse(DATEIN < yr, yr, DATEIN)) # if the individual entered long before yr, set their DATEIN to yr
        temp2 <- temp2 %>% mutate(daysInJob = DATEOUT - DATEIN) # count the number of days within this interval
        
        days.m <- temp2 %>% group_by(MACH2) %>% summarize(days.m = sum(daysInJob)) # determine number of days doing machining, assembly, and "OFF"
        days.p <- temp2 %>% group_by(Plant) %>% summarize(days.p = sum(daysInJob)) # determine number of days at each plant in one year
      
        temp3 <- data.frame(STUDYNO = id, ndays.GAN = max(days.p$days.p[days.p$Plant == "gan"], 0), ndays.HAN = max(days.p$days.p[days.p$Plant == "han"], 0), ndays.SAN = max(days.p$days.p[days.p$Plant == "san"], 0), 
                       ndays.mach = max(days.m$days.m[days.m$MACH2 == "machining"], 0), ndays.assembly = max(days.m$days.m[days.m$MACH2 == "assembly"], 0), ndays.off = max(days.m$days.m[days.m$MACH2 == "off"], 0), 
                       yearWork = iter1, origin = yr)
        dta_tv <- bind_rows(dta_tv, temp3)
        iter1 <- iter1 + 1
        }
    } else {}
    iter0 <- iter0 + 1
  }
  return(dta_tv)
}


# time_varying_function_par <- function(dta, els){
#   # This function produces the time-varying dataset for use in the IPS analysis
#   
#   dta_tv <- data.frame(STUDYNO = NULL, ndays.Plant1 = NULL, ndays.Plant2 = NULL, ndays.Plant3 = NULL, ndays.mach = NULL, ndays.assembly = NULL, ndays.off = NULL, yearWork = NULL, origin = NULL)
#   #studynos <- dta %>% select(STUDYNO) %>% distinct()
#   
#   out <- foreach(id = els, .packages = c("dplyr", "date"), .combine = 'rbind') %dopar% {
#   #iter0 <- 1
#   #for (id in els){
#     #print(c(iter0, "of ", length(els)))
#     temp <- dta %>% filter(STUDYNO == id)
#     dates <- temp %>% mutate(origin = min(DATEIN), 
#                              origout = max(DATEOUT)) %>% select(STUDYNO, origin, origout, year_left_work) %>% distinct()
#     dates <- dates %>% group_by(STUDYNO) %>% mutate(origout = ifelse(date.mdy(origout)$year != year_left_work, 
#                                                                      julian(as.POSIXct(paste0(year_left_work,"-01-01")), origin = as.POSIXct("1960-01-01")), 
#                                                                      origout)) %>% select(-year_left_work)
#     
#     iter1 <- 0
#     
#     if (dates$origin <= dates$origout){
#       for (yr in seq(dates$origin, dates$origout, by = 365)){
#         temp2 <- temp %>% filter(DATEIN < yr + 365 & DATEOUT > yr) # identify all entries for individual who was still at work or entered work within (yr, yr + 365)
#         temp2 <- temp2 %>% mutate(DATEOUT = ifelse(DATEOUT > yr + 365, yr + 365, DATEOUT),  # if individual doesn't leave work in the interval, cut the number of days to yr + 365
#                                   DATEIN = ifelse(DATEIN < yr, yr, DATEIN)) # if the individual entered long before yr, set their DATEIN to yr
#         temp2 <- temp2 %>% mutate(daysInJob = DATEOUT - DATEIN) # count the number of days within this interval
#         
#         days.m <- temp2 %>% group_by(MACH2) %>% summarize(days.m = sum(daysInJob)) # determine number of days doing machining, assembly, and "OFF"
#         days.p <- temp2 %>% group_by(Plant) %>% summarize(days.p = sum(daysInJob)) # determine number of days at each plant in one year
#         
#         temp3 <- data.frame(STUDYNO = id, ndays.GAN = max(days.p$days.p[days.p$Plant == "gan"], 0), ndays.HAN = max(days.p$days.p[days.p$Plant == "han"], 0), ndays.SAN = max(days.p$days.p[days.p$Plant == "san"], 0), 
#                             ndays.mach = max(days.m$days.m[days.m$MACH2 == "machining"], 0), ndays.assembly = max(days.m$days.m[days.m$MACH2 == "assembly"], 0), ndays.off = max(days.m$days.m[days.m$MACH2 == "off"], 0), 
#                             yearWork = iter1, origin = yr)
#         dta_tv <- bind_rows(dta_tv, temp3)
#         iter1 <- iter1 + 1
#       }
#     } else {}
#     dta_tv <- dta_tv %>% distinct()
#     return(dta_tv)
#     #iter0 <- iter0 + 1
#   }
#   return(out)
# }
