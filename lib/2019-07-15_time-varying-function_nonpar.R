time_varying_function_nonpar <- function(dta_in = work_hist, els = ids){
  # This function produces the time-varying dataset for use in the IPS analysis
  
  dta <- dta_in %>%
    mutate(MACH2 = case_when(MACH == "AS" ~ "assembly",
                             MACH == "" ~ "off",
                             TRUE ~ "machining")) %>%
    mutate(MACH2 = as.factor(MACH2),
           Plant = as.factor(Plant))
  
  pb <- txtProgressBar(max = length(els), style = 3)
  
  dta_tv <- data.frame(STUDYNO = NULL, 
                       ndays.Plant1 = NULL, 
                       ndays.Plant2 = NULL, 
                       ndays.Plant3 = NULL, 
                       ndays.mach = NULL, 
                       ndays.assembly = NULL, 
                       ndays.off = NULL, 
                       yearWork = NULL, 
                       origin = NULL)
  
  iter <- 1
  for(id in els){
    # Updated Progress Bar
    setTxtProgressBar(pb, iter)
    
    # Select individual of interest
    temp <- dta %>% 
      filter(STUDYNO == id)
    
    # Identify their range of work history records
    dates <- temp %>% 
      mutate(origin = min(DATEIN),
             origout = max(DATEOUT)) %>% 
      select(STUDYNO, origin, origout, year_left_work) %>%
      distinct()
    
    # Reconcile differences in end of employment records
    dates <- dates %>% 
      group_by(STUDYNO) %>% 
      mutate(origout = ifelse(date.mdy(origout)$year != year_left_work,
                              julian(as.POSIXct(paste0(year_left_work,"-01-01")), 
                                     origin = as.POSIXct("1960-01-01")),
                              origout)) %>% 
      select(-year_left_work)
    
    # For positive length work records, set up a sequence of years
    if (dates$origin <= dates$origout){
      seq_dates <- sapply(paste0(seq(lubridate::year(as.Date(dates$origin, origin = "1960-01-01")),
                                     lubridate::year(as.Date(dates$origout, origin = "1960-01-01"))), "-01-01"),
                          function(x) {julian(as.POSIXct(x), origin = as.POSIXct("1960-01-01"))})
      
      # For each year in that sequence...
      yr_count <- 1
      for (yr in seq_dates){
        temp2 <- temp %>% filter(DATEIN < yr + 365 & DATEOUT > yr) # identify all entries for individual who was still at work or entered work within (yr, yr + 365)
        temp2 <- temp2 %>% mutate(DATEOUT = ifelse(DATEOUT > yr + 365, yr + 365, DATEOUT),  # if individual doesn't leave work in the interval, cut the number of days to yr + 365
                                  DATEIN = ifelse(DATEIN < yr, yr, DATEIN)) # if the individual entered long before yr, set their DATEIN to yr
        temp2 <- temp2 %>% mutate(daysInJob = DATEOUT - DATEIN) # count the number of days within this interval
        
        days.m <- temp2 %>% 
          group_by(MACH2, .drop = FALSE) %>% 
          summarize(days.m = sum(daysInJob)) # determine number of days doing machining, assembly, and "OFF"
        
        days.p <- temp2 %>% 
          group_by(Plant, .drop = FALSE) %>% 
          summarize(days.p = sum(daysInJob)) # determine number of days at each plant in one year
        
        temp3 <- data.frame(STUDYNO = id, 
                            ndays.GAN = days.p$days.p[days.p$Plant == "gan"],
                            ndays.HAN = days.p$days.p[days.p$Plant == "han"],
                            ndays.SAN = days.p$days.p[days.p$Plant == "san"],
                            ndays.mach = days.m$days.m[days.m$MACH2 == "machining"],
                            ndays.assembly = days.m$days.m[days.m$MACH2 == "assembly"],
                            ndays.off = days.m$days.m[days.m$MACH2 == "off"],
                            yearWork = yr_count, 
                            origin = yr)
        
        dta_tv <- bind_rows(dta_tv, temp3)
        yr_count <- yr_count + 1
      }
    } else {}
    
    iter <- iter + 1
    
    # dta_tv <- dta_tv %>% 
    #   distinct()
  }
  
  close(pb)
  
  return(dta_tv)
}
