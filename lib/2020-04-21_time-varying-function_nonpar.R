time_varying_function_nonpar <- function(dta_in = work_hist){
  # This function produces the time-varying dataset for use in the IPS analysis
  
  dta <- dta_in %>%
    mutate(MACH2 = case_when(MACH == "AS" ~ "assembly",
                             MACH == "" ~ "off",
                             TRUE ~ "machining")) %>%
    mutate(MACH2 = as.factor(MACH2),
           Plant = as.factor(Plant))
  
  # data.table implementation of workflow outlined in 2019-07-15 version
  require(data.table); require(lubridate)
  dta <- as.data.table(dta)
  
  # Identify their range of work history records
  dta[,`:=`(
    origin = min(DATEIN),
    origout = max(DATEOUT)
  ), by = .(STUDYNO)]
  
  # Reconcile differences in end of employment records
  dta[date.mdy(origout)$year != year_left_work, `:=`(
    origout = julian(as.POSIXct(paste0(year_left_work,"-01-01")), 
                     origin = as.POSIXct("1960-01-01"))
  )]
  
  # Any records negative?
  if (nrow(dta[origout < origin]) > 0) {
    warning("Please check why there are negative rows in dta_in.")
  }
  
  # Make job history DATEIN and DATETOUT class Date
  dta[,`:=`(
    origin = as.Date(origin, origin = "1960-01-01"),
    origout = as.Date(origout, origin = "1960-01-01"),
    DATEIN = as.Date(DATEIN, origin = "1960-01-01"),
    DATEOUT = as.Date(DATEOUT, origin = "1960-01-01"),
    record.id = 1:.N
  )]
  
  # Make each row no longer than a year
  dta_tv <- dta[,.(
    STUDYNO,
    year = seq(year(DATEIN), year(DATEOUT)),
    DATEIN,
    DATEOUT,
    Plant,
    MACH2,
    origin = origin,
    origout = origout
  ), by = .(record.id)]
  
  setorder(dta, STUDYNO, DATEIN, DATEOUT)
  
  # Calculate number of days at start, end, and middle of record
  dta_tv[year(DATEIN) == year & year(DATEOUT) == year,`:=`(
    daysInJob = time_length(difftime(DATEOUT + days(1), DATEIN), "year")
  )]
  dta_tv[year(DATEIN) == year & year(DATEOUT) > year,`:=`(
    daysInJob = time_length(difftime(as.Date(paste0(year + 1, "-01-01")), DATEIN), "year")
  )]
  dta_tv[year(DATEIN) < year & year(DATEOUT) == year,`:=`(
    daysInJob = time_length(difftime(DATEOUT + days(1),  paste0(year, "-01-01")), "year")
  )]
  dta_tv[year(DATEIN) < year & year(DATEOUT) > year,`:=`(
    daysInJob = time_length(difftime(as.Date(paste0(year + 1, "-01-01")),  paste0(year, "-01-01")), "year")
  )]
  
  # Sum up by year 
  dta_tv <- dta_tv[,.(
    ndays.GAN = sum(daysInJob[Plant == "gan"]),
    ndays.HAN = sum(daysInJob[Plant == "han"]),
    ndays.SAN = sum(daysInJob[Plant == "san"]),
    ndays.mach = sum(daysInJob[MACH2 == "machining"]),
    ndays.assembly = sum(daysInJob[MACH2 == "assembly"]),
    ndays.off = sum(daysInJob[MACH2 == "off"])
  ), by = .(STUDYNO, year)]
  
  # Number of days in year
  dta_tv[,`:=`(
    daysInYear = time_length(difftime(as.Date(paste0(year + 1, "-01-01")),
                                      as.Date(paste0(year, "-01-01"))), 'day')
  )]
  
  # Divide counts by days in year
  cols <- paste0("ndays.", c("GAN", "HAN", "SAN", "mach", "assembly", "off"))
  dta_tv[ ,(cols) := lapply(.SD, "/", daysInYear), .SDcols = cols]
  
  # make yearWork
  setorder(dta_tv, STUDYNO, year)
  dta_tv[,`:=`(
    yearWork = 1:.N
  ), by = .(STUDYNO)]
  
  # rename year
  names(dta_tv)[names(dta_tv) == "year"] <- "origin"
  
  dta_tv <- as.data.frame(dta_tv)
  
  # detach packages
  detach(package:data.table, unload = T)
  detach(package:lubridate, unload = T)
  
  return(dta_tv)
}
