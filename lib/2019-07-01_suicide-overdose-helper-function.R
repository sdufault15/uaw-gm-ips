
self_injury_function <- function(NOTHING){
  # TAKES NOTHING BUT RETURNS A LIST OF SUICIDE AND OVERDOSE CODES
  # (as of 7/1/2019)
  
  suicideCodes9 <- c(as.vector(sapply(950:959, function(x){paste0(x,0:9)})),paste0(950:959,"x"))
  accidentalPoison9 <- c(as.vector(sapply(850:860, function(x){paste0(x,0:9)})), paste0(850:860,"x"))
  undeterminedintent9 <- c(as.vector(sapply(980, function(x){paste0(x,0:9)})), paste0(980,"x"))
  suicideCodes10 <- c(paste0("X", 60:84), "Y87", "U03")
  accidentalPoison10 <- c(paste0("X", 40:44), paste0("Y", c(10:14)))#, paste0("Y", c(45,47,49))) # accidental and intent-undetermined
  
  suicide_codes <- c(suicideCodes9, suicideCodes10)
  overdose_codes <- c(accidentalPoison9, accidentalPoison10, undeterminedintent9)
  
  output <- list(suicide_codes = suicide_codes, overdose_codes = overdose_codes)
  
  return(output)
}

