rankhospital <- function(state, outcome, rank) {
  ## Read outcome data
  outcome.csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome.csv[outcome.csv$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == "Not Available",]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- NA
  outcome.csv[outcome.csv$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == "Not Available",]$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- NA
  outcome.csv[outcome.csv$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == "Not Available",]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- NA
  
  ## Check that state and outcome are valid
  state = toupper(state)
  state_list <- unique(outcome.csv[,7])
  outcome = tolower(outcome)
  outcome_list <- c("heart attack", "pneumonia", "heart failure")
  if (state %in% state_list == FALSE){
    stop
    geterrmessage("invalid state")
  }
  else if (outcome %in% outcome_list == FALSE){
    stop
    geterrmessage("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  else {
    if (outcome == "heart attack"){
      ha.mort.vals <- subset(outcome.csv, State==state, select = c(State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name))
      ha.mort.nums <- subset(ha.mort.vals, is.na(as.numeric(ha.mort.vals[,2])) == FALSE)
      hordered <- ha.mort.nums[order(ha.mort.nums[,2], ha.mort.nums[,3]),]
      hordered <- cbind(hordered, 1:length(hordered[,2]))
      hordered[rank, 3]
    }
    else if (outcome == "pneumonia"){
      pneu.mort.vals <- subset(outcome.csv, State==state, select = c(State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name))
      pneu.mort.nums <- subset(pneu.mort.vals, is.na(as.numeric(pneu.mort.vals[,2])) == FALSE)
      pordered <- pneu.mort.nums[order(pneu.mort.nums[,2], pneu.mort.nums[,3]),]
      pordered <- cbind(pordered, 1:length(pordered[,2]))
      pordered[rank, 3]
    }
    else if (outcome == "heart failure"){
      hf.mort.vals <- subset(outcome.csv, State==state, select = c(State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name))
      hf.mort.nums <- subset(hf.mort.vals, is.na(as.numeric(hf.mort.vals[,2])) == FALSE)
      fordered <- hf.mort.nums[order(hf.mort.nums[,2], hf.mort.nums[,3]),]
      fordered <- cbind(fordered, 1:length(fordered[,2]))
      fordered[rank, 3]
    }
  }
}