best <- function(state, outcome) {
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
        hordered[1, 3]
        #max.ha <- min(ha.mort.nums[,2])
        #amax.state <- subset(ha.mort.nums, ha.mort.nums[,2] == max.ha, select = Hospital.Name)
        #asorted <- sort(amax.state[,1])
        #asorted[1]
      }
        else if (outcome == "pneumonia"){
          pneu.mort.vals <- subset(outcome.csv, State==state, select = c(State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name))
          pneu.mort.nums <- subset(pneu.mort.vals, is.na(as.numeric(pneu.mort.vals[,2])) == FALSE)
          pordered <- pneu.mort.nums[order(pneu.mort.nums[,2], pneu.mort.nums[,3]),]
          pordered[1, 3]
          #max.pneu <- min(pneu.mort.nums[,2])
          #pmax.state <- subset(pneu.mort.nums, pneu.mort.nums[,2] == max.pneu, select = Hospital.Name)
          #psorted <- sort(pmax.state[,1])
          #psorted[1]
      }
        else if (outcome == "heart failure"){
          hf.mort.vals <- subset(outcome.csv, State==state, select = c(State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name))
          hf.mort.nums <- subset(hf.mort.vals, is.na(as.numeric(hf.mort.vals[,2])) == FALSE)
          max.hf <- min(hf.mort.nums[,2])
          fmax.state <- subset(hf.mort.nums, hf.mort.nums[,2] == max.hf, select = Hospital.Name)
          fsorted <- sort(fmax.state[,1])
          fsorted[1]
      }
    }
}