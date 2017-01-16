rankall <- function( outcome, num = "best") {
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- data[ , 7]
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  #if ((state %in% states) == FALSE) { stop(print("invalid state")) }
  #if ((outcome %in% outcomes) == FALSE) { stop(print("invalid outcome")) }
  
  if (outcome == "heart attack") { outcome_column <- 11 }
  else if (outcome == "heart failure") { outcome_column <- 17 }
  else { outcome_column <- 23 }
  
  data[, outcome_column] <- as.numeric(data[,outcome_column])
  
  #for (state_val in unique(states)) {}
  s <- split(data,data$State)
  
  df <- data.frame(hospital = character(), state = character(),stringsAsFactors = FALSE)
  
  lapply(s, function(x){
    #x[, outcome_column] <- as.numeric(x[,outcome_column])
    bad <- is.na(x[, outcome_column])
    desired_data <- x[!bad, ]
    
    outcome_column_name <- names(desired_data)[outcome_column]
    hospital_column_name <- names(desired_data)[2]
    index <- with(desired_data, order(desired_data[outcome_column_name], desired_data[hospital_column_name]))
    ordered_desired_data <- desired_data[index, ]
    
    #if nume is either "best" or "worst", then interpret it to the
    #corresponding numerical value
    if (is.character(num) == TRUE) {
      if (num == "best") {
        num = 1
      }
      else if (num == "worst") {
        num = length(ordered_desired_data[, outcome_column])
      }
    }
    #return the hospital name with the outcome ranking of num
    ordered_desired_data[num, 2]
    
  })
  
}