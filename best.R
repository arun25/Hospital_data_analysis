##create a method 'best' 

best <- function(state,outcome){
        ## read outcome data
        complete_data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        complete_data[,11] <- as.numeric(complete_data[,11]);
        complete_data[,17] <- as.numeric(complete_data[,17]);
        complete_data[,23] <- as.numeric(complete_data[,23]);
        complete_data <- na.omit(complete_data)
        vec_state <- c(unique(complete_data$State))
        vec_outcome <- c("heart attack", "heart failure", "pneumonia")
        if(!is.element(state,vec_state)){
          stop("invalid state")
        }
        if(!is.element(outcome,vec_outcome)){
          stop("invalid outcome")
        }
        
        
        data_by_state <- complete_data[complete_data$State == state,]
        if(outcome == "heart attack"){hosp.name <- data_by_state$Hospital.Name[which.min(data_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)]}
        if(outcome == "heart failure"){hosp.name <- data_by_state$Hospital.Name[which.min(data_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)]}
        if(outcome == "pneumonia"){hosp.name <- data_by_state$Hospital.Name[which.min(data_by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)]}
        hosp.name
        
}