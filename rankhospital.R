## Coursera Course Title: R Programming
## Coursera Course ID: rprog-011
## Programming Assignment 3: Hospital Quality
## Student: Ruben Leon

## Assigment Description:  Ranking hospitals by outcome in a state

## The function called rankhospital takes three arguments: the 2-character 
## abbreviated name of a state (state), an outcome (outcome), and the ranking 
## of a hospital in that state for that outcome (num). The function reads the
## outcome-of-care-measures.csv file and returns a character vector with the 
## name of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
     ## Read outcome data
     ocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Coerce the rate death columns to be numeric
     ocm[, 11] <- suppressWarnings(as.numeric(ocm[, 11]))
     ocm[,17] <- suppressWarnings(as.numeric(ocm[,17]))
     ocm[,23] <- suppressWarnings(as.numeric(ocm[,23]))    
     
     ## Check that state and outcome are valid
     valid_states <- unique(ocm$State)
     if (!state %in% valid_states) { stop('invalid state') }
     
     valid_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
     if (!outcome %in% valid_outcomes) { stop('invalid outcome') }
     
     ## Subset data to match state and outcome values. Also exludes hospitals
     ## that do not have data on particular outcome     
     sset <- NULL
     if(outcome == "heart attack"){
          sset <- subset(ocm, State==state & !is.na(ocm[11]), select = c(2,7,11))          
          
     }else if(outcome == "heart failure"){
          sset <- subset(ocm, State==state & !is.na(ocm[17]), select = c(2,7,17))                    
          
     }else if(outcome == "pneumonia"){
          sset <- subset(ocm, State==state & !is.na(ocm[23]), select = c(2,7,23))
     }    
     
     ## Rename outcome name column with a generic name
     names(sset)[3]<-"outcome"     
     
     ## If the number given by num is larger than the number of hospitals in the
     ## state, then the function return NA     
     if (num=="best") {
          k <- 1
     } else if (num =="worst") {
          k <- length(sset$outcome)
     } else if (num>=1 & num <= length(sset$outcome) ) {
          k <- num
     } else {
          return(NA)
     }     
          
     ## Sort outcome asc and hospital names on alphabetical order
     order_by_HospitalName_asc <- order(sset$outcome, sset$Hospital.Name) 
     sset_ordered <- sset[order_by_HospitalName_asc,]
          
     ## Return hospital name in that state with the given rank
     ## 30-day death rate     
     return (sset_ordered$Hospital.Name[k])     
}
