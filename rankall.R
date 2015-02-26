## Coursera Course Title: R Programming
## Coursera Course ID: rprog-011
## Programming Assignment 3: Hospital Quality
## Student: Ruben Leon

## Assigment Description:  Ranking hospitals in all states

## The function called rankall takes two arguments: an outcome name (outcome) and a hospital ranking
## (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num

rankall <- function(outcome, num = "best") {
     ## Read outcome data
     ocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Coerce the rate death columns to be numeric
     ocm[, 11] <- suppressWarnings(as.numeric(ocm[, 11]))
     ocm[,17] <- suppressWarnings(as.numeric(ocm[,17]))
     ocm[,23] <- suppressWarnings(as.numeric(ocm[,23]))    
     
     ## Check that state and outcome are valid
     ## valid_states <- unique(ocm$State)
     ## if (!state %in% valid_states) { stop('invalid state') }
     
     valid_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
     if (!outcome %in% valid_outcomes) { stop('invalid outcome') }

     ## Initialize the to columns that are going to be returned as a data frame
     state <- ocm$State
     state <- sort(unique(state))     
     hospital <- rep("NA", length(state))     
     
     for (i in 1:length(state)) {
          ## Subset data to match state and outcome values. Also exludes hospitals
          ## that do not have data on particular outcome     
          sset <- NULL
          if(outcome == "heart attack"){
               sset <- subset(ocm, State==state[i] & !is.na(ocm[11]), select = c(2,7,11))          
               
          }else if(outcome == "heart failure"){
               sset <- subset(ocm, State==state[i] & !is.na(ocm[17]), select = c(2,7,17))                    
               
          }else if(outcome == "pneumonia"){
               sset <- subset(ocm, State==state[i] & !is.na(ocm[23]), select = c(2,7,23))
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
               next
          }     
          
          ## Sort outcome asc and hospital names on alphabetical order
          order_by_HospitalName_asc <- order(sset$outcome, sset$Hospital.Name) 
          sset_ordered <- sset[order_by_HospitalName_asc,]
          
          hospital[i] <- sset_ordered$Hospital.Name[k]          
     }
     
     ## For each state, find the hospital of the given rank
     ## Return a data frame with the hospital names and the
     ## (abbreviated) state name
     return(data.frame(hospital=hospital, state=state))
}
