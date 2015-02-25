## Coursera Course Title: R Programming
## Coursera Course ID: rprog-011
## Programming Assignment 3: Hospital Quality
## Student: Ruben Leon

## Assigment Description:   Finding the best hospital in a state

## The function called best takes two arguments: the 2-character abbreviated name of 
## a state and an outcome name. The function reads the outcome-of-care-measures.csv file
## and returns a character vector with the name of the hospital that has the 
## best (i.e. lowest) 30-day mortality for the specified outcome in that state.

best <- function(state, outcome) {
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
     
     ## Sort hospital names on alphabetical order
     order_by_HospitalName_asc <- order(sset$Hospital.Name) 
     sset_ordered <- sset[order_by_HospitalName_asc,]
     
     ## Find minimum outcome value
     outcome_min <- min(sset_ordered$outcome)
     
     ## Return hospital name in that state with lowest 30-day death
     ## rate     
     return (sset_ordered[sset_ordered$outcome==outcome_min,]$Hospital.Name[1])
}