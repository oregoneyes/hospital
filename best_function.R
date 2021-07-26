# Setting directory and loading tidyverse ---

library(tidyverse)


# create 'best function' that takes the state's abbreviation and an outcome and returns
# a character vector with the hospital name with the best 30 day mortality for that
# outcome.

# hosptial name - outcome$hospital.Name
# state - state abb.
# outcome - outcome of interest ('heart attack', 'heart failure', 'pneumonia')

best <- function(state, outcome){
    
    setwd("C:/Users/Russell/projects/data/hospital/")
    
    # Read data in from outcomes file into outcomes variable
    
    outcomes <- read.csv('outcome-of-care-measures.csv')
    
    #subset outcome data and return dataframe with state, hospital name, and outcomes variables
    
    reduce_df <- outcomes %>% select(State, Hospital.Name,
                 Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                 Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                 Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    
    # rename columns to make dataframe reduce_df more readable
    
    reduce_df <- reduce_df %>% rename(st = State,
                 name = Hospital.Name,
                 ha = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                 hf = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                 pneumonia = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    
    # filter reduce_df by state and store in raw_state_data dataframe
    
    raw_state_data <- filter(reduce_df, st == state)
    
    # reduce raw_state_data df to include on outcome of interest - ha, hf, pneumonia
    
    raw_state_outcome <- raw_state_data %>% select(st, name, outcome)
    
    # remove rows where outcome data is 'Not Available'
    
    raw_state_outcome <- filter(raw_state_outcome, raw_state_outcome[,3] != 'Not Available')
    
    # sort raw_state_outcome by outcome as.numeric and then by hospital name
    
    raw_state_outcome <- arrange(raw_state_outcome, as.numeric(raw_state_outcome[,3]), name)
    
    # return hospital name and hospital mortality information
    
    ans <- c(raw_state_outcome[1,2], raw_state_outcome[1,3])
    
   
}

ans <- best('MT', 'pneumonia')