## Running the depending functions
source("rankhospital.R")


## Returns a character vector with the name of the hospital that has the best (i.e. lowest) 
## 30-day mortality for the specified outcome in that state.

best2 <- function(state, outcome) {
	rankhospital(state, outcome, num = "best")
}



## old implementation of best

best_old <- function(state, outcome) {
	## Check that state and outcome are valid
	##states <- unique(data$State)
	if (!(state %in% state.abb)) {
		stop("invalid state")
	}

	outcome_codes <- c("heart attack" = "Heart.Attack", "heart failure" = "Heart.Failure", "pneumonia" = "Pneumonia")
	if (!(outcome %in% names(outcome_codes))) {
		stop("invalid outcome")
	}

	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	## Return hospital name in that state with lowest 30-day death rate
	outcome_code_full <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", 
				   outcome_codes[outcome], sep = "")
	#message(outcome_code_full)
				   
	sub_data <- data[data$State == state, c("Hospital.Name", outcome_code_full)]
	sub_data <- sub_data[sub_data[, 2] != "Not Available", ]
	sub_data[, 2] <- as.numeric(sub_data[, 2])
	sub_data_sorted <- sub_data[order(sub_data[2], sub_data[1]), ]
	sub_data_sorted[1, 1]
}
