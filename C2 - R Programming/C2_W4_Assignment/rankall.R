## Running the depending functions
source("rankhospital.R")


## Returns a 2-column data frame containing the hospital in each state that has the 
## ranking specified in num.

rankall <- function(outcome, num = "best") {
	
	## Check that input parameters are valid

	outcome_codes <- c("heart attack" = "Heart.Attack", "heart failure" = "Heart.Failure", "pneumonia" = "Pneumonia")
	if (!(outcome %in% names(outcome_codes))) {
		stop("invalid outcome")
	}
	
	if((num != "best") && (num != "worst") && (!check.integer(num))) {
		stop("invalid num parameter")
	}
	
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	## For each state, find the hospital of the given rank return a data frame with the hospital 
	## names and the (abbreviated) state name
	
	outcome_code_full <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", 
				   outcome_codes[outcome], sep = "")
	
	sub_data <- data[, c("Hospital.Name", "State", outcome_code_full)]
	names(sub_data) <- c("Hospital", "State", "Rank")
	sub_data <- sub_data[sub_data$Rank != "Not Available", ]
	sub_data$Rank <- as.numeric(sub_data$Rank)
	
	#return(sub_data)
	
	result <- NULL
	unique_states <- unique(data$State)
	split_data <- split(sub_data, sub_data$State)
	for (state in unique_states) {
		state_data <- split_data[[state]]
		hospital <- rank_by_state(state_data, num)$Hospital
		result <- c(result, hospital)
	}
	result <- data.frame(result, unique_states)
	names(result) <- c("hospital", "state")
	result[order(result$state), ]
}


## Helper function for sorting by State

rank_by_state <- function (frame, num) {
	frame <- frame[order(frame$Rank, frame$Hospital), ]
	
	if (num == "best") {
		frame[1, ]
	}
	else if (num == "worst") {
		frame[nrow(frame), ]
	} else {
		frame[num, ]
	}
}
