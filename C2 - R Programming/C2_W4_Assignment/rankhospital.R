## Returns a character vector with the name of the hospital that has the ranking specified 
## by the num argument.

rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	## Check that input parameters are valid
	if (!(state %in% unique(data$State))) {
		stop("invalid state")
	}
	
	outcome_codes <- c("heart attack" = "Heart.Attack", "heart failure" = "Heart.Failure", "pneumonia" = "Pneumonia")
	if (!(outcome %in% names(outcome_codes))) {
		stop("invalid outcome")
	}
	
	if((num != "best") && (num != "worst") && (!check.integer(num))) {
		stop("invalid num parameter")
	}
	
	## Return hospital name in that state with the given rank
	## 30-day death rate

	outcome_code_full <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", 
				   outcome_codes[outcome], sep = "")

	sub_data <- data[data$State == state, c("Hospital.Name", outcome_code_full)]
	names(sub_data) <- c("Hospital", "Rank")
	sub_data <- sub_data[sub_data$Rank != "Not Available", ]
	sub_data$Rank <- as.numeric(sub_data$Rank)
	sub_data_sorted <- sub_data[order(sub_data$Rank, sub_data$Hospital), ]
	
	if (num == "best") {
		sub_data_sorted$Hospital[1]
	}
	else if (num == "worst") {
		sub_data_sorted$Hospital[nrow(sub_data_sorted)]
	} else {
		sub_data_sorted$Hospital[num]
	}
}


## Helper function to check if a number is integer

check.integer <- function(x) {
	x == round(x)
}