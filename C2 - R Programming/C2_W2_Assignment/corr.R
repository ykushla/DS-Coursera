## https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

source("complete.R")

corr <- function (directory, threshold = 0) {
	complete_cases <- complete(directory)
	threshold_cases <- complete_cases[complete_cases$nobs > threshold, ]
	
	data <- vector(mode = "numeric", length = 0)
	
	for (i in threshold_cases$id) {
		file_name <- file.path(directory, 
				       paste(sprintf("%03d", i), ".csv", 
				             sep = ""))
		
		matter_data <- read.csv(file_name, header = TRUE)
		filtered_data <- subset.data.frame(matter_data, !is.na(Date) & 
						   	!is.na(sulfate) &
						   	!is.na(nitrate) &
						   	!is.na(ID))
		
		data <- c(data, cor(filtered_data$sulfate, filtered_data$nitrate))
	}
	
	data
}