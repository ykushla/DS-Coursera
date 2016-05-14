## https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

complete <- function (directory, id = 1:332) {
	data <- data.frame()

	for (i in id) {
		file_name <- file.path(directory, paste(sprintf("%03d", i), 
							".csv", sep = ""))
		
		matter_data <- read.csv(file_name, header = TRUE)
		filtered_data <- subset.data.frame(matter_data, !is.na(Date) & 
						   	!is.na(sulfate) &
						   	!is.na(nitrate) &
						   	!is.na(ID))

		data <- rbind(data, c(i, nrow(filtered_data)))
	}
	
	names(data) <- c("id", "nobs")
	data
}