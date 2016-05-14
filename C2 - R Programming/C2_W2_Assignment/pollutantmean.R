## https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

pollutantmean <- function (directory, pollutant, id = 1:332) {
	data <- NULL
	
	for (i in id) {
		file_name <- file.path(directory, paste(sprintf("%03d", i), 
							".csv", sep = ""))

		matter_data <- read.csv(file_name, header = TRUE)
		data <- rbind(data, matter_data)
	}
	
	mean(data[[pollutant]], na.rm = TRUE)
}
