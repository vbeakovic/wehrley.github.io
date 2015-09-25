# Read csv function
readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv(url(paste(path.name, file.name, sep = "")), colClasses = column.types, na.strings = missing.types)
}