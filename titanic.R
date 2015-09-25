# Read csv function
readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv(url(paste(path.name, file.name, sep = "")), colClasses = column.types, na.strings = missing.types)
}

titanic.path <- "https://github.com/vbeakovic/wehrley.github.io/blob/master/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',     # PassengerId
                        'factor',      # Survived
                        'factor',      # Pclass
                        'character',   # Name
                        'factor',      # Sex
                        'numeric',     # Age
                        'integer',     # SibSp
                        'integer',     # Parch
                        'character',   # Ticket
                        'numeric',     # Fare
                        'character',   # Cabin
                        'factor'       # Embarked
                        )
test.column.types <- train.column.types[-2] # no survived column in test.csv)

train.raw <- readData(titanic.path, train.data.file, train.column.types, missing.types)
df.train <- train.raw
