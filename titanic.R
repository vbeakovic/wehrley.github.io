# Read csv function
readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv(url(paste(path.name, file.name, sep = "")), colClasses = column.types, na.strings = missing.types)
}

titanic.path <- "https://raw.github.com/vbeakovic/wehrley.github.io/master/"
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

# Read in data from GitHub repo and prepare data frames
train.raw <- readData(titanic.path, train.data.file, train.column.types, missing.types)
df.train <- train.raw
test.raw <- readData(titanic.path, test.data.file, test.column.types, missing.types)
df.infer <- test.raw

## exploring missing data
require(Amelia)
missmap(df.train, main = "Titanic Training Data - Missing Map", col = c("yellow", "black"), legend = FALSE)


## Exploratory data analysis but using ggplot2
age.barplot <- ggplot(df.train, aes(x = Survived)) + geom_bar(stat = "bin", fill = "black", colour = "black" ) + ggtitle("Survived (passenger fate)") + scale_y_continuous("") + scale_x_discrete("", labels = c("Perished", "Survived"))
age.barplot

