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


require(ggplot2)
## Exploratory data analysis but using ggplot2
age.barplot <- ggplot(df.train, aes(x = Survived)) + geom_bar(stat = "bin", fill = "black", colour = "black" ) + ggtitle("Survived (passenger fate)") + scale_y_continuous("") + scale_x_discrete("", labels = c("Perished", "Survived"))
age.barplot

pclass.barplot <- ggplot(df.train, aes(x = Pclass)) + geom_bar(stat = "bin", fill = "firebrick", colour = "black" ) + ggtitle("Pclass (passenger traveling class)") + scale_y_continuous("") + scale_x_discrete("", labels = c("first", "second", "third")) 
pclass.barplot

sex.barplot <- ggplot(df.train, aes(x = Sex)) + geom_bar(stat = "bin", fill = "darkviolet", colour = "black" ) + ggtitle("Sex (gender)") + scale_y_continuous("") + scale_x_discrete("", labels = c("female", "male")) 
sex.barplot

age.hist <- ggplot(df.train, aes(x = Age)) + geom_histogram(breaks = seq(0, 80, by = 10), fill = "brown", colour = "black") + labs(x = "", y = "Frequency") + labs(title = "Age")
age.hist

sibsp.barplot <- ggplot(df.train, aes(x = as.factor(SibSp))) + geom_bar(stat = "bin", fill = "darkblue", colour = "black" ) + labs(title = "SibSp (siblings + spouse aboard)") + labs(x = "", y = "")  
sibsp.barplot

parch.barplot <- ggplot(df.train, aes(x = as.factor(Parch))) + geom_bar(stat = "bin", fill = "gray50", colour = "black" ) + labs(title = "Parch (parents + kids aboard)") + labs(x = "", y = "")  
parch.barplot

fare.hist <- ggplot(df.train, aes(x = Fare)) + geom_histogram(breaks = seq(0, 500, by = 50), fill = "darkgreen", colour = "black") + labs(x = "", y = "Frequency") + labs(title = "Fare (fee paid for ticket[s])")
fare.hist

embarked.barplot <- ggplot(df.train[!is.na(df.train$Embarked), ], aes(x = Embarked)) + geom_bar(stat = "bin", fill = "sienna", colour = "black" ) + ggtitle("Embarked (port of embarkation)") + scale_y_continuous("") + scale_x_discrete("", labels = c("Cherbourg", "Queenstown", "Southampton")) 
embarked.barplot

require(vcd)
# Survival rate by passenger class
mosaicplot(df.train$Pclass ~ df.train$Survived, main = "Passenger Fate by Traveling Class", shade = FALSE, color = TRUE, xlab = "Pclass", ylab = "Survived")

# Survival rate by gender
mosaicplot(df.train$Sex ~ df.train$Survived, main = "Passenger Fate by Gender", shade = FALSE, color = TRUE, xlab = "Sex", ylab = "Survived")

# Passenger fate by age
age.boxplot <- ggplot(df.train, aes(x = Survived, y = Age)) + labs(x = "Survived", y = "Age") + labs(title = "Passenger Fate by Age") + stat_boxplot(geom ='errorbar', coef = 1.5) + geom_boxplot()
age.boxplot

# Passenger Fate by Port of Embarkation
mosaicplot(df.train$Embarked ~ df.train$Survived, main = "Passenger Fate by Port of Embarkation", shade = FALSE, color = TRUE, xlab = "Embarked", ylab = "Survived")

require(corrgram)
require(plyr)
corrgram.data <- df.train
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, c("C" = 1, "Q" = 2, "S" = 3))
corrgram.vars <- c("Survived", "Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[ , corrgram.vars], order = FALSE, lower.panel = panel.ellipse, upper.panel = panel.pie, text.panel = panel.txt, main = "Titanic Training Data")


# Imputation
summary(df.train$Age)
names(df.train)

class.age.boxplot <- ggplot(df.train, aes(x = Pclass, y = Age)) + labs(x = "Pclass", y = "Age") + labs(title = "Passenger Traveling Class by Age") + stat_boxplot(geom ='errorbar', coef = 1.5) + geom_boxplot()
class.age.boxplot
head(df.train$Name, n = 10L)
