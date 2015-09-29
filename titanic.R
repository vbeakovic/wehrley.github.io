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


# Function for extracting honorific (i.e. title) from the Name feature
getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return(data$Title)
} 

df.train$Title <- getTitle(df.train)
str(df.train$Title)
unique(df.train$Title)

options(digits = 5)
require(Hmisc)
bystats(df.train$Age, df.train$Title, fun = function(x) c(Mean = mean(x), Median = median(x)))

# list of honorific titles with missing Age value(s) requiring imputation
title.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")

# Impute median per honorific title
imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[which(filter.var == v)] <- impute(impute.var[which(filter.var == v)])
  }
  return(impute.var)
}

df.train$Age[which(df.train$Title == "Dr")]

df.train$Age <- imputeMedian(df.train$Age, df.train$Title, title.na.train)

df.train$Age[which(df.train$Title == "Dr")]
options(digits = 5)
summary(df.train$Age)
summary(df.train$Embarked)
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'

summary(df.train$Fare)

subset(df.train, Fare < 7)[order(subset(df.train, Fare < 7)$Fare, subset(df.train, Fare < 7)$Pclass), c("Age", "Title", "Pclass", "Fare")]
df.train$Fare[which(df.train$Fare == 0)] <- NA

df.train$Fare <- imputeMedian(df.train$Fare, df.train$Pclass, as.numeric(levels(df.train$Pclass)))

df.train$Title <- factor(df.train$Title, c("Capt","Col","Major","Sir","Lady","Rev","Dr","Don","Jonkheer","the Countess","Mrs","Ms","Mr","Mme","Mlle","Miss","Master"))

age.tile.boxplot <- ggplot(df.train, aes(x = Title, y = Age)) + labs(x = "Title", y = "Age") + labs(title = "Passenger Age by Title") + stat_boxplot(geom ='errorbar', coef = 1.5) + geom_boxplot()
age.tile.boxplot

# Function for assigning a new title value to old title(s)
levels(df.train$Title)[18] <- "Noble"
changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[which(data$Title == honorific)] <- new.title
  }
  return(data$Title)
}

# Title consolidation
df.train$Title <- changeTitles(df.train, c("Capt", "Col", "Don", "Dr", "Jonkheer", "Lady", "Major", "Rev", "Sir"), "Noble")
df.train$Title <- changeTitles(df.train, c("the Countess", "Ms"), "Mrs")
df.train$Title <- changeTitles(df.train, c("Mlle", "Mme"), "Miss")
df.train$Title <- as.factor(df.train$Title)
levels(df.train$Title)

# Feature engineering, Title already done
require(plyr)
require(stringr)
# test a character as an EVEN single digit
isEven <- function(x) x %in% c("0", "2", "4", "6", "8")
isOdd <- function(x) x %in% c("1", "3", "5", "7", "9")

# function to add features to training or test data frames
featureEngrg <- function(data) {
  data$Fate <- data$Survived
  data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))

  data$Boat.dibs <- "No"
  data$Boat.dibs[which(data$Sex == "female" | data$Age < 15)] <- "Yes"
  data$Boat.dibs <- as.factor(data$Boat.dibs)
  
  data$Family <- data$SibSp + data$Parch
  
  data$Fare.pp <- data$Fare / (data$Family + 1)
  
  data$Class <- data$Pclass
  data$Class <- revalue(data$Class, c("1" = "First", "2" = "Second", "3" = "Third"))
  
  data$Deck <- substring(data$Cabin, 1, 1)
  data$Deck[which(is.na(data$Deck))] <- "UNK"
  data$Deck <- as.factor(data$Deck)
  
  data$cabin.last.digit <- str_sub(data$Cabin, -1)
  data$Side <- "UNK"
  data$Side[which(isEven(data$cabin.last.digit))] <- "port"
  data$Side[which(isOdd(data$cabin.last.digit))] <- "starboard"
  data$Side <- as.factor(data$Side)
  data$cabin.last.digit <- NULL
  return(data)
}


df.rm <- featureEngrg(df.train)
names(df.rm)
str(df.rm)
summary(df.rm)

train.keeps <- c("Fate", "Sex", "Boat.dibs", "Age", "Title", "Class", "Deck", "Side", "Fare", "Fare.pp", "Embarked", "Family")
df.train.munged <- df.rm[train.keeps]

require(caret)
set.seed(23)
training.rows <- createDataPartition(df.train$Survived, p = 0.8, list = FALSE)
train.batch <- df.train.munged[training.rows,]
test.batch <- df.train.munged[-training.rows,]
essio
Titanic.logit.1 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare, data = train.batch, family=binomial("logit"))
Titanic.logit.1
anova(Titanic.logit.1, test = "Chisq")

Titanic.logit.2 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare.pp, data = train.batch, family=binomial("logit"))
anova(Titanic.logit.2, test = "Chisq")

Titanic.logit.3 <- glm(Fate ~ Sex + Class + Age + Family + Embarked, data = train.batch, family=binomial("logit"))
anova(Titanic.logit.3, test = "Chisq")

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3, summaryFunction = twoClassSummary, classProbs = TRUE)
set.seed(35)
glm.tune1 <- train(Fate ~ Sex + Class + Age + Family + Embarked, data = train.batch, method = "glm", metric = "ROC", trControl = cv.ctrl)
glm.tune1
summary(glm.tune1)


set.seed(35)
glm.tune2 <- train(Fate ~ Sex + Class + Age + Family + I(Embarked == "S"), data = train.batch, method = "glm", metric = "ROC", trControl = cv.ctrl)
summary(glm.tune2)

train.batch$Title <- droplevels(train.batch$Title)
levels(train.batch$Title)
glm.tune3 <- train(Fate ~ Sex + Class + Title + Age + Family + I(Embarked == "S"), data = train.batch, method = "glm", metric = "ROC", trControl = cv.ctrl)
summary(glm.tune3)

glm.tune4 <- train(Fate ~ Class + I(Title == "Mr") + I(Title == "Noble") + Age + Family + I(Embarked == "S"), data = train.batch, method = "glm", metric = "ROC", trControl = cv.ctrl)
summary(glm.tune4)

set.seed(35)
glm.tune5 <- train(Fate ~ Class + I(Title == "Mr") + I(Title == "Noble") + Age + Family + I(Embarked == "S") + I(Title == "Mr" & Class == "Third"), data = train.batch, method = "glm", metric = "ROC", trControl = cv.ctrl)
summary(glm.tune5)


# Boosting
ada.grid <- expand.grid(.iter = c(50, 100), .maxdepth = c(4, 8), .nu = c(0.1, 1))
set.seed(35)
ada.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, data = train.batch, method = "ada", metric = "ROC", tuneGrid = ada.grid, trControl = cv.ctrl)
ada.tune

# Random Forest
rf.grid <- data.frame(.mtry = c(2, 3))
set.seed(35)
rf.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, data = train.batch, method = "rf", metric = "ROC", tuneGrid = rf.grid, trControl = cv.ctrl)
rf.tune

# Support vector machine
svm.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, data = train.batch, method = "svmRadial", tuneLength = 9, preProcess = c("center", "scale"), metric = "ROC", trControl = cv.ctrl)
svm.tune

test.batch$Title <- droplevels(test.batch$Title)
levels(test.batch$Title)
glm.pred <- predict(glm.tune5, test.batch)
confusionMatrix(glm.pred, test.batch$Fate)

ada.pred <- predict(ada.tune, test.batch)
confusionMatrix(ada.pred, test.batch$Fate)

rf.pred <- predict(rf.tune, test.batch)
confusionMatrix(rf.pred, test.batch$Fate)

# ROC curves
require(pROC)
glm.probs <- predict(glm.tune5, test.batch, type = "prob")
glm.ROC <- roc(response = test.batch$Fate, predictor = glm.probs$Survived, levels = levels(test.batch$Fate)) 
plot(glm.ROC, type = "S")

ada.probs <- predict(ada.tune, test.batch, type = "prob")
ada.ROC <- roc(response = test.batch$Fate, predictor = ada.probs$Survived, levels = levels(test.batch$Fate)) 
plot(ada.ROC, add = TRUE, col = "green")

rf.probs <- predict(rf.tune, test.batch, type = "prob")
rf.ROC <- roc(response = test.batch$Fate, predictor = rf.probs$Survived, levels = levels(test.batch$Fate)) 
plot(ada.ROC, add = TRUE, col = "red")

svm.probs <- predict(svm.tune, test.batch, type = "prob")
svm.ROC <- roc(response = test.batch$Fate, predictor = svm.probs$Survived, levels = levels(test.batch$Fate)) 
plot(svm.ROC, add = TRUE, col = "blue")


cv.values <- resamples(list(Logit = glm.tune5, Ada = ada.tune, RF = rf.tune, SVM = svm.tune))
dotplot(cv.values, metric = "ROC")


## Preparation of Kaggle submittal
df.infer$Title <- getTitle(df.infer)


## Impute missing Age values
df.infer$Title <- changeTitles(df.infer, c("Dona", "Ms"), "Mrs")
titles.na.test <- c("Master", "Mrs", "Miss", "Mr")
df.infer$Age <- imputeMedian(df.infer$Age, df.infer$Title, titles.na.test)

## Consolidate titles
df.infer$Title <- changeTitles(df.infer, c("Col", "Dr", "Rev"), "Noble")
df.infer$Title <- changeTitles(df.infer, c("Mlle", "Mme"), "Miss")
df.infer$Title <- as.factor(df.infer$Title)
levels(df.infer$Title)


## Impute missing fares
df.infer$Fare[which(df.infer$Fare == 0)] <- NA
df.infer$Fare <- imputeMedian(df.infer$Fare, df.infer$Pclass, as.numeric(levels(df.infer$Pclass)))

# add the other features
df.infer <- featureEngrg(df.infer)
str(df.infer)

## data prepped for casting predictions
test.keeps <- train.keeps[-1]
pred.these <- df.infer[test.keeps]

## use the logistic regression model to generate predictions
Survived <- predict(ada.pred, newdata = pred.these)

## reformat predictions to 0 or 1 and link to PassengerId in data frame
Survived <- revalue(Survived, c("Survived" = 1, "Perished" = 0))
predictions <- as.data.frame(Survived)
predictions$PassengerId <- df.infer$PassengerId

# write predictions to csv file for submission to Kaggle
write.csv(predictions[, c("PassengerId", "Survived")], file = "Titanic_predictions.csv", row.names = FALSE, quote = FALSE)
