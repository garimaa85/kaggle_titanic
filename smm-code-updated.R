## Reading the train.csv

train.raw <- read.csv("/Users/tusharporwal/kaggle_titanic/train.csv", header= TRUE, colClasses = c("integer", "factor", "factor",    "character", "factor", "numeric",  "integer",   "integer", "character", "numeric",   "character", "factor"), na.strings= c("NA", ""))

## Reading the test.csv

test.raw<- read.csv("/Users/tusharporwal/kaggle_titanic/test.csv", header= TRUE, colClasses = c("integer", "factor",    "character", "factor", "numeric",  "integer",   "integer", "character", "numeric",   "character", "factor"), na.strings= c("NA", ""))

df.train<- train.raw
df.test<- test.raw
Library(Amelia)
require(Amelia)

## map missing data by provided feature

missmap(df.train, main="Titanic Training Data - Missings Map", 
+         col=c("yellow", "black"), legend=FALSE)

## Visualizing Data for Better Understanding
barplot(table(df.train$Survived),
names.arg = c("Perished", "Survived"),main="Survived (passenger fate)", col="red")
barplot(table(df.train$Pclass), 
names.arg = c("first", "second", "third"),main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(df.train$Sex), main="Sex (gender)", col="green")
hist(df.train$Age, main="Age", xlab = NULL, col="orange")
barplot(table(df.train$SibSp), main="SibSp (siblings + spouse aboard)", col="darkblue")
barplot(table(df.train$Parch), main="Parch (parents + kids aboard)", col="purple")
 hist(df.train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, col="brown")
 barplot(table(df.train$Embarked), names.arg = c("Cherbourg", "Queenstown", "Southampton"),main="Embarked (port of embarkation)", col="blue")
 ## more passengers perished than survived
 ##about twice as many passengers in 3rd class than in either 1st or 2nd
 ##male passengers far outnumbered females

> barplot(table(df.train$Embarked), 
+         names.arg = c("Cherbourg", "Queenstown", "Southampton"),
+         main="Embarked (port of embarkation)", col="blue")
 boxplot(df.train$Age ~ df.train$Survived, 
+         main="Passenger Fate by Age",
+         xlab="Survived", ylab="Age")
library(stringr)
library(plyr)
names(df.train) 
 df.train$Name <- as.character(df.train$Name)
 df.train$Name[1]
strsplit(df.train$Name[1], split='[,.]')[[1]]
strsplit(df.train$Name[1], split='[,.]')[[1]]
 strsplit(df.train$Name[1], split='[,.]')[[1]][2]
 df.train$Title <- sapply(df.train$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
 df.train$Title <- sub(' ', '', df.train$Title)
table(df.train$Title)
## Finding titles with atleast one missing value 
options(digits=2)
 require(Hmisc)
 bystats(df.train$Age, df.train$Title, fun=function(x)c(Mean=mean(x),Median=median(x)))

## list of titles with Missing Values Only
titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")

## Imputing Missing Ages
 titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")
 imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
impute.var[ which( filter.var == v)] <- impute(impute.var[ 
 which( filter.var == v)])
}
return (impute.var)}
df.train$Age[which(df.train$Title=="Dr")]
df.train$Age <- imputeMedian(df.train$Age, df.train$Title, 
 titles.na.train)
df.train$Age[which(df.train$Title=="Dr")]
summary(df.train$Age)

##Replacing missing values in Embarked Variable with "S", the most commonly used value
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'

## Analyzing Fare Values
summary(df.train$Fare)
subset(df.train, Fare < 7)[order(subset(df.train, Fare < 7)$Fare, subset(df.train, Fare < 7)$Pclass), 
c("Age", "Title", "Pclass", "Fare")]
df.train$Fare[ which( df.train$Fare == 0 )] <- NA
df.train$Fare <- imputeMedian(df.train$Fare, df.train$Pclass, 
as.numeric(levels(df.train$Pclass)))

## Feature Engineering

df.train$Title <- factor(df.train$Title, c("Capt","Col","Major","Sir","Lady","Rev",
"Dr","Don","Jonkheer","the Countess","Mrs", "Ms","Mr","Mme","Mlle","Miss","Master"))

changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}

df.train$Title <- changeTitles(df.train, c("Capt", "Dr", "Sir", "Major", "Col","Lady", "Rev"),  "Sir")
df.train$Title <- changeTitles(df.train, c("Mlle", "Mme"),"Miss")
df.train$Title <- changeTitles(df.train, c("the Countess", "Ms"), 
"Mrs")
 df.train$Title <- as.factor(df.train$Title)
 summary(df.train)

require(plyr) 
require(stringr) 
isEven <- function(x) x %in% c("0","2","4","6","8")
isOdd <- function(x) x %in% c("1","3","5","7","9")
featureEngrg <- function(data) {
		
df.train$survived1 <- revalue(df.train$survived1, c("1" = "Survived", "0" = "Perished")) 
data$Class <- revalue(data$Class, c("1"="First", "2"="Second","3"="Third"))
df.train$Family<- df.train$SibSp + df.train$Parch + 1
df.train$Boat.pref <- "No"
df.train$Boat.pref[which(df.train$Sex == "female" | df.train$Age  <15)] <- "Yes"
df.train$Boat.pref <- as.factor(df.train$Boat.pref)
df.train$FFare<- df.train$Fare/ (df.train$Family)
## Cabin Marking
df.train$Deck <- substring(df.train$Cabin, 1, 1)
> df.train$Deck[ which( is.na(df.train$Deck ))] <- "UNK"
> df.train$Deck <- as.factor(df.train$Deck)

## Assigning Sides: Port and Starboard
df.train$cabin.last.digit <- str_sub(df.train$Cabin, -1)
> df.train$Side <- "UNK"
> summary(df.train)
df.train$Side[which(isEven(df.train$cabin.last.digit))] <- "port"
> df.train$Side[which(isOdd(df.train$cabin.last.digit))] <- "starboard"
> df.train$Side <- as.factor(df.train$Side)
> df.train$cabin.last.digit <- NULL
return(data)
}
df.train <- featureEngrg(df.train)
> summary(df.train)

## New Features: Boat.pref, Deck, Side, FFare

train.final <- c("Survived", "Sex", "Boat.pref", "Age", "Title", "Pclass", "Deck", "Side", "Fare", "FFare", "Embarked", "Family")
df.train.clean <- df.train[train.final]
summary(df.train.clean)

## We now have a clean data to fit a model 

##70/30 split for training data and testing data.
library(caret)
library(pROC)
set.seed(23) ## random number generator to ensure eandom sampling
 training.rows <- createDataPartition(df.train.clean$Survived, p = 0.7, list = FALSE)
train.set <- df.train.clean[training.rows, ]
test.set <- df.train.clean[-training.rows, ]


## Model Fitting: Logistic Regression(For Comparing)
## Assesing the first Model:Using chi-square statistic "goodness of fit"null deviance shows how well passenger survival is predicted by a "null" model

logitfit1 <- glm(Survived ~ Sex + Pclass + Age + Family + Embarked + Fare, data = train.set, family=binomial("logit"))
logitfit1

## Deviance reduced by (832-541)= 291 points on 8 degrees of freedom


## Using anova to analyse model better
anova(logitfit1, test= "Chisq")
##Conclusion: Sex and Class features are very significant, and therefore support our hypotheses


## Testing: dropping, adding variables
glm(Survived ~ Sex + Pclass + Age + Family + Embarked, 
data = train.set, family=binomial("logit"))

library(caret)
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
summaryFunction = twoClassSummary,
 classProbs = TRUE)
> set.seed(35)
> glm.tune.1 <- train(Survived ~ Sex + Pclass + Age + Family + Embarked, data = train.set,
method = "glm",
metric = "ROC",
 trControl = cv.ctrl)
summary(glm.tune.1)

## Creating Random Forest on randomly pre-selected predictor variables for each node, designated mtry
##Since,no. of features are very small, there really isn't much scope for tuning mtry in this case. Inorder to improve efficiency mtree . By default square root of the total number of avaliable variables is taken. We have used mtry=2 here

rf.tune <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Family+ Embarked , data=train.set,ntree=5000, importance=TRUE)
importance(rf.tune)

## Conditional Tree
rf.ctune <- ctree(as.factor(Survived) ~ Pclass + Sex + Age + Family+ Embarked , data=train.set)
rf.ctune 
plot(rf.ctune)

## Random Forest Model Evaluation
rf.pred <- predict(rf.tune, test.set)
confusionMatrix(rf.pred, test.set$Survived)

