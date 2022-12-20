#LOGISTIC REGRESSION LECTURE
df.train <- read.csv('titanic_train.csv')
print(head(df.train))
print('      ')
str(df.train)

#Missing map
library("Amelia")
missmap(df.train, main = 'Missing Map', col = c('yellow','black'), legend = FALSE)

library("ggplot2")

#Plots for understanding the data
ggplot(df.train, aes(Survived)) + geom_bar()
ggplot(df.train, aes(Pclass)) + geom_bar(aes(fill = factor(Pclass)))
ggplot(df.train, aes(Sex)) + geom_bar(aes(fill = factor(Sex)))
ggplot(df.train, aes(Age)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
ggplot(df.train, aes(SibSp)) + geom_bar()
ggplot(df.train, aes(Fare)) + geom_histogram(fill = 'green', color = 'black', alpha = 0.5)

#Plots for Age groups by class
pl <- ggplot(df.train, aes(Pclass, Age))
pl <- pl + geom_boxplot(aes(group=Pclass, fill=factor
      (Pclass), alpha=0.4))
pl + scale_y_continuous(breaks = seq(min(0),max(80),by=2)) + theme_bw()

#Imputation of Age based on Class

impute_age <- function(age,class){
  out <- age
  for(i in 1:length(age)){
    if(is.na(age[i])){
      if(class[i] == 1){
        out[i] <- 37
      }else if (class[i] == 2){
        out[i] <- 29
      }else{
        out[i] <- 24
      }
    }else{
      out[i] <- age[i]
    }
  }
  return(out)
}

fixed.ages <- impute_age(df.train$Age, df.train$Pclass)

df.train$Age <- fixed.ages

print(missmap(df.train, main = 'Imputation Check', col = c('yellow','black'), 
              legend = FALSE))

#Removal of irrelevant variables

library(dplyr)
df.train <- select(df.train,-PassengerId,-Name,-Ticket,-Cabin)
head(df.train)

str(df.train)

#Converting Integer datatypes to Factors

df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)

str(df.train)

#Creating the Logistic Regression Model

log.model <- glm(Survived ~ . , family = binomial(link = 'logit'), 
                 data = df.train)


library(caTools)
set.seed(101)
split <- sample.split(df.train$Survived, SplitRatio = 0.7)
final.train <- subset(df.train, split == TRUE)
final.test <- subset(df.train, split == FALSE)
final.log.model <- glm(Survived ~ .,family = binomial(link='logit'), data = final.train)
summary(final.log.model)

#Prediction

fitted.probabilities <- predict(final.log.model, final.test, type = 'response')
fitted.results <- ifelse(fitted.probabilities>0.5,1,0)
misClassError <- mean(fitted.results != final.test$Survived)
print(1 - misClassError)

#Confusion Matrix
table(final.test$Survived, fitted.probabilities>0.5)
