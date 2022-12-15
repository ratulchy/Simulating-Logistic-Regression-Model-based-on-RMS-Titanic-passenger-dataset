
library('tidyverse')
#Read CSV file 

loans.raw <- read.csv('Loan_Data.csv', header = TRUE, sep = ",", na.strings = c(""))
print(loans.raw)
print('      ')
str(loans.raw)

#Removing observations with missing variables

library("Amelia")
missmap(loans.raw, main = 'Missing Map', col = c('yellow','black'), legend = FALSE)
loans <- na.omit(loans.raw)
missmap(loans, main = 'Missing Map', col = c('yellow','black'), legend = FALSE)

str(loans)

#Converting Integer datatypes to Factors

loans$Gender <- factor(loans$Gender)
loans$Married <- factor(loans$Married)
loans$Education <- factor(loans$Education)
loans$Loan_Status <- factor(loans$Loan_Status)
str(loans)


#Removing irrelevant variables for our logistic regression models

library(dplyr)
loans <- select(loans,-Loan_ID)

str(loans)

#figuring out which variables are significant
Mlogsig <- glm(Loan_Status ~ . , family = binomial, 
               data = loans)
summary(Mlogsig)

#Creating the Logistic Regression Model

Mlog1 <- glm(Loan_Status ~ Married + 
               Credit_History + Property_Area, family = binomial, 
                 data = loans)
summary(Mlog1)

Mlog2 <- glm(Loan_Status ~ Married + 
               Credit_History + Property_Area + Married * Credit_History, family = binomial, 
             data = loans)
summary(Mlog2)

Mlog3 <- glm(Loan_Status ~ Married + 
               Credit_History + Property_Area + I(Married * Property_Area), family = binomial, 
             data = loans)
summary(Mlog3)

Mlog4 <- glm(Loan_Status ~ Married + 
               Credit_History + Property_Area + Credit_History * Property_Area , family = binomial, 
             data = loans)
summary(Mlog4) 

Mlog5 <- glm(Loan_Status ~ Married + 
               Credit_History + Property_Area + I(Credit_History)^2 , family = binomial, 
             data = loans)
summary(Mlog5)  
