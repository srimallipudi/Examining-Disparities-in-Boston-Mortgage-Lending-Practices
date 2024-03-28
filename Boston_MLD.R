library(vtable)
library(tidyverse)
library(margins)
library(stargazer)
library(dplyr)
library(broom)
install.packages("caret")
library(caret)

#Remove objects (data) from your workspace
rm(list=ls(all=TRUE))

#Set working directory by clicking on Session --> Set Working Directory --> To Source File Location
MLD <- read.csv("MLD Data File.csv", header=TRUE)  # import data

#Print variable names on the screen
colnames(MLD)

#Generate Descriptive Statistics
vtable(MLD, lush = TRUE) #take a careful look -- there are some problems
sumtable(MLD)

#Impose appropriate sample selection criteria here
MLDsubsample <- subset(MLD, LOANPRC <= 1 & MALE != "." & MARRIED != "." & (GDLIN == 1 | GDLIN == 0))
sumtable(MLDsubsample)
summary(MLDsubsample)
MLDsubsample$MALE <- as.numeric(MLDsubsample$MALE)
MLDsubsample$MARRIED <- as.numeric(MLDsubsample$MARRIED)

# Generating Correlation matrix
married <- MLDsubsample[,"MARRIED"]
guidelines <- MLDsubsample[,"GDLIN"]
obligations <- MLDsubsample[,"OBRAT"]
black <- MLDsubsample[,"BLACK"]
hispanic <- MLDsubsample[,"HISPAN"]
male <- MLDsubsample[,"MALE"]
approved <- MLDsubsample[,"APPROVE"]
loan_amount <- MLDsubsample[,"LOANPRC"]

round(cor(cbind(approved, married, guidelines, obligations, black, hispanic, male, loan_amount)), 3)

# Descriptive statistics by race group
MLDsubsample_Black <- subset(MLD, LOANPRC <= 1 & MALE != "." & MARRIED != "." & BLACK == 1 & (GDLIN == 1 | GDLIN == 0))
MLDsubsample_Black$MALE <- as.numeric(MLDsubsample_Black$MALE)
MLDsubsample_Black$MARRIED <- as.numeric(MLDsubsample_Black$MARRIED)
summary(MLDsubsample_Black)

MLDsubsample_hispanic <- subset(MLD, LOANPRC <= 1 & MALE != "." & MARRIED != "." & HISPAN == 1 & (GDLIN == 1 | GDLIN == 0))
MLDsubsample_hispanic$MALE <- as.numeric(MLDsubsample_hispanic$MALE)
MLDsubsample_hispanic$MARRIED <- as.numeric(MLDsubsample_hispanic$MARRIED)
summary(MLDsubsample_hispanic)

MLDsubsample_whites <- subset(MLD, LOANPRC <= 1 & MALE != "." & MARRIED != "." & HISPAN == 0 & BLACK == 0 & (GDLIN == 1 | GDLIN == 0))
MLDsubsample_whites$MALE <- as.numeric(MLDsubsample_whites$MALE)
MLDsubsample_whites$MARRIED <- as.numeric(MLDsubsample_whites$MARRIED)
summary(MLDsubsample_whites)

#Estimate Logit Model
LogitModel = glm(APPROVE ~ OBRAT + GDLIN + LOANPRC + MALE + MARRIED + BLACK + HISPAN, data = MLDsubsample, 
                 family = "binomial")
summary(LogitModel)

#Generate Log-Likelihood
logLik(LogitModel)

LogitModel.or <- exp(coef(LogitModel))

#Generating SE for odds ratios
var.diag <- diag(vcov(LogitModel))
LogitModel.or.se <- sqrt(LogitModel.or^2 * var.diag)

# Another way to generate SE for odds ratios
model2.df <- tidy(LogitModel)
model2.df %>% 
  mutate(or = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(LogitModel)),  # Variance of each coefficient
         or.se = sqrt(or^2 * var.diag))

#Logit model with only Intercept
LogitModel_Intercept = glm(APPROVE ~ 1, data = MLDsubsample, 
                 family = binomial(link = 'logit'))
summary(LogitModel_Intercept)

logLik(LogitModel_Intercept)

#Pseudo R2 for logit model with all variables
1 - logLik(LogitModel)/logLik(LogitModel_Intercept)

LogitModel2 = glm(APPROVE ~ OBRAT + GDLIN + MARRIED + BLACK + HISPAN, data = MLDsubsample, 
                 family = "binomial")
summary(LogitModel2)
logLik(LogitModel2)

1 - logLik(LogitModel2)/logLik(LogitModel_Intercept)

LogitModel3 = glm(APPROVE ~ OBRAT + GDLIN + LOANPRC + MARRIED + BLACK + HISPAN, data = MLDsubsample, 
                  family = "binomial")
summary(LogitModel3)
logLik(LogitModel3)

#Pseudo R2 for restricted logit model 
1 - logLik(LogitModel3)/logLik(LogitModel_Intercept)

# Calculation likelihood ratio
2 * (logLik(LogitModel) - logLik(LogitModel3))

#Generate Odds Ratios
LogitModel3.or <- exp(coef(LogitModel3))

#Generating SE for odds ratios
var.diag <- diag(vcov(LogitModel3))
LogitModel3.or.se <- sqrt(LogitModel3.or^2 * var.diag)

# Another way to generate SE for odds ratios
model.df <- tidy(LogitModel3)
model.df %>% 
  mutate(or = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(LogitModel3)),  # Variance of each coefficient
         or.se = sqrt(or^2 * var.diag))

#Define prototypical loan applicants
prototype1 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 1, MARRIED = 1, BLACK = 1, HISPAN = 0)
prototype2 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 1, MARRIED = 0, BLACK = 1, HISPAN = 0)
prototype3 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 0, MARRIED = 1, BLACK = 1, HISPAN = 0)
prototype4 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 0, MARRIED = 0, BLACK = 1, HISPAN = 0)
prototype5 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 1, MARRIED = 1, BLACK = 0, HISPAN = 1)
prototype6 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 1, MARRIED = 0, BLACK = 0, HISPAN = 1)
prototype7 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 0, MARRIED = 1, BLACK = 0, HISPAN = 1)
prototype8 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 0, MARRIED = 0, BLACK = 0, HISPAN = 1)
prototype9 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 1, MARRIED = 1, BLACK = 0, HISPAN = 0)
prototype10 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 1, MARRIED = 0, BLACK = 0, HISPAN = 0)
prototype11 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 0, MARRIED = 1, BLACK = 0, HISPAN = 0)
prototype12 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 0, MARRIED = 0, BLACK = 0, HISPAN = 0)


#Predict probabilities for prototypical individuals
prototype1$predictedprob <- predict (LogitModel3, newdata = prototype1, type ="response")
prototype2$predictedprob <- predict (LogitModel3, newdata = prototype2, type ="response")
prototype3$predictedprob <- predict (LogitModel3, newdata = prototype3, type ="response")
prototype4$predictedprob <- predict (LogitModel3, newdata = prototype4, type ="response")
prototype5$predictedprob <- predict (LogitModel3, newdata = prototype5, type ="response")
prototype6$predictedprob <- predict (LogitModel3, newdata = prototype6, type ="response")
prototype7$predictedprob <- predict (LogitModel3, newdata = prototype7, type ="response")
prototype8$predictedprob <- predict (LogitModel3, newdata = prototype8, type ="response")
prototype9$predictedprob <- predict (LogitModel3, newdata = prototype9, type ="response")
prototype10$predictedprob <- predict (LogitModel3, newdata = prototype10, type ="response")
prototype11$predictedprob <- predict (LogitModel3, newdata = prototype11, type ="response")
prototype12$predictedprob <- predict (LogitModel3, newdata = prototype12, type ="response")

prototype1
prototype2
prototype3
prototype4
prototype5
prototype6
prototype7
prototype8
prototype9
prototype10
prototype11
prototype12

#Estimate Probit Model
ProbitModel = glm(APPROVE ~ OBRAT + GDLIN + LOANPRC + MARRIED + BLACK + HISPAN, data = MLDsubsample, 
                  family = "binomial" (link = "probit"))
summary(ProbitModel)
logLik(ProbitModel)

#Proit model with only Intercept
ProbitModel_Intercept = glm(APPROVE ~ 1, data = MLDsubsample, 
                           family = binomial(link = 'probit'))
summary(ProbitModel_Intercept)
logLik(ProbitModel_Intercept)

1 - logLik(ProbitModel)/logLik(LogitModel_Intercept)


#Define prototypical loan applicants - Probit model
prototype1 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 1, MARRIED = 1, BLACK = 1, HISPAN = 0)
prototype2 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 1, MARRIED = 0, BLACK = 1, HISPAN = 0)
prototype3 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 0, MARRIED = 1, BLACK = 1, HISPAN = 0)
prototype4 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 0, MARRIED = 0, BLACK = 1, HISPAN = 0)
prototype5 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 1, MARRIED = 1, BLACK = 0, HISPAN = 1)
prototype6 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 1, MARRIED = 0, BLACK = 0, HISPAN = 1)
prototype7 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 0, MARRIED = 1, BLACK = 0, HISPAN = 1)
prototype8 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 0, MARRIED = 0, BLACK = 0, HISPAN = 1)
prototype9 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 1, MARRIED = 1, BLACK = 0, HISPAN = 0)
prototype10 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 1, MARRIED = 0, BLACK = 0, HISPAN = 0)
prototype11 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 0, MARRIED = 1, BLACK = 0, HISPAN = 0)
prototype12 <- data.frame(OBRAT=mean(MLDsubsample$OBRAT), LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN = 0, MARRIED = 0, BLACK = 0, HISPAN = 0)


#Predict probabilities for prototypical individuals
prototype1$predictedprob <- predict (ProbitModel, newdata = prototype1, type ="response")
prototype2$predictedprob <- predict (ProbitModel, newdata = prototype2, type ="response")
prototype3$predictedprob <- predict (ProbitModel, newdata = prototype3, type ="response")
prototype4$predictedprob <- predict (ProbitModel, newdata = prototype4, type ="response")
prototype5$predictedprob <- predict (ProbitModel, newdata = prototype5, type ="response")
prototype6$predictedprob <- predict (ProbitModel, newdata = prototype6, type ="response")
prototype7$predictedprob <- predict (ProbitModel, newdata = prototype7, type ="response")
prototype8$predictedprob <- predict (ProbitModel, newdata = prototype8, type ="response")
prototype9$predictedprob <- predict (ProbitModel, newdata = prototype9, type ="response")
prototype10$predictedprob <- predict (ProbitModel, newdata = prototype10, type ="response")
prototype11$predictedprob <- predict (ProbitModel, newdata = prototype11, type ="response")
prototype12$predictedprob <- predict (ProbitModel, newdata = prototype12, type ="response")


prototype1
prototype2
prototype3
prototype4
prototype5
prototype6
prototype7
prototype8
prototype9
prototype10
prototype11
prototype12

#Probit - marginal effect at the mean
mean <- model.frame(ProbitModel) %>%
  map_df(mean)

Probit.atmean <- margins(ProbitModel, at = mean[-1])
summary(Probit.atmean)

#Probit - average marginal effect
Probit.AME <- margins(ProbitModel)
summary(Probit.AME)

(Probit.pred <- (fitted(ProbitModel) > 0.5) %>% as.numeric %>% as.factor) 
(actual <- MLDsubsample$APPROVE %>% as.factor)
caret::confusionMatrix(Probit.pred, actual, positive = '1')

(Logit.pred <- (fitted(LogitModel3) > 0.5) %>% as.numeric %>% as.factor) 
caret::confusionMatrix(Logit.pred, actual, positive = '1')

(Logit2.pred <- (fitted(LogitModel) > 0.5) %>% as.numeric %>% as.factor) 
caret::confusionMatrix(Logit2.pred, actual, positive = '1')
