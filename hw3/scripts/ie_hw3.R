library(tidyverse)
library(AER)

## Load data
census = read_csv("hw3/data/census1980.csv")

## Interpret year of birth and quarter of birth as factors, not numeric
census = census %>%
  mutate(YOB = factor(YOB)) %>%
  mutate(QOB = factor(QOB))

#### Question 1 ---------

q1_model = census %>%
  lm(formula = LWKLYWGE ~ EDUC + MARRIED + REGION + YOB)

### Report the estimates for the coefficients and their standard errors

summary(q1_model)$coef[,1:2]

### Question 4 ------------

census = census %>%
  mutate(YOB_int_QOB = factor(interaction(YOB, QOB)))

census$YOBxQOB = NA

## This for-loop will take a while... (took me 6 minutes with 16 GB of RAM, 10th gen i7 processor). Super inefficient, but it works: here were are saying to create a new factor, the interaction of YOB and QOB, only if the year is not y39 and the quarter is not q4 -- this  should solve the issue of collinearity.
## 

for(i in 1:dim(census)[1]){
  if(census$QOB[i] != "q4" & census$YOB[i] != "y39"){
    census$YOBxQOB[i] = paste(census$YOB[i], census$QOB[i], sep = ".")
  }
}



census = census %>%
  mutate(YOBxQOB = factor(YOBxQOB))

## Testing the difference between easily created variable (using mutate fxn) and from using the for loop...
## First is the easy model
q4_model = census %>%
  lm(formula = EDUC ~ YOB_int_QOB + QOB + MARRIED + REGION + YOB)

# This is the model with the for-loop generated interaction term
q4_model2 = census %>%
  lm(formula = EDUC ~ YOBxQOB + QOB + MARRIED + REGION + YOB)

summary(q4_model)
summary(q4_model2)

## We find that there is effectively 0 difference in the models, no reason to use such an inefficient for loop

# There is collinearity. Let's see the coefficients without na values
na.omit(summary(q4_model)$coef)[,1:2]


### Question 6 ------------

#using ivreg, and again comparing the models using the 2 different interaction terms

q6_model = ivreg(LWKLYWGE ~ EDUC + MARRIED + REGION + YOB | YOB_int_QOB + QOB, data = census)

q6_model2 = ivreg(LWKLYWGE ~ EDUC + MARRIED + REGION + YOB | YOBxQOB + QOB, data = census)


## Here we find that though these 2 interaction terms made a very similar  model in Q4, in question 6 the choice of the interaction term makes a very big difference on the coefficients.. We may have a big estimation problem here.

summary(q6_model, diagnostics = TRUE)

### Question 8

# Hausman test ----
betaOLS_educ <- coefficients(q1_model)[2]
varOLS_educ <- vcov(q1_model)[2,2]
betaIV_educ <- coefficients(q6_model)[2]
varIV_educ <- vcov(q6_model)[2,2]

hausman <- (betaOLS_educ - betaIV_educ) / sqrt(varIV_educ - varOLS_educ)
hausman

# |t| = 0.7: We can't reject H0 at 10% level


