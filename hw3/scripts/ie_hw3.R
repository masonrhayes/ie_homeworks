library(tidyverse)
library(AER)

## Load data
census = read_csv("data/census1980.csv")

## Interpret year of birth and quarter of birth as factors, not numeric
census = census %>%
  mutate(YOB = as.factor(YOB)) %>%
  mutate(QOB = as.factor(QOB))

#### Question 1 ---------

q1_model = census %>%
  lm(formula = LWKLYWGE ~ EDUC + MARRIED + REGION + YOB)

### Report the estimates for the coefficients and their standard errors

summary(q1_model)$coef[,1:2]

### Question 4 ------------

census = census %>%
  mutate(YOB_int_QOB = as.factor(interaction(YOB, QOB)))

q4_model = census %>%
  lm(formula = EDUC ~ YOB_int_QOB + QOB + MARRIED + REGION + YOB)

summary(q4_model)

# There is collinearity since the interaction terms include each quarter.. How to fix this ??
# The following fixes it but it is not ideal..
na.omit(summary(q4_model)$coef)

