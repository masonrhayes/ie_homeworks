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

### Question 2 ------------

