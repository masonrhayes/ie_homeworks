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

q4_model = census %>%
  lm(formula = EDUC ~ YOB * QOB + QOB + MARRIED + REGION + YOB)

summary(q4_model)


### Question 6 ------------

#using ivreg; instruments are YOB * QOB interaction term and QOB

q6_model = ivreg(LWKLYWGE ~ EDUC + MARRIED + REGION + YOB | YOB * QOB + QOB + MARRIED + REGION + YOB, data = census)

## See a summary of the model and diagnostics to show the Wu-Hausman and Sargan stats

q6_diagnostics = summary(q6_model, diagnostics = TRUE)

q6_diagnostics

diagnostic_stats = q6_diagnostics$diagnostics

diagnostic_stats

## Wu-Hausman stat = 1.80; Sargan stat = 24.61. We reject null for each --> we  reject that educ is exogenous, and we reject that the instruments are invalid; implying that education is indeed endogenous and that the instruments are likely correlated with the error term.


### Question 8 ------- No we calculate manually the (practical) Hausman test

# (Practical) Hausman test ----
betaOLS_educ <- coefficients(q1_model)[2]
varOLS_educ <- vcov(q1_model)[2,2]
betaIV_educ <- coefficients(q6_model)[2]
varIV_educ <- vcov(q6_model)[2,2]

hausman <- (betaIV_educ - betaOLS_educ) / sqrt(varIV_educ - varOLS_educ)
hausman

# |t| = 0.7: We can't reject H0 at 10% level

### Question 9 --------
### 

q6_residuals = q6_model$residuals

q9_model = lm(data = census, formula = q6_residuals ~ YOB * QOB + QOB + MARRIED + REGION + YOB)

summary(q9_model)