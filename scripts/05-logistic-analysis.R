# ##############################################################
# analysis
# ##############################################################

install.packages("car")

library(tidyverse)
library(here)
library(car)

# ##############################################################
# checkout wd
here()

getwd()
setwd(here())
getwd()

# ##############################################################
# loading data
selects2019 <- readRDS("data/selects2019_logistic.rds")

# ##############################################################
# log

# removing nas
cleaned_data <- selects2019 %>%
  drop_na(gender, age, income, education, gini_steink, turnout)

# regression model
logistic_model <- glm(turnout ~ gender + age + income + education + gini_steink, 
                      data = cleaned_data, 
                      family = binomial(link = "logit"))

# sum
summary(logistic_model)

# Calculate odds ratios
exp(coef(logistic_model))

# ##############################################################
# diagnostics

# multicollinearity
vif(logistic_model)

# predicted probabilities
predicted_probabilities <- predict(logistic_model, type = "response")

# adding predictions to the cleaned data frame
cleaned_data <- cleaned_data %>%
  mutate(predicted_turnout = predicted_probabilities)



