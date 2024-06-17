# Load necessary libraries
library(tidyverse)
library(here)
library(pscl)
library(broom)
library(performance)
library(ggeffects)

# ##############################################################
# analysis
# ##############################################################

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
logistic_model <- glm(turnout ~ gender + age + income + education + gini, 
                      data = cleaned_data, 
                      family = binomial(link = "logit"))

# sum
summary(cleaned_data)
summary(logistic_model)

# odds ratios
exp(coef(logistic_model))

# ##############################################################
# diagnostics

# predicted probabilities
predicted_probabilities <- predict(logistic_model, type = "response")

# adding predictions to the cleaned data frame
cleaned_data <- cleaned_data %>%
  mutate(predicted_turnout = predicted_probabilities)

# pseudo R2 values
pseudo_r2 <- pR2(logistic_model)
print(pseudo_r2)

# ##############################################################
# Generate and plot effects for each predictor
effects_gender <- ggpredict(logistic_model, terms = "gender")
plot_effects_gender <- plot(effects_gender) + labs(title = "Effect of Gender on Turnout")
# save
ggsave(filename = "img/effects_gender.png", plot = plot_effects_gender, width = 8, height = 6, dpi = 300)

effects_age <- ggpredict(logistic_model, terms = "age")
plot_effects_age <- plot(effects_age) + labs(title = "Effect of Age on Turnout")
# save
ggsave(filename = "img/effects_age.png", plot = plot_effects_age, width = 8, height = 6, dpi = 300)

effects_income <- ggpredict(logistic_model, terms = "income")
plot_effects_income <- plot(effects_income) + labs(title = "Effect of Income on Turnout")
# save
ggsave(filename = "img/effects_income.png", plot = plot_effects_income, width = 8, height = 6, dpi = 300)

effects_education <- ggpredict(logistic_model, terms = "education")
plot_effects_edu <- plot(effects_education) + labs(title = "Effect of Education on Turnout")
# save
ggsave(filename = "img/effects_edu.png", plot = plot_effects_edu, width = 8, height = 6, dpi = 300)

effects_gini <- ggpredict(logistic_model, terms = "gini_steink")
plot_effects_gini <- plot(effects_gini) + labs(title = "Effect of Gini Coefficient on Turnout")
# save
ggsave(filename = "img/effects_gini.png", plot = plot_effects_gini, width = 8, height = 6, dpi = 300)

