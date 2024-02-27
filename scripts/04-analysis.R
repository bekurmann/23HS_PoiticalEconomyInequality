# ##############################################################
# analysis
# ##############################################################

# ##############################################################
# loading data
data <- readRDS("data/data.rds")

# model 1
# turnout 2023 ~ gini_steink_percent
model1 <- lm(wahlbeteiligung2023 ~ 
               gini_steink_percent + 
               median_steink +
               # control
               einwohner + 
               auslaenderanteil +
               sozialhilfequote +
               leerwohnungsziffer + 
               anteil0bis19 +
               anteil20bis64 +
               flaeche_landwirtschaft,
             data = data)

model1_2019 <- lm(wahlbeteiligung2019 ~ 
               gini_steink_percent + 
               median_steink +
               # control
               einwohner + 
               auslaenderanteil +
               sozialhilfequote +
               leerwohnungsziffer + 
               anteil0bis19 +
               anteil20bis64 +
               flaeche_landwirtschaft,
             data = data)

plot(x = data$gini_steink, y = data$wahlbeteiligung2019)
plot(x = data$gini_steink, y = data$wahlbeteiligung2023)

summary(model1)
summary(model1_2019)
# model 2
# turnout ~ median income
model2 <- lm(wahlbeteiligung2023 ~ median_steink, data = data)
summary(model2)

# model 3
#
model3 <- lm()
summary(model3)