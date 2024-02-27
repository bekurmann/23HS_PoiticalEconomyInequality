# ##############################################################
# analysis
# ##############################################################

library(here)

# checkout wd
here()

getwd()
setwd(here())
getwd()

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
               anteil20bis64 +
               anteil65plus + 
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
               anteil20bis64 +
               anteil65plus + 
               flaeche_landwirtschaft,
             data = data)

model1_no_welfareRate <- lm(wahlbeteiligung2023 ~ 
               gini_steink_percent + 
               median_steink +
               # control
               einwohner + 
               auslaenderanteil +
               leerwohnungsziffer + 
               anteil20bis64 +
               anteil65plus + 
               flaeche_landwirtschaft,
             data = data)

model1_no_welfareRate_2019 <- lm(wahlbeteiligung2019 ~ 
                              gini_steink_percent + 
                              median_steink +
                              # control
                              einwohner + 
                              auslaenderanteil +
                              leerwohnungsziffer + 
                              anteil20bis64 +
                              anteil65plus + 
                              flaeche_landwirtschaft,
                            data = data)

summary(model1)
summary(model1_2019)
summary(model1_no_welfareRate)
summary(model1_no_welfareRate_2019)

# why anteil65plus is NA
summary(data)

# model 2
# turnout ~ median income
model2 <- lm(wahlbeteiligung2023 ~ median_steink, data = data)
summary(model2)

# model 3
#
model3 <- lm()
summary(model3)