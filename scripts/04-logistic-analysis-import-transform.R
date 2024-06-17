# ##############################################################
# analysis
# ##############################################################

library(tidyverse)
library(here)
library(readxl)

# ##############################################################
# checkout wd
here()

getwd()
setwd(here())
getwd()

# ##############################################################
# loading data
selects2019 <- readRDS("data/raw_selects2019.rds")

# ##############################################################
# select
selects2019 <- selects2019 %>% 
  select(c(userid, age, sex, f11100, f28910, f21310, f10000)) %>% 
  rename(
    id = userid,
    canton = f10000,
    age = age,
    gender = sex,
    turnout = f11100,
    income = f28910,
    education = f21310)

# ##############################################################
# transform

# gender
selects2019 <- selects2019 %>%
  mutate(
    gender = case_when(
      gender == 1 ~ 1,
      gender == 2 ~ 2,
      TRUE ~ NA_real_
    )
  )

# turnout
selects2019 <- selects2019 %>%
  mutate(
    turnout = case_when(
      turnout == 4 ~ 1,
      turnout %in% c(-99, -98, -97) ~ NA_real_,
      TRUE ~ 0
    )
  )

# income
selects2019 <- selects2019 %>%
  mutate(
    income = case_when(
      income %in% 1:4 ~ 1,
      income %in% 5:9 ~ 2,
      income %in% 10:15 ~ 3,
      income %in% c(-99, -98, -97) ~ NA_real_
    )
  )

# eductaion
selects2019 <- selects2019 %>%
  mutate(
    education = case_when(
      education %in% 1:5 ~ 1,
      education %in% 6:11 ~ 2,
      education %in% 12:14 ~ 3,
      education %in% c(-99, -98, -97) ~ NA_real_
    )
  )

# ##############################################################
# add gini on canton level
# canton number in SELECTS is the same as BfS canton number -> join

# income data
raw_income <- read_excel(path = "data/raw_np-2019.xlsx",
                         sheet = "Kantone - Cantons")

raw_income <- raw_income %>% 
  select(c(Einheit, ktnr, ktname, mean_steink, median_steink, gini_steink)) %>% 
  filter(Einheit == "Total")

selects2019 <- selects2019 %>%
  left_join(raw_income, by = c("canton" = "ktnr"))

# ##############################################################
# overview
summary(selects2019)

# ##############################################################
# export
saveRDS(selects2019, file = "data/selects2019_logistic.rds")




