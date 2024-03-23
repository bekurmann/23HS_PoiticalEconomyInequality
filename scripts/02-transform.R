# ##############################################################
# transform
# ##############################################################

# dependencies
library(tidyverse)
library(here)

# checkout wd
here()

getwd()
setwd(here())
getwd()

# read raw data
raw_data <- readRDS("data/raw_income_data.rds")

# ##############################################################
# Calculate any additional variables or transformations you need
# For example, you can calculate the Gini Index as a percentage
data <- raw_data %>%
  mutate(gini_steink_percent = gini_steink * 100)

# ##############################################################
# selecting only necessary
data <- data %>% 
  select(-c(wahl_jahr,
            `Ind_01_02_2010/2019_UM_2`,
            Ind_01_03_2019_UM_3,
            Ind_01_09_2019_UM_4,
            Ind_01_10_2019_UM_4,
            Ind_01_11_2019_UM_4,
            Ind_01_12_2019_UM_4,
            Ind_01_13_2019_UM_16,
            Ind_01_14_2019_UM_6,
            Ind_04_01_2016_UM_11,
            `Ind_04_02_2004/2009_UM_2`,
            `Ind_04_03_1979/1985_UM_10`,
            `Ind_04_05_1979/1985_UM_10`,
            `Ind_04_06_2004/2009_UM_2`,
            `Ind_04_07_2004/2009_UM_2`,
            Ind_06_03_2018_UM_16,
            Ind_06_04_2018_UM_16,
            Ind_06_05_2018_UM_16,
            Ind_06_06_2018_UM_16,
            Ind_06_07_2018_UM_9,
            Ind_06_08_2018_UM_9,
            Ind_06_09_2018_UM_9,
            Ind_06_10_2018_UM_9,
            Ind_08_04_2018_UM_12,
            Ind_14_10_2019_UM_2))

# renaming columns
data <- data %>% 
  rename(einwohner = Ind_01_01_2019_UM_6,
         auslaenderanteil = Ind_01_08_2019_UM_2,
         anteil0bis19 = Ind_01_04_2019_UM_2,
         anteil20bis64 = Ind_01_05_2019_UM_2,
         anteil65plus = Ind_01_06_2019_UM_2,
         flaeche_landwirtschaft = `Ind_04_04_2004/2009_UM_2`,
         leerwohnungsziffer = Ind_08_01_2020_UM_2,
         sozialhilfequote = Ind_11_01_2019_UM_2,
         anteil_fdp2019 = Ind_14_01_2019_UM_2,
         anteil_cvp2019 = Ind_14_02_2019_UM_2,
         anteil_sp2019 = Ind_14_03_2019_UM_2,
         anteil_svp2019 = Ind_14_04_2019_UM_2,
         anteil_evpcsp2019 = Ind_14_05_2019_UM_2,
         anteil_glp2019 = Ind_14_06_2019_UM_2,
         anteil_bdp2019 = Ind_14_07_2019_UM_2,
         anteil_psa2019 = Ind_14_08_2019_UM_2,
         anteil_gps2019 = Ind_14_09_2019_UM_2,
         wahlbeteiligung2023 = wahlbeteiligung,
         wahlbeteiligung2019 = letzte_wahl_wahlbeteiligung)

# Check the structure of your transformed data
str(data)

# ##############################################################
# save data as RDS
saveRDS(data, file = "data/income_data.rds")
