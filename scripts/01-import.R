# ##############################################################
# data import and merging
# ##############################################################

# dependencies
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("sf")
#install.packages("viridis")
#installpackages("here")

# loading
library(tidyverse)
library(readxl)
library(here)

# checkout wd
here()

getwd()
setwd(here())
getwd()

# ##############################################################
# reading raw income data ---
# source: https://www.estv.admin.ch/estv/de/home/die-estv/steuerstatistiken-estv/allgemeine-steuerstatistiken/direkte-bundessteuer.html#accordion1699024072321
# «natürliche Personen - mit + ohne direkte Bundessteuer»
# legend of the variables I need
# mean_steink     Durchschnittliches steuerbares Einkommen, in Franken
# median_steink   Steuerbares Medianeinkommen, in Franken
# gini_steink     Gini-Koeffizient des steuerbaren Einkommens
?read_excel()
raw_income <- read_excel(path = "data/raw_np-2019.xlsx",
                       sheet = "Gemeinden - Communes")

raw_income <- raw_income %>% 
  select(c(ktnr, ktname, gdenr, gdename, mean_steink, median_steink, gini_steink))

# ##############################################################
# reading raw participation data
# source: https://opendata.swiss/de/dataset/eidg-wahlen-2023
# variables I need: 
# wahl_jahr, kanton_nummer, kanton_bezeichnung, gemeinde_nummer, gemeinde_bezeichnung, wahlbeteiligung, letzte_wahl_beteiligung
# 
raw_turnout2019 <- read_delim(file = "data/sd-t-17.02-NRW2023-wahlbeteiligung-appendix.csv", delim = ";")

raw_turnout2019 <- raw_turnout2019 %>% 
  select(c(wahl_jahr, kanton_nummer, kanton_bezeichnung, 
           gemeinde_nummer, gemeinde_bezeichnung, wahlbeteiligung,
           letzte_wahl_wahlbeteiligung))

# ##############################################################
# reading raw municipality data
# source: https://www.bfs.admin.ch/bfs/de/home/statistiken/regionalstatistik/regionale-portraets-kennzahlen/gemeinden.html
# docs: https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken/tabellen.assetdetail.16484444.html ("Anhang")
raw_municipality <- read_delim(file = "data/ts-x-21.03.01.csv")

# getting rid of NA rows
raw_municipality <- raw_municipality %>% 
  select(-c(PERIOD_COMP, STATUS))

# gotta make it wider
raw_municipality <- raw_municipality %>%
  pivot_wider(
    names_from = c(INDICATORS, PERIOD_REF, UNIT_MES),  # Column to take names for the new columns
    values_from = VALUE,  # Column which contains the values to fill in the new columns
    # If you have a year column and want to include it in the transformation:
    names_sep = "_", 
    names_glue = "{INDICATORS}_{PERIOD_REF}_{UNIT_MES}"
  ) 

# group by CODE_REGION
raw_municipality <- raw_municipality %>% 
  group_by(CODE_REGION)

# ##############################################################
# merging
# joining together raw_income and raw_turnout2019
merge_income_turnout <- merge(raw_income, raw_turnout2019, by.x = "gdenr", by.y = "gemeinde_nummer")

# joining together merge_income_turnout and raw_municipality
raw_data <- merge(merge_income_turnout, raw_municipality, by.x = "gdenr", by.y ="CODE_REGION")

# save raw data as RDS
saveRDS(raw_data, file = "data/raw_data.rds")

