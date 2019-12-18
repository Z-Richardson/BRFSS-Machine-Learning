# -----------------------------------------------------------------------------
# Script Name: Colorado BRFSS Seatbelt Sample
#
# Purpose of Script: Create the sample to use in a BRFSS seatbelt machine 
#                    learning analysis
#
# Author: Zachary Richardson, Ph.D.
#
# Date Created: 2019-12-05
#
# Copyright (c) Zachary Richardson, 2019
# Email: zachinthelab@gmail.com
# Blog: thelab.ghost.io
# --------------------------------------
# Notes: Create the sample to use in a BRFSS seatbelt machine learning
#        analysis
# -----------------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(fuzzyjoin)
library(stringr)
setwd("~./BRFSS Project")
# -----------------------------------------------------------------------------
# Import BRFSS Data & Subset to Include Only Colorado Data
setwd("~./BRFSS Project/data/brfss_raw")

files <- list.files(pattern="*.csv")
myfiles <- lapply(files, read.csv)

for(i in 1:length(myfiles)) {
  ifelse(i+2 < 10,
         assign(paste0("co_0", i+2, ".df"), myfiles[[i]] %>% subset(., X_state == 8)),
         assign(paste0("co_", i+2, ".df"), myfiles[[i]] %>% subset(., X_state == 8)))
 }

save(list = ls(pattern = glob2rx("co_*.df")), file = "co_brfssdata.RData")
# rm(files, myfiles)
# -----------------------------------------
# Variables to include in basic model:
# - Age
# - County (ctycode or ctycode1 for 11 and 12 data)
# - Gender
# - Income
# - Marital Status
# - General Health
# - Currently Smokes Cigs (X_rfsmok3)
# - Race/Ethnicity
rm(list = ls())
load("co_brfssdata.RData")
varlist1 <- c("age", "ctycode", "sex", "income2", "marital", 
              "genhlth", "X_rfsmok3", "seatbelt", "iyear", "race2")
varlist2 <- c("age", "ctycode", "sex", "income2", "marital", 
              "genhlth", "X_rfsmok3", "iyear", "race2")
# -----------------------------------------------------------------------------
# ML Sample Years = 06, 08, 10-12
winsmpl.lst <- list((co_06.df %>% select(., varlist1) %>% mutate(year = 2006)),
                    (co_08.df %>% select(., varlist1) %>% mutate(year = 2008)),
                    (co_10.df %>% select(., varlist1) %>% mutate(year = 2010)),
                    (co_11.df %>% rename(., ctycode = ctycode1) 
                              %>% select(., varlist1) %>% mutate(year = 2011)),
                    (co_12.df %>% rename(., ctycode = ctycode1) 
                              %>% select(., varlist1) %>% mutate(year = 2012)))

# ML Out of Sample Years = 05, 07, 09
outsmpl.lst <- list((co_05.df %>% select(., varlist2) %>% mutate(year = 2005)),
                    (co_07.df %>% select(., varlist2) %>% mutate(year = 2007)),
                    (co_09.df %>% select(., varlist2) %>% mutate(year = 2009)))
# -----------------------------------------------------------------------------
# Grab some more data for the urban and rural codes -
urbrur.df <- read.csv("NCHSURCodes2013.csv")
 co_urb.df <- urbrur.df %>% 
   select(FIPS.code, State.Abr., X2006.code) %>%
   subset(., State.Abr. == "CO") %>%
   mutate(ctycode = str_sub(as.character(.data$FIPS.code),-3)) %>%
   mutate_at(vars(ctycode), as.integer)

 co_urb.2mrg <- co_urb.df %>%
   rename(rur.code = X2006.code) %>%
   mutate(urban = ifelse(rur.code < 4, "Urban", "Rural")) %>%
   mutate_at(vars(urban), as.factor) %>%
   select(ctycode, urban)
# -----------------------------------------
# Create some labels that will be used when labeling variables
 gender <- c("female", "male")
 inclab <- c("(-inf, $10,000)", "[$10,000, $15,000)", "[$15,000, $20,000)",
             "[$20,000, $25,000)", "[$25,000, $35,000)", "[$35,000, $50,000)",
             "[$50,000, $75,000)", "[$75,000, inf)")
 married <- c("married", "not currently", "never married")
 health <- c("Excellent", "Very good", "Good", "Fair","Poor")
 smoke <- c("no", "yes")
 belts <- c("no", "yes")
 
# Create Sample Data Frames (Include basic removal of non-responses from variables)
insample.df <- do.call(rbind, winsmpl.lst) %>%
  rename(., income = income2, smoker = X_rfsmok3) %>%
  mutate_at(vars(marital, genhlth, smoker, seatbelt, race2), na_if, 9) %>%
  mutate_at(vars(seatbelt), na_if, 7) %>%
  mutate(seatbelt = ifelse(seatbelt == 1, 2, 1)) %>%
  mutate(race2 = ifelse(race2 > 2, race2 + 1, race2)) %>%
  mutate(race2 = ifelse(race2 == 9, 3, race2)) %>%
  mutate(race2 = ifelse(race2 > 4, 4, race2)) %>% 
  mutate(marital = ifelse((marital > 1 & marital < 5), 2, marital)) %>%
  mutate(marital = ifelse(marital > 4, 3, marital)) %>%
  mutate_at(vars(income), na_if, 99) %>%
  mutate_at(vars(income), na_if, 77) %>%
  mutate_at(vars(ctycode), na_if, 777) %>%
  mutate_at(vars(ctycode), na_if, 888) %>%
  mutate_at(vars(ctycode), na_if, 999) %>%
  mutate(seatbelt = factor(.data$seatbelt, levels = c(1,2), labels = belts)) %>%
  mutate(sex = factor(.data$sex, levels = c(1,2), labels = gender)) %>%
  mutate(income = factor(.data$income, levels = c(1:8), labels = inclab)) %>%
  mutate(marital = factor(.data$marital, levels = c(1:3), labels = married)) %>%
  mutate(genhlth = factor(.data$genhlth, levels = c(1:5), labels = health)) %>%
  mutate(smoker = factor(.data$smoker, levels = c(1,2), labels = smoke)) %>%
  inner_join(co_urb.2mrg) %>%
  rename(gender = sex, race.cat = race2) %>%
  select(seatbelt, age, gender:smoker, race.cat)

outsample.df <- do.call(rbind, outsmpl.lst) %>%
  rename(., income = income2, smoker = X_rfsmok3) %>%
  mutate(race2 = ifelse(race2 > 2, race2 + 1, race2)) %>%
  mutate(race2 = ifelse(race2 == 9, 3, race2)) %>%
  mutate(race2 = ifelse(race2 > 4, 4, race2)) %>% 
  mutate(marital = ifelse((marital > 1 & marital < 5), 2, marital)) %>%
  mutate(marital = ifelse(marital > 4, 3, marital)) %>%
  mutate_at(vars(income), na_if, 99) %>%
  mutate_at(vars(income), na_if, 77) %>%
  mutate_at(vars(ctycode), na_if, 777) %>%
  mutate_at(vars(ctycode), na_if, 888) %>%
  mutate_at(vars(ctycode), na_if, 999) %>%
  mutate(sex = factor(.data$sex, levels = c(1,2), labels = gender)) %>%
  mutate(income = factor(.data$income, levels = c(1:8), labels = inclab)) %>%
  mutate(marital = factor(.data$marital, levels = c(1:3), labels = married)) %>%
  mutate(genhlth = factor(.data$genhlth, levels = c(1:5), labels = health)) %>%
  mutate(smoker = factor(.data$smoker, levels = c(1,2), labels = smoke)) %>%
  inner_join(co_urb.2mrg) %>%
  rename(gender = sex, race.cat = race2) %>%
  select(age, gender:smoker, race.cat)

  # rm(winsmpl.lst, outsmpl.lst)
save(insample.df, outsample.df, file = "brfss_small_ml_dfs.RData")
# -----------------------------------------------------------------------------
