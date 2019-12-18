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
load("co_brfssdata.RData")
varlist1 <- c("age", "ctycode", "sex", "income2", "marital", 
              "genhlth", "X_rfsmok3", "seatbelt", "iyear")
varlist2 <- c("age", "ctycode", "sex", "income2", "marital", 
              "genhlth", "X_rfsmok3", "iyear")
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

# -----------------------------------------
# Create Sample Data Frames (Include basic removal of non-responses from variables)
insample.df <- do.call(rbind, winsmpl.lst) %>%
  rename(., income = income2, smoker = X_rfsmok3) %>%
  mutate_at(vars(marital, genhlth, smoker, seatbelt), na_if, 9) %>%
  mutate_at(vars(income), na_if, 99) %>%
  mutate_at(vars(income), na_if, 77) %>%
  mutate_at(vars(ctycode), na_if, 777) %>%
  mutate_at(vars(ctycode), na_if, 888) %>%
  mutate_at(vars(ctycode), na_if, 999) %>%
  mutate(sex = factor(.data$sex, levels = c(1,2), labels = c("female","male"))) %>%
  
  rename(gender = sex)

outsample.df <- do.call(rbind, outsmpl.lst) %>%
  rename(., income = income2, smoker = X_rfsmok3) %>%
  mutate_at(vars(marital, genhlth, smoker), na_if, 9) %>%
  mutate_at(vars(income), na_if, 99) %>%
  mutate_at(vars(income), na_if, 77) %>%
  mutate_at(vars(ctycode), na_if, 777) %>%
  mutate_at(vars(ctycode), na_if, 888) %>%
  mutate_at(vars(ctycode), na_if, 999) %>%
  mutate(female = ifelse(sex == 2, "Female", "Male")) %>%
  mutate_at(vars(female), as.factor) %>%
  select(-sex)

  # rm(winsmpl.lst, outsmpl.lst)
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
# Update DF with an inner_join so that if a county is NA or not in the list
# then it is droppsed from the sample.
insample.df <- insample.df %>%
  inner_join(co_urb.2mrg)
 
outsample.df <- outsample.df %>%
  inner_join(co_urb.2mrg)

save(insample.df, outsample.df, file = "brfss_small_ml_dfs.RData")
# -----------------------------------------------------------------------------
