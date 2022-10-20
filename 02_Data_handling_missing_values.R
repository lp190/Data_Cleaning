## Data Cleaning Part 2 ##

rm(list=ls())
getwd()

setwd("A:/Datasets/Data Cleaning")

library(tidyverse)
library(mice)  # package for categorical and numeric imputations

# set seed for reproducibility
set.seed(5)

# Time to read datasets
punjab_data <- read_csv("gdp_Punjab1.csv")
austin_pet <- read_csv("aac_shelter_outcomes.csv")

# Check punjab data
str(punjab_data)
head(punjab_data)