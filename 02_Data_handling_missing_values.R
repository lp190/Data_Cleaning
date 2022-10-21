## Data Cleaning Part 2 ##

rm(list=ls())
getwd()

setwd("A:/Datasets/Data Cleaning")

library(tidyverse)
library(mice)  # package for categorical and numeric imputations

# set seed for reproducibility
set.seed(5)

# Time to read datasets

punjab_data2 <- read_csv("gdp_Punjab2.csv")
austin_pet <- read_csv("aac_shelter_outcomes.csv")

# Play with punjab dataset

View(punjab_data2) # Bunch of missing values for this one
str(punjab_data2)
head(punjab_data2)

# Gdp growth rate of Bhatinda, 2003-04
punjab_data2$Bathinda[[11]]
punjab_data2[[5]][[11]]  # Does the same thing as above

punjab_data2[[4]][[11]] #for barnala district
punjab_data2[[4]][11]   # this does the same job as above

# Patiala city
punjab_data2$Patiala[[1]]

# Looks good, but tedious
##################################################

## Check out austin shelter outcome data
View(austin_pet) # huge data with 78256 observations and 12 variable
austin_pet$name[[100]] # name of 100th pet

# Tedious as usual

####################################################################

# That's why we use tidyverse; quite versatile package ;)
# Work with Punjab data first of all

# Seperate GDP and Growth data
pun_gdp <- punjab_data2 %>%
  filter(Description == 'GDP (in Rs. Cr.)')
pun_growth <- punjab_data2 %>%
  filter(Description == 'Growth Rate % (YoY)')

# This way make two sets of data
View(pun_gdp) # Check out that small ASS GDP data ;)
View(pun_growth) # Check out for growth data

## Check out for missing at random
# Is my data missing at random??? (MAR)
# For pet data
head(austin_pet) # There are indeed bunch of missing values
                 # name and outcome type; missing values

# Also missing values in gdp punjab2 data
head(punjab_data2) # Barnala GDPs missing values, for eg







  