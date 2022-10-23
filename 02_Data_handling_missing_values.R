## Data Cleaning Part 2 ##

rm(list=ls())
getwd()

setwd("A:/Datasets/Data Cleaning")

library(tidyverse)
library(mice)  # package for categorical and numeric imputations

### Other ways of plotting missing values
library(naniar)
library(visdat)

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



## Think hard why could the data be missing. 
## Missing completely at random vs Missing at random???? Think!


###########################################################################

# Check out the punjab growth rate data
head(pun_growth)

#### There are missing data with NA. It's virtually impossible to catch all the 
#    data with your eyes. So, better to visualize and check them out, which is
#    exactly what I am going to do in next steps.

### vis_dat; visualizes the whole df at once

vis_dat(punjab_data2)  ## Quite a nice plot, missing data discernible
vis_dat(austin_pet)  ## This data set is quite big for that, can't 
                      # really plot the missing data.


## vis_miss provides a summary of whether the data is missing or no
vis_miss(punjab_data2)
vis_miss(airquality)
vis_miss(austin_pet)  ## This data too large for visualizing

## gg_miss_var
gg_miss_var(punjab_data2)
gg_miss_var(austin_pet, facet = outcome_type)  + 
  labs(y ="Guckmal missing values")  
## Works here, SUPER!!

# Data frame with information on whether the value in each cell is missing
missing_by_column <- pun_gdp %>%
  is.na %>%   # check if each cell is na
  as_tibble() %>%  # Converstion to data-frame (used to be as_data_frame)
  mutate(row_number = 1:nrow(.)) %>% #add a column with the row number
  gather(variable, is_missing, -row_number) # Conversion of wide data to narrow
                                            # Schick function from tidyr package
## Plot the missing values in our data frame
ggplot(missing_by_column, aes(x = variable, y = row_number,
  fill = is_missing)) + geom_tile() + theme_minimal() + 
  scale_fill_grey(name = "", labels = c("Present", "Missing")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8)) +
  labs(x = "Variables in Dataset", y = "Rows/observations")

### Three missing data from Barnala district! SCHICK oder?

### Plot the missing values from the punjab_growth
pun_growth   # Seems lots of missing data, mal schauen

missing_by_col <- pun_growth %>%
  is.na %>%
  as_tibble() %>% # Converstion to data-frame
  mutate(row_number = 1:nrow(.)) %>% # add a clumn with the row number
  gather(variable, is_missing, -row_number) # Conversion of wide data to narrow
  

### Plot the missing values in our pun_grwoth data frame
ggplot(missing_by_col, aes(x = variable, y = row_number,
  fill = is_missing)) + geom_tile() + theme_minimal() + 
  scale_fill_grey(name = "", labels = c("Present", "Missing")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8)) +
  labs(x = "Variables in Dataset", y = "Rows/observations")





## Now what are the randomly missing values; what should be there??
## Imputations

# Automate the imputation process using MICE
