## Data Cleaning Part 2 ##
## Data sets used 
 #-> 1. gdp_Punjab2.csv 
 #-> 2. aac_shelter_outcomes.csv
 # Both datasets, you can find in Datasets


rm(list=ls())
getwd()

setwd("A:/Datasets/Data Cleaning")

library(tidyverse)
library(mice)
library(dplyr)# package for categorical and numeric imputations

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

# There is some kind of error, so rename the column Fatehgarh Sahib
names(punjab_data2)[7] <- "Fatehgarh_sahib"
names(punjab_data2)[19] <- "Sahibzaha_ajit_singh_nagar"
names(punjab_data2)[21] <- "Sahid_bhagat_singh_nagar"
names(punjab_data2)[22] <- "Taran_tarn"

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


# df with information on whether the value in each cell is missing
missi_by_col <- pun_gdp %>%
  is.na %>% # chech if each cell is NA
  as_tibble()%>% #### as_data_frame() deprecated to tibble(); converstion to df
  mutate(row_number = 1:nrow(.)) %>% # add a column with the row number
  gather(variable, is_missing, -row_number) # Converstion of wide data to narrow

# Plot th missing values in our data frame
ggplot(missi_by_col, aes(x = variable, y = row_number, 
  fill = is_missing)) + geom_tile() + theme_minimal() + 
  scale_fill_grey(name = "", labels = c("Present", "Missing")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8)) +
  labs(x = "Variables in Dataset", y = "Observations")


### Three missing data from Barnala district! SCHICK oder?

pun_gdp$Barnala = ifelse(pun_gdp$Barnala == "NA", NA, pun_gdp$Barnala)

## Multivariate Imputation By Chained Equations
### Imputations of the missing data
## Initialize and empty model
emp <- mice(pun_gdp, maxit = 0)
method <- emp$method
predictorMatrix <- emp$predictorMatrix

imputed_data <- mice(pun_gdp, method, predictorMatrix, m = 3)

imputed_data <- complete(imputed_data)

### Imputations failed above, no basis for imputations apparently, i suppose

## Multivariate Imputation By Chained Equations

## Lets do it for growth

empty <- mice(pun_growth, maxit = 0)
method <- empty$method
predictorMatrix <- empty$predictorMatrix

# first make a bunch of guess
imputed <- mice(pun_growth, method, predictorMatrix, m = 5)

# then pick one for each var
imputed <- complete(imputed)
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

## First create an empty model
##................... failed at first


### Changed all the col names, which i have done avove looks good



# check out the imputed data
head(imputed_data)


#### It failed!!


########## try with gdp_growth
head(pun_growth)

# Initialize an empty model to take the parameters from
empty <- mice(pun_growth, maxit = 0)
method <- empty$method
predictorMatrix <- empty$predictorMatrix

# first make a bunch of guess
imputed <- mice(pun_growth, method, predictorMatrix, m = 7)

# then pick one for each var
imputed <- complete(imputed)
head(imputed)


## Worked here!! WOW!

## Nevertheless check out with the graph
gg_miss_var(pun_gdp)
gg_miss_var(imputed_data)

## Check for pun_growth
gg_miss_var(pun_growth)
gg_miss_var(imputed) ## This was the successfully 
                      # imputed dataset
