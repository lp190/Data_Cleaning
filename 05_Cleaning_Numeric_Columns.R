## Part 5: Data cleaning Challenge: Cleaning Numeric Columns for two datasets
## hospital data and airbnb data

# Directory set

# Load libraries
library(tidyverse)
library(lubridate)
### Other ways of plotting missing values
library(naniar)
library(visdat)

# Load datasets

hospital <- read_csv("inpatientCharges.csv")

airbnb <- read_csv("listings.csv")
# These data do not look good at first glance

# So clean up columns names, for that
hospital_jan <- hospital %>%
  janitor::clean_names()

# janitor::clean_names() fixes the column names, GREAT!

# Removing in non-numeric characters
# parse_number() for removing the character in numeric data

# character vector of numbers
to_parse <- c(100, "10,000", "%100", "$50")

# check to make sure it's numeric
class(to_parse)

# parse numbers
parsed_numbers <- parse_number(to_parse)

#check class
class(parsed_numbers)

# check what it looks loke now
parsed_numbers

## this is an example, now gotta try with the real data
# get only columns with the data type "character" so that correctly parsed
# numerical values stay the same and identify falsely parsed characters
character_columns <- hospital_jan[, sapply(hospital_jan, class) ==
  "character"]
str(character_columns)


# looking at the columns, the last three columns are incorrectly parsed
# select three three columns and parse each of them

# select columns with "charge" or "pay" in the name
money_columns <- character_columns %>%
  select(contains("charge"), contains("pay")) # those three columns selected

# parsing time
money_columns_parsed <- sapply(money_columns, parse_number) %>%
  as_tibble()
# check the structure
str(money_columns_parsed)  # ist tatsächlich numeric


## replace the old data with parsed version
hospital_jan_parsed <- hospital_jan %>%
  # removes the old columns
  select(-contains("charge"), -contains("pay")) %>%
  # add the columns we parsed earlier
  bind_cols(money_columns_parsed)

# double check that our data types are correct
str(hospital_jan_parsed)

# is indeed correct, good stuff!
#############################################################
## Done with hospital data set ##############################

# Work on Airbnb listings #
# Look into characters first


# Beofre all that


# host_response_rate and host_acceptance_rate somehow
#  have a problem  it is a percentage, that's why it seems
# change to decimal percentage

airbnb$host_response_rate <- 
  as.numeric(sub("%","", airbnb$host_response_rate))/100 # converted 

airbnb$host_acceptance_rate <-
  as.numeric(sub("%","", airbnb$host_acceptance_rate))/100 # converted 



# Filter columns with character and check for falsely labelled "character"
airbnb_char <- airbnb[, sapply(airbnb, class) ==
                        "character"]
str(airbnb_char)
glimpse(airbnb_char)  # lots of false characters


# Check for logical
airbnb_logical <-airbnb[, sapply(airbnb, class)==
                          "logical"]
glimpse(airbnb_logical)

# check for numericals
airbnb_numeric <- airbnb[, sapply(airbnb, class) ==
                             "numeric"]
str(airbnb_numeric) # or glimpse does the job too
glimpse(airbnb_numeric)

# There are a lot of falsely labelled columns, lets try and filter them

### watch out: zipcode, price, weekly_price, monthly_price, 
#   security_deposit, cleaning_fee, extra_people
#   7 for numeric falsely labelled "char"

## could be turned to 
#  logical argument: bed_type, cancellation_policy, room_type
## could be


# start with numerical parsing
# select columns where numerical parsing is  to be done 

air_to_be_parsed <- airbnb_char%>%
  select(contains("price"), contains("weekly_price"), 
  contains("security_deposit"),
  contains("monthly_price"),
  contains("cleaning_fee"),
  contains("zipcode"), contains("extra_people"))

str(air_to_be_parsed)

# the parsing part
air_parsed <- sapply(air_to_be_parsed, parse_number)  %>%
  as_tibble()
str(air_parsed)
    
## Finally, they are all numeric now!!
## replace the old data with parsed version
airbnb_parsed <- airbnb %>%
  # removes the old columns
  select(-contains("price"), -contains("weekly_price"), 
         -contains("security_deposit"),
         -contains("monthly_price"),
         -contains("cleaning_fee"),
         -contains("zipcode"), -contains("extra_people")) %>%
  # add the columns we parsed earlier
  bind_cols(air_parsed)

# double check that our data types are correct
str(airbnb_parsed)
glimpse(airbnb_parsed)

#####################################################################
#####################################################################
# Parsing dates and time in drone strikes data in pakistan
# lubridate package for parsing date
pak_drone <- read_csv("Pakistan_drone_attack_ver9.csv")
# fix the column names, janitor would help
pak_drone <- pak_drone %>%
  janitor::clean_names()

# check out the dataset,esp the date
pak_drone$date[1:10]

# data is in formation of day of week, month, day of month,year
# no lubridate parsing function for handling the day of the week
# so remove them, once parsed, can be brought back

# for removing the day of the week, and following comma

remove_day <- function(column){
  no_day <- str_replace_all(column, '[A-Za-z]*day,', '')
  return(no_day)
}

remove_day(pak_drone$date[1:10])

# this works
# now apply this function to remove day of the week, then parse the remaining 
# data with the mdy() function

# tidy dates & convert them to date format
date <- pak_drone%>%
  select(date) %>% #for the date date column 
  mutate(date_formatted = remove_day(date)) %>% #remove the day of the week
  mutate(date_formatted = mdy(date_formatted)) # convert to date format

# check the heads out
head(date)

# add formatted dates to the dataframe
pak_drone$date_formatted <- date$date_formatted

### done with this dataset ###
#########################################################################
#########################################################################

# Work on mass shooting data
# correctly parse the date column from the mass_shootings datates
# load data first

mass_shooting <- read_csv("Mass Shootings Dataset Ver 5.csv")

# fix the column names, use janitor
mass_shooting <- mass_shooting %>%
  janitor::clean_names()

# parse the date
date <- mass_shooting%>%
  select(date) %>% #for the date date column 
  mutate(date_parsed = mdy(date))

# add the data to the frame
mass_shooting$date_p <- date$date_parsed


## Done with the parsing business and cleaning numeric columns