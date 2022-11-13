## Part 5: Data cleaning Challenge: Ceaning Numeric Columns

# Directory set

# Load libraries
library(tidyverse)
library(lubridate)

# Load datasets
airbnb <- read_csv("listings.csv")
hospital <- read_csv("inpatientCharges.csv")
pak_drone <- read_csv("Pakistan_drone_attack_ver9.csv")
mass_shootings <- read_csv("Mass Shootings Dataset Ver 5.csv")

# These data do not look good at first glance
# especially for the names of the columns for hospital, drone_strikes,
# & mass_shootings
# So clean up columns names, for that

hospital_jan <- hospital %>%
  janitor::clean_names()
drone_strikes_jan <-pak_drone %>%
  janitor::clean_names()
mass_shootings_jan <-mass_shootings %>%
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





