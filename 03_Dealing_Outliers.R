#### Data Cleaning Part 3; dealing with outliers ####


# libraries used
library(tidyverse)
library(outliers)

# load data
d1 <- read_csv("deputies_dataset.csv")
d2 <- read_csv("dirty_deputies_v2.csv")

# Check out the data
str(d1)
str(d2)

