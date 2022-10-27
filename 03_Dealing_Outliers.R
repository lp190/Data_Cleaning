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

## Identifying outliers with visualization

## Mathematically, outlier -> obs that is more than three
# standard deviations from the mean (sometimes 2.5 or 2)
# Z-score

# Easiest way to see outliers is by plotting data
## For Univariate outliers, use boxplot from ggplolot

## Use box plot to check for outliers in Brazil's
# refund values

ggplot(d2, aes(x = "refund_value", y = refund_value)) + geom_boxplot()

## Outliers look interesting, line near zero is the actual box


## Multivariate outliers (Categorical + numeric)
## now looking at the outliers for each different refund 
ggplot(d2, aes(refund_description, refund_value)) + geom_boxplot() + coord_flip() 
  # coord_flip()  flips our plot for easy reading
  # looking at the above boxplot, dissemation of parliamentary activity has extreme
  # outlier
  # Similarly, mail costs and consulting, research ...

## Multivariate outliers (Numeric + numeric); scatter plot
ggplot(d2, aes(party_nmembers, refund_value)) + geom_point()

## outliers in refund_value across the different 
# number of party members

#### WOrk with d1
ggplot(d1, aes(x = "receipt_value", y = receipt_value)) + geom_boxplot()

## Find receipt value according to receipt description
ggplot(d1, aes(receipt_description, receipt_value)) + geom_point()

##########################################################################


## Identifying rows containing outliers
## Find z-score that are away from zero, What is z score?? USed to standardize
##  Lukasz Komsta' s outliers package; score() can be used to find z score in 
## in every column; finding out the outliers column

## Working with d1 again
z_score_receipt <- scores(d1$receipt_value)
# set a threshold of z-score making a new column out of them
outl <- z_score_receipt > 3 | z_score_receipt < -3
# creating a new column if refund values are outlier
d1$outl <- outl

## Working with d2 now
zscore_refund <- scores(d2$refund_value)
# Threshold
outlier_d2 <-  zscore_refund > 3 | zscore_refund < -3
d2$outlier_d2 <- outlier_d2

#############################################
# Column of outliers are obtained in both datasets
# Time to plot outlier and non-outlier values

# D1; receipt data first
ggplot(d1, aes(receipt_description, receipt_value)) + 
  geom_boxplot() + coord_flip() + facet_wrap(~outl)

# d2: refund data second

ggplot(d2, aes(refund_description, refund_value)) +
  geom_boxplot() + coord_flip() + facet_wrap(~outlier_d2)


## Another way of doing outliers
d1_outliers <- d2[zscore_receipt > 3 | zscore_receipt < -3, ]
head(d1_outliers)

d2_outliers <- d1[z_score_refund > 3 | z_score_refund < -3, ]
View(d2_outliers)


########################################################################
# Now that outliers have been identified, time to deal with them
########################################################################

### Some strategies for handling outlier

##### 1. Omitting the row with outlier. Recommended when:

# - time contrainst to deal with them
# - large amount of data with no outliers
# - outliers due to measurement or data entry errors

#### Let's perform this in R
## Only select rows where there are no outliers acc. to the set threshold

rem_outlier_d1 <- d1[d1$outl == F,]

# 2975608 obs remaining from 3014902
# dropped 39294 obs

##### 2. Analyzing outlier and inliers separately
# subsetting data into outliers and inliers. Recommended when:

# - want to find the causes of outliers
# - outliers might be from different underlying population, for eg
#   weight in kg and gm
# - a lot of time to discover and analyze outliers

# Code to make subset of outliers and inliers

d1_outliers <- d1[z_score_receipt > 3 | z_score_receipt  < -3,] #only outliers
View(d1_outliers)

d1_inliers <- d1[z_score_receipt < 3 & z_score_receipt > -3,] # only inliers
View(d1_inliers)


##### 3. Remove and replace; imputation
# Outlier could be because of error or missing values
# maynot represent the true values
# They could then me omitted and imputed like the missing values. RECOMMENDED when:

# - Outliers due to measurement errors
# - NAs are assigned with values for eg: -99999, which are obv. outliers
## Coding time; replace outliers with NA
d1[z_score_receipt > 3 | z_score_receipt < -3, "receipt_value"] <- NA
summary(d1$receipt_value) # checks for NAs

# Converted to NA and now deal with the NAs


#####################################################################



##### Deal with outliers in d2; refund data

