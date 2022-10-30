#### Data Cleaning Part 3; dealing with outliers ####

##
setwd("C:/Users/Pandey/Desktop/Data Cleaning")

# libraries used
library(tidyverse)
library(outliers)



# 
# load data
d1 <- read_csv("deputies_dataset.csv")
d2 <- read_csv("dirty_deputies_v2.csv")

# Check out the data
str(d1)
str(d2)

## Identifying outliers with visualization

## Mathematically, outlier -> obs that are more than three
# standard deviations from the mean (sometimes 2.5 or 2)
# Z-score

# Easiest way to see outliers is by plotting data
## For Univariate outliers, use boxplot from ggplolot

## Use box plot to check for outliers in Brazil's
# refund data

ggplot(d2, aes(x = "refund_value", y = refund_value)) + geom_boxplot() 

## Outliers look interesting, line near zero is the actual box


## Multivariate outliers (Categorical + numeric)
## now looking at the outliers for each different refund 
ggplot(d2, aes(x = refund_description, y = refund_value)) + geom_boxplot() 
# does not look that great

ggplot(d2, aes(refund_description, refund_value)) + geom_boxplot() + coord_flip() 
  # coord_flip()  flips our plot for easy reading
  # looking at the above boxplot, dissemation of parliamentary activity has extreme
  # outlier
  # Similarly, mail costs and consulting, research ...

# Depending on the parties
ggplot(d2, aes(political_party, refund_value)) + geom_boxplot() + coord_flip()

## Multivariate outliers (Numeric + numeric); scatter plot
ggplot(d2, aes(party_nmembers, refund_value)) + geom_point()

## outliers in refund_value across the different 
# number of party members

#### Work with d1
ggplot(d1, aes(x = "receipt_value", y = receipt_value)) + geom_boxplot()

## Find receipt value according to receipt description
ggplot(d1, aes(receipt_description, receipt_value)) + geom_point() + coord_flip()
##########################################################################
##########################################################################


## Identifying rows containing outliers
## Find z-score that are away from zero, What is z score?? Used to standardize
##  Lukasz Komsta's outliers package; scores() can be used to find z score in 
## in every column; finding out the outliers column

## Working with d1 again; outliers for receipt_value
zscore_d1 <- scores(d1$receipt_value) # calculate zscores of all receipt_value
# create a categorical var such that zscore so -3 > that zscore >3  are outliers
out_d1 <- zscore_d1 > 3 | zscore_d1 < -3

# add the logical var into the dataset. add out_d1 into the main dataset
d1$out_d1 <- out_d1

## Now check the data out?? Check the end columns
View(d1)
############################################################
############################################################

## Working with d2 now; outliers for refund_value
zscore_d2 <- scores(d2$refund_value)

# Categorical var such that outliers (-3 > zscore > 3) true, else false
out_d2 <- zscore_d2 > 3 | zscore_d2 < -3

# mutating the categorical var to the main dataset d2
d2$out_d2 <- out_d2
############################################################
##########################################################


# Column of outliers are obtained in both datasets
# Time to plot outlier and inlier values seperately

# D1; receipt data first
ggplot(d1, aes(receipt_description, receipt_value )) + geom_boxplot() +
  facet_wrap(~out_d1) + coord_flip()  # add coord_flip for inverting, readable


# D2: refund data second; same thing as in d1

ggplot(d2, aes(refund_description, refund_value)) + geom_boxplot() + 
  facet_wrap(~out_d2) + coord_flip()


## Another way of doing only outliers

d1_out <- d1[zscore_d1 > 3 | zscore_d1 < -3, ]


d2_out <- d2[zscore_d2 > 3 | zscore_d2 < -3, ]


########################################################################
# Now that outliers have been identified, time to deal with them
########################################################################

### Some strategies for handling outliers

##### 1. Omitting the row with outliers. Recommended when:

# - time constraint to deal with them
# - large amount of data with no outliers
# - outliers due to measurement or data entry errors

#### Let's perform this in R
## Only select rows where there are no outliers acc. to the set threshold
## Dropping obs with outliers, not the best solution

no_outliers_d1 <- d1[d1$out_d1 == F,]
# 2975608 obs remaining from 3014902
# dropped 39294 obs

## Save the data with no outliers for subsequent data analysis
write.table(no_outliers_d1, file = "d1_ohne_outliers.csv",
            sep = "\t", row.names = F)

##### 2. Analyzing outlier and inliers separately
# subsetting data into outliers and inliers. Recommended when:

# - want to find the causes of outliers
# - outliers might be from different underlying population, for eg
#   weight in kg and gm
# - a lot of time to discover and analyze outliers

# Code to make subset of outliers and inliers
# outliers
d1_out <- d1[zscore_d1 > 3 | zscore_d1 < -3,] # only outliers

# inliers only
d1_in <- d1[d1$out_d1 == F,]

##### 3. Remove and replace; imputation
# Outlier could be because of error or missing values
# maynot represent the true values
# They could then be omitted and imputed like the missing values. RECOMMENDED when:

# - Outliers due to measurement errors
# - NAs are assigned with values for eg: -99999, which are obv. outliers

## Coding time; replace outliers with NA
d1[zscore_d1 >3 | zscore_d1 < -3, "receipt_value"] <- NA




summary(d1$receipt_value) # checks for NAs
# 39294 NAs
# Converted to NA and now deal with the NAs

# Maybe try and save NA -ed  dataset

# try 
## 1. write.table()  this sucks, i tried

# write.table(d1, file = "NA-ed_d1.csv",
            # sep = "\t", row.names = F)

### Works  

## 2. fwrite() from data.table package if the data is huge
## 3. library(WriteXLS)


 
# Check it out again, csv looks weird 

#####################################################################



##### Deal with outliers in d2; refund data
## There are three options.
## Option 1: eliminating the obs with outliers. 
# Coding time baby

no_outlier_d2 <- d2[d2$out_d2 == F,]

# Save the no outliers data; this sucks too
#write.table(no_outlier_d2, file = "d2_ohne_outliers.csv",
            #sep = "\t", row.names = F)

# resulting dataset has 333769 out of d2 = 339089 obs, 5329 obs dropped
## this could be used for empirical analysis

## Option 2: subsetting outliers and inliers and studying them individually
## Coding time baby
# d2 outliers
d2_out <- d2[zscore_d2 > 3 | zscore_d2 < -3,]

# d2 inliers
d2_in <- d2[d2$out_d2 == F,]

## After seperating outliers and inliers, independent study can be done, alright!


## Option 3: assigning NA to "outliers and imputing them later

d2[zscore_d2 > 3 | zscore_d2 < - 3, "refund_value"] <- NA

## Save the NA-ed file; this sucks, i tried

#write.table(d2, file = "NA-ed_d2.csv",
            #sep = "\t", row.names = F)  # don't run this, this is a fail

#############################################################################
## Now that d1 and d2 are NA-ed for outliers
#####################XXXXXXXXXX Imputing the NAs XXXXXXX#####################
#####################################################################


#### Wait, imputation does not work here at all; that was dumb
### 

