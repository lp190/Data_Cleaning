# Clear everything ing first of all
rm(list = ls())

# Check directory
getwd()

# Set directory
setwd("A:/Datasets")


##### 1. Load txt data with read_lines from library(tidyverse) #####
library(tidyverse)

# 51 texts
afr <- read_lines("afr.txt")
str(afr)
afr

# isiZulu language
zulu <- read_lines("zulu.txt")  # 29 texts
str(zulu)
sort(zulu) # for sorting alphabetically

# Looks good

    ##############################################################


##### 2. Load excel file xls and xlsx using library(readxl) #####  
# load air quality excel data read_excel from library(readxl)
library(readxl)
earlwood <- read_excel("Earlwood.xls") 

# Check out the data
str(earlwood) # 8,784 obs and 15 variables
head(earlwood) # Checking some data

# Looks good
 
    ###############################################################


# Load Canadian justice data also read_excel
cjs <- read_excel("njs2.xlsx")

#check out the data
str(cjs)  # 1,867 observations and 4 variables
head(cjs)

# Looks good

    ###############################################################
  
#### 3. Load json file for checking data #####
# Load emoji data sets from reddit using read_json from library(jsonlite)

library(jsonlite)
reddit_emoji <- read_json("reddit.json") # 3226 memes

# Check out the data
str(reddit_emoji)
head(reddit_emoji)

# Looks a bit complicated, aber ich weiss man muss
# it's a heirarchical dataset

reddit_emoji[[1]][[2]]  # this checks the info on second meme of the dataset
reddit_emoji$'_default'$'2' # same function as the above code

reddit_emoji[[1]][[50]][[4]] # this checks for the author of th 50th meme 
reddit_emoji$'_default'$'50'$'author' # same function as the above code

# Find out about the 100th meme
reddit_emoji$'_default'$'100'
reddit_emoji[[1]][[100]]  # same function as the above code

# Print the authors name for the last 10 memes
for (i in 3216:3226){
  print(reddit_emoji[[1]][[i]]$author)
}





