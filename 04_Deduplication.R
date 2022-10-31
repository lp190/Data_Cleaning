### Data Cleaning Part 4 ###
# Deduplication ##

# Setup
setwd("C:/Users/Pandey/Desktop/Data Cleaning")

#libraries
library(tidyverse)
library(scrubr)

# load dataset
steam <- read_csv( "steam-200K.csv", 
                   show_col_types = FALSE, col_names = F)

#add column names
names(steam) <- c("used_id", "game_title", 
                  "behaviour_name", "value", "x")

# drop the last column and converting it to tibble (as_tibble)
steam <- as_tibble(steam[,1:4])


# load ign file
ign <- read_csv("ign.csv",
                show_col_types = FALSE)

# drop the column with row numbers
ign <- as_tibble(ign[,2:11])

# same things as above
ign_1 <- select(ign, -...1)

########################################################
# Data loading is done #################################

# Duplication?
# 1. Exact duplication
# 2.partial duplication and removing them: record linkage 


## Visualizing duplication
## To plot duplicates,
### 1. create a logical vector indicating : duplicated in the df

# get the row numbers of duplicated rows; ing

duplicated_rows <- tibble(duplicated = duplicated(ign),
                             row = 1:nrow(ign)) %>%
  filter(duplicated == T)

# plotting duplicated row numbers in black lines
ggplot(duplicated_rows, aes(xintercept = row)) + 
  geom_vline(aes(xintercept = row)) +
  ggtitle("Indexes of duplicated rows") +
  coord_flip() + scale_x_reverse()
# Now that duplicated rows have been found
#################################################

## Find and remove exact duplicates
# check the head of ign

head(ign)

# partial duplicate; for eg NHL 13 with same score and release date

# count the number of duplicated row in dataset
duplicated(ign) %>%
  sum()  # 48 duplicated rows in our dataset

# get distinct rows from the dataset ign
ign_distinct <- distinct(ign) # resulting in 18577 obs down from
# 18625 obs

# double check if we removed all the duplicated rows
## how many rows removed?

nrow(ign) - nrow(ign_distinct) # shows 48; that's right

###################################################

# Removal of complete duplication; exact duplication; this must go out
# period!!!

ign_distinct <- ign %>%
  distinct()

# check the no of rows dropped
nrow(ign) - nrow(ign_distinct) # 48
#####################################

# Removal of partial duplication
# based on duplicated title
ign_distinct_title <- ign_distinct %>%
  distinct(title, .keep_all = TRUE)
#this reults in unique game titles, egal played in what kind of platform



# check the no of rows dropped
nrow(ign_distinct) - nrow(ign_distinct_title) # 5988

# Although titles are same, they are played on different platform
# I would keep them, maybe seperate them into two groups
# Experience of playing in different platforms might be different
############################################################

## Removal of partial dupli
# based on duplicated title and platform
ign_distinct_title_platform <- ign_distinct %>%
  distinct(title, platform, .keep_all = T )

# check the no of rows dropped
nrow(ign_distinct) - nrow(ign_distinct_title_platform)

# that means, even though games are same, but played in 
# different platform.




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX###
############################################################



## for stem data

dupli_row <- tibble(duplicated = duplicated(steam),
                    row = 1:nrow(steam)) %>%
                      filter(duplicated == T)

## plotting the duplicated row numbers in black lines
ggplot(dupli_row, aes(xintercept = row)) + 
  geom_vline(aes(xintercept = row)) +
  ggtitle("Indexes of duplicated rows") + 
  coord_flip() + scale_x_reverse()

## that's a shit tonnes of duplicated rows :(


head(steam)

# number of duplicates for steam
duplicated(steam) %>%
  sum() # 710 duplicates


## get completely distinct rows for stem data
steam_distinct <- steam %>%
  distinct()

 # resulting in 199290 obs down from 200000

# double check
nrow(steam) - nrow(steam_distinct)
  # checked, looks good

# Done with removing exact duplication

## Now seperate the play and purchase data
##################################################

steam_distinct_play <- steam_distinct %>%
  filter(behaviour_name == "play")

steam_distinct_purchase <- steam_distinct %>%
  filter(behaviour_name == "purchase")


nrow(steam_distinct_play) + nrow(steam_distinct_purchase) == nrow(steam_distinct)
#great!!



