################################################################################################################# 
                                        ##### PROJECT STARTUP  #####
################################################################################################################# 

### PACKAGES ~ GENERAL AND ALWAYS USEFUL ###
# only need to install packages once, then use library to load the packages (every time)
install.packages("xxx")
library(xxx)

################################################################################################################# 

# INSTALLING COMMON PACKAGES - do not need to install every time - check the packages list and see if they are already there
install.packages('dplyr') 

## MUST LOAD PACKAGES IN EVERY TIME 
library(dbplyr)
library(dplyr)
library(tidyverse)
library(ggplot2)

library(magrittr)
library(lubridate)

library(DBI)
library(RSQLite)

# if you need to remove a package for any reason (updating etc) 
detach()


################################################################################################################# 
# TEMPLATE FOR IMPORTING DATA

# set working directory to location of data 
data <- read.csv("File_name.csv", header = TRUE, sep = "," , na.omit("")) 
View(data)

Unique_mmsiNumbers_Oct2018 <- read.csv("Unique_mmsiNumbers_Oct2018.csv", header = TRUE, sep = "," , na.omit("")) 
View(Unique_mmsiNumbers_Oct2018)


################################################################################################################# 
# USEFUL FUNCTIONS

# different ways to remove an entire dataframe 
rm() # will remove dataframe or anything else that needs redone
remove() # does the same thing but maybe easier to remember 

# to remove a column in existing dataframe 
dataframe$column_name <- NULL 

# to rename columns 
colnames(data_frame_name)[colnames(data_frame_name) == "old_name"] <- "new_name" 

colnames(Buzz_Master)[colnames(Buzz_Master) == "buzz_rate"] <- "Buzz_Rate" 

##### BULK REMOVE COLUMNS BASED ON A CRITERIA IN THE COLUMN NAME 

library(dplyr)

# Remove columns with "low95" in their names
Buzz_Noise_Monitor_Oct2018 <- Buzz_Noise_Monitor_Oct2018 %>%
  select(-contains("_low95"))

## its a good idea ot make a backup dataframe just in case this removes something important by accident 


################################################################################################################# 
####### reformatting data ######

# First check and see what format the data are in 
str(Jan2021_Feb2021_Buzz)

# Step 1: Remove extra quotes (in this case the data were characters with "" around them which needed removed before converting)
Jan2021_Feb2021_Buzz$X.Exposure_500m. <- gsub("\"", "", Jan2021_Feb2021_Buzz$X.Exposure_500m.)

# Step 2: Convert to numeric
Jan2021_Feb2021_Buzz$X.Exposure_3k. <- as.numeric(Jan2021_Feb2021_Buzz$X.Exposure_3k.)

################################################################################################################# 

# PLOTS

ggplot(dataframe, aes(x= X_data, y = Y_data))+
  geom_bar()










