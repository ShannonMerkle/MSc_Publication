################################################################################################################# 
                                        ##### PROJECT STARTUP  #####
################################################################################################################# 

# TEMPLATE FOR IMPORTING DATA

# set working directory to location of data 
data <- read.csv("File_name.csv", header = TRUE, sep = "," , na.omit("")) 
View(data)

Unique_mmsiNumbers_Oct2018 <- read.csv("Unique_mmsiNumbers_Oct2018.csv", header = TRUE, sep = "," , na.omit("")) 
View(Unique_mmsiNumbers_Oct2018)


################################################################################################################# 
######## PACKAGES ########

### PACKAGES ~ GENERAL AND ALWAYS USEFUL ###
# only need to install packages once, then use library to load the packages (every time)
install.packages("xxx")
library(xxx)

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
############## USEFUL THINGS TO REMEMBER ##############
################################################################################################################# 
# USEFUL FUNCTIONS

# different ways to remove an entire dataframe 
rm() # will remove dataframe or anything else that needs redone
remove() # does the same thing but maybe easier to remember 

# to remove a column in existing dataframe 
dataframe$column_name <- NULL 

# to rename columns 
colnames(data_frame_name)[colnames(data_frame_name) == "old_name"] <- "new_name" 

##### BULK REMOVE COLUMNS BASED ON A CRITERIA IN THE COLUMN NAME 

library(dplyr)

# Remove columns with "low95" in their names
Buzz_Noise_Monitor_Oct2018 <- Buzz_Noise_Monitor_Oct2018 %>%
  select(-contains("_low95"))

## its a good idea to make a backup dataframe just in case this removes something important by accident 


################################################################################################################# 
####### REFORMATTING DATA ######
################################################################################################################# 

# Can check and see what format each column of data is in currently
str(dataframe_name)

# modifying the data in a column to a new format 
dataframe_name$column_name <- as.numeric(dataframe_name$column_name)
dataframe_name$column_name <- as.character(dataframe_name$column_name)
dataframe_name$column_name <- as.integer(dataframe_name$column_name)

##### DATETIME DATA #######

### THIS SAVES THE DATA IN YYYY-mm-ddn HH:MM:SS format 
dataframe_name$DateTime_column_name <- as.POSIXct(dataframe_name$DateTime_column_name, format="%Y-%m-%d %H:%M:%S")

# if you want to convert to date only -- MAKE A NEW COLUMN AS YOU CANNOT REVERT THE DATA ONCE CONVERTED 
dataframe_name$NEW_DateTime_column_name <- as.POSIXct.Date(dataframe_name$DateTime_column_name, format= "%Y-%m-%d")

####################### OTHER USEFUL REFORMATTING ############################################################### 

# Remove extra quotes (in this case the data were characters with "" around them which needed removed before converting)
Jan2021_Feb2021_Buzz$X.Exposure_500m. <- gsub("\"", "", Jan2021_Feb2021_Buzz$X.Exposure_500m.)





################################################################################################################# 
############ PLOTS ##################
################################################################################################################# 

library(ggplot2)

ggplot(dataframe, aes(x= X_data, y = Y_data))+
  geom_bar()

################################################################################################################# 
############## SAVING DATAFRAMES TO RPROJECT #################
################################################################################################################# 

## IF YOU WANT TO SAVE A DATAFRAME IN A PHYSICAL FORMAT ## then you can push this up to GitHub and will save in project folder
saveRDS(Buzz_Master, "Buzz_Master_20241015")

################################################################################################################# 
############## SQLITE CONNECTION #################
################################################################################################################# 









