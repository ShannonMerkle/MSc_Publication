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
install.packages("pscl") 

## MUST LOAD PACKAGES IN EVERY TIME 
library(DBI)
library(RSQLite)
library(dbplyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(nnet)
library(mgcv)
library(lubridate)
library(magrittr)
library(gridExtra)
library(patchwork)
library(AER)
library(car)
library(pscl)
library(suncalc)
library(tidyr)


# if you need to remove a package for any reason (updating etc) 
detach()


################################################################################################################# 
############## USEFUL THINGS TO REMEMBER ##############
################################################################################################################# 
# USEFUL FUNCTIONS

# ADDING BLANK COLUMNS
Dataframe_name$NEW_column_name <- NA

# REMOVING DATAFRAMES
rm() # will remove dataframe or anything else that needs redone
remove() # does the same thing but maybe easier to remember 

# REMOVING COLUMNS
dataframe$column_name <- NULL 

# RENAMING COLUMNS 
colnames(data_frame_name)[colnames(data_frame_name) == "old_name"] <- "new_name" 


# BULK REMOVE COLUMNS BASED ON A CRITERIA IN THE COLUMN NAME 
  library(dplyr)
  # Remove columns with "low95" in their names
Buzz_Noise_Monitor_Oct2018 <- Buzz_Noise_Monitor_Oct2018 %>%
  select(-contains("_low95"))
## its a good idea to make a backup dataframe just in case this removes something important by accident 

# BULK IDENTIFY OBJECTS IN ENVIRONMENT BASED ON NAMING PATTERN 
ls(pattern = "^Model_table_EvCounts")

# BULK REMOVE OBJECTS IN ENVIRONMENT BASED ON NAMING PATTERN - check the above first and make sure you want to remove!
rm(list = ls(pattern = "^Model_table"))


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

### THIS SAVES THE DATA IN YYYY-mm-dd HH:MM:SS format 
dataframe_name$DateTime_column_name <- as.POSIXct(dataframe_name$DateTime_column_name, format="%Y-%m-%d %H:%M:%S")

# if you want to convert to date only -- MAKE A NEW COLUMN AS YOU CANNOT REVERT THE DATA ONCE CONVERTED 
dataframe_name$NEW_DateTime_column_name <- as.POSIXct.Date(dataframe_name$DateTime_column_name, format= "%Y-%m-%d")

####################### OTHER USEFUL REFORMATTING ############################################################### 

# Remove extra quotes (in this case the data were characters with "" around them which needed removed before converting)
Jan2021_Feb2021_Buzz$X.Exposure_500m. <- gsub("\"", "", Jan2021_Feb2021_Buzz$X.Exposure_500m.)

# print the first 10 rows of a column in the dataframe, can check to see any oddities 
print(head(Dataframe_name$column_name, 10))

# Renaming Headers in Tables before exporting them 
Click_Event_Time_of_Day_Table <- Click_Event_Time_of_Day_Table %>% 
  rename(
    "Time of Day" = Var1,
    "Count of Events" = Freq
  )

### EXPORT AS PNG - need to run individually (not sure if this works for tables)
png("Annual Recording Heatmap.png", width = 800, height = 600, res = 150) 

grid.table(Click_Event_Time_of_Day_Table, rows = NULL)

dev.off()



################################################################################################################# 
############ PLOTS ##################
################################################################################################################# 

library(ggplot2)

ggplot(dataframe, aes(x= X_data, y = Y_data))+
  geom_bar()

# position: stack, dodge, identity (stat = identity?)
################################################################################################################# 
############## SAVING DATAFRAMES TO RPROJECT #################
################################################################################################################# 

## IF YOU WANT TO SAVE A DATAFRAME IN A PHYSICAL FORMAT ## then you can push this up to GitHub and will save in project folder
saveRDS(DataFrame_Name, "New_File_Name.rds")

## !! IMPORTANT: needs .rds in the file name or it will not save correctly!! 

saveRDS(Buzz_Noise_Monitor_Oct2018, "Buzz_Noise_Monitor_Oct2018.rds")
saveRDS(Sound_Acq_TOTAL, "Sound_Acquisition_TOTAL.rds")

saveRDS(Buzz_Master, "Buzz_Master_20241204.rds")


## then read the RDS back in as a dataframe: 
# can click on the object in the project files (of RStudio) or use command 
readRDS("Buzz_Noise_Monitor_Oct2018.rds")

saveRDS(Buzz_Noise_Monitor_Oct2018, "Buzz_Noise_Monitor_Oct2018_20241108.rds")

################################################################################################################# 
############## SQLITE CONNECTION #################
################################################################################################################# 

## SEE SQLITE CONNECTION FILE IN MSc_Publication R Project for detailed notes and examples 

# FIRST 
# NEED TO CONNECT TO THE DATABASE
ConnectedDatabase <- dbConnect(SQLite(), "/ entire pathway ")

  # some tests to make sure the database is connected 
  dbListTables(ConnectedDatabase)

# SECOND
## create a DATAFRAME IN RSTUDIO to work with the database TABLE 
### from here you can call it and view just like it was a csv - I am assuming everything from here is normal 
Dataframe_name <- dbReadTable(ConnectedDatabase, "table_name") 
View(Dataframe_name)

collect() ## another way to read tables into R and assign a dataframe 

################################################################################################################# 
############## GITHUB CONNECTION #################
################################################################################################################# 


# Connect to GitHub by going to Tools->Project Setup->Git connection-> Select files and Commit
# Make repository in GitHub if not already existing 
# then copy the information for existing bit not the TERMINAL window of R (Below...not the console)
# will ask for username and password but this will not work, must use TOKEN ... will likely need to create a new token in Github
## create token go to github profile..settings..scroll all the way to the bottom left..<> Developer Settings ....Create new Token
## copy token clipboard (and best to copy to text file)
## Input token code into the first popup AND INTO THE USERNAME AND PASSWORD POPUPS (even though this is really dumb)

## See GITHUB notes script for more detailed notes on pushes/pulls/merges/rebase etc

