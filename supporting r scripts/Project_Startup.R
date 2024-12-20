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
library(nnet)
library(mgcv)

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
## create token go to github profile..settings..scroll all the way to the bottom left..<> Developer Settings ....Create new Tokem
##copy token clipboard (and best to copy to text file)
## Input token code into the first popup AND INTO THE USERNAME AND PASSWORD POPUPS (even though this is really dumb)

## TO SAVE DATAFRAME INTO GITHUB
  # FIRST
    ## save dataframe as physical RDS file -- will go into Rproject 
saveRDS(Buzz_Master, "Buzz_Master_20241015")
  # SECOND
    ## Then you should see it in the Git tab and be able to stage-commit-Git Push 

