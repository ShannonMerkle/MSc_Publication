#############################################################################################################################

## typical packages to load, in addition to the SQL specific packages 

## install any packages you do not already have 
library(DBI)
library(RSQLite)
library(dplyr)
library(dbplyr) # database version of dplyr 
library(tidyverse)

#############################################################################################################################
# EXAMPLE 
## first connect to the database - 
###    if the database is in the RProject folder, just connect to that folder, if not list the entire path in ""
ConnectdatabaseTest <- dbConnect(SQLite(), "/Volumes/NAtlST_EgSE/MSc_Project/MASTER\ DATA/DATABASES/CURRENT_DATABASES/MASTER_13May2020-22Sept2020_AIS_CDet_JOINED_NM.sqlite3")

# some tests to make sure the database is connected 
dbListTables(databaseTest)
dbListFields(databaseTest, "PORPOISE_EXPOSURE") 

## create a DATAFRAME IN RSTUDIO to work with the database TABLE 
### from here you can call it and view just like it was a csv - I am assuming everything from here is normal 
Porpoise_dataTest <- dbReadTable(databaseTest, "PORPOISE_EXPOSURE") 
View(Porpoise_dataTest)

collect() ## another way to read tables into R and assign a dataframe 

# remove(Porpoise_dataTest)
# remove(databaseTest)

#############################################################################################################################

# CREATE CONNECTIONS TO ALL 6 W0RKING DATABASES
connect2018Oct09 <- dbConnect(SQLite(), "/Volumes/NAtlST_EgSE/MSc_Project/MASTER\ DATA/DATABASES/CURRENT_DATABASES/MASTER_2018Oct09-2019March28_AIS_CDet_JOINED_NM.sqlite3")
connect2020May13 <- dbConnect(SQLite(), "/Volumes/NAtlST_EgSE/MSc_Project/MASTER\ DATA/DATABASES/CURRENT_DATABASES/MASTER_2020May13-2020Sept22_AIS_CDet_JOINED_NM.sqlite3")
connect2020Sept22 <- dbConnect(SQLite(), "/Volumes/NAtlST_EgSE/MSc_Project/MASTER\ DATA/DATABASES/CURRENT_DATABASES/MASTER_2020Sept22_2020Dec31_AIS_CDet_JOINED_NBM.sqlite3")
connect2021Jan01 <- dbConnect(SQLite(), "/Volumes/NAtlST_EgSE/MSc_Project/MASTER\ DATA/DATABASES/CURRENT_DATABASES/MASTER_2021Jan01_2021Feb17_AIS_CDet_JOINED_NM.sqlite3")
connect2021Feb17 <- dbConnect(SQLite(), "/Volumes/NAtlST_EgSE/MSc_Project/MASTER\ DATA/DATABASES/CURRENT_DATABASES/MASTER_2021Feb17-2021Sept13_AIS_CDet_JOINED.sqlite3")
connect2021Sept13 <- dbConnect(SQLite(), "/Volumes/NAtlST_EgSE/MSc_Project/MASTER\ DATA/DATABASES/CURRENT_DATABASES/MASTER_2021Sept13-2022Feb22_AIS_CDet_JOINED_NBM.sqlite3")

  
# TEST TO MAKE SURE THEY HAVE ALL FORMATTED TABLES - just an extra check, not necessary 
dbListTables(connect2018Oct09)
dbListFields(connect2018Oct09, "Sound_Acquisition") 

# CREATE NEW DATAFRAMES FOR EACH PORPOISE_EXPOSURE TABLE IN EACH DATABASE
Sound_Acq_Sept2021 <- dbReadTable(connect2021Sept13, "Sound_Acquisition")

View(Noise_Monitor_2018Oct09)































