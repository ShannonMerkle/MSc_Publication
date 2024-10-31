## Packages to load
library(dplyr)
library(tibble)
library(ggplot2)
library(DBI)
library(RSQLite)
library(dbplyr)
library(tidyverse)

## Set working directory, !!change path to where you want it!!
setwd("C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc")

#############################################################################################################################
## Get DB (following SQLite_Connection.R by SM)

# first connect to the database - 
###    if the database is in the RProject folder, just connect to that folder, if not list the entire path in ""
database1 <- dbConnect(SQLite(), "C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc/MASTER_2018Oct09-2019March28_AIS_CDet_JOINED_NM.sqlite3")
database2 <- dbConnect(SQLite(), "C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc/MASTER_2020May13-2020Sept22_AIS_CDet_JOINED_NM (3).sqlite3")
database3 <- dbConnect(SQLite(), "C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc/MASTER_2020Sept22_2020Dec31_AIS_CDet_JOINED_NBM.sqlite3")
database4 <- dbConnect(SQLite(), "C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc/MASTER_2021Feb17-2021Sept13_AIS_CDet_JOINED.sqlite3")
database5 <- dbConnect(SQLite(), "C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc/MASTER_2021Jan01_2021Feb17_AIS_CDet_JOINED_NM.sqlite3")
database6 <- dbConnect(SQLite(), "C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc/MASTER_2021Sept13-2022Feb22_AIS_CDet_JOINED_NBM.sqlite3")


# some tests to make sure the database is connected 
dbListTables(database1)
dbListTables(database2)
dbListFields(database1, "PORPOISE_EXPOSURE") 
dbListFields(database2, "PORPOISE_EXPOSURE") 

# create a DATAFRAME IN RSTUDIO to work with the database TABLE 
###   from here you can call it and view just like it was a csv
db1 <- dbReadTable(database1, "PORPOISE_EXPOSURE") 
db2 <- dbReadTable(database2, "PORPOISE_EXPOSURE")
db3 <- dbReadTable(database3, "PORPOISE_EXPOSURE") 
db4 <- dbReadTable(database4, "PORPOISE_EXPOSURE")
db5 <- dbReadTable(database5, "PORPOISE_EXPOSURE") 
db6 <- dbReadTable(database6, "PORPOISE_EXPOSURE")

#############################################################################################################################
## Convert timestamp to ICI 

# Remove duplicates based on ClickNumber
dfs <- list(db1, db2, db3, db4, db5, db6)


# Loop through each dataframe in the list and apply the distinct function to delete duplicates 
for (i in seq_along(dfs)) {
  dfs[[i]] <- dfs[[i]] %>%
    distinct(across(-rowid), .keep_all = TRUE)
}

# Convert UTC to POSIXct with UTC timezone
for (i in seq_along(dfs)) {
    dfs[[i]]$UTC <- ymd_hms(dfs[[i]]$UTC, tz = "UTC")
}
  

# Combine UTCMilliseconds to the POSIXct timestamp
for (i in seq_along(dfs)) {
  dfs[[i]]$combined_timestamp <- dfs[[i]]$UTC + milliseconds(dfs[[i]]$UTCMilliseconds)
}

# Ensure the data frame is sorted by the combined timestamp
for (i in seq_along(dfs)) {
  dfs[[i]] %>% arrange(combined_timestamp)
}

# Calculate the difference in UTCMilliseconds between consecutive timestamps
for (i in seq_along(dfs)) {
  dfs[[i]] <- dfs[[i]] %>%
    mutate(ICI = c(NA, diff(as.numeric(combined_timestamp)) * 1000))
}

#############################################################################################################################
## Bin into events
###   A new event occurs when the ICI is > 10 mins. 
# Initialize the first event
for (i in seq_along(dfs)) {
  dfs[[i]]$event <- 1 
}

# Define the threshold in UTCMilliseconds (10 minutes = 600,000 UTCMilliseconds)
threshold <- 10 * 60 * 1000

# Assign events based on the threshold
for (i in seq_along(dfs)) {
  for (j in 2:nrow(dfs[[i]])) {
    if (!is.na(dfs[[i]]$ICI[j]) && dfs[[i]]$ICI[j] > threshold) {
      dfs[[i]]$event[j] <- dfs[[i]]$event[j - 1] + 1
    } else {
      dfs[[i]]$event[j] <- dfs[[i]]$event[j - 1]
    }
  }
}

#############################################################################################################################
# Generate binary buzz calls based on < 10 ms ICI
for (i in seq_along(dfs)) {
  dfs[[i]] <- dfs[[i]] %>%
    mutate(buzz = ifelse(ICI < 10 & ICI > 0, 1, 0),
           scan = ifelse(ICI > 10 & ICI < 200, 1, 0), 
           present = ifelse(ICI > 200, 1, 0)
         )
}


# write each df as a csv 
#list of df names for printing
csv_names <- c("df1.csv", "df2.csv", "df3.csv", "df4.csv", "df5.csv", "df6.csv")  

# Loop through the list and write each dataframe to a CSV
for (i in seq_along(dfs)) {
  write.csv(dfs[[i]], file = csv_names[i], row.names = FALSE)
}

# Create db_buzz by grouping db and summarising by events
for (i in seq_along(dfs)) {
  dfs[[i]] <- dfs[[i]] %>%
    group_by(event) %>%
      summarize(
      total_clicks = n(),  # Count total data points per encounterID
      buzz_calls = sum(buzz == 1),  # Count number of 1s per encounterID
      buzz_rate = buzz_calls / total_clicks, # Calculate percentage of 1s
      scan_calls = sum(scan == 1),  # Count number of 1s per scan call
      scan_rate = scan_calls / total_clicks, # Calculate percentage of scans
      presence_calls = sum(present == 1), #Count of sum 
      Exposure_3k = max(Exposure_3k),  # Use max to determine if any value is 1
      Exposure_500m = max(Exposure_500m), # Use max to determine if any value is 1
      first_time = min(UTC), 
      last_time = max(UTC)
  ) %>%
  ungroup()
}

# Remove events with fewer than 6 rows
for (i in seq_along(dfs)) {
  dfs[[i]] <- filter(dfs[[i]], total_clicks > 6)
}

## save df
# write each df as a csv 
#list of df names for printing
csv_names2 <- c("df1_buzz.csv", "df2_buzz.csv", "df3_buzz.csv", "df4_buzz.csv", "df5_buzz.csv", "df6_buzz.csv")  

# Loop through the list and write each dataframe to a CSV
for (i in seq_along(dfs)) {
  write.csv(dfs[[i]], file = csv_names2[i], row.names = FALSE)
}


# Bind rows together
combined_df_buzz <- do.call(rbind, dfs)
write.csv(combined_df_buzz, "combined_df_buzz.csv", row.names = FALSE)

#############################################################################################################################
## get master df
db_master <- read.csv("C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc/buzz_master.csv")
db_master$Exposure_3k <- as.factor(db_master$Exposure_3k)
db_master$Exposure_500m <- as.factor(db_master$Exposure_500m)

db_exposure <- count(db_master, Exposure_500m)
db_exposure2 <- count(db_master, Exposure_3k)


## Model
buzz_model1 <- lm(buzz_rate ~ Exposure_500m + Exposure_3k, data = db_master)
summary(buzz_model1) ## not overdispersed

buzz_model2 <- lm(buzz_rate ~ Exposure_3k, data = db_master)
summary(buzz_model2)

buzz_model3 <- lm(buzz_rate ~ Exposure_500m, data = db_master)
summary(buzz_model3)


plot(buzz_model1) ## looks okay 

## visualise
db_500m_plot <- ggplot(db_master, aes(x = Exposure_500m, y = buzz_rate)) +
  geom_boxplot() +
  labs(x = "Vessel Presence (within 500m)", y = "Buzz Rate") +
  theme_minimal()

## Print to working directory:
tiff('db_buzz_test_signif_500m.tiff', units="in", width=6, height=4, res=1000, compression = 'lzw')

db_500m_plot

dev.off()
