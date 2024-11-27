###################################################################################################################################
### VESSEL PRESENCE/ABSENCE  ### 
###################################################################################################################################
## ALL CODE NECESSARY FOR CREATING AND EDITING THE VESSEL_PRESENCE DATAFRAME FOR VESSEL POSITIVE MINUTES


# PACKAGES USED IN THIS NOTEBOOK 
library(dplyr)


## FIRST CREATE THE DATAFRAME 

  # Define the start and end dates
start_date <- as.POSIXct("2018-10-08 00:00:00")
end_date <- as.POSIXct("2021-12-31 23:59:59")

  # Generate a sequence of timestamps for every minute in the date range
timestamps <- seq(from = start_date, to = end_date, by = "min")

  # Create a data frame from the timestamps
Vessel_Presence <- data.frame(
  Year = format(timestamps, "%Y"),
  Month = format(timestamps, "%m"),
  Day = format(timestamps, "%d"),
  Hour = format(timestamps, "%H"),
  Minute = format(timestamps, "%M")
)

  
# CREATING A COHESIVE DATETIME COLUMN IN THE DATAFRAME
Vessel_Presence <- Vessel_Presence %>%
  mutate(datetime = as.POSIXct(paste(Year, Month, Day, Hour, Minute, sep = "-"),
                               format = "%Y-%m-%d-%H-%M"))

# Create a datetime column in AIS_3kRadius_Master (not necessary but keeps the UTC column in tact)
AIS_3kRadius_Master <- AIS_3kRadius_Master %>%
  mutate(datetime = as.POSIXct(UTC, format = "%Y-%m-%d-%H-%M"))
## this did not work properly so need to fully edit the datetime column to make the seconds :00 

# manually changing the seconds to :00 
AIS_3kRadius_Master <- AIS_3kRadius_Master %>%
  mutate(datetime = as.POSIXct(format(UTC, "%Y-%m-%d %H:%M:00")))
str(AIS_3kRadius_Master)

## ADD RADIUS COLUMNS
Vessel_Presence$Vessel_500m <- NA

## must remove rows with NA in UTC/datetime first
AIS_3kRadius_Master <- AIS_3kRadius_Master %>%
  filter(!is.na(datetime))

# Loop through each row of Vessel_Presence
for (i in 1:nrow(Vessel_Presence)) {
  # Get the datetime for the current row
  current_datetime <- Vessel_Presence$datetime[i]
  
  print(paste("Processing Time", current_datetime)) # sign of life 
  
  # Check for matches in AIS_3kRadius_Master
  if (any(AIS_3kRadius_Master$datetime == current_datetime)) {
    Vessel_Presence$Vessel_3k[i] <- 1  # Mark 1 if a match is found
  } else {
    Vessel_Presence$Vessel_3k[i] <- 0  # Mark 0 if no match is found
  }
}


################################################################################################
## NOW ATTACHING AN EVENT_ID IF APPLICABLE TO THE VESSEL PRESENCE DATAFRAME 

# load the dplyr package 

# CHECK THE FORMAT OF THE Start_Time and End_Time columns 
str(Buzz_Master)

# Convert Start_Time and End_Time in Buzz_Master to POSIXct format - if they need changed 
Buzz_Master <- Buzz_Master %>%
  mutate(Start_Time = as.POSIXct(Start_Time, format = "%Y-%m-%d %H:%M:%S"),
         End_Time = as.POSIXct(End_Time, format = "%Y-%m-%d %H:%M:%S"))

# Initialize the Event_ID column in Vessel_Presence to 0
Vessel_Presence$Event_ID <- 0


## QUICKER VERSION OF THE ORIGINAL LOOP - looking at number of events instead of number of datetime rows

# Start loop
for (j in 1:nrow(Buzz_Master)) {
  
  # set temp variables 
  event_start <- Buzz_Master$Start_Time[j]
  event_end <- Buzz_Master$End_Time[j]
  event_id <- Buzz_Master$Event_ID[j]
  
  print(paste("Processing Event ID", event_id)) # adding sign of life 
  
  # Filter rows in Vessel_Presence that fall within the event's time range
  matching_rows <- which(Vessel_Presence$datetime >= event_start & Vessel_Presence$datetime <= event_end)
  
  # Assign Event_ID to matching rows
  Vessel_Presence$Event_ID[matching_rows] <- event_id
}

################################################################################################
## NEED TO REMOVE THE TIMES WHEN AIS NOT RECORDING - should have done this earlier

### creating a variable of dates to remove 
range1 <- seq(as.Date("2019-03-29"), as.Date("2019-12-31"), by = "day")
range2 <- seq(as.Date("2020-01-01"), as.Date("2020-05-11"), by = "day")

# Combine the two ranges into one variable 'To_remove'
To_remove <- c(range1, range2)

# Print the result
print(To_remove)

str(Vessel_Presence)
Vessel_Presence$datetime <- as.POSIXct(Vessel_Presence$datetime, tz = "UTC")

# Extract the date part of the datetime column
Vessel_Presence$date_only <- as.Date(Vessel_Presence$datetime)

# Filter out rows where the date part matches any date in 'To_remove'
Vessel_Presence_filtered <- Vessel_Presence[!Vessel_Presence$date_only %in% To_remove, ]

# Optionally, remove the temporary 'date_only' column
Vessel_Presence_filtered$date_only <- NULL



################################################################################################3
## BASIC DESCRIPTIVE STATS ## 

# proportion of X time with vessels present 
# proportion of that same X time with porpoise presence (PPM)

VesselPresenceTOTAL <- Vessel_Presence %>%
  group_by(Vessel_3k) %>%
  summarise(Count = n()) %>%
  ungroup()

print(VesselPresenceTOTAL)

## modified for filtered dataframe 
1110300 # total minutes 
834014 # vessel absence 
276286 # vessel presence 

834014/1110300 # 75% of the time 
276286/1110300 # 25% of the time 

#############################
## ADDING TEMPORAL ELEMENT 

#ToD
Vessel_Presence$Hour <- as.numeric(Vessel_Presence$Hour)
str(Vessel_Presence)

Vessel_Presence$Time_of_day <-with(Vessel_Presence, ifelse(Hour %in% c(12,13, 14, 15, 16, 17), "DAY",
                                                   ifelse(Hour %in% c(18, 19, 20, 21, 22, 23), "EVENING",
                                                          ifelse(Hour %in% c(00,01, 02, 03, 04, 05), "NIGHT",
                                                                 ifelse(Hour %in% c(06, 07, 08, 09, 10, 11), "MORNING", NA))))) 
# season
Vessel_Presence$Month <- as.numeric(Vessel_Presence$Month)
str(Vessel_Presence)

Vessel_Presence$Season <- with(Vessel_Presence, ifelse(Month %in% c(12, 1, 2), "WINTER",
                                               ifelse(Month %in% c(3, 4, 5), "SPRING",
                                                      ifelse(Month %in% c(6, 7, 8), "SUMMER",
                                                             ifelse(Month %in% c(9, 10, 11), "AUTUMN", NA)))))

## now getting visual for vessel presence/absence based on diurnal cycle 

## now making a variable for Time of Day 
Vessel_Presence_plot_variable_ToD <- Vessel_Presence %>%
  group_by(Time_of_day, Vessel_3k) %>%
  summarise(Count = n()) %>%
  ungroup()


# Create the bar plot
ggplot(Vessel_Presence_plot_variable_ToD , aes(x = Time_of_day, y = Count, fill = Vessel_3k)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Vessel Presence on Diurnal Cycle", 
       x = "Time of Day", 
       y = "Number of Vessels", 
       fill = "Vessels in Exposure Zone") +
  theme_minimal()

#######

Vessel_Presence_plot_variable_Season <- Vessel_Presence %>%
  group_by(Season, Vessel_3k) %>%
  summarise(Count = n()) %>%
  ungroup()

# Create the bar plot
ggplot(Vessel_Presence_plot_variable_Season , aes(x = Season, y = Count, fill = Vessel_3k)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Vessel Presence Seasonally", 
       x = "Season", 
       y = "Number of Vessels", 
       fill = "Vessels in Exposure Zone") +
  theme_minimal()



### temporal stats - EACH TOTAL WILL BE 277560
# Morning: 176547 (63%), 101013 (36%) 
# Day: 180647 (65%), 96913 (34%)
# Evening: 232680 (83%) , 44880 (16%) 
# Night: 244140 (88%), 33480 (12%)

################################################################################################
## TRY THIS AGAIN WITH A SUBSET 

## testing the count: saving the number BEFORE EDITING the datetime column 
start_date <- as.POSIXct("2021-01-01", tz = "UTC")
end_date <- as.POSIXct("2021-02-28 23:59:59", tz = "UTC")

VesselPresence_2021Jan_2021Feb_Count_BEFORE <- Vessel_Presence %>%
  filter(datetime >= start_date & datetime <= end_date) %>%
  summarize(count_1s = sum(Vessel_3k == 1))

print(VesselPresence_2021Jan_2021Feb_Count_BEFORE)

## editing the datetime column so seconds == :00

AIS_3kRadius_Master <- AIS_3kRadius_Master %>%
  mutate(datetime = as.POSIXct(format(UTC, "%Y-%m-%d %H:%M:00")))

## create a subset of vessel presence and AIS_3kRadius_Master

Vessel_Presence_2021Jan_2021Feb <- Vessel_Presence %>% 
  filter(datetime >= start_date & datetime <= end_date)

AIS_3k_subset <- AIS_3kRadius_Master %>%
  filter(datetime >= start_date & datetime <= end_date)
View(AIS_3k_subset)


## now running on a subset of the 3k_AIS data 

# Loop through each row of Vessel_Presence
for (i in 1:nrow(Vessel_Presence_2021Jan_2021Feb)) {
  # Get the datetime for the current row
  current_datetime <- Vessel_Presence_2021Jan_2021Feb$datetime[i]
  
  print(paste("Processing Time", current_datetime)) # sign of life 
  
  # Check for matches in AIS_3kRadius_Master
  if (any(AIS_3k_subset$datetime == current_datetime)) {
    Vessel_Presence_2021Jan_2021Feb$Vessel_3k[i] <- 1  # Mark 1 if a match is found
  } else {
    Vessel_Presence_2021Jan_2021Feb$Vessel_3k[i] <- 0  # Mark 0 if no match is found
  }
}

### now get a new count for these data 

VesselPresence_2021Jan_2021Feb_Count_AFTER <- Vessel_Presence_2021Jan_2021Feb %>%
  filter(datetime >= start_date & datetime <= end_date) %>%
  summarize(count_1s = sum(Vessel_3k == 1))

print(VesselPresence_2021Jan_2021Feb_Count_BEFORE)
print(VesselPresence_2021Jan_2021Feb_Count_AFTER)

###########################################################################################################################









