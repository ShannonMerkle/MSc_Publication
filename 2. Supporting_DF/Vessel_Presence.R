###################################################################################################################################
### VESSEL PRESENCE/ABSENCE  ### 
###################################################################################################################################
## ALL CODE NECESSARY FOR CREATING AND EDITING THE VESSEL_PRESENCE DATAFRAME FOR VESSEL POSITIVE MINUTES

# this dataframe ultimately gives you vessel positive minutes AND porpoise positive minutes based on the event start/end time
  # ADDING RECORDING EFFORT IN MINUTE FORMAT TO THIS TO GET A COMPLETELY EVEN TIME BIN

## ALSO CALCULATING VESSEL OVERLAP AND ATTACHING TO Buzz_Master 



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

## ADD RADIUS COLUMNS - 3k and 500m
Vessel_Presence$Vessel_500m <- NA
Vessel_Presence$Vessel_3k <- NA

## must remove rows with NA in UTC/datetime first
AIS_3kRadius_Master <- AIS_3kRadius_Master %>%
  filter(!is.na(datetime))

## NOW START THE LOOP TO PAIR VESSEL PRESENCE TO THE MINUTE - for 3k Vessel Exposure Radius 
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
  # ALSO DONE IN MINUTE TIME BINS - essentially gives you porpoise positive minutes based on event Click Event start/end times 

# START OF LOOP 
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

########################################################################################## 
# CALCULATING RECORDING EFFORT MINUTES - this is based on the code from Recording_Effort but adapted to minute format 

#### Part of this code comes from Recording Effort script - check there if anything does not make sense 

str(Sound_Acq_TOTAL)

Sound_Acq_TOTAL <- Sound_Acq_TOTAL %>%
  mutate(UTC_minute = floor_date(UTC, "minute"))


## maybe do not need this bit??
Recording_Effort <- Recording_Effort %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC"))

# MAIN CODE FOR BINARY RESPONSE TO RECORDING EFFORT 
Vessel_Presence$Recording_Effort <- ifelse(Vessel_Presence$datetime %in% Sound_Acq_TOTAL$UTC_minute, 1, 0)
View(Vessel_Presence)


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


################################################################################################

# Table with PROPORTION, Vessel Presence, ToD, Hour, Season, Month 
Model_table_PorpoiseProp_VesselPresence_ToD_Hour_Season_Month <- Vessel_Presence %>%
  group_by(Time_of_day, Hour, Season, Month, Vessel_3k) %>%
  summarise(
    Total_Count = n(),
    Porpoise_Present = sum(Porpoise_Event),
    .groups = "drop"  # Ensures the result is an ungrouped dataframe
  ) %>%
  mutate(
    Proportion_Porpoise_Event = Porpoise_Present / Total_Count
  )

  
################################################################################################
#### WITH VESSEL PRESENCE ACTING AS MINUTE TIME BIN DATAFRAME 
  ## Calculate the proportion of time each event spends with vessel present on a minute time bin 

# Summarize the data to calculate the vessel overlap for each Event_ID
Porpoise_Event_Vessel_Overlap <- Vessel_Presence %>%
  group_by(Event_ID) %>%
  summarize(
    Total_Minutes = n(), # Total minutes for each Event_ID
    Vessel_Presence_Minutes = sum(Vessel_3k), # Sum of Vessel_3k (vessel presence)
    .groups = "drop" # Prevents grouped output
  ) %>%
  mutate(
    Vessel_Overlap = (Vessel_Presence_Minutes / Total_Minutes) * 100, # Percentage overlap
    Vessel_Any = ifelse(Vessel_Presence_Minutes > 0, 1, 0) # Binary indicator for any vessel presence
  )
View(Porpoise_Event_Vessel_Overlap)
remove(Porpoise_Event_Vessel_Overlap)

# now join to Buzz_Master
Buzz_Master <- Buzz_Master %>%
  left_join(
    Porpoise_Event_Vessel_Overlap %>%
      select(Event_ID, Total_Minutes, Vessel_Presence_Minutes, Vessel_Overlap), 
    by = "Event_ID"
  )

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

834014/1110300 # absent 75% of the time 
276286/1110300 # present 25% of the time 

################################################################################################
## PLOTTING VESSEL PRESENCE/ABSENCE BASED ON TEMPORAL CATEGORIES 
## these plots look good but the legend scale is weird

## ToD Table 
Vessel_Presence_plot_variable_ToD <- Vessel_Presence %>%
  group_by(Time_of_day, Vessel_3k) %>%
  summarise(Count = n()) %>%
  ungroup()


# plotting vessel presence vs ToD for vessel presence LAYERED with the larger overlapping the smaller (position)
ggplot(Vessel_Presence_plot_variable_ToD , aes(x = Time_of_day, y = Count, fill = Vessel_3k)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Vessel Presence on Diurnal Cycle", 
       x = "Time of Day", 
       y = "Number of Vessels", 
       fill = "Vessels in Exposure Zone") +
  theme_minimal()

# Season table 
Vessel_Presence_plot_variable_Season <- Vessel_Presence %>%
  group_by(Season, Vessel_3k) %>%
  summarise(Count = n()) %>%
  ungroup()

# plotting vessel presence vs Season in LAYERED format 
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




