###################################################################################################################################
### GETTING NOISE MONITOR DATA FOR THE OCT2018 DATABASE AS A PROXY FOR ALL EVENTS ### 
###################################################################################################################################

# SCRIPT SUMMARY 

# FIRST:
#   Creates Buzz Noise Monitor from a subset of Buzz_Master using Noise_Monitor_2018Oct09

###################################################################################################################################
## RUN A DATE TIME FORMAT CODE FIRST TO GET EVERYTHING IN MATCHING FORMATS TO CORRELATE

# Ensure Noise_Monitor$UTC is POSIXct
Noise_Monitor_2018Oct09$UTC <- as.POSIXct(Noise_Monitor_2018Oct09$UTC, format="%Y-%m-%d %H:%M:%S")

# Ensure Buzz_Events$Start_Event and Buzz_Events$End_Event are POSIXct - I THINK THIS IS A BUZZ_MASTER SUBSET 
Oct2018_March2019_Buzz$Start_Time <- as.POSIXct(Oct2018_March2019_Buzz$Start_Time, format="%Y-%m-%d %H:%M:%S")
Oct2018_March2019_Buzz$End_Time <- as.POSIXct(Oct2018_March2019_Buzz$End_Time, format="%Y-%m-%d %H:%M:%S")


###################################################################################################################################
####### EVENT_ID CORRELATED TO NOISE MONITOR ###############

## this code finds any noise monitor data that falls between each event ID (both ambient and vessel presence) and correlated 
#   THIS TAKES A LITTLE WHILE 

# Create new dataframe to add data into 
Buzz_Noise_Monitor_Oct2018_NEW <- data.frame()
remove(Buzz_Noise_Monitor_Oct2018_NEW)


## setting up the loop
for (i in 1:nrow(Buzz_Master_Subset_Oct2018)) {
  
  # Get the current event's start and end times
  start_time <- Buzz_Master_Subset_Oct2018$Start_Time[i]
  end_time <- Buzz_Master_Subset_Oct2018$End_Time[i]
  
  # Find rows in Noise_Monitor that have UTC between start_time and end_time
  event_noise_temp <- Noise_Monitor_2018Oct09[Noise_Monitor_2018Oct09$UTC >= start_time & Noise_Monitor_2018Oct09$UTC <= end_time, ]

  # If there are matching rows, add Event_ID_Number to them
  if (nrow(event_noise_temp) > 0) {
    event_noise_temp$Event_ID <- Buzz_Master_Subset_Oct2018$Event_ID[i]
    
    # ADDING SIGN OF LIFE 
    print(paste("Processing Event ID:", Buzz_Master_Subset_Oct2018$Event_ID[i]))
    
    # Append the matching rows to the result dataframe
    Buzz_Noise_Monitor_Oct2018_NEW <- rbind(Buzz_Noise_Monitor_Oct2018_NEW, event_noise_temp)
  }
}

# View the new dataframe
View(Buzz_Noise_Monitor_Oct2018_NEW)

###################################################################################################################################
################ CORRELATE COLUMNS FOR EXPOSURE 500M OR NO EXPOSURE ##################

# Perform a left join to correlate Event_ID and copy the Exposure_500m value
Buzz_Noise_Monitor_Oct2018 <- Buzz_Noise_Monitor_Oct2018 %>%
  left_join(Oct2018_March2019_Buzz %>% select(Event_ID, Exposure_500m), by = "Event_ID") 

### now create a categorical column for "Ambient" or "Vessel_Noise" based on Exposure Columns 

# Add the Noise_Exposure column based on the criteria
Buzz_Noise_Monitor_Oct2018 <- Buzz_Noise_Monitor_Oct2018 %>%
  mutate(Noise_Exposure = case_when(
    Exposure_500m == 1 ~ "Vessel_Noise",
    Exposure_500m == 0 & Exposure_3k == 0 ~ "Ambient"
  ))

View(Buzz_Noise_Monitor_Oct2018)

####### ADDING THESE COLUMNS TO THE NOISE MONITOR EVENT DATA FOR MODELING (from Buzz_Master_&AIS script)
# Merging Vessel_Count and Average_Speed columns from Buzz_Master into Vessel_Presence by Event_ID
Buzz_Noise_Monitor_Oct2018 <- merge(Buzz_Noise_Monitor_Oct2018, 
                                    Buzz_Master[ , c("Event_ID", "Vessel_Count", "Average_Speed")], 
                                    by = "Event_ID", 
                                    all.x = TRUE)

###################################################################################################################################

## Creating single median column for all bands <2000Hz

# Assuming your dataframe is called `df` and the relevant columns are A, B, and C


Buzz_Noise_Monitor_Oct2018$Median_2000Hz <- apply(Buzz_Noise_Monitor_Oct2018[, c("ThirdOctave_447_561_median", 
                                                                                 "ThirdOctave_561_709_median",
                                                                                 "ThirdOctave_709_894_median",
                                                                                 "ThirdOctave_894_1118_median",
                                                                                 "ThirdOctave_1118_1414_median",
                                                                                 "ThirdOctave_1414_1788_median",
                                                                                 "ThirdOctave_1788_2236_median")], 
                                                  1, median, na.rm = TRUE)

saveRDS(Noise_Monitor_2018Oct09, "Noise_Monitor_2018Oct09.rds")
saveRDS(Noise_Monitor_2020May12, "Noise_Monitor_2020May12.rds")
saveRDS(Noise_Monitor_2021Jan01, "Noise_Monitor_2021Jan01.rds")


###################################################################################################################################

## MUST TAKE A LOGARITHIC MEAN INSTEAD OF STANDARD GEOMETRIC MEAN - dB are in log scale 
# Load in dataframes from the Noise Monitor in SQLite connection

# SET A VALUE FOR ALL PATTERNS TO REMOVE FROM NOISE MONITOR DATAFRAMES
patterns_to_remove <- c("UTCMilliseconds", "PCLocalTime", "PCTime", 
                        "UpdateOf", "Channel", "UID", 
                        "ChannelBitmap", "SequenceBitmap",
                        "_Min", "_Max", "_mean", "_17888_22360", "_14142_17888", 
                        "_11180_14142", "_8944_11180", "_7099_8944", 
                        "_5612_7099", "_4472_5612", "_3549_4472", 
                        "_2806_3549", "_2236_2806")

# REMOVING FROM OCT 2018 DATAFRAME
Noise_Monitor_2018Oct09 <- Noise_Monitor_2018Oct09 %>%
  select(-matches(paste(patterns_to_remove, collapse = "|"), ignore.case = TRUE))
View(Noise_Monitor_2018Oct09)

# REMOVING FROM MAY 2020 DATAFRAME
Noise_Monitor_2020May13 <- Noise_Monitor_2020May13 %>%
  select(-matches(paste(patterns_to_remove, collapse = "|"), ignore.case = TRUE))
View(Noise_Monitor_2020May13)

# REMOVING FROM JAN 2021 DATAFRAME
Noise_Monitor_2021Jan01 <- Noise_Monitor_2021Jan01  %>%
  select(-matches(paste(patterns_to_remove, collapse = "|"), ignore.case = TRUE))
View(Noise_Monitor_2021Jan01)


#########
## GETTING THE LOG MEDIAN OF <2000Hz COLUMNS - median columns 

# Define the relevant third-octave columns
ThirdOctave_2000Hz_median <- c("ThirdOctave_447_561_median", 
                          "ThirdOctave_561_709_median", 
                          "ThirdOctave_709_894_median", 
                          "ThirdOctave_894_1118_median", 
                          "ThirdOctave_1118_1414_median", 
                          "ThirdOctave_1414_1788_median", 
                          "ThirdOctave_1788_2236_median")

# Convert dB to linear scale for each column - FOR ALL 3 NOISE DF 
linear_MEDIAN_2018Oct09 <- 10^(Noise_Monitor_2018Oct09[ThirdOctave_2000Hz_median] / 10)
linear_MEDIAN_2020May13 <- 10^(Noise_Monitor_2020May13[ThirdOctave_2000Hz_median] / 10)
linear_MEDIAN_2021Jan01 <- 10^(Noise_Monitor_2021Jan01[ThirdOctave_2000Hz_median] / 10)

# Calculate the mean and median in linear scale - FOR ALL 3 NOISE DF 
linear_median2_2018Oct09 <- apply(linear_MEDIAN_2018Oct09, 1, median, na.rm = TRUE)
linear_median2_2020May13 <- apply(linear_MEDIAN_2020May13, 1, median, na.rm = TRUE)
linear_median2_2021Jan01 <- apply(linear_MEDIAN_2021Jan01, 1, median, na.rm = TRUE)

# Convert back to log dB and add into dataframe - FOR ALL 3 NOISE DF 
Noise_Monitor_2018Oct09$LogMedian_2000Hz <- 10 * log10(linear_median2_2018Oct09)
Noise_Monitor_2020May13$LogMedian_2000Hz <- 10 * log10(linear_median2_2020May13)
Noise_Monitor_2021Jan01$LogMedian_2000Hz <- 10 * log10(linear_median2_2021Jan01)



###############
## NOW DO THE SAME FOR LOWER95 
# define the columns 
ThirdOctave_2000Hz_low95 <- c("ThirdOctave_447_561_low95", 
                               "ThirdOctave_561_709_low95", 
                               "ThirdOctave_709_894_low95", 
                               "ThirdOctave_894_1118_low95", 
                               "ThirdOctave_1118_1414_low95", 
                               "ThirdOctave_1414_1788_low95", 
                               "ThirdOctave_1788_2236_low95")

# Convert dB to linear scale for each column - FOR ALL DF
linear_medainLOW95_2018Oct09 <- 10^(Noise_Monitor_2018Oct09[ThirdOctave_2000Hz_low95] / 10)
linear_medainLOW95_2020May13 <- 10^(Noise_Monitor_2020May13[ThirdOctave_2000Hz_low95] / 10)
linear_medainLOW95_2021Jan01 <- 10^(Noise_Monitor_2021Jan01[ThirdOctave_2000Hz_low95] / 10)

# Calculate the mean and median in linear scale - FOR ALL DF
linear_median2LOW95_2018Oct09 <- apply(linear_medainLOW95_2018Oct09, 1, median, na.rm = TRUE)
linear_median2LOW95_2020May13 <- apply(linear_medainLOW95_2020May13, 1, median, na.rm = TRUE)
linear_median2LOW95_2021Jan01 <- apply(linear_medainLOW95_2021Jan01, 1, median, na.rm = TRUE)

# Convert back to log dB and add into dataframe - FOR ALL DF
Noise_Monitor_2018Oct09$LogMedian_low95_2000Hz <- 10 * log10(linear_median2LOW95_2018Oct09)
Noise_Monitor_2020May13$LogMedian_low95_2000Hz <- 10 * log10(linear_median2LOW95_2020May13)
Noise_Monitor_2021Jan01$LogMedian_low95_2000Hz <- 10 * log10(linear_median2LOW95_2021Jan01)

View(Noise_Monitor_2018Oct09)

#################
## SAME FOR HIGH95 - FOR ALL DF
ThirdOctave_2000Hz_high95 <- c("ThirdOctave_447_561_high95", 
                              "ThirdOctave_561_709_high95", 
                              "ThirdOctave_709_894_high95", 
                              "ThirdOctave_894_1118_high95", 
                              "ThirdOctave_1118_1414_high95", 
                              "ThirdOctave_1414_1788_high95", 
                              "ThirdOctave_1788_2236_high95")

# Convert dB to linear scale for each column - FOR ALL DF
linear_medianHIGH95_2018Oct09 <- 10^(Noise_Monitor_2018Oct09[ThirdOctave_2000Hz_high95] / 10)
linear_medianHIGH95_2020May13 <- 10^(Noise_Monitor_2020May13[ThirdOctave_2000Hz_high95] / 10)
linear_medianHIGH95_2021Jan01 <- 10^(Noise_Monitor_2021Jan01[ThirdOctave_2000Hz_high95] / 10)

# Calculate the mean and median in linear scale - FOR ALL DF
linear_median2HIGH95_2018Oct09 <- apply(linear_medianHIGH95_2018Oct09, 1, median, na.rm = TRUE)
linear_median2HIGH95_2020May13 <- apply(linear_medianHIGH95_2020May13, 1, median, na.rm = TRUE)
linear_median2HIGH95_2021Jan01 <- apply(linear_medianHIGH95_2021Jan01, 1, median, na.rm = TRUE)

# Convert back to log dB and add into dataframe
Noise_Monitor_2018Oct09$LogMedian_high95_2000Hz <- 10 * log10(linear_median2HIGH95_2018Oct09)
Noise_Monitor_2020May13$LogMedian_high95_2000Hz <- 10 * log10(linear_median2HIGH95_2020May13)
Noise_Monitor_2021Jan01$LogMedian_high95_2000Hz <- 10 * log10(linear_median2HIGH95_2021Jan01)


View(Noise_Monitor_2018Oct09)
View(Noise_Monitor_2020May13)
View(Noise_Monitor_2021Jan01)

##### ONCE ALL OF THIS IS DONE - SAVE THE DANG RDS FILES 

saveRDS(Noise_Monitor_2018Oct09, "Noise_Monitor_2018Oct09.rds")
saveRDS(Noise_Monitor_2020May13, "Noise_Monitor_2020May13.rds")
saveRDS(Noise_Monitor_2021Jan01, "Noise_Monitor_2021Jan01.rds")


# filtering out NA values - WRITE CODE TO IDENTIFY/PRINT BEFORE REMOVING 
Noise_Monitor_2018Oct09 <- Noise_Monitor_2018Oct09 %>%
  filter(!is.na(UTC) & !is.na(LogMedian_2000Hz))




