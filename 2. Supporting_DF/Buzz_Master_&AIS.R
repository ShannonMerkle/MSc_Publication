###################################################################################################################################
### GENERAL BUZZ MASTER EDITING AND AIS JOIN  ### 
###################################################################################################################################

## NOTE: this series of code needs the raw AIS_3k csv file (called AIS_3kRadius_2018Oct_2019March)
## also needs the unique mmsi numbers csv to lookup vessel type 

## NOTE: Buzz_Master_OLD is the dataframe before rerunning to correct criteria, Buzz_Master is the NEWEST, corrected version 
## combined_df_buzz is currently called Buzz_Master but that may need to change based on the larger database 

Buzz_Master <- read.csv("combined_df_buzz.csv", header = TRUE, sep = ",")
View(Buzz_Master)

## remove the first events (from each database) which have NA for everything 
Buzz_Master <- Buzz_Master %>%
  filter(!is.na(buzz_calls))

## Ensuring that Start_Time and End_Time columns are in datetime format 
Buzz_Master$Start_Time <- as.POSIXct(Buzz_Master$Start_Time, format="%Y-%m-%d %H:%M:%S")
Buzz_Master$End_Time <- as.POSIXct(Buzz_Master$End_Time, format="%Y-%m-%d %H:%M:%S")

str(Buzz_Master)

## Re-generating unique Event_ID numbers for each event based on Start_Time
Buzz_Master <- Buzz_Master %>%
  arrange(Start_Time) %>% 
  mutate(Event_ID = row_number())  # Generate a new Event_ID starting from 1

###################################################################################################################################
### NOW ADDING SOME OF THE ADDITIONAL COLUMNS 

### Creating a column that gives the duration of the click event in seconds
Buzz_Master$Click_Train_Length <- as.numeric(difftime(Buzz_Master$End_Time, Buzz_Master$Start_Time, units = "secs"))

## TO DISTINGUISH CLICK TRAINS WHICH HAVE BUZZES FROM THOSE THAT DO NOT 
Buzz_Master$Buzz_Train <- ifelse(Buzz_Master$Buzz_Clicks >= 6, '1', '0')

# now do the same for scan clicks
Buzz_Master$Scan_Train <- ifelse(Buzz_Master$Scan_Clicks >= 6, '1', '0')

## PROPORTION OF CLICKS WHICH ARE BUZZES IN EACH TRAIN - did not do this second time 

Buzz_Master$Buzz_to_Total_Percentage <- Buzz_Master$Buzz_Rate*100

# rounding decimal places to 2 
Buzz_Master$Buzz_to_Total_Percentage <- round(Buzz_Master$Buzz_to_Total_Percentage, 2)

## Creating an additional column to categorize the MAXIMUM type (scan trains have NO or <6 buzz clicks)
Buzz_Master <- Buzz_Master %>%
  mutate(Click_Train_Type = case_when(
    Buzz_Train == 1 ~ "Buzz",
    Scan_Train == 1 & Buzz_Train == 0 ~ "Scan",
    Buzz_Train == 0 & Scan_Train == 0 ~ "Presence"
  ))

## Now doing the same to create a column to categorize vessel presence
Buzz_Master <- Buzz_Master %>%
  mutate(Vessel_Exposure = case_when(
    Exposure_3k == 1 & Exposure_500m == 0 ~ "Exposure",
    Exposure_500m == 1 & Exposure_3k == 1 ~ "Max Exposure",
    Exposure_3k == 0 & Exposure_500m == 0 ~ "No Exposure"
   ))

Vessel_Exposure_Event_Counts <- table(Buzz_Master$Vessel_Exposure)
View(Vessel_Exposure_Event_Counts)

###################################################################################################################################
####### BRIEF COUNT STATISTICS 

## Buzz vs Non-Buzz trains 
countBuzz_YES <- sum(Buzz_Master$Buzz_Train == 'YES', na.rm = TRUE)
countBuzz_YES

countBuzz_NO <- sum(Buzz_Master$Buzz_Train == 'NO', na.rm = TRUE)
countBuzz_NO

## Count of Exposures and NON-exposures 
countExposures_3k <- sum(Buzz_Master$Exposure_3k == 1)
countExposures_3k

countNO_Exposure <- sum(Buzz_Master$Exposure_500m == 0)
countNO_Exposure

###################################################################################################################################
## MMSI NUMBER CONNECTION
#### This is working on the entire database, however the AIS data does not cover this entire dataset 

# FIRST - Ensure datetime format - needed to update the format of these to include UTC since the AIS did 
Buzz_Master$Start_Time <- as.POSIXct(Buzz_Master$Start_Time, format="%Y-%m-%d %H:%M:%S")
Buzz_Master$End_Time <- as.POSIXct(Buzz_Master$End_Time, format="%Y-%m-%d %H:%M:%S")
AIS_3kRadius_2018Oct_2019March$UTC <- as.POSIXct(AIS_3kRadius_2018Oct_2019March$UTC, format="%Y-%m-%d %H:%M:%S")

str(Buzz_Master)
print(head(Buzz_Master$End_Time, 10))

print(head(AIS_3kRadius_2018Oct_2019March$UTC, 10))

## make sure mmsiNumbers are in numeric format 
AIS_3kRadius_2018Oct_2019March$mmsiNumber <- as.numeric(AIS_3kRadius_2018Oct_2019March$mmsiNumber)

############################## NOW START THE LOOP TO LOOK FOR MMSI NUMBERS THAT MATCH ##############################

# Loop through each event in Buzz_Master
for (i in 1:nrow(Buzz_Master)) {
  
  ## these are great to give evidence that the code is actually doing something -- like a sign of life code 
  print(paste("Processing Event_ID:", Buzz_Master$Event_ID[i]))
  
  if (Buzz_Master$Exposure_3k[i] == 1) {
    
    vessels_in_event <- unique(AIS_3kRadius_2018Oct_2019March$mmsiNumber[
      AIS_3kRadius_2018Oct_2019March$UTC >= Buzz_Master$Start_Time[i] &
        AIS_3kRadius_2018Oct_2019March$UTC <= Buzz_Master$End_Time[i]
    ])
    
    print(paste("Found vessels:", toString(vessels_in_event)))  # Check found vessels -- another sign of life
    
    if (length(vessels_in_event) > 0) {
      for (j in 1:length(vessels_in_event)) {
        column_name <- paste0("mmsiNumber_", j)
        Buzz_Master[i, column_name] <- vessels_in_event[j]
      }
    } else {
      Buzz_Master[i, "mmsiNumber_1"] <- NA
    }
    
  } else {
    Buzz_Master[i, "mmsiNumber_1"] <- NA
  }
}

# Fill any empty mmsiNumber columns with NA
Buzz_Master[is.na(Buzz_Master)] <- NA

###################################################################################################################################
#################### MAKING AN AIS EVENTS DATA FRAME  ########################################
# this is not completely necessary but may be useful to have so remake this 

# Initialize an empty dataframe for storing the filtered AIS data
AIS_Events <- data.frame()

############## SIMILAR AS ABOVE BUT INSTEAD ADDING DATA INTO NEW DATAFRAME WITH ALL AIS INFO
# Loop through Buzz_Master where Exposure_3k == 1
for (i in 1:nrow(Buzz_Master)) {
  # Print the current row being processed
  print(paste("Processing row:", i, "Exposure_3k:", Buzz_Master$Exposure_3k[i]))
  
  # Only process events where Exposure_3k is 1
  if (Buzz_Master$Exposure_3k[i] == 1) {
    # Extract Start_Time, End_Time, and Event_ID for the current row
    start_time <- Buzz_Master$Start_Time[i]
    end_time <- Buzz_Master$End_Time[i]
    event_id <- Buzz_Master$Event_ID[i]
    
    print(paste("Processing Event_ID:", event_id)) # sign of life 
    
    # Filter AIS data that falls between Start_Time and End_Time
    ais_filtered <- AIS_3kRadius_2018Oct_2019March[AIS_3kRadius_2018Oct_2019March$UTC >= start_time & 
                                                     AIS_3kRadius_2018Oct_2019March$UTC <= end_time, ]
    
    print(paste("Matches found for Event_ID", event_id, ":", nrow(ais_filtered))) # sign of success 
    
    # Check if ais_filtered has any rows before adding Event_ID and appending
    if (nrow(ais_filtered) > 0) {
      # Add a column to indicate the corresponding Event_ID
      ais_filtered$Event_ID <- event_id
      
      # Append the filtered data to AIS_Events dataframe
      AIS_Events <- rbind(AIS_Events, ais_filtered)
    }
  }
}
  
View(AIS_Events)

###################################################################################################################################
#### NOW ADDING IN VESSEL TYPE PER MMSI NUMBER 

## because these have NA in the columns they ALL need to be CONVERT INTO CHARACTERS FOR NA - integers and numbers will not work 
Buzz_Master$mmsiNumber_1 <- as.character(Buzz_Master$mmsiNumber_1)
Buzz_Master$mmsiNumber_2 <- as.character(Buzz_Master$mmsiNumber_2)
Buzz_Master$mmsiNumber_3 <- as.character(Buzz_Master$mmsiNumber_3)
Buzz_Master$mmsiNumber_4 <- as.character(Buzz_Master$mmsiNumber_4)
Buzz_Master$mmsiNumber_5 <- as.character(Buzz_Master$mmsiNumber_5)
Buzz_Master$mmsiNumber_6 <- as.character(Buzz_Master$mmsiNumber_6)
Buzz_Master$mmsiNumber_7 <- as.character(Buzz_Master$mmsiNumber_7)
Buzz_Master$mmsiNumber_8 <- as.character(Buzz_Master$mmsiNumber_8)
Buzz_Master$mmsiNumber_9 <- as.character(Buzz_Master$mmsiNumber_9)

Unique_mmsiNumbers_Oct2018$mmsiNumber <- as.character(Unique_mmsiNumbers_Oct2018$mmsiNumber)

# check conversions 
str(Buzz_Master)
str(Unique_mmsiNumbers_Oct2018)
    
## FIRST add in the new column - did not work without this for some reason -- CHECK AND SEE IF NEEDS MORE COLUMNS WITH NEW DATA 
Buzz_Master$mmsiNumber_Type_1 <- NA
Buzz_Master$mmsiNumber_Type_2 <- NA
Buzz_Master$mmsiNumber_Type_3 <- NA
Buzz_Master$mmsiNumber_Type_4 <- NA
Buzz_Master$mmsiNumber_Type_5 <- NA
Buzz_Master$mmsiNumber_Type_6 <- NA
Buzz_Master$mmsiNumber_Type_7 <- NA
Buzz_Master$mmsiNumber_Type_8 <- NA
Buzz_Master$mmsiNumber_Type_9 <- NA


## THEN START THE LOOP -- NEEDS TO BE RUN FOR EACH MMSI NUMBER COLUMNS 

####### mmsiNumber_1
# Loop through each row in Buzz_Master to find vessel categories
for (i in 1:nrow(Buzz_Master)) {
  
  # Check mmsiNumber_1
  if (!is.na(Buzz_Master$mmsiNumber_1[i])) {
    
    #adding in sign of life - nice to see that is is processing live and how many it is processing through 
    print(paste("Processing mmsiNumber:", Buzz_Master$mmsiNumber_1[i]))
    
    # assign a temporary variable to the current mmsiNumber_1 integer 
    mmsiTemp <- Buzz_Master$mmsiNumber_1[i]
    
    # Find the matching mmsiNumber in Unique_mmsiNumbers_Oct2018 - assinging another temporary variable
    vessel_typeTemp <- Unique_mmsiNumbers_Oct2018$Vessel_Category[Unique_mmsiNumbers_Oct2018$mmsiNumber == mmsiTemp]
    
    # If a match is found, assign it to mmsiNumber_Type_1 in Buzz_Master
    if (length(vessel_typeTemp) > 0) {
      Buzz_Master$mmsiNumber_Type_1[i] <- vessel_typeTemp
    } else {
      Buzz_Master$mmsiNumber_Type_1[i] <- NA  # If no match, assign NA
    }
  }
}  

####### mmsiNumber_2
# Loop through each row in Buzz_Master to find vessel categories
for (i in 1:nrow(Buzz_Master)) {
  
  # Check mmsiNumber_1
  if (!is.na(Buzz_Master$mmsiNumber_2[i])) {
    
    #adding in sign of life - nice to see that is is processing live and how many it is processing through 
    print(paste("Processing mmsiNumber:", Buzz_Master$mmsiNumber_2[i]))
    
    # assign a temporary variable to the current mmsiNumber_1 integer 
    mmsiTemp <- Buzz_Master$mmsiNumber_2[i]
    
    # Find the matching mmsiNumber in Unique_mmsiNumbers_Oct2018 - assinging another temporary variable
    vessel_typeTemp <- Unique_mmsiNumbers_Oct2018$Vessel_Category[Unique_mmsiNumbers_Oct2018$mmsiNumber == mmsiTemp]
    
    # If a match is found, assign it to mmsiNumber_Type_1 in Buzz_Master
    if (length(vessel_typeTemp) > 0) {
      Buzz_Master$mmsiNumber_Type_2[i] <- vessel_typeTemp
    } else {
      Buzz_Master$mmsiNumber_Type_2[i] <- NA  # If no match, assign NA
    }
  }
}  

####### mmsiNumber_3
# Loop through each row in Buzz_Master to find vessel categories
for (i in 1:nrow(Buzz_Master)) {
  
  # Check mmsiNumber_1
  if (!is.na(Buzz_Master$mmsiNumber_3[i])) {
    
    #adding in sign of life - nice to see that is is processing live and how many it is processing through 
    print(paste("Processing mmsiNumber:", Buzz_Master$mmsiNumber_3[i]))
    
    # assign a temporary variable to the current mmsiNumber_1 integer 
    mmsiTemp <- Buzz_Master$mmsiNumber_3[i]
    
    # Find the matching mmsiNumber in Unique_mmsiNumbers_Oct2018 - assinging another temporary variable
    vessel_typeTemp <- Unique_mmsiNumbers_Oct2018$Vessel_Category[Unique_mmsiNumbers_Oct2018$mmsiNumber == mmsiTemp]
    
    # If a match is found, assign it to mmsiNumber_Type_1 in Buzz_Master
    if (length(vessel_typeTemp) > 0) {
      Buzz_Master$mmsiNumber_Type_3[i] <- vessel_typeTemp
    } else {
      Buzz_Master$mmsiNumber_Type_3[i] <- NA  # If no match, assign NA
    }
  }
}  

####### mmsiNumber_4
# Loop through each row in Buzz_Master to find vessel categories
for (i in 1:nrow(Buzz_Master)) {
  
  # Check mmsiNumber_1
  if (!is.na(Buzz_Master$mmsiNumber_4[i])) {
    
    #adding in sign of life - nice to see that is is processing live and how many it is processing through 
    print(paste("Processing mmsiNumber:", Buzz_Master$mmsiNumber_4[i]))
    
    # assign a temporary variable to the current mmsiNumber_1 integer 
    mmsiTemp <- Buzz_Master$mmsiNumber_4[i]
    
    # Find the matching mmsiNumber in Unique_mmsiNumbers_Oct2018 - assinging another temporary variable
    vessel_typeTemp <- Unique_mmsiNumbers_Oct2018$Vessel_Category[Unique_mmsiNumbers_Oct2018$mmsiNumber == mmsiTemp]
    
    # If a match is found, assign it to mmsiNumber_Type_1 in Buzz_Master
    if (length(vessel_typeTemp) > 0) {
      Buzz_Master$mmsiNumber_Type_4[i] <- vessel_typeTemp
    } else {
      Buzz_Master$mmsiNumber_Type_4[i] <- NA  # If no match, assign NA
    }
  }
}  

####### mmsiNumber_5
# Loop through each row in Buzz_Master to find vessel categories
for (i in 1:nrow(Buzz_Master)) {
  
  # Check mmsiNumber_1
  if (!is.na(Buzz_Master$mmsiNumber_5[i])) {
    
    #adding in sign of life - nice to see that is is processing live and how many it is processing through 
    print(paste("Processing mmsiNumber:", Buzz_Master$mmsiNumber_5[i]))
    
    # assign a temporary variable to the current mmsiNumber_1 integer 
    mmsiTemp <- Buzz_Master$mmsiNumber_5[i]
    
    # Find the matching mmsiNumber in Unique_mmsiNumbers_Oct2018 - assinging another temporary variable
    vessel_typeTemp <- Unique_mmsiNumbers_Oct2018$Vessel_Category[Unique_mmsiNumbers_Oct2018$mmsiNumber == mmsiTemp]
    
    # If a match is found, assign it to mmsiNumber_Type_1 in Buzz_Master
    if (length(vessel_typeTemp) > 0) {
      Buzz_Master$mmsiNumber_Type_5[i] <- vessel_typeTemp
    } else {
      Buzz_Master$mmsiNumber_Type_5[i] <- NA  # If no match, assign NA
    }
  }
}  

####### mmsiNumber_6
# Loop through each row in Buzz_Master to find vessel categories
for (i in 1:nrow(Buzz_Master)) {
  
  # Check mmsiNumber_1
  if (!is.na(Buzz_Master$mmsiNumber_6[i])) {
    
    #adding in sign of life - nice to see that is is processing live and how many it is processing through 
    print(paste("Processing mmsiNumber:", Buzz_Master$mmsiNumber_6[i]))
    
    # assign a temporary variable to the current mmsiNumber_1 integer 
    mmsiTemp <- Buzz_Master$mmsiNumber_6[i]
    
    # Find the matching mmsiNumber in Unique_mmsiNumbers_Oct2018 - assinging another temporary variable
    vessel_typeTemp <- Unique_mmsiNumbers_Oct2018$Vessel_Category[Unique_mmsiNumbers_Oct2018$mmsiNumber == mmsiTemp]
    
    # If a match is found, assign it to mmsiNumber_Type_1 in Buzz_Master
    if (length(vessel_typeTemp) > 0) {
      Buzz_Master$mmsiNumber_Type_6[i] <- vessel_typeTemp
    } else {
      Buzz_Master$mmsiNumber_Type_6[i] <- NA  # If no match, assign NA
    }
  }
}  

####### mmsiNumber_7
# Loop through each row in Buzz_Master to find vessel categories
for (i in 1:nrow(Buzz_Master)) {
  
  # Check mmsiNumber_1
  if (!is.na(Buzz_Master$mmsiNumber_7[i])) {
    
    #adding in sign of life - nice to see that is is processing live and how many it is processing through 
    print(paste("Processing mmsiNumber:", Buzz_Master$mmsiNumber_7[i]))
    
    # assign a temporary variable to the current mmsiNumber_1 integer 
    mmsiTemp <- Buzz_Master$mmsiNumber_7[i]
    
    # Find the matching mmsiNumber in Unique_mmsiNumbers_Oct2018 - assinging another temporary variable
    vessel_typeTemp <- Unique_mmsiNumbers_Oct2018$Vessel_Category[Unique_mmsiNumbers_Oct2018$mmsiNumber == mmsiTemp]
    
    # If a match is found, assign it to mmsiNumber_Type_1 in Buzz_Master
    if (length(vessel_typeTemp) > 0) {
      Buzz_Master$mmsiNumber_Type_7[i] <- vessel_typeTemp
    } else {
      Buzz_Master$mmsiNumber_Type_7[i] <- NA  # If no match, assign NA
    }
  }
}  

####### mmsiNumber_8
# Loop through each row in Buzz_Master to find vessel categories
for (i in 1:nrow(Buzz_Master)) {
  
  # Check mmsiNumber_1
  if (!is.na(Buzz_Master$mmsiNumber_8[i])) {
    
    #adding in sign of life - nice to see that is is processing live and how many it is processing through 
    print(paste("Processing mmsiNumber:", Buzz_Master$mmsiNumber_8[i]))
    
    # assign a temporary variable to the current mmsiNumber_1 integer 
    mmsiTemp <- Buzz_Master$mmsiNumber_8[i]
    
    # Find the matching mmsiNumber in Unique_mmsiNumbers_Oct2018 - assinging another temporary variable
    vessel_typeTemp <- Unique_mmsiNumbers_Oct2018$Vessel_Category[Unique_mmsiNumbers_Oct2018$mmsiNumber == mmsiTemp]
    
    # If a match is found, assign it to mmsiNumber_Type_1 in Buzz_Master
    if (length(vessel_typeTemp) > 0) {
      Buzz_Master$mmsiNumber_Type_8[i] <- vessel_typeTemp
    } else {
      Buzz_Master$mmsiNumber_Type_8[i] <- NA  # If no match, assign NA
    }
  }
}  

####### mmsiNumber_9
# Loop through each row in Buzz_Master to find vessel categories
for (i in 1:nrow(Buzz_Master)) {
      
      # Check mmsiNumber_1
 if (!is.na(Buzz_Master$mmsiNumber_9[i])) {
   
   #adding in sign of life - nice to see that is is processing live and how many it is processing through 
   print(paste("Processing mmsiNumber:", Buzz_Master$mmsiNumber_9[i]))
        
        # assign a temporary variable to the current mmsiNumber_1 integer 
        mmsiTemp <- Buzz_Master$mmsiNumber_9[i]
        
        # Find the matching mmsiNumber in Unique_mmsiNumbers_Oct2018 - assinging another temporary variable
        vessel_typeTemp <- Unique_mmsiNumbers_Oct2018$Vessel_Category[Unique_mmsiNumbers_Oct2018$mmsiNumber == mmsiTemp]
        
        # If a match is found, assign it to mmsiNumber_Type_1 in Buzz_Master
        if (length(vessel_typeTemp) > 0) {
          Buzz_Master$mmsiNumber_Type_9[i] <- vessel_typeTemp
        } else {
          Buzz_Master$mmsiNumber_Type_9[i] <- NA  # If no match, assign NA
   }
  }
}      

################################################################################################################################### 
####### FINALLY ADDING IN AVERAGE SPEED PER MMSI NUMBER PER EVENT 

## check if this is numeric - if not convert with as.numeric
AIS_Events$speedOverGround <- as.numeric(AIS_Events$speedOverGround)

# Adding in the columns ahead of time to make it easier 
Buzz_Master$mmsiNumber_Speed1 <- NA
Buzz_Master$mmsiNumber_Speed2 <- NA
Buzz_Master$mmsiNumber_Speed3 <- NA
Buzz_Master$mmsiNumber_Speed4 <- NA
Buzz_Master$mmsiNumber_Speed5 <- NA
Buzz_Master$mmsiNumber_Speed6 <- NA
Buzz_Master$mmsiNumber_Speed7 <- NA
Buzz_Master$mmsiNumber_Speed8 <- NA
Buzz_Master$mmsiNumber_Speed9 <- NA

############################ NOW RUN THE LOOPS FOR EACH INDIVIDUAL MMSI NUMBER COLUMN ############################

########## mmsiNumber_1
## NOW STARTING THE LOOP 
for (i in 1:nrow(Buzz_Master)) {
  
  # CREATE TEMP VARIABLE - Get the Event_ID and mmsiNumber_1 for the current row
  current_event_id <- Buzz_Master$Event_ID[i]
  current_mmsi <- Buzz_Master$mmsiNumber_1[i]
  
  # ADDING SIGN OF LIFE 
  print(paste("Processing Event ID:", Buzz_Master$Event_ID[i]))
  
  # Only proceed if mmsiNumber_1 is not NA
  if (!is.na(current_mmsi)) {
    
    # CREATE COMPARISON - Find matching rows in AIS_Events for the current Event_ID and mmsiNumber
    matching_ais_entries <- AIS_Events[AIS_Events$Event_ID == current_event_id & 
                                         AIS_Events$mmsiNumber == current_mmsi, ]
    # If there are matching rows, calculate the average speed
    if (nrow(matching_ais_entries) > 0) {
      avg_speed <- mean(matching_ais_entries$speedOverGround, na.rm = TRUE)
      
      # Assign the average speed to mmsiNumber_Speed1 in Buzz_Master
      Buzz_Master$mmsiNumber_Speed1[i] <- avg_speed
    } else {
      # If no matches, assign NA
      Buzz_Master$mmsiNumber_Speed1[i] <- NA
    }
  }
}

########## mmsiNumber_2
## NOW STARTING THE LOOP 
for (i in 1:nrow(Buzz_Master)) {
  
  # CREATE TEMP VARIABLE - Get the Event_ID and mmsiNumber_1 for the current row
  current_event_id <- Buzz_Master$Event_ID[i]
  current_mmsi <- Buzz_Master$mmsiNumber_2[i]
  
  # ADDING SIGN OF LIFE 
  print(paste("Processing Event ID:", Buzz_Master$Event_ID[i]))
  
  # Only proceed if mmsiNumber_1 is not NA
  if (!is.na(current_mmsi)) {
    
    # CREATE COMPARISON - Find matching rows in AIS_Events for the current Event_ID and mmsiNumber
    matching_ais_entries <- AIS_Events[AIS_Events$Event_ID == current_event_id & 
                                         AIS_Events$mmsiNumber == current_mmsi, ]
    # If there are matching rows, calculate the average speed
    if (nrow(matching_ais_entries) > 0) {
      avg_speed <- mean(matching_ais_entries$speedOverGround, na.rm = TRUE)
      
      # Assign the average speed to mmsiNumber_Speed1 in Buzz_Master
      Buzz_Master$mmsiNumber_Speed2[i] <- avg_speed
    } else {
      # If no matches, assign NA
      Buzz_Master$mmsiNumber_Speed2[i] <- NA
    }
  }
}

########## mmsiNumber_3
## NOW STARTING THE LOOP 
for (i in 1:nrow(Buzz_Master)) {
  
  # CREATE TEMP VARIABLE - Get the Event_ID and mmsiNumber_1 for the current row
  current_event_id <- Buzz_Master$Event_ID[i]
  current_mmsi <- Buzz_Master$mmsiNumber_3[i]
  
  # ADDING SIGN OF LIFE 
  print(paste("Processing Event ID:", Buzz_Master$Event_ID[i]))
  
  # Only proceed if mmsiNumber_1 is not NA
  if (!is.na(current_mmsi)) {
    
    # CREATE COMPARISON - Find matching rows in AIS_Events for the current Event_ID and mmsiNumber
    matching_ais_entries <- AIS_Events[AIS_Events$Event_ID == current_event_id & 
                                         AIS_Events$mmsiNumber == current_mmsi, ]
    # If there are matching rows, calculate the average speed
    if (nrow(matching_ais_entries) > 0) {
      avg_speed <- mean(matching_ais_entries$speedOverGround, na.rm = TRUE)
      
      # Assign the average speed to mmsiNumber_Speed1 in Buzz_Master
      Buzz_Master$mmsiNumber_Speed3[i] <- avg_speed
    } else {
      # If no matches, assign NA
      Buzz_Master$mmsiNumber_Speed3[i] <- NA
    }
  }
}

########## mmsiNumber_4
## NOW STARTING THE LOOP 
for (i in 1:nrow(Buzz_Master)) {
  
  # CREATE TEMP VARIABLE - Get the Event_ID and mmsiNumber_1 for the current row
  current_event_id <- Buzz_Master$Event_ID[i]
  current_mmsi <- Buzz_Master$mmsiNumber_4[i]
  
  # ADDING SIGN OF LIFE 
  print(paste("Processing Event ID:", Buzz_Master$Event_ID[i]))
  
  # Only proceed if mmsiNumber_1 is not NA
  if (!is.na(current_mmsi)) {
    
    # CREATE COMPARISON - Find matching rows in AIS_Events for the current Event_ID and mmsiNumber
    matching_ais_entries <- AIS_Events[AIS_Events$Event_ID == current_event_id & 
                                         AIS_Events$mmsiNumber == current_mmsi, ]
    # If there are matching rows, calculate the average speed
    if (nrow(matching_ais_entries) > 0) {
      avg_speed <- mean(matching_ais_entries$speedOverGround, na.rm = TRUE)
      
      # Assign the average speed to mmsiNumber_Speed1 in Buzz_Master
      Buzz_Master$mmsiNumber_Speed4[i] <- avg_speed
    } else {
      # If no matches, assign NA
      Buzz_Master$mmsiNumber_Speed4[i] <- NA
    }
  }
}

########## mmsiNumber_5
## NOW STARTING THE LOOP 
for (i in 1:nrow(Buzz_Master)) {
  
  # CREATE TEMP VARIABLE - Get the Event_ID and mmsiNumber_1 for the current row
  current_event_id <- Buzz_Master$Event_ID[i]
  current_mmsi <- Buzz_Master$mmsiNumber_5[i]
  
  # ADDING SIGN OF LIFE 
  print(paste("Processing Event ID:", Buzz_Master$Event_ID[i]))
  
  # Only proceed if mmsiNumber_1 is not NA
  if (!is.na(current_mmsi)) {
    
    # CREATE COMPARISON - Find matching rows in AIS_Events for the current Event_ID and mmsiNumber
    matching_ais_entries <- AIS_Events[AIS_Events$Event_ID == current_event_id & 
                                         AIS_Events$mmsiNumber == current_mmsi, ]
    # If there are matching rows, calculate the average speed
    if (nrow(matching_ais_entries) > 0) {
      avg_speed <- mean(matching_ais_entries$speedOverGround, na.rm = TRUE)
      
      # Assign the average speed to mmsiNumber_Speed1 in Buzz_Master
      Buzz_Master$mmsiNumber_Speed5[i] <- avg_speed
    } else {
      # If no matches, assign NA
      Buzz_Master$mmsiNumber_Speed5[i] <- NA
    }
  }
}

########## mmsiNumber_6
## NOW STARTING THE LOOP 
for (i in 1:nrow(Buzz_Master)) {
  
  # CREATE TEMP VARIABLE - Get the Event_ID and mmsiNumber_1 for the current row
  current_event_id <- Buzz_Master$Event_ID[i]
  current_mmsi <- Buzz_Master$mmsiNumber_6[i]
  
  # ADDING SIGN OF LIFE 
  print(paste("Processing Event ID:", Buzz_Master$Event_ID[i]))
  
  # Only proceed if mmsiNumber_1 is not NA
  if (!is.na(current_mmsi)) {
    
    # CREATE COMPARISON - Find matching rows in AIS_Events for the current Event_ID and mmsiNumber
    matching_ais_entries <- AIS_Events[AIS_Events$Event_ID == current_event_id & 
                                         AIS_Events$mmsiNumber == current_mmsi, ]
    # If there are matching rows, calculate the average speed
    if (nrow(matching_ais_entries) > 0) {
      avg_speed <- mean(matching_ais_entries$speedOverGround, na.rm = TRUE)
      
      # Assign the average speed to mmsiNumber_Speed1 in Buzz_Master
      Buzz_Master$mmsiNumber_Speed6[i] <- avg_speed
    } else {
      # If no matches, assign NA
      Buzz_Master$mmsiNumber_Speed6[i] <- NA
    }
  }
}

########## mmsiNumber_7
## NOW STARTING THE LOOP 
for (i in 1:nrow(Buzz_Master)) {
  
  # CREATE TEMP VARIABLE - Get the Event_ID and mmsiNumber_1 for the current row
  current_event_id <- Buzz_Master$Event_ID[i]
  current_mmsi <- Buzz_Master$mmsiNumber_7[i]
  
  # ADDING SIGN OF LIFE 
  print(paste("Processing Event ID:", Buzz_Master$Event_ID[i]))
  
  # Only proceed if mmsiNumber_1 is not NA
  if (!is.na(current_mmsi)) {
    
    # CREATE COMPARISON - Find matching rows in AIS_Events for the current Event_ID and mmsiNumber
    matching_ais_entries <- AIS_Events[AIS_Events$Event_ID == current_event_id & 
                                         AIS_Events$mmsiNumber == current_mmsi, ]
    # If there are matching rows, calculate the average speed
    if (nrow(matching_ais_entries) > 0) {
      avg_speed <- mean(matching_ais_entries$speedOverGround, na.rm = TRUE)
      
      # Assign the average speed to mmsiNumber_Speed1 in Buzz_Master
      Buzz_Master$mmsiNumber_Speed7[i] <- avg_speed
    } else {
      # If no matches, assign NA
      Buzz_Master$mmsiNumber_Speed7[i] <- NA
    }
  }
}

########## mmsiNumber_8
## NOW STARTING THE LOOP 
for (i in 1:nrow(Buzz_Master)) {
  
  # CREATE TEMP VARIABLE - Get the Event_ID and mmsiNumber_1 for the current row
  current_event_id <- Buzz_Master$Event_ID[i]
  current_mmsi <- Buzz_Master$mmsiNumber_8[i]
  
  # ADDING SIGN OF LIFE 
  print(paste("Processing Event ID:", Buzz_Master$Event_ID[i]))
  
  # Only proceed if mmsiNumber_1 is not NA
  if (!is.na(current_mmsi)) {
    
    # CREATE COMPARISON - Find matching rows in AIS_Events for the current Event_ID and mmsiNumber
    matching_ais_entries <- AIS_Events[AIS_Events$Event_ID == current_event_id & 
                                         AIS_Events$mmsiNumber == current_mmsi, ]
    # If there are matching rows, calculate the average speed
    if (nrow(matching_ais_entries) > 0) {
      avg_speed <- mean(matching_ais_entries$speedOverGround, na.rm = TRUE)
      
      # Assign the average speed to mmsiNumber_Speed1 in Buzz_Master
      Buzz_Master$mmsiNumber_Speed8[i] <- avg_speed
    } else {
      # If no matches, assign NA
      Buzz_Master$mmsiNumber_Speed8[i] <- NA
    }
  }
}

########## mmsiNumber_9
## NOW STARTING THE LOOP 
for (i in 1:nrow(Buzz_Master)) {
  
    # CREATE TEMP VARIABLE - Get the Event_ID and mmsiNumber_1 for the current row
    current_event_id <- Buzz_Master$Event_ID[i]
    current_mmsi <- Buzz_Master$mmsiNumber_9[i]
    
    # ADDING SIGN OF LIFE 
    print(paste("Processing Event ID:", Buzz_Master$Event_ID[i]))
    
    # Only proceed if mmsiNumber_1 is not NA
    if (!is.na(current_mmsi)) {

      # CREATE COMPARISON - Find matching rows in AIS_Events for the current Event_ID and mmsiNumber
      matching_ais_entries <- AIS_Events[AIS_Events$Event_ID == current_event_id & 
                                           AIS_Events$mmsiNumber == current_mmsi, ]
      # If there are matching rows, calculate the average speed
      if (nrow(matching_ais_entries) > 0) {
        avg_speed <- mean(matching_ais_entries$speedOverGround, na.rm = TRUE)
        
        # Assign the average speed to mmsiNumber_Speed1 in Buzz_Master
        Buzz_Master$mmsiNumber_Speed9[i] <- avg_speed
      } else {
        # If no matches, assign NA
        Buzz_Master$mmsiNumber_Speed9[i] <- NA
      }
    }
}

##########################################################################################################################
### GETTING A VESSEL COUNT FOR EACH CLICK EVENT 

## vessel count
Buzz_Master$Vessel_Count <- rowSums(!is.na(Buzz_Master[ , paste0("mmsiNumber_", 1:9)]))

## average vessel speed of all present vessels
Buzz_Master$Average_Speed <- rowMeans(Buzz_Master[ , paste0("mmsiNumber_Speed", 1:9)], na.rm = TRUE)

# convert speed NaN to NA
Buzz_Master$Average_Speed[is.nan(Buzz_Master$Average_Speed)] <- NA

####### ADDING THESE COLUMNS TO THE NOISE MONITOR EVENT DATA FOR MODELING 
# Merging Vessel_Count and Average_Speed columns from Buzz_Master into Vessel_Presence by Event_ID
Buzz_Noise_Monitor_Oct2018 <- merge(Buzz_Noise_Monitor_Oct2018, 
                         Buzz_Master[ , c("Event_ID", "Vessel_Count", "Average_Speed")], 
                         by = "Event_ID", 
                         all.x = TRUE)





