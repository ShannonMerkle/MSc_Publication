###################################################################################################################################
### VARIOUS EDITING TO THE BUZZ MASTER DATABASE ONCE CREATED ### 
###################################################################################################################################

## TO DISTINGUISH CLICK TRAINS WHICH HAVE BUZZES FROM THOSE THAT DO NOT 
Buzz_Master$Buzz_Train <- ifelse(Buzz_Master$Buzz_Clicks >= 6, 'YES', 'NO')

## PROPORTION OF CLICKS WHICH ARE BUZZES IN EACH TRAIN

Buzz_Master$Buzz_to_Total_Percentage <- Buzz_Master$Buzz_Rate*100

# rounding decimal places to 2 
Buzz_Master$Buzz_to_Total_Percentage <- round(Buzz_Master$Buzz_to_Total_Percentage, 2)

countBuzz_YES <- sum(Buzz_Master$Buzz_Train == 'YES', na.rm = TRUE)
countBuzz_YES

countBuzz_NO <- sum(Buzz_Master$Buzz_Train == 'NO', na.rm = TRUE)
countBuzz_NO

countExposures_3k <- sum(Buzz_Master$Exposure_3k == 0)
countExposures_3k

### Creating a column that gives the duration of the click event in seconds
Buzz_Master$Click_Train_Length <- as.numeric(difftime(Buzz_Master$End_Time, Buzz_Master$Start_Time, units = "secs"))

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

## create new mmsiNumber columns
Buzz_Master$mmsiNumber_1 <- NA
Buzz_Master$mmsiNumber_2 <- NA


## make sure mmsiNumbers are in numeric format 
AIS_3kRadius_2018Oct_2019March$mmsiNumber <- as.numeric(AIS_3kRadius_2018Oct_2019March$mmsiNumber)

############################## this works you just have to run it at the end like a smart person 
### RUN THE LOOP 
# Loop through each event in Buzz_Master
for (i in 1:nrow(Buzz_Master)) {
  
  ## these are great to give evidence that the code is actually doing something -- like a sign of life code 
  print(paste("Processing Event_ID:", Buzz_Master$Event_ID[i]))
  print(paste("Start Time:", Buzz_Master$Start_Time[i]))
  print(paste("End Time:", Buzz_Master$End_Time[i]))
  
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
## BEFORE REALIZING THE TOP CODE DID WORK I RAN CODE FOR JUST A SINGLE EVENT AND CREATED A NEW DATAFRAME TO VEIW IT 

Buzz_Master_BACKUP$Start_Time <- as.POSIXct(Buzz_Master$Start_Time, format="%Y-%m-%d %H:%M:%S")

str(Buzz_Master)
print(head(Buzz_Master_BACKUP$Start_Time, 10))

## trying it with one event_ID == 18 

# Extract the Start_Time and End_Time for Event_ID 18
event_id_18 <- Buzz_Master[Buzz_Master$Event_ID == 18, ]

# Get Start_Time and End_Time
start_time_18 <- event_id_18$Start_Time
end_time_18 <- event_id_18$End_Time

# Print Start_Time and End_Time for verification
print(paste("Start Time:", start_time_18))
print(paste("End Time:", end_time_18))

# Filter the AIS dataframe to find entries within the event's time range
AIS_entries_in_event_18 <- AIS_3kRadius_2018Oct_2019March[AIS_3kRadius_2018Oct_2019March$UTC >= start_time_18 & 
                                                            AIS_3kRadius_2018Oct_2019March$UTC <= end_time_18, ]

# Print the filtered AIS entries
View(AIS_entries_in_event_18)


#################### NOW TRYING IT ON A LARGER SCALE ########################################

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
    
    print(paste("Processing Event_ID:", event_id, "Start:", start_time, "End:", end_time))
    
    # Filter AIS data that falls between Start_Time and End_Time
    ais_filtered <- AIS_3kRadius_2018Oct_2019March[AIS_3kRadius_2018Oct_2019March$UTC >= start_time & 
                                                     AIS_3kRadius_2018Oct_2019March$UTC <= end_time, ]
    
    print(paste("Matches found for Event_ID", event_id, ":", nrow(ais_filtered)))
    
    # Check if ais_filtered has any rows before adding Event_ID and appending
    if (nrow(ais_filtered) > 0) {
      # Add a column to indicate the corresponding Event_ID
      ais_filtered$Event_ID <- event_id
      
      # Append the filtered data to AIS_Events dataframe
      AIS_Events <- rbind(AIS_Events, ais_filtered)
    }
  }
}

### CREATED A NEW DATAFRAME WITH ALL AIS DATA FROM DIFFERENT EVENTS -- STILL COULD BE USEFUL 
View(AIS_Events)

###################################################################################################################################
#### NOW ADDING IN VESSEL TYPE PER MMSI NUMBER 

str(Buzz_Master)
str(Unique_mmsiNumbers_Oct2018)

## because these have NA in the columns they ALL need to be converted to characters - integers and numbers will not work 
Buzz_Master$mmsiNumber_9 <- as.character(Buzz_Master$mmsiNumber_9)
Unique_mmsiNumbers_Oct2018$mmsiNumber <- as.character(Unique_mmsiNumbers_Oct2018$mmsiNumber)
    
## FIRST add in the new column - did not work without this for some reason 
Buzz_Master$mmsiNumber_Type_9 <- NA

## THEN START THE LOOP 
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

## then remove temp variables - not necessary, just to declutter 
remove()
  
################################################################################################################################### 
## FINALLY ADDING IN AVERAGE SPEED PER MMSI NUMBER PER EVENT 

str(AIS_Events)

# Adding in the columns ahead of time to make it easier 
Buzz_Master$mmsiNumber_Speed9 <- NA

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










