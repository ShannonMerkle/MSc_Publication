###################################################################################################################################
### GETTING NOISE MONITOR DATA FOR THE OCT2018 DATABASE AS A PROXY FOR ALL EVENTS ### 
###################################################################################################################################

###################################################################################################################################
## RUN A DATE TIME FORMAT CODE FIRST TO GET EVERYTHING IN MATCHING FORMATS TO CORRELATE

# Ensure Noise_Monitor$UTC is POSIXct
Noise_Monitor_2018Oct09$UTC <- as.POSIXct(Noise_Monitor_2018Oct09$UTC, format="%Y-%m-%d %H:%M:%S")

# Ensure Buzz_Events$Start_Event and Buzz_Events$End_Event are POSIXct
Oct2018_March2019_Buzz$Start_Time <- as.POSIXct(Oct2018_March2019_Buzz$Start_Time, format="%Y-%m-%d %H:%M:%S")
Oct2018_March2019_Buzz$End_Time <- as.POSIXct(Oct2018_March2019_Buzz$End_Time, format="%Y-%m-%d %H:%M:%S")


###################################################################################################################################
####### EVENT_ID CORRELATED TO NOISE MONITOR ###############

## this code finds any noise monitor data that falls between each event ID (both ambient and vessel presence) and correlated 

# Create new dataframe to add data into 
Buzz_Noise_Monitor_Oct2018 <- data.frame()


## setting up the loop
for (i in 1:nrow(Oct2018_March2019_Buzz)) {
  
  # Get the current event's start and end times
  start_time <- Oct2018_March2019_Buzz$Start_Time[i]
  end_time <- Oct2018_March2019_Buzz$End_Time[i]
  
  # Find rows in Noise_Monitor that have UTC between start_time and end_time
  event_noise_temp <- Noise_Monitor_2018Oct09[Noise_Monitor_2018Oct09$UTC >= start_time & Noise_Monitor_2018Oct09$UTC <= end_time, ]

  # If there are matching rows, add Event_ID_Number to them
  if (nrow(event_noise_temp) > 0) {
    event_noise_temp$Event_ID <- Oct2018_March2019_Buzz$Event_ID[i]
    
    # Append the matching rows to the result dataframe
    Buzz_Noise_Monitor_Oct2018 <- rbind(Buzz_Noise_Monitor_Oct2018, event_noise_temp)
  }
}

# View the new dataframe
View(Buzz_Noise_Monitor_Oct2018)


###################################################################################################################################
#### REMOVE ALL THE EXCESS COLUMNS THAT ARE NOT NEEDED 

## Decide what parameter you want to use 

library(dplyr)

# Remove all median columns -- MAKE A BACKUP OF THE DATAFRAME FIRST JUST IN CASE 
Buzz_Noise_Monitor_Oct2018 <- Buzz_Noise_Monitor_Oct2018 %>%
  select(-contains("_mean"))

# remove all low95 columns -- but maybe this is something we want to use??
Buzz_Noise_Monitor_Oct2018 <- Buzz_Noise_Monitor_Oct2018 %>%
  select(-contains("_low95"))

Buzz_Noise_Monitor_Oct2018 <- Buzz_Noise_Monitor_Oct2018 %>%
  select(-contains("_high95"))

# Check the result
View(Buzz_Noise_Monitor_Oct2018)

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

###################################################################################################################################
################################ VISUALS ################################

## TESTING FOR SPECIFIC EVENT 

event_59<- Buzz_Noise_Monitor_Oct2018[Buzz_Noise_Monitor_Oct2018$Event_ID == 59, ]

hist(event_59$ThirdOctave_1414_1788_mean, main="Histogram of Vessel Noise for Event ID 59", 
     xlab="Noise Levels", col="blue", border="black")

####### TIME BASED PLOTS ########################
library(ggplot2)
library(tidyr)

# TIME BASED PLOT FOR SINGLE OCTAVE BAND 
ggplot(data = event_59, aes(x = UTC, y = ThirdOctave_561_709_mean)) +
  geom_line() +   # Creates a line plot
  labs(title = "Noise Progression Over Time for Event 59",
       x = "Time (UTC)",
       y = "Noise Level (ThirdOctave_561_709_mean)") +
  theme_minimal() # Optional: apply a clean theme

############# NOW MAKING PLOT WITH MULTIPLE OCTAVE BANDS IN IT 

# Reshape the data into long format
event_59_bands <- event_59 %>%
  pivot_longer(cols = c(ThirdOctave_447_561_mean, 
                        ThirdOctave_894_1118_mean, 
                        ThirdOctave_2806_3549_mean),
               names_to = "Frequency_Band",
               values_to = "Noise_Level")

# Now time to plot 
ggplot(data = event_59_bands, aes(x = UTC, y = Noise_Level, color = Frequency_Band)) +
  geom_line() +
  labs(title = "Noise Progression Over Time for Different Frequency Bands",
       x = "Time (UTC)",
       y = "Noise Level",
       color = "Frequency Band") +
  theme_minimal() # Optional: apply a clean theme

###################################################################################################################################

## TEST MODEL 

<<<<<<< HEAD:supporting r scripts/Noise Monitor for Click Events.R
model <- lm(ThirdOctave_561_709_mean ~ Noise_Exposure, data = Buzz_Noise_Monitor_Oct2018)
summary(model)
=======
TESTmodel <- lm(ThirdOctave_447_561_median + ThirdOctave_447_561_median ~ Noise_Exposure, data = Buzz_Noise_Monitor_Oct2018)
summary(TESTmodel)

### Looking at the large variance in the ambient category 

ambient_data <- subset(Buzz_Noise_Monitor_Oct2018, Noise_Exposure == "Ambient")
summary(ambient_data$ThirdOctave_709_894_mean)

sd(ambient_data$ThirdOctave_709_894_mean)





>>>>>>> updating scripts:Noise Monitor for Click Events.R
