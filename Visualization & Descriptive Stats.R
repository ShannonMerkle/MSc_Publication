###################################################################################################################################
## not sure if we want to keep this but for now visualization script!

###################################################################################################################################
############ FINAL RECORDING EFFORT HEATMAP ##################### 

library(ggplot2)
library(gridExtra)

Recording_Effort_Heatmap_Plot <- ggplot(Total_Recording_Effort, aes(x = Day, y = Month, fill = Total_Recording_Day)) +
  geom_tile(color = "black") +
  scale_fill_stepsn(colors = c("gray95", "gray80", "gray60", "gray40", "gray25"),
                    breaks = c(1, 2, 3, 4),
                    limits = c(0, 4), # makes the legend solid instead of continuous gray scale 
                    guide = guide_colorbar(frame.colour = "black", frame.linewidth = 0.2)) +
  scale_x_continuous(breaks = 1:31, expand = c(0, 0)) +  # Remove space around tiles
  scale_y_continuous(breaks = 1:12, expand = c(0, 0)) +  # Remove space around tiles
  coord_fixed() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + # Add a frame
  labs(fill = "Recording Days", title = "Recording Effort from 2018 through 2021") +
  theme(text = element_text(family = "Times")) # change the font 

Recording_Effort_Heatmap_Plot


###################################################################################################################################
########### CLICK TRAIN TYPE - PLOTS ##############

### Click train type for ALL temporal patterns 
ggplot(Buzz_Master, 
        aes(x = factor(Click_Train_Type ,levels = c("Buzz", "Scan", "Presence")), y = , fill = Click_Train_Type)) + 
  geom_bar()+
  labs(title = "Click Train Types for All Temporal",
       x = "Click Train Type", 
       y = "Count", 
       fill = "Total Click Train Type") +
  theme_minimal()

## add the table with numbers into the plot above 
Click_Event_Type_Total <- table(Buzz_Master$Click_Train_Type)
View(Click_Event_Type_Total)
  

########## TEMPORAL PLOTS ############

#### SEASONAL PLOTS AGAINST CLICK TRAIN TYPE 

# First make a variable to group the season to click train type 
event_counts_plot_variable_Seasonal <- Buzz_Master %>%
  group_by(Season, Click_Train_Type) %>%
  summarise(Count = n()) %>%
  ungroup()


# Create the bar plot
ggplot(event_counts_plot_variable_Seasonal, aes(x = Season, y = Count, fill = Click_Train_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Click Events per Season", 
       x = "Season", 
       y = "Number of Events", 
       fill = "Event Type") +
  theme_minimal()


#### DIURNAL PLOTS AGAINST CLICK TRAIN TYPE 

## now making a variable for Time of Day 
event_counts_plot_variable_Time_of_Day <- Buzz_Master %>%
  group_by(Time_of_day, Click_Train_Type) %>%
  summarise(Count = n()) %>%
  ungroup()

# Create the bar plot
ggplot(event_counts_plot_variable_Time_of_Day, aes(x = Time_of_day, y = Count, fill = Click_Train_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Click Events per Time of Day", 
       x = "Time of Day", 
       y = "Number of Events", 
       fill = "Event Type") +
  theme_minimal()


######### CLICK TRAIN TYPE VS VESSEL EXPOSURE 

event_counts_plot_variable_Vessel_Exposure <- Buzz_Master %>%
  group_by(Vessel_Exposure, Click_Train_Type) %>%
  summarise(Count = n()) %>%
  ungroup()

ggplot(event_counts_plot_variable_Vessel_Exposure, aes(x = Vessel_Exposure, y = Count, fill = Click_Train_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Click Train Type Counts at Different Vessel Exposure", 
       x = "Vessel Exposure", 
       y = "Number of Events", 
       fill = "Event Type") +
  theme_minimal()

###################################################################################################################################
################################# TABLES #################################

##### RECORDING HOURS 


# where did all of this code go??? 


## RENAMING HEADERS 
Recording_Yearly_Table <- Recording_Yearly_Table %>% 
  rename(
    "Recording Hours" = "Total_Recording_Hours"
    )

# SAVE THE TABLE AS A PNG - note it will save to the working directory - make sure that is the Project Folder 
# need to run each of these lines individually 

png("Recording Effort Yearly Table.png", width = 800, height = 600, res = 150) 

grid.table(Recording_Yearly_Table, rows = NULL)

dev.off()

#### NOW FOR THE CLICK TRAIN EVENTS - ALL TYPES 

## First you have to convert the values into dataframes 
Click_Event_Seasonal_Table <- as.data.frame(Event_Seasonal_Counts)
Click_Event_Time_of_Day_Table <- as.data.frame(Event_Time_of_Day_Counts)

## now rename the column headers
Click_Event_Time_of_Day_Table <- Click_Event_Time_of_Day_Table %>% 
  rename(
    "Time of Day" = Var1,
    "Count of Events" = Freq
  )


### EXPORT AS PNG 
png("Annual Recording Heatmap.png", width = 800, height = 600, res = 150) 

grid.table(Click_Event_Time_of_Day_Table, rows = NULL)

dev.off()

########### VESSEL EXPOSURE 

# basic table 
Vessel_Exposures_Total <- table(Buzz_Master$Vessel_Exposure)
View(Vessel_Exposures_Total)

### BAR PLOT OF BUZZ_TRAIN BINARY TO SEE HOW 0 INFLATED IT IS 
# Count the occurrences of 1s and 0s in Buzz_Train
buzz_train_counts <- table(Buzz_Master$Buzz_Train)

# Plot the counts
barplot(
  buzz_train_counts,
  main = "Counts of Buzz_Train Values",
  xlab = "Buzz_Train",
  ylab = "Count",
  names.arg = c("0 (No Buzz)", "1 (Buzz)"),
  col = c("skyblue", "orange")
)

# Optionally, print the counts for reference
print(buzz_train_counts)




