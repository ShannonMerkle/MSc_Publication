###################################################################################################################################
## not sure if we want to keep this but for now visualization script!

###################################################################################################################################
############ FINAL RECORDING EFFORT HEATMAP ##################### 

library(ggplot2)
library(gridExtra)

# heatmap plot for recording DAYS - should we do hours or minutes on a more fluid scale?
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
# PROPORTION OF PORPOISE POSITIVE MINUTES FOR DAY AND NIGHT and VESSEL POSITIVE MINUTES 

## Daylight and Month table with proportion of Porpoise presence and proportion of vessel presence 
Model_table_PorpoiseProportion_VesselPresence_Month_Daylight <- Vessel_Presence %>% 
  group_by(Daylight, Month) %>%
  summarise(
    Total_Count = n(), 
    Porpoise_Positive_Minutes = sum(Porpoise_Event), 
    Vessel_Positive_Minutes = sum(Vessel_3k),
    .groups = "drop"
  )%>%
  mutate(Proportion_Porpoise_Event = Porpoise_Positive_Minutes / Total_Count
  )%>%
  mutate(Proportion_Vessel_Presence = Vessel_Positive_Minutes / Total_Count
  )

View(Model_table_PorpoiseProportion_VesselPresence_Month_Daylight)  



## second
Model_table_PorpoiseProportion_VesselPresence_Month_Daylight_Vessel3k <- Vessel_Presence %>% 
  group_by(Daylight, Month) %>%
  summarise(
    Total_Count = n(), 
    Porpoise_Positive_Minutes = sum(Porpoise_Event), 
    Porpoise_Positive_Minutes_Vessel = sum(Porpoise_Event * (Vessel_3k == 1)), # Count when Vessel_3k is 1
    .groups = "drop"
  ) %>%
  mutate(
    Proportion_Porpoise_Event = Porpoise_Positive_Minutes / Total_Count *100
  )%>%
  mutate(
    Proportion_Porpoise_Event_Vessel = Porpoise_Positive_Minutes_Vessel / Total_Count*100
  )

View(Model_table_PorpoiseProportion_VesselPresence_Month_Daylight_Vessel3k)



# PLOT WITH PORPOISE POSITIVE MINUTES
ggplot(Model_table_PorpoiseProportion_VesselPresence_Month_Daylight, 
       aes(x = Month, y = Proportion_Porpoise_Event, fill = Daylight)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Day" = "skyblue", "Night" = "darkblue")) +
  scale_x_continuous(breaks = 1:12, expand = c(0, 0)) + # makes the x axis count in 12 parts sections instead of default 
  labs(
    title = "Proportion of Porpoise-Positive Minutes for Day and Night",
    x = "Month",
    y = "Proportion of Porpoise-Positive Minutes",
    fill = "Daylight"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# PLOT WITH VESSEL POSITIVE MINUTES 
ggplot(Model_table_PorpoiseProportion_VesselPresence_Month_Daylight, 
       aes(x = Month, y = Proportion_Vessel_Presence, fill = Daylight)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Day" = "skyblue", "Night" = "darkblue")) +
  scale_x_continuous(breaks = 1:12, expand = c(0, 0)) + # makes the x axis count in 12 parts sections instead of default 
  labs(
    title = "Proportion of Vessel-Positive Minutes for Day and Night",
    x = "Month",
    y = "Proportion of Vessel-Positive Minutes",
    fill = "Daylight"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


##### MAKE A FIGURE WITH DAY AND NIGHT PORPOISE EVENTS (minutes), 
  # INSIDE THAT TOTAL BAR HAVE ONE BAR SHOWING EVENTS MINUTES WIHTOUT VESSELS AND ANOTHER BAR WITH MINUTES OVERLAPPING WITH VESSELS 
  ## 

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
  

###########################################
## NOW DOING THE SAME FOR DAY VS NIGHT 

## now making a variable for Time of Day 
event_counts_plot_variable_Daylight <- Buzz_Master %>%
  group_by(Daylight, Click_Train_Type) %>%
  summarise(Count = n()) %>%
  ungroup()

# Daylight Plot
ggplot(event_counts_plot_variable_Daylight, aes(x = Daylight, y = Count, fill = Click_Train_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Click Events During Daylight vs Darkness", 
       x = "Daylight", 
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




######
df_long <- Model_table_PorpoiseProportion_VesselPresence_Month_Daylight_Vessel3k %>%
  pivot_longer(
    cols = c(Proportion_Porpoise_Event, Proportion_Porpoise_Event_Vessel),
    names_to = "Type",
    values_to = "Count"
  )

View(df_long)

## BEST PLOT 
ggplot(df_long, aes(x = Month, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  facet_wrap(~Daylight, ncol = 1) +
  scale_fill_manual(values = c("Proportion_Porpoise_Event" = "skyblue", "Proportion_Porpoise_Event_Vessel" = "navy")) +
  scale_x_continuous(breaks = 1:12, expand = c(0, 0)) + 
  labs(
    title = "Porpoise Positive Minutes by Month and Daylight",
    x = "Month",
    y = "Count",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

########################################
#### ATTEMPT NUMBER TWO AT A BARPLOT WITH BOTH DAYLIGHT CATEGORIES IN ONE PLOT - using sample data 

# Example data
data <- data.frame(
  Month = rep(1:12, each = 4),
  Time_of_Day = rep(c("Daylight", "Night"), each = 2, times = 12),
  Vessel_Presence = rep(c("With Vessels", "Without Vessels"), times = 24),
  Minutes = sample(50:500, 48, replace = TRUE)
)
View(data)

data$Month_Time <- interaction(data$Month, data$Time_of_Day, sep = " - ")
data$Time_Month <- interaction(data$Time_of_Day, data$Month, sep = " - ")

## this works!! same colors for each 
ggplot(data, aes(x = factor(Time_Month), y = Minutes, fill = Vessel_Presence)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  labs(x = "Month - Time of Day", y = "Minutes", title = "Porpoise Minutes by Month and Time of Day",
       fill = "Vessel Presence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## now adjusting so the colors are different based on time of day 
time_colors <- c("Daylight" = "steelblue", "Night" = "darkorange")

ggplot(data, aes(x = factor(Time_Month), y = Minutes, fill = Vessel_Presence)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6, aes(color = Time_of_Day)) +
  scale_fill_manual(values = c("With Vessels" = "lightgray", "Without Vessels" = "darkgray")) + # Fill for Vessel_Presence
  scale_color_manual(values = time_colors) + # Outline color for Time_of_Day
  labs(
    x = "Time of Day - Month",
    y = "Minutes",
    title = "Porpoise Minutes by Month and Time of Day",
    fill = "Vessel Presence",
    color = "Time of Day"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##### now applying the above code to the actual data - making vessel presence a column for extra rows 
test_df <- Vessel_Presence %>% 
  group_by(Daylight, Month, Vessel_3k) %>%
  summarise(
    Total_Count = n(), 
    Porpoise_Positive_Minutes = sum(Porpoise_Event), 
    #Porpoise_Positive_Minutes_Vessel = sum(Porpoise_Event * (Vessel_3k == 1)), # Count when Vessel_3k is 1
    .groups = "drop"
  ) %>%
  mutate(
    Percentage_PPM = Porpoise_Positive_Minutes / Total_Count *100
  #)%>%
  #mutate(
    #Proportion_Porpoise_Event_Vessel = Porpoise_Positive_Minutes_Vessel / Total_Count*100
  )

View(test_df)
test_df$Percentage_PPM <- round(test_df$Percentage_PPM, 2)

test_df$Time_Month <- interaction(test_df$Daylight, test_df$Month, sep = " - ")
test_df$Month_Time <- interaction(test_df$Month, test_df$Daylight, sep = " - ")

# attempt plot with REAL test_df data - THIS WORKS 
ggplot(test_df, aes(x = factor(Time_Month), y = Percentage_PPM, fill = Vessel_3k)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  labs(x = "Month - Daylight", y = "Minutes", title = "Porpoise Minutes by Month and Time of Day",
       fill = "Vessel Presence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# NOW ADJUST THE COLOR OF BARS - and legend 
time_colors <- c("Day" = "darkorange", "Night" = "steelblue")

## needed to set Vessel_3k as a factor instead of a number 
test_df$Vessel_3k <- as.factor(test_df$Vessel_3k)

# NEW PLOT - change position between dodge and stack, I think dodge is actually easier to read 
ggplot(test_df, aes(x = factor(Time_Month), y = Percentage_PPM, fill = Vessel_3k)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, aes(color = Daylight)) +
  scale_fill_manual(values = c("1" = "lightgray",  "0" = "darkgray")) + # Fill for Vessel_Presence
  scale_color_manual(values = time_colors) + # Outline color for Time_of_Day
  labs(
    x = "Daylight by Month",
    y = "Percentage of Porpoise Positive Minutes",
    title = "Percentage of Porpoise Positive Minutes by Month and Daylight",
    fill = "Vessel Presence",
    color = "Time of Day"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)
  )


