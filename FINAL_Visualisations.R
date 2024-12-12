###################################################################################################################################
#### FINAL VISUALIZATIONS AND PLOTS #####

###################################################################################################################################

# FINAL RECORDING EFFORT HEATMAP 

# heatmap plot for recording DAYS - should we do hours or minutes on a more fluid scale?
Recording_Effort_Heatmap_Plot <- ggplot(Summary_Recording_Effort, aes(x = Day, y = Month, fill = Total_Recording_Day)) +
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

# FINAL Percentage of Porpoise Positive Minutes with and without vessels by Daylight 

# make a new dataframe for data grouped in a specific way - needed a Vessel_3k column 
plot1_df <- Vessel_Presence %>% 
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

# round the percentrage to only 2 decimal spaces
plot1_df$Percentage_PPM <- round(test_df$Percentage_PPM, 2)

# create the interction column 
plot1_df$Time_Month <- interaction(test_df$Daylight, test_df$Month, sep = " - ")

## needed to set Vessel_3k as a factor instead of a number 
plot1_df$Vessel_3k <- as.factor(test_df$Vessel_3k)

# NOW ADJUST THE COLOR OF BARS - and legend 
time_colors <- c(
  "Day.0" =  "darkseagreen2",  # Day, Vessel Absence
  "Day.1" = "darkseagreen4",   # Day, Vessel Presence
  "Night.0" = "slategray2",  # Night, Vessel Absence
  "Night.1" = "slategrey"   # Night, Vessel Presence
)

# NEW PLOT - change position between dodge and stack, I think dodge is actually easier to read 

ggplot(plot1_df, aes(x = factor(Time_Month), y = Percentage_PPM, fill = interaction(Daylight, Vessel_3k))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  scale_fill_manual(
    values = time_colors,
    labels = c(
      "Day.0" = "Day (No Vessel)",
      "Day.1" = "Day (Vessel)",
      "Night.0" = "Night (No Vessel)",
      "Night.1" = "Night (Vessel)"
    )
  ) +
  labs(
    x = "Daylight by Month",
    y = "Percentage of Porpoise Positive Minutes",
    title = "Percentage of Porpoise Positive Minutes by Month and Daylight",
    fill = "Daylight and Vessel Presence"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



