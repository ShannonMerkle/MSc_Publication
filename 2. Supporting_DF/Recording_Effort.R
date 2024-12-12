################################################################################################################# 
##### DATA VISUALIZATION FOR RECORDING EFFORT #####
################################################################################################################# 

##### NOTE: THIS SCRIPT MAKES A RECORDING EFFORT ON AN HOURLY TIMEBIN, 
  ### RECORDING EFFORT HAS BEEN ADDED TO VESSEL PRESENCE ON A MINUTE TIME BIN!!! 

# DATAFRAMES
Sound_Acq_TOTAL
Recording_Effort # hourly recording effort across all years
Total_Recording_Effort # summary of recording hours of all years together NOW Summary_Recording_Effort
Vessel_Presence # has recording effort on minute time bin 
Summary_Recording_Effort # SAME AS Total_Recording_Effort BUT WITH A NAME THAT MAKES MORE SENSE 

## dataframes and tables have been cleaned significantly - all this code should work 
################################################################################################################# 
# MAKE A DATAFRAME WITH ALL POSSIBLE HOURS - Recording_Effort

# Define the start and end dates
start_date <- as.POSIXct("2018-10-08 00:00:00")
end_date <- as.POSIXct("2021-12-31 23:59:59")

# Generate a sequence of timestamps for every  in the date range
timestamps <- seq(from = start_date, to = end_date, by = "hour")

# Create a data frame from the timestamps
Recording_Effort <- data.frame(
  Year = format(timestamps, "%Y"),
  Month = format(timestamps, "%m"),
  Day = format(timestamps, "%d"),
  Hour = format(timestamps, "%H")
)

# CREATING A COHESIVE DATETIME COLUMN IN THE DATAFRAME
Recording_Effort <- Recording_Effort %>%
  mutate(UTC_hour = as.POSIXct(paste(Year, Month, Day, Hour, sep = "-"),
                               format = "%Y-%m-%d-%H"))
View(Recording_Effort)

#############################################################################################################################
############## MAKING THE SOUND ACQUISITION ON DATAFRAME ##############
# LOAD IN ALL SOUND ACQUISITION ON TABLES FROM THE SQLITE CONNECTION 
# NOW CREATE ONE LARGE SOUND ACQUISITION DATAFRAME FROM ALL THE INDIVIDUALS FILES

Sound_Acq_TOTAL <- bind_rows(df1_df2_etc)

## lost this original code for some reason 
## added in temporal columns - hour, month, ToD, Season although not really necessary

# remove some of the unnecessary columns
Sound_Acq_TOTAL$Server_Time <- NULL 

 
######################################################################################### 
# NOW CALCULATE RECORDING EFFORT BASED ON IF DATETIME PRESENT IN Sound_Acq_TOTAL

str(Sound_Acq_TOTAL)

Sound_Acq_TOTAL <- Sound_Acq_TOTAL %>%
  mutate(UTC_hour = floor_date(UTC, "hour"))

## maybe do not need this bit??
Recording_Effort <- Recording_Effort %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC"))

## ASSIGNING HOURS PRESENCE IN Sound_Acq_TOTAL AS RECORDING EFFORT IN DATAFRAME - hourly timebin 
Recording_Effort$recording_effort <- ifelse(Recording_Effort$UTC_hour %in% Sound_Acq_TOTAL$UTC_hour, 1, 0)

############################################################################################ 
## Manually remove those dates that recorded BUT WERE NOT USABLE DATA 

# Create the two date ranges
range1 <- seq(as.Date("2019-03-29"), as.Date("2019-12-31"), by = "day")
range2 <- seq(as.Date("2020-01-01"), as.Date("2020-05-11"), by = "day")

# Combine the two ranges into one variable 'To_remove'
To_remove <- c(range1, range2)

# add a date formatted column to make it easier 
Recording_Effort <- Recording_Effort %>%
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d"))

# Now remove them 
Recording_Effort <- Recording_Effort %>%
  mutate(recording_effort = ifelse(Date %in% To_remove, 0, recording_effort))


################################################################################################################# 
## CREATING DATAFRAME WITH ALL YEARS TOGETHER TO CALC RECORDING HOURS AND DAYS ###

#### NOW Summary_Recording_Effort

library(dplyr)

Total_Recording_Effort <- Recording_Effort %>%
  group_by(Month, Day) %>%
  summarize(Total_Recording_Hours = sum(recording_effort, na.rm = TRUE))

View(Total_Recording_Effort)

Total_Recording_Effort <- Total_Recording_Effort %>%
  mutate(Total_Recording_Day = ceiling(Total_Recording_Hours / 24))

Total_Recording_Effort$Day <- as.numeric(Total_Recording_Effort$Day)

## ADDING IN TOTAL RECORDING MINUTES FROM THE VESSEL PRESENCE DATAFRAME

# add in Year to group_by if you want to examine each year individually 
Vessel_Presence_Summary <- Vessel_Presence %>%
  group_by(Month, Day) %>%
  summarize(Total_Recording_Minutes = sum(Recording_Effort), .groups = "drop")
View(Vessel_Presence_Summary)

# had to make sure all day and month entires were integers (or number, just consistent with each other)

# joining the two together 
Total_Recording_Effort <- Total_Recording_Effort %>%
  left_join(Vessel_Presence_Summary, by = c("Month", "Day"))

# adding a 0 to any that had no recording minutes (NA)
Total_Recording_Effort$Total_Recording_Minutes[is.na(Total_Recording_Effort$Total_Recording_Minutes)] <- 0






################################################################################################################# 
## add seasonal and diurnal categories to Recording_Effort table

str(Recording_Effort)
Recording_Effort$Year <- as.numeric(Recording_Effort$Year)

#Season
Recording_Effort$Season <- with(Recording_Effort, ifelse(Month %in% c(12, 1, 2), "WINTER",
                                               ifelse(Month %in% c(3, 4, 5), "SPRING",
                                                      ifelse(Month %in% c(6, 7, 8), "SUMMER",
                                                             ifelse(Month %in% c(9, 10, 11), "AUTUMN", NA)))))

#ToD
Recording_Effort$Time_of_day <-with(Recording_Effort, ifelse(Hour %in% c(12,13, 14, 15, 16, 17), "DAY",
                                                   ifelse(Hour %in% c(18, 19, 20, 21, 22, 23), "EVENING",
                                                          ifelse(Hour %in% c(00,01, 02, 03, 04, 05), "NIGHT",
                                                                 ifelse(Hour %in% c(06, 07, 08, 09, 10, 11), "MORNING", NA)))))  


################################################################################################################# 
############ FINAL HEATMAP PLOT ##################### 

## USE VERSION IN SCRIPT FINAL_Visualisations - and use df Summary_Recording_Effort 

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

## To add in the tables below the heatmap 

# need to convert the tables to 'grobs'

Recording_Table_Diurnal_GROB <- tableGrob(Recording_Table_Diurnal)
Recording_Table_Seasonal_GROB <- tableGrob(Recording_Table_Season)

# this is the code that combines the heatmap plot with the tables - need to adjust names 
grid.arrange(Recording_Effort_Heatmap_Plot,
             arrangeGrob(Recording_Table_Seasonal_GROB, Recording_Table_Diurnal_GROB, ncol = 2),  # Tables side-by-side
             ncol = 1, heights = c(4, 1))
################################################################################################################# 







