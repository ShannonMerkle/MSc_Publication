################################################################################################################# 
##### DATA VISUALIZATION FOR RECORDING EFFORT #####
################################################################################################################# 

## ***** NOTE THE NAME OF THE ORIGINAL DATAFRAME HAS CHANGED ********
## was data_visualizaion_heatmap, modified into Total_Recording_Hours to be easier to work with 



### CREATING A NEW DATAFRAME WITH ALL DATES FOR GIVEN YEAR TO WORK WITH ### 
# Define the start and end dates for the year
start_date_all <- as.Date("2018-01-01")
end_date_all <- as.Date("2021-12-31")

# Generate a sequence of dates for the entire year
dates_all <- seq(from = start_date_all, to = end_date_all, by = "day")

# Create a dataframe with these dates
data_visualization_heatmap <- data.frame(Date = dates_all)

# Print the dataframe
View(data_visualization_heatmap)

#################### next phase ########################

# Ensure the Porpoise_2019$UTC is in Date format (convert if needed)
Porpoise_2019$Date <- as.Date(Porpoise_2019$UTC)

# Add a new column 'Porpoise_Present' in date_df where 1 indicates a match and 0 indicates no match
date_df$Porpoise_Present <- ifelse(date_df$Date %in% Porpoise_2019$UTC, 1, 0)

# Print the updated dataframe
print(date_df)

############## version of this with all the dataframes together ##############

# Convert the UTC column in each dataframe to just the date
Sound_Acq_Sept2021$Date <- as.Date(Sound_Acq_Sept2021$UTC)

# Combine the dates from all three dataframes
Total_Sound_Acq <- unique(c(Sound_Acq_Feb2021$Date, Sound_Acq_Jan2021$Date, Sound_Acq_May2020$Date, Sound_Acq_Oct2018$Date, Sound_Acq_Sept2020$Date, Sound_Acq_Sept2021$Date))

# Add a new column 'Porpoise_Present' to date_df, marking 1 if the date is present in any of the dataframes
data_visualization_heatmap$Recording_Effort <- ifelse(data_visualization_heatmap$Date %in% Total_Sound_Acq, 1, 0)

# Print the updated dataframe
View(data_visualization_heatmap)


### creating a variable of dates to change to 0 ###
# Create the two date ranges
range1 <- seq(as.Date("2019-03-29"), as.Date("2019-12-31"), by = "day")
range2 <- seq(as.Date("2020-01-01"), as.Date("2020-05-11"), by = "day")

# Combine the two ranges into one variable 'To_remove'
To_remove <- c(range1, range2)

# Print the result
print(To_remove)

### Now set those dates to 0 ###

data_visualization_heatmap$Recording_Effort <- ifelse(data_visualization_heatmap$Date %in% To_remove, 0, data_visualization_heatmap$Recording_Effort)

################################################################################################################# 

## NOW PREPPING TO PLOT THIS 

# data_visualization_heatmap$Year <- as.Date(data_visualization_heatmap$Date, '%Y')
data_visualization_heatmap$Day <- format(as.Date(data_visualization_heatmap$Date, format = '%Y-%m-%d'), '%d')
View(data_visualization_heatmap)


################################################################################################################# 
## creating a different type of dataframe to work from - Total_Recording_Hours ###
library(dplyr)

Total_Recording_Hours <- data_visualization_heatmap %>%
  group_by(Month, Day) %>%
  summarize(Total_Recording_Effort = sum(Recording_Effort, na.rm = TRUE))

View(Total_Recording_Hours)

################################################################################################################# 

############ FINAL HEATMAP PLOT ##################### 

library(ggplot2)

# THIS ONE LOOKS THE BEST RIGHT NOW 
ggplot(Total_Recording_Hours, aes(x = Day, y = Month, fill = Total_Recording_Effort)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "gray95", high = "gray25") +
  coord_fixed() 

## maybe on a different version of R this would look nicer??
ggplot(Total_Recording_Hours, aes(x = Day, y = Month, fill = Total_Recording_Effort)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "#b0c4de", high = "#000080") +
  coord_fixed() 




 
################################################################################################################# 





