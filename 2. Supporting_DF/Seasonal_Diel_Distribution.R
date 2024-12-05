###################################################################################################################################
### SEASONAL AND DIEL PRESENCE AND DISTRIBUTION ### 
###################################################################################################################################

###################################################################################################################################
## ADDING MONTH AND HOUR COLUMNS TO THE MASTER DATASET 
Buzz_Master <- read.csv("buzz_MASTER.csv", header = TRUE, sep = ",")
View(Buzz_Master)

Buzz_Master <- Buzz_Master %>% 
  mutate(Month = format(as.Date(Start_Time, format= "%Y-%m-%d %H:%M:%S"), "%m"))

Buzz_Master <- Buzz_Master %>% 
  mutate(Hour = format(as.POSIXct(Start_Time, format="%Y-%m-%d %H:%M:%S"), "%H"))


###################################################################################################################################
#### SEASONAL CLASSIFICATION - based on criteria from original paper 

Buzz_Master$Month <- as.numeric(Buzz_Master$Month)
str(Buzz_Master)

Buzz_Master$Season <- with(Buzz_Master, ifelse(Month %in% c(12, 1, 2), "WINTER",
                             ifelse(Month %in% c(3, 4, 5), "SPRING",
                                    ifelse(Month %in% c(6, 7, 8), "SUMMER",
                                           ifelse(Month %in% c(9, 10, 11), "AUTUMN", NA)))))



#################################################################################################################################

### TIME OF DAY DISTINCTIONS BASED ON LOCAL TIME (UTC will be +1)
# 05:00 - 10:59 CEST = Dawn 
# 11:00 - 16:59 CEST = Day
# 17:00 - 22:59 CEST = Dusk
# 23:00 - 04:59 CEST = Night 

## UTC conversions -- jsut remember to convert back for discussions and note daylights savings time 
# 06:00 - 11:59 = Dawn
# 12:00 - 17:59 = Day 
# 18:00 - 23:59 = Dusk 
# 00:00 - 05:59 = Night 

Buzz_Master$Hour <- as.numeric(Buzz_Master$Hour)
str(Buzz_Master)

## SAME CODE BUT FOR TIME OF DAY - AS UTC!!
Buzz_Master$Time_of_day <-with(Buzz_Master, ifelse(Hour %in% c(12,13, 14, 15, 16, 17), "DAY",
                                                                     ifelse(Hour %in% c(18, 19, 20, 21, 22, 23), "EVENING",
                                                                            ifelse(Hour %in% c(00,01, 02, 03, 04, 05), "NIGHT",
                                                                                   ifelse(Hour %in% c(06, 07, 08, 09, 10, 11), "MORNING", NA)))))                                                                    

#################################################################################################################################
####### QUICK DATA VISUALIZATION #################

## FIRST MAKE TABLES TO GROUP THE DATA 

# Months table 
Montly_Counts <- table(Buzz_Master$Month)
View(Montly_Counts)

# Seasonal table
Seasonal_Counts <- table(Buzz_Master$Season)
View(Seasonal_Counts)

# Hourly table
Hourly_Counts <- table(Buzz_Master$Hour)
View(Hourly_Counts)

# Time of day Table 
Time_of_Day_Counts <- table(Buzz_Master$Time_of_day)
View(Time_of_Day_Counts)

######### VERY BASIC HISTOGRAMS #############

# these do not really work 
hist(Buzz_Master$Hour)
hist(Buzz_Master$Month)

############ BETTER BARPLOTS #############
library(ggplot2)

## Barplot for Time of Day 
ggplot(Buzz_Master, 
       aes(x = factor(Time_of_day,levels = c("NIGHT", "MORNING", "DAY", "EVENING")), y = , color = Time_of_day)) + 
  geom_bar()+
  labs(x = "Time of Day", y = "Count", title = "Time of Day")+
  theme(legend.position = "none")

# Barplot for Season
ggplot(Buzz_Master, 
       aes(x = factor(Season,levels = c("SPRING", "SUMMER", "AUTUMN", "WINTER")), y = , color = Season)) + 
  geom_bar()+
  labs(x = "Season", y = "Count", title = "Seasons")+
  theme(legend.position = "none")


################################################################################################################# 

############## ADDING IN DAYLIGHT COLUMN WITH DAWN AND DUSK 

# library(suncalc)

## Sunrise and Sunset 
start_date <- as.Date("2018-10-01")
end_date <- as.Date("2021-12-31")

date_seq <- seq.Date(from = start_date, to = end_date, by = "day")

Daylight_UTC <- getSunlightTimes(date = date_seq, lat = 55.51, lon = 09.72)
View(Daylight_UTC)

# Convert to UTC - the data are already in UTC but just double checking 
Daylight_UTC$UTC_Sunrise <- with_tz(Daylight_UTC$sunrise, tzone = "UTC")
Daylight_UTC$UTC_Sunset <- with_tz(Daylight_UTC$sunset, tzone = "UTC")

## SAME AS ABOVE BUT IN VESSEL_PRESENCE (backup)
Vessel_Presence_bckup$UTC_Sunrise <- with_tz(Vessel_Presence_bckup$sunrise, tzone = "UTC")
Vessel_Presence_bckup$UTC_Sunset <- with_tz(Vessel_Presence_bckup$sunset, tzone = "UTC")

## NOW TO COMBINE WITH BUZZ_MASTER 

# first make sure the formatting of 
Daylight_UTC <- Daylight_UTC %>%
  mutate(UTC_date = as.Date(date, format = "%Y-%m-%d"))

# make a column in Buzz_Master with only date in it (same with Vessel Presence)
Buzz_Master <- Buzz_Master %>%
  mutate(UTC_StartDate = as.Date(Start_Time))

Vessel_Presence <- Vessel_Presence %>%
  mutate(date = as.Date(datetime))

# Join Buzz_Master 
Buzz_MasterDaylight <- Buzz_Master %>%
  left_join(Daylight_UTC, by = c("UTC_StartDate" = "UTC_date"))
View(Buzz_MasterDaylight)

# Join - SAME AS ABOVE BUT WITH VESSEL PRESENCE
Vessel_Presence_bckup <- Vessel_Presence %>%
  left_join(Daylight_UTC, by = c("date" = "UTC_date"))
View(Vessel_Presence_bckup)

# Classify events as Day or Night - this works now 
Buzz_Master_backup <- Buzz_MasterDaylight %>%
  mutate(
    Daylight = case_when(
      Start_Time >= UTC_Sunrise & Start_Time < UTC_Sunset ~ "Day",  # Between sunrise and sunset
      Start_Time < UTC_Sunset | Start_Time >= UTC_Sunset ~ "Night",  # Before sunrise or after sunset
      TRUE ~ NA_character_  # Catch any errors
    )
  )

View(Buzz_Master)

## SAME FOR VESSEL_PRESENCE 
Vessel_Presence_bckup <- Vessel_Presence_bckup %>%
  mutate(
    Daylight = case_when(
      datetime >= UTC_Sunrise & datetime < UTC_Sunset ~ "Day",  # Between sunrise and sunset
      datetime < UTC_Sunset | datetime >= UTC_Sunset ~ "Night",  # Before sunrise or after sunset
      TRUE ~ NA_character_  # Catch any errors
    )
  )

### DAWN AND DUSK 

Buzz_Master <- Buzz_Master %>%
  mutate(Dawn_Dusk = case_when(
    Start_Time >= dawn & Start_Time <= UTC_Sunrise ~ "Dawn", 
    Start_Time >= UTC_Sunset & Start_Time <= dusk ~ "Dusk", 
    TRUE ~ NA_character_
  ))
View(Buzz_Master)

# now with VESSEL_PRESENCE
Vessel_Presence_bckup <- Vessel_Presence_bckup %>%
  mutate(Dawn_Dusk = case_when(
    datetime >= dawn & datetime <= UTC_Sunrise ~ "Dawn", 
    datetime >= UTC_Sunset & datetime <= dusk ~ "Dusk", 
    TRUE ~ NA_character_
  ))

# remove the unnecessar columns in Vessel_presece
Vessel_Presence_bckup$night <- NULL

# move over most recent to vessel presence and remove working copy
Vessel_Presence_backup <- Vessel_Presence
Vessel_Presence <- Vessel_Presence_bckup
remove(Vessel_Presence_bckup)

## a little bit of column re-ordering

# Columns to prioritize
priority_cols <- c("colA", "colB", "colC")

priority_cols <- c("Event_ID", 
                   "Start_Time", 
                   "End_Time", 
                   "Click_Train_Type",
                   "Click_Train_Length", 
                   "Total_Minutes",
                   "Total_Clicks",
                   "Buzz_Clicks", 
                   "Buzz_Rate",
                   "Scan_Clicks", 
                   "Scan_Rate", 
                   "Presence_Clicks", 
                   "Buzz_Train", 
                   "Scan_Train", 
                   "Month",
                   "Hour",
                   "Time_of_day",
                   "Season",
                   "Daylight",
                   "Dawn_Dusk",
                   "dawn", 
                   "dusk", 
                   "UTC_Sunrise", 
                   "UTC_Sunset",
                   "Exposure_3k", 
                   "Exposure_500m", 
                   "Vessel_Exposure",
                   "Vessel_Count",
                   "Average_Speed", 
                   "Vessel_Presence_Minutes",
                   "Vessel_Overlap"
                  )




# Rearrange dataframe
Buzz_Master <- Buzz_Master[, c(priority_cols, setdiff(names(Buzz_Master), priority_cols))]
View(Buzz_Master)


