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




