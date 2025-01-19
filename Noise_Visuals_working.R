############################################################################################################################
###### NOISE VISUALS 

library(ggplot2)
library(tidyr)


## VISUAL FOR ONE EVENT 

event_59<- Buzz_Noise_Monitor_Oct2018[Buzz_Noise_Monitor_Oct2018$Event_ID == 59, ]

hist(event_59$ThirdOctave_1414_1788_mean, main="Histogram of Vessel Noise for Event ID 59", 
     xlab="Noise Levels", col="blue", border="black")

####### TIME BASED PLOTS ########################


# TIME BASED PLOT FOR SINGLE OCTAVE BAND 
ggplot(data = event_59, aes(x = UTC, y = Median_2000Hz)) +
  geom_line() +   # Creates a line plot
  labs(title = "Noise Over Time for Event 59",
       x = "Time (UTC)",
       y = "Median of 2000Hz bands") +
  theme_minimal() # Optional: apply a clean theme

############# NOW MAKING PLOT WITH MULTIPLE OCTAVE BANDS IN IT 

# Reshape the data into long format
event_59_bands <- event_59 %>%
  pivot_longer(cols = c(LogMedian_2000Hz, 
                        LogMedian_low95_2000Hz, 
                        LogMedian_high95_2000Hz),
               names_to = "Frequency_Band",
               values_to = "Noise_Level")

# Now time to plot 
ggplot(data = event_59_bands, aes(x = UTC, y = Noise_Level, color = Frequency_Band)) +
  geom_line() +
  labs(title = "Noise Across Single Click Event",
       x = "Time (UTC)",
       y = "Noise Level",
       color = "Frequency Band") +
  theme_minimal() # Optional: apply a clean theme

############################################################################################################################
### ATTEMPTING TO MAKE A LONG PLOT FOR VISUALIZING NOISE

# Step 1: Extract year and month from the UTC column
Noise_Monitor_2018Oct09 <- Noise_Monitor_2018Oct09 %>%
  mutate(Month_Year = format(as.POSIXct(UTC, tz = "UTC"), "%Y-%m"))

# Filter the data for the month '2018-10'
Noise_Monitor_2018Oct09_201810 <- Noise_Monitor_2018Oct09 %>%
  filter(Month_Year == "2018-10")
# Filter the data for the month '2018-11'
Noise_Monitor_2018Oct09_201811 <- Noise_Monitor_2018Oct09 %>%
  filter(Month_Year == "2018-11")
# Filter the data for the month '2018-12'
Noise_Monitor_2018Oct09_201812 <- Noise_Monitor_2018Oct09 %>%
  filter(Month_Year == "2018-12")
# Filter the data for the month '2019-01'
Noise_Monitor_2018Oct09_201901 <- Noise_Monitor_2018Oct09 %>%
  filter(Month_Year == "2019-01")
# Filter the data for the month '2019-02'
Noise_Monitor_2018Oct09_201902 <- Noise_Monitor_2018Oct09 %>%
  filter(Month_Year == "2019-02")
# Filter the data for the month '2019-03'
Noise_Monitor_2018Oct09_201903 <- Noise_Monitor_2018Oct09 %>%
  filter(Month_Year == "2019-03")

#for the 2021Jan01 dataframe 
Noise_Monitor_2021Jan01$UTC <- as.POSIXct(Noise_Monitor_2021Jan01$UTC, format="%Y-%m-%d %H:%M:%S")

Noise_Monitor_2021Jan01 <- Noise_Monitor_2021Jan01 %>% 
  mutate(Month_Year = format(as.POSIXct(UTC, tz= "UTC"), "%Y-%m"))

Noise_Monitor_2021Jan01_202101 <- Noise_Monitor_2021Jan01 %>% 
  filter(Month_Year == "2021-01")
Noise_Monitor_2021Jan01_202102 <- Noise_Monitor_2021Jan01 %>% 
  filter(Month_Year == "2021-02")

# same for 2020May13 dataframe
Noise_Monitor_2020May13$UTC <- as.POSIXct(Noise_Monitor_2020May13$UTC, format="%Y-%m-%d %H:%M:%S")

Noise_Monitor_2020May13 <- Noise_Monitor_2020May13 %>%
  mutate(Month_Year = format(as.POSIXct(UTC, tz= "UTC"), "%Y-%m"))

Noise_Monitor_2020May13_202005 <- Noise_Monitor_2020May13 %>%
  filter(Month_Year == "2020-05")
Noise_Monitor_2020May13_202006 <- Noise_Monitor_2020May13 %>%
  filter(Month_Year == "2020-06")
Noise_Monitor_2020May13_202007 <- Noise_Monitor_2020May13 %>%
  filter(Month_Year == "2020-07")
Noise_Monitor_2020May13_202008 <- Noise_Monitor_2020May13 %>%
  filter(Month_Year == "2020-08")
Noise_Monitor_2020May13_202009 <- Noise_Monitor_2020May13 %>%
  filter(Month_Year == "2020-09")

  
########## PLOTS ###########
# FOR LOTS OF DATA 
library(plotly)

# This plot works but it slows down computer a lot - OCT 2018 
plot_ly(Noise_Monitor_2018Oct09_201810, x = ~UTC, y = ~LogMedian_2000Hz, type = 'scatter', mode = 'markers',
        marker = list(size = 2, opacity = 0.5)) %>%
  layout(title = "Noise Over Time for October 2018",
         xaxis = list(title = "Time (UTC)"),
         yaxis = list(title = "Median Noise (2000Hz bands, dB)"))
# NOV 2018 
plot_ly(Noise_Monitor_2018Oct09_201811, x = ~UTC, y = ~LogMedian_2000Hz, type = 'scatter', mode = 'markers',
        marker = list(size = 2, opacity = 0.5)) %>%
  layout(title = "Noise Over Time for November 2018",
         xaxis = list(title = "Time (UTC)"),
         yaxis = list(title = "Median Noise (2000Hz bands, dB)"))
# DEC 2018 
plot_ly(Noise_Monitor_2018Oct09_201812, x = ~UTC, y = ~LogMedian_2000Hz, type = 'scatter', mode = 'markers',
        marker = list(size = 2, opacity = 0.5)) %>%
  layout(title = "Noise Over Time for December 2018",
         xaxis = list(title = "Time (UTC)"),
         yaxis = list(title = "Median Noise (2000Hz bands, dB)"))
# JAN 2019
plot_ly(Noise_Monitor_2018Oct09_201901, x = ~UTC, y = ~LogMedian_2000Hz, type = 'scatter', mode = 'markers',
        marker = list(size = 2, opacity = 0.5)) %>%
  layout(title = "Noise Over Time for January 2019",
         xaxis = list(title = "Time (UTC)"),
         yaxis = list(title = "Median Noise (2000Hz bands, dB)"))
# FEB 2019
plot_ly(Noise_Monitor_2018Oct09_201902, x = ~UTC, y = ~LogMedian_2000Hz, type = 'scatter', mode = 'markers',
        marker = list(size = 2, opacity = 0.5)) %>%
  layout(title = "Noise Over Time for February 2019",
         xaxis = list(title = "Time (UTC)"),
         yaxis = list(title = "Median Noise (2000Hz bands, dB)"))
# MARCH 2019
plot_ly(Noise_Monitor_2018Oct09_201903, x = ~UTC, y = ~LogMedian_2000Hz, type = 'scatter', mode = 'markers',
        marker = list(size = 2, opacity = 0.5)) %>%
  layout(title = "Noise Over Time for March 2019",
         xaxis = list(title = "Time (UTC)"),
         yaxis = list(title = "Median Noise (2000Hz bands, dB)"))

## JAN 2021 DATAFRAME
# Jan 2021
plot_ly(Noise_Monitor_2021Jan01_202101, x = ~UTC, y = ~LogMedian_2000Hz, type = 'scatter', mode = 'markers',
        marker = list(size = 2, opacity = 0.5)) %>%
  layout(title = "Noise Over Time for January 2021",
         xaxis = list(title = "Time (UTC)"),
         yaxis = list(title = "Median Noise (2000Hz bands, dB)"))

# Feb 2021
plot_ly(Noise_Monitor_2021Jan01_202102, x = ~UTC, y = ~LogMedian_2000Hz, type = 'scatter', mode = 'markers',
        marker = list(size = 2, opacity = 0.5)) %>%
  layout(title = "Noise Over Time for February 2021",
         xaxis = list(title = "Time (UTC)"),
         yaxis = list(title = "Median Noise (2000Hz bands, dB)"))

# MAY 2020 
plot_ly(Noise_Monitor_2020May13_202005, x = ~UTC, y = ~LogMedian_2000Hz, type = 'scatter', mode = 'markers',
        marker = list(size = 2, opacity = 0.5)) %>%
  layout(title = "Noise Over Time for May 2020",
         xaxis = list(title = "Time (UTC)"),
         yaxis = list(title = "Median Noise (2000Hz bands, dB)"))

# JUNE 2020 
plot_ly(Noise_Monitor_2020May13_202006, x = ~UTC, y = ~LogMedian_2000Hz, type = 'scatter', mode = 'markers',
        marker = list(size = 2, opacity = 0.5)) %>%
  layout(title = "Noise Over Time for June 2020",
         xaxis = list(title = "Time (UTC)"),
         yaxis = list(title = "Median Noise (2000Hz bands, dB)"))

# SEPT 2020 
plot_ly(Noise_Monitor_2020May13_202009, x = ~UTC, y = ~LogMedian_2000Hz, type = 'scatter', mode = 'markers',
        marker = list(size = 2, opacity = 0.5)) %>%
  layout(title = "Noise Over Time for September 2020",
         xaxis = list(title = "Time (UTC)"),
         yaxis = list(title = "Median Noise (2000Hz bands, dB)"))


## LOOKING AT A FEW DAYS INDIVIDUALLY 
# add a column for day 
Noise_Monitor_2020May13 <- Noise_Monitor_2020May13 %>%
  mutate(Month_Year_Day = format(as.POSIXct(UTC, tz = "UTC"), "%Y-%m-%d"))

# Filter the data for the date
Noise_Monitor_2020May13_20200922 <- Noise_Monitor_2020May13 %>%
  filter(Month_Year_Day == "2020-09-22")

plot_ly(Noise_Monitor_2020May13_20200922, x = ~UTC, y = ~LogMedian_2000Hz, type = 'scatter', mode = 'markers',
        marker = list(size = 2, opacity = 0.5)) %>%
  layout(title = "Noise Over Time for 22 September 2020",
         xaxis = list(title = "Time (UTC)"),
         yaxis = list(title = "Median Noise (2000Hz bands, dB)"))
         




