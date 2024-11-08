## ICI Analysis ----

## Packages
library(dplyr)
library(tibble)
library(ggplot2)

## Set working directory, !!change path to where you want it!!
setwd("C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc")

## Get data ----
# If the data has time stamps
timed <- tibble(
  encounter_ID = c(rep("1A", 4), rep("1B", 3)),
  click_ID = c(0, 1, 2, 3, 0, 1, 2),
  timestamp = as.POSIXct(c("2024-03-14 12:00:00.000", "2024-03-14 12:00:00.005", "2024-03-14 12:00:00.010", "2024-03-14 12:00:00.020",
                           "2024-03-14 12:01:00.000", "2024-03-14 12:01:00.020", "2024-03-14 12:01:00.040"), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
)

# If the data has ICI already calculated, !!change path to where you have stored this!!
ICI <- read.csv("C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc/ICIFunction/Data.csv")

## Timestamp to ICI function ----
calculate_ICI <- function(data) {
  # Sort the data by encounter_ID and timestamp
  data <- data %>% 
    arrange(encounter_ID, timestamp)
  
  # Calculate the time difference between consecutive click_IDs within each encounter_ID
  data <- data %>%
    group_by(encounter_ID) %>%
    mutate(time_difference = timestamp - lag(timestamp))
  
  # Replace NA values with 0 in the time_difference column
  data$time_difference[is.na(data$time_difference)] <- 0
  
  # Convert time_difference to numeric
  data$time_difference <- as.numeric(data$time_difference)
  
  return(data)
}

## Call the function with example data
timed <- calculate_ICI(timed)


## ICI to buzz rate function ----
calculate_buzz <- function(data) {
  data <- data %>%
    mutate(buzz = ifelse(ICI < 0.01 & ICI > 0, 1, 0))
  
  df_buzz <- data %>%
    group_by(encounter_numb) %>%
    summarize(total_clicks = n(),  # Count total data points per encounterID
              buzz_calls = sum(buzz == 1),  # Count number of 1s per encounterID
              buzz_rate = (buzz_calls / total_clicks))  # Calculate percentage of 1s
  
  return(df_buzz)
}

# Call the function with your dataframe and assign the result to a new dataframe
ICI_buzz <- calculate_buzz(ICI)


## Sample analysis ----
#Make vessel  presence absence variable
ICI_buzz <- ICI_buzz %>%
  mutate(vessel_type = ifelse(substr(encounter_numb, nchar(encounter_numb), nchar(encounter_numb)) == "A", "no_vessel", "vessel"))


buzz_model1 <- glm(cbind(buzz_calls, (total_clicks-buzz_calls))~vessel_type, data = ICI_buzz, family=binomial)
summary(buzz_model1) ## overdispersed

buzz_model2 <- glm(cbind(buzz_calls, (total_clicks-buzz_calls))~vessel_type, data = ICI_buzz, family=quasibinomial())
summary(buzz_model2)

plot(buzz_model2) ## looks okay 

## Visualise ----
# Add predicted values to the original dataframe
ICI_buzz$predicted_buzz_rate <- predict(buzz_model2, type = "response")

# Plot
test <- ggplot(ICI_buzz, aes(x = vessel_type, y = buzz_rate, fill = vessel_type)) +
  geom_boxplot() +
  labs(x = "Vessel Type", y = "Buzz Rate") +
  theme_minimal() +
  guides(fill = FALSE)

test

## Print to working directory:
tiff('april_buzz_test.tiff', units="in", width=6, height=4, res=1000, compression = 'lzw')

test

dev.off()

