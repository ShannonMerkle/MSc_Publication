#######################################################################################################################

################################## WORKING MODELS ##################################

## load all necessary packages 

## stat note: using YEAR, Month, and Daylight (with year) to give repitition to the model 
  # aka Night of month 12 has happened multiple times across years, as opposed to without year it has only happened once
  # more repitition equals better statistical power which will help the model! 

## adding an interaction term to a model that does not have a large degree of freedom can negatively impact the model without 
  # helping it explain variation 

## running a gam with a smoother is a more complicated model that loses individual information 
  # (for example, cannot compare different months against each other in a gam because its smoothed)
  # gams better for looking at general trends (months overall) than looking at individual months against each other
  # in the case of our temporal models, glm is simpler and provides more information 

## WEIGHTING - using the PPM as a proportion greatly reduced our sample size by grouping too much together and LOST US STATISTICAL POWER
  # in our case using a more raw form of our event data with a weight variable gives us much more data and much more statistical power = BETTER 


##############################################################################################
########### TEMPORAL MODELS ###################################################

### MODEL WITH PROPORTION OF PORPOISE POSITIVE MINUTES 
## making a table with Year, Month, Daylight - to turn into dataframe and work from 
Model_table_PorpoiseProportion_VesselPresence_Year_Month_Daylight <- Vessel_Presence %>% 
  group_by(Year, Month, Daylight) %>%
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

View(Model_table_PorpoiseProportion_VesselPresence_Year_Month_Daylight)

# making a dataframe
Temporal_df_Year_Month_Daylight <- data_frame(Model_table_PorpoiseProportion_VesselPresence_Year_Month_Daylight)

saveRDS(Temporal_df_Year_Month_Daylight, "temporal_df.rds")

###### model run on Rach's script - VERY LOW STATISTICAL POWER due to low degree of freedom and ultimately low n 
  ## chose not to use this type of model 

## instead used a glm with count of Events that were naturally weighted with HOUR (double check the timeframe weight to methods section)
  # had much better statistical power, was simpler, and better described the visual trends observed in plot


#################################################################################################################

### MODELS WITH VESSEL OVERLAP 

# Looking at what happens when porpoise are presence (and if vessels are presence)
# then looking at what happens when vessels are presence (and what porpoises are doing)

## but main bulk is to look at what happens when PORPOISE OVERLAP WITH VESSELS (and not focus as much on when porpoises are not present)
  # then can allow us to lead into what are the impacts WHEN THEY DO OVERLAP - examine other variables like click type, duration, etc




library(ggplot2)
library(reshape2)

# Ensure Month is a factor with proper ordering
Model_table_EvCounts_RecEffort_Month$Month <- factor(
  Model_table_EvCounts_RecEffort_Month$Month,
  levels = c("January", "February", "March", "April", "May", "June", 
             "July", "August", "September", "October", "November", "December")
)

# Melt the data for grouping
stacked_data <- melt(Model_table_EvCounts_RecEffort_Month,
                     id.vars = "Month",
                     measure.vars = c("Event_Count", "Recording_Effort_Hours"),
                     variable.name = "Type",
                     value.name = "Count")

# Plot with dodged bars
ggplot(stacked_data, aes(x = Month, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Event_Count" = "blue", "Recording_Effort_Hours" = "gray"),
                    name = "Type",
                    labels = c("Porpoise Events", "Recording Effort")) +
  theme_minimal() +
  labs(title = "Porpoise Click Events and Recording Effort by Month",
       x = "Month",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







