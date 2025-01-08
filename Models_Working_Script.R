#######################################################################################################################

################################## WORKING MODELS ##################################

## load all necessary packages 

## stat note: using YEAR, Month, and Daylight (with year) to give repetition to the model 
  # aka Night of month 12 has happened multiple times across years, as opposed to without year it has only happened once
  # more repetition equals better statistical power which will help the model! 

## adding an interaction term to a model that does not have a large degree of freedom can negatively impact the model without 
  # helping it explain variation 

## running a gam with a smoother is a more complicated model that loses individual information 
  # (for example, cannot compare different months against each other in a gam because its smoothed)
  # gams better for looking at general trends (months overall) than looking at individual months against each other
  # in the case of our temporal models, glm is simpler and provides more information 

## WEIGHTING - using the PPM as a proportion greatly reduced our sample size by grouping too much together and LOST US STATISTICAL POWER
  # in our case using a more raw form of our event data with a weight variable gives us much more data and much more statistical power = BETTER 

## THE ESTIMATE IS A SLOPE OF CHANGE 
## for binomial (and quasibinomial) models the estimates are in log odds -> need to convert exp(log-odd)

## INTERPRETING INTERACTIONS:if there is an interaction, the intercept will be the opposite of the interaction term 
  # For an example use Final Model called ModelBuzz_VesselOverlap_Temporal
  # the intercept is -1.7367, meaning daytime and with NO vessel overlap
  # the estimate for Vessel_Overlap is -0.0053 meaning that for each unit increase in vessel overlap, 
    # the buzz rate decreases by 0.0053 (log odds)
  # the estimate for night is -0.1856 meaning that buzz rate is lower at night when vessel overlap is 0
  # the positive interaction term of 0.00547 adjusts the slope of vessel overlap effect at night (meaning it )
    # meaning that the effect is less negative (weaker) at night 
    ## HOW THIS WORKS OUT IS: Day + overlap is the intercept + the estimate for Vessel_Overlap 
      # to interpret the intercept YOU ADD THE INTERACTION VALUE TO THE ORIGINAL ESTIMATE **
      # **** this means that you add the vessel_overlap:night interaction to the vessel_overlap estimate (NOT THE INTERCEPT)

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


################################################

## VESSEL IMPACT MODELS 

# Data are not really a true proportion because we still have a binary 1 and 0, with anything greater than 0 being a proportion
  # a better way to do this is run a binomial with a cbind of success vs total/success (proportion aspect) 
    ## the model knows what to do with this more than a true proportion or simple binomial 
  
  # Zero inflated is more for when you are lacking data and did not expect the number of 0 you have
    # in our case we have a lot of data and remotely expected the number of 0 we have (but NOT normally distributed)
  # THEREFORE instead of running a zero inflated model we can run a quasi-binomial which accounts for NON-NORMAL DIST) 
    # (and the reg binomial very overdispersed anyway) quasibinomial is best then 

# CHECKS
  # look at a histogram hist() of the data
  # run model - check for overdispersion
  # check Q-Q plots 
  # check residuals 

############# FOR EVENTS 60 MINUTES OR SHORTER (eliminates 1800 events of 10,900 total) - different for non 0 buzzrate 

## Buzz Rate * Daylight + month as random effect
  # buzzing more at night 

## Buzz rate and daylight (no random effect or interaction)
  # buzz more during the day 

## Buzz rate ~ Exposure_3k*Daylight + Month random effect - BEST MODEL 
  # overall vessel present = buzz LESS
  # During the night when vessels are present = buzz less than when vessels are present
  # when a vessel is not present they buzz LESS at night than day, when a vessel IS present buzz more at night than day
    # but still less overall than when a vessel is not present 

## Buzz_Rate ~ Vessel_Overlap*Daylight + Month as random effect (ONLY WHEN VESSELS ARE PRESENT)
  # overall as overlap increases, buzz rate decreases (not significant)
  # trend holds during day
  # at night buzz rate increases as vessel overlap increases 

## Buzz_Rate ~ Vessel_Overlap*Daylight + Month random effect (WITH 0 INCLUDED)
  # vessel overlap is significant 
  # as vessel overlap increases, buzz rate decreases 
  # looking at a gradient of vessel overlap, when a vessel is there for more time in the event = more vessel time, less buzz time 

# mention looking at duration and having little effect compared to buzz rate 

###### FINALS FOR VESSEL IMPACT 

# model2
# model5

##### BASIC NOISE MODELS

# modelp2
# model2
# model4 

#######################################################################################################################
################################## FINAL MODELS ##################################

model3 # Temporal Model 1 (?? NOT SURE IF THIS IS RIGHT)
model4 # Temporal Model 2 (weighted glm)
modelv1 # Vessel Presence + Temporal
moodlevo # Vessel Overlap Events + Temporal 
model2 # Buzz Rate + Vessel Impacts + Temporal 
model5 # Buzz Rate



## TEMPORAL VARIABLES 

# QUESTIONS:  
# 1. When are porpoises most active in a day - does that vary by season? and 
# 2. When are porpoises most active within a year and does that vary between years?

# Data: temporal_df (daydf) -- now called 
#     Vessel_Presence (daydf2) -- now called 

model3 <- glm(Proportion_Porpoise_Event ~ factor(Month)*Daylight + (1|Year), data = daydf)
summary(model3)

## Model Summary: I DON'T THINK THIS WAS ACTUALLY THE BEST ONE - nothing significant and no weights 

## GLM of season and year with a random effect of year 
model4 <- glm(Porpoise_Event ~ factor(Month)*Daylight + (1|Year), data = daydf2,
              family = binomial(link = "logit"),
              weights = Recording_Effort)
summary(model4)
plot(model4)
# MODEL SUMMARY: 
## They vocalise more at night (stderror = 0.02, z = 46.82, p < 0.001), this is 
## consistent across seasons and throughout years. They vocalise the most between 
## spring - autumn, but less so in the winter. 
## IS THIS ACTUALLY THE BEST ONE?? 

## TEMPORAL + VESSEL PRESENCE 

## GLM of season and year with a random effect of year 
# BEST MODEL
modelv1 <- glm(Vessel_3k ~ factor(Month)*Daylight + (1|Year), data = daydf2,
               family = binomial(link = "logit"),
               weights = Recording_Effort)
summary(modelv1)

## Vessel are present less in the night than the day, this is true across season and year. 
## They are most present from July - September. 

## TEMPORAL + VESSEL OVERLAP 

## GLM of season and year with a random effect of year 
# BEST MODEL 
moodelvo <- glm(Overlap ~ factor(Month)*Daylight + (1|Year), data = daydf3, 
                family = binomial(link="logit"), 
                weights = Recording_Effort)

summary(moodelvo)

##############################################################################################################

## VESSEL IMPACTS + TEMPORAL VARIABLES 

## Quasibinomial with environmental variables 
# BEST MODEL
model2 <- glm(Buzz_Rate ~ Exposure_3k*Daylight + (1|Month), data = buzzdf4, family = quasibinomial)
summary(model2)

# Diagnostics
plot(model2)

################################################
## Vessel overlap instead of exposure binomial 
buzzdf5 <- filter(buzzdf4, buzzdf4$Vessel_Overlap != 0)

## Overlap with vessel time 
# BEST MODEL
model5 <- glm(Buzz_Rate ~ Vessel_Overlap*Daylight, data = buzzdf4, family = quasibinomial)
summary(model5)

ggplot(buzzdf4, aes(x = Vessel_Overlap, y = Buzz_Rate)) +
  geom_smooth(method = "lm") +
  labs(x = "Vessel Overlap", y = "Proportional Buzz Rate")





