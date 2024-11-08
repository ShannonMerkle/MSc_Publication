###################################################################################################################################
### ALL ADVANCED STATS AND MODELING ### 
###################################################################################################################################
## COPYING TABLES FROM THE VISUALIZATION AND DESCRIPTIVE STATS NOTEBOOK 

Click_Event_Seasonal_Table
Click_Event_Time_of_Day_Table
Click_Event_Type_Total

Diurnal_Recording_Effort_per_Hour 

Recording_Diurnal_Table
Recording_Seasonal_Table
Recording_Yearly_Table
Total_Recording_Hours

# VALUES (not a dataframe)
Vessel_Exposures_Total
Event_Hourly_Counts
Event_Montly_Counts
Event_Time_of_Day_Counts

###################################################################################################################################
################ ISOLATING ALL CLICK EVENTS TEMPORALY ##########

##### RUNNING A CHI SQUARED TO LOOK AT EACH CATEGORICAL VARIABLE'S SIGNIFICANCE 

# Time of Day 
Chisq_Diurnal <- chisq.test(Click_Event_Time_of_Day_Table$`Count of Events`)
print(Chisq_Diurnal)
## VERY small p-value -- Time of Day SIGNIFICANT 

# Seasonal 
Chisq_Season <- chisq.test(Click_Event_Seasonal_Table$`Count of Events`)
print(Chisq_Season)
## VERY small p-value -- Season SIGNIFICANT 

####### NOW RUNNING A POST-HOC PAIRWISE CHI-SQUARED TO LOOK AT EACH CATEGORY INDIVIDUALLY 

event_table <- with(Click_Event_Time_of_Day_Table, table(`Time of Day`, `Count of Events`))

# Perform pairwise chi-squared tests with Bonferroni correction
pairwise_results_DIURNAL <- pairwise.prop.test(
  x = Click_Event_Time_of_Day_Table$`Count of Events`,
  n = rep(sum(Click_Event_Time_of_Day_Table$`Count of Events`), length(Click_Event_Time_of_Day_Table$`Count of Events`)),
  p.adjust.method = "bonferroni"
)

print(pairwise_results_DIURNAL)

# RESULTS: 1. Day, 2. Evening, 3. Morning, 4. Night 
# 1 vs 2 - Day vs Evening: 0.078 - NOT sig 
# 1 vs 3 - Day vs Morning: SIGNIFICANT 
# 2 vs 3 - Evening vs Morning: SIGNIFICNAT 
# 1 vs 4 - Day vs Night: 1.0 - NOT sig
# 2 vs 4 - Evening vs Night: 0.093 - NOT sig 
# 3 vs 4 - Morning vs Night: SIGNIFICANT 

## SEASONAL PAIRWISE 

# not sure why we do this?
event_table_season <- with(Click_Event_Seasonal_Table, table(Season, `Count of Events`))

# Perform pairwise chi-squared tests with Bonferroni correction
pairwise_results_SEASONAL <- pairwise.prop.test(
  x = Click_Event_Seasonal_Table$`Count of Events`,
  n = rep(sum(Click_Event_Seasonal_Table$`Count of Events`), length(Click_Event_Seasonal_Table$`Count of Events`)),
  p.adjust.method = "bonferroni"
)

print(pairwise_results_SEASONAL)


# RESULTS: 1. Autumn, 2. Spring, 3. Summer, 4. Winter
# 1 vs 2 - autumn vs spring - SIG
# 1 vs 3 - autumn vs summer - SIG
# 2 vs 3 - spring vs summer - SIG 
# 1 vs 4 - autumn vs winter - SIG 
# 2 vs 4 - spring vs winter - NOT SIG 0.5
# 3 vs 4 - summer vs winter - SIG  

######### TEMPORAL WITH CLICK TRAIN TYPE AS A VARIABLE 


### CATEGORICAL MODEL 

library(nnet)
Click_Train_test_model <- multinom(Click_Train_Type ~ Vessel_Exposure, data = Buzz_Master)
summary(Click_Train_test_model)


library(nnet)
str(Buzz_Master)

# Convert Click_Type to a factor if it isn't already
Buzz_Master$Click_Train_Type <- as.factor(Buzz_Master$Click_Train_Type)
Buzz_Master$Time_of_day <- as.factor(Buzz_Master$Time_of_day)

Multinomial_Diurnal <- multinom(Click_Train_Type ~ Time_of_day, data = Buzz_Master)
summary(Multinomial_Diurnal)

## this alone does not give you significance, need to look at a z-score and then convert to p-value

# Displaying z-scores and p-values in the model summary
Multinomial_Diurnal_Results <- summary(Multinomial_Diurnal)
z_scores <- Multinomial_Diurnal_Results$coefficients / Multinomial_Diurnal_Results$standard.errors
p_values <- (2 * (1 - pnorm(abs(z_scores))))

print(p_values)

# Combine into a table for easy interpretation
Multinomial_Diurnal_Results_Table <- cbind(Multinomial_Diurnal_Results$coefficients, Multinomial_Diurnal_Results$standard.errors, z_scores, p_values)
colnames(Multinomial_Diurnal_Results_Table) <- c("Coefficient", "Std. Error", "z value", "Pr(>|z|)")
print(Multinomial_Diurnal_Results_Table)

###################################################################################################################################
####################### MODELING WITH A GAM #######################

## each gam is two phases, the first is creating a usable table with event counts and the necessary varibale, 
# second phase is running the actual gam - then run summary to look as results

library(mgcv)

## establishing intercepts - this allows you to control what the intercepts are and make interpretation easier 
Buzz_Master$Time_of_day <- relevel(factor(Buzz_Master$Time_of_day), ref = "NIGHT")

##################### SINGLE VARIABLE MODELS WITH NO INTERACTIONS OR SMOOTHERS #####################

## 1.1 Basic GAM - Season only 
gam_event_counts_Season <- Buzz_Master %>%
  group_by(Season) %>%
  summarise(Event_Count = n())

gam_model_Season <- gam(Event_Count ~ factor(Season),
                        data = gam_event_counts_Season, 
                        family = poisson(link = "log"))

summary(gam_model_Season) ## all highly sig

## 1.2 Basic GAM - ToD only 
gam_event_counts_ToD <- Buzz_Master %>%
  group_by(Time_of_day) %>%
  summarise(Event_Count = n())

gam_model_ToD <- gam(Event_Count ~ factor(Time_of_day),
                     data = gam_event_counts_ToD, 
                     family = poisson(link = "log"))

summary(gam_model_ToD) 

## 1.3 GAM with hour instead of ToD 
gam_event_counts_Hour<- Buzz_Master %>%
  group_by(Hour) %>%
  summarise(Event_Count = n())

gam_model_Hour<- gam(Event_Count ~ factor(Hour),
                     data = gam_event_counts_Hour, 
                     family = poisson(link = "log"))

summary(gam_model_Hour)

##################### MULTI-VARIABLE MODELS WITH NO INTERACTIONS OR SMOOTHERS #####################

## 2.1 Basic GAM with ToD and Season, no interaction
gam_event_counts_ToD_Season <- Buzz_Master %>%
  group_by(Time_of_day, Season) %>%
  summarise(Event_Count = n())


gam_model_ToD_Season <- gam(Event_Count ~ factor(Time_of_day) + 
                   factor(Season),
                 data = gam_event_counts_ToD_Season, 
                 family = poisson(link = "log"))

summary(gam_model_ToD_Season)

## 2.2 Basic GAM with Hour (instead of ToD) and Season, no interaction 
gam_event_counts_Hour_Season <- Buzz_Master %>%
  group_by(Hour, Season) %>%
  summarise(Event_Count = n())

gam_model_Hour_Season <- gam(Event_Count ~ factor(Hour) + 
                              factor(Season),
                            data = gam_event_counts_Hour_Season, 
                            family = poisson(link = "log"))

summary(gam_model_Hour_Season)

## "Compared to daytime, morning and evening show significant differences 
# in porpoise click events, with lower counts in the morning and slightly higher counts in the evening. 
# Nighttime counts do not differ significantly from daytime."

##################### MODELS WITH HOUR SMOOTHER #####################

# create table to work from 
gam_event_counts_ToD_Hour_Season <- Buzz_Master %>%
  group_by(Hour, Time_of_day, Season) %>%
  summarise(Event_Count = n())

## 2.1 GAM with Time of Day, Season, and an hourly smoother but NO INTERACTION
gam_model_ToD_Season_HourSmoother <- gam(Event_Count ~ s(Hour, bs = "cs") + factor(Time_of_day) + factor(Season),
                                         data = gam_event_counts_ToD_Hour_Season, 
                                         family = poisson(link = "log"))

summary(gam_model_ToD_Season_HourSmoother)

## 2.2 GAM same as above but with AN INTERACTION 
gam_model_ToD_Season_HourSmoother_interaction <- gam(Event_Count ~ s(Hour, bs = "cs") + 
                   factor(Time_of_day) * factor(Season),
                 data = gam_event_counts_ToD_Hour_Season, 
                 family = poisson(link = "log"))

summary(gam_model_ToD_Season_HourSmoother_interaction)

## 2.3 GAM with ToD and Season plus INTERACTION, no smoother 
gam_model_ToD_Season_interaction <- gam(Event_Count ~ factor(Time_of_day) * factor(Season),
                                                     data = gam_event_counts_ToD_Hour_Season, 
                                                     family = poisson(link = "log"))

summary(gam_model_ToD_Season_interaction)





############################# MODELS FOR VESSEL PRESENCE #################################

# FIRST MAKE SURE EVERYTHING IS A FACTOR 
Vessel_Presence$Season <- factor(Vessel_Presence$Season)
Vessel_Presence$Time_of_day <- factor(Vessel_Presence$Time_of_day)
Vessel_Presence$Vessel_3k <- as.factor(Vessel_Presence$Vessel_3k)

str(Vessel_Presence)

## CREATE A BINARY COLUMN FOR PORPOISE EVENT ID 
Vessel_Presence$Porpoise_Event <- ifelse(Vessel_Presence$Event_ID > 0, 1, 0)

################### MODELS ###########################

## 3.1 GAM just looking at vessel presence impacted by temporal patters 
gam_model_VesselPresence_ToD_Season <- gam(Vessel_3k ~ Season + Time_of_day,
                           family = binomial(link = "logit"),
                           data = Vessel_Presence)

summary(gam_model_VesselPresence_ToD_Season)


## 3.2 GAM now looking at porpoise events with vessel presence alone 
gam_model_PorpoiseEvent_VesselPresence <- gam(Porpoise_Event ~ Vessel_3k,
                                              family = binomial(link = "logit"),
                                              data = Vessel_Presence)

summary(gam_model_PorpoiseEvent_VesselPresence) # SIG BUT VERY LITTLE DEVIANCE EXPLAINED 


## 3.3 Now full GAM with Porpoise events, vessel presence, and seasonal patterns
gam_model_PorpoiseEvent_VesselPresence_Season_ToD <- gam(Porpoise_Event ~ Vessel_3k + 
                                                factor(Season) + factor(Time_of_day),
                                              family = binomial(link = "logit"),
                                              data = Vessel_Presence)

summary(gam_model_PorpoiseEvent_VesselPresence_Season_ToD) # MOST SIG BUT AGAIN LITTLE DEVIANCE EXPLAINED


## 3.4 Same as above but + INTERACTION
gam_model_PorpoiseEvent_VesselPresence_Season_ToD_interaction <- gam(Porpoise_Event ~ Vessel_3k * factor(Time_of_day) + 
                                                         factor(Season),
                                                         family = binomial(link = "logit"),
                                                         data = Vessel_Presence)

summary(gam_model_PorpoiseEvent_VesselPresence_Season_ToD_interaction) # still very significant but model fit 

################################ MODELS WITH VESSEL IMPACT ##################################

gam_even_counts_ToD_Hour_Season_VesselExposure <- Buzz_Master %>% 
  group_by(Hour, Time_of_day, Season, Vessel_Exposure) %>%
  summarise(Event_Count = n())

## 4.1 Basic GAM with Vessel Exposure, ToD, Season, and no interactions
gam_model_ToD_Season_VesselExposure <- gam(Event_Count ~ factor(Time_of_day) + 
                                             factor(Season) + factor(Vessel_Exposure),
                                           data = gam_even_counts_ToD_Hour_Season_VesselExposure, 
                                           family = poisson(link = "log"))

summary(gam_model_ToD_Season_VesselExposure) ## Adjusted R-Sq and Deviance Explained drop substantially - model complexity 
          
## 4.2 same variable as above plus an INTERACTION                               
gam_model_ToD_Season_VesselExposure_interaction <- gam(Event_Count ~ factor(Time_of_day) * factor(Vessel_Exposure) + 
                                             factor(Season),
                                           data = gam_even_counts_ToD_Hour_Season_VesselExposure, 
                                           family = poisson(link = "log"))                                          

summary(gam_model_ToD_Season_VesselExposure_interaction) ## INTERACTION HELPS A LOT!!    


######################################################################################################################
##############################################   - DATA SUBSET -  ###################################################


############### NOISE MONITOR AND VESSEL VS AMBIENT #############

noise_lm <- lm(ThirdOctave_447_561_median ~ Noise_Exposure, data = Buzz_Noise_Monitor_Oct2018)
noise_lm <- lm(ThirdOctave_561_709_median ~ Noise_Exposure, data = Buzz_Noise_Monitor_Oct2018)
noise_lm <- lm(ThirdOctave_709_894_median ~ Noise_Exposure, data = Buzz_Noise_Monitor_Oct2018)
noise_lm <- lm(ThirdOctave_894_1118_median ~ Noise_Exposure, data = Buzz_Noise_Monitor_Oct2018)
noise_lm <- lm(ThirdOctave_1118_1414_median ~ Noise_Exposure, data = Buzz_Noise_Monitor_Oct2018)
noise_lm <- lm(ThirdOctave_1414_1788_median ~ Noise_Exposure, data = Buzz_Noise_Monitor_Oct2018)
noise_lm <- lm(ThirdOctave_1788_2236_median ~ Noise_Exposure, data = Buzz_Noise_Monitor_Oct2018)
noise_lm <- lm(ThirdOctave_2236_2806_median ~ Noise_Exposure, data = Buzz_Noise_Monitor_Oct2018)

summary(noise_lm)



noise_lm <- lm(ThirdOctave_2236_2806_median ~ Noise_Exposure, data = Buzz_Noise_Monitor_Oct2018)





