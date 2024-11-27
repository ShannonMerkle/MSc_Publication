###################################################################################################################################
### ALL ADVANCED STATS AND MODELING ### 
###################################################################################################################################

###################### Model summaries ##########################

####### TEMPORAL MODELS - GAMS
  # Temporal non-weighted
summary(gam_model_ToD_Season)
summary(gam_model_ToD_Season_interaction) 
summary(gam_model_ToD_Season_HourSmoother) # slightly better than weighted 
summary(gam_model_ToD_Season_HourSmoother_interaction) # similar to weighted 

summary(gam_model_HourSmoother_MonthlySmoother) # slightly better than weighted 
summary(gam_model_HourSmoother_MonthlySmoother_interaction)

summary(gam_model_ToD_MonthlySmoother)
summary(gam_model_ToD_Season_MonthlySmoother)
summary(gam_model_ToD_Season_interaction_MonthlySmoother)

  # weighted temporal models
summary(gam_model_WEIGHTED_ToD_Season)
summary(gam_model_WEIGHTED_ToD_Season_interaction)

summary(gam_model_WEIGHTED_ToD_Season_HourSmoother)
summary(gam_model_WEIGHTED_ToD_Season_HourSmoother_interaction) # similar to non-weighted

summary(gam_model_WEIGHTED_HourSmoother_MonthlySmoother)
summary(gam_model_WEIGHTED_HourSmoother_MonthlySmoother_interaction) # Good model but VERY BAD UNDERDISPERSION - plots weird 

summary(gam_model_WEIGHTED_ToD_MonthlySmoother)
summary(gam_model_WEIGHTED_ToD_Season_MonthlySmoother)
summary(gam_model_WEIGHTED_ToD_Season_interaction_MonthlySmoother) ## !! BEST MODEL BY FAR -- good residuals, smoother is good, weight correlation is strong (0.999) 


###### VESSEL PRESENCE - GAMS
summary(gam_model_VesselPresence_ToD_Season)

summary(gam_model_PorpoiseEvent_VesselPresence)
summary(gam_model_PorpoiseEvent_VesselPresence_Season_ToD)
summary(gam_model_PorpoiseEvent_VesselPresence_Season_ToD_interaction)

summary(gam_model_PorpoiseEvent_VesselPresence_Season_ToD_HourlySmoother)
summary(gam_model_PorpoiseEvent_VesselPresence_Season_ToD_HourlySmoother_interaction)

summary(gam_model_PorpoiseEvent_VesselPresence_ToD_MonthlySmoother)
summary(gam_model_PorpoiseEvent_VesselPresence_ToD_MonthlySmoother_interaction)

summary(gam_model_PorpoiseEvent_VesselPresence_Season_MonthlySmoother_ToD)
summary(gam_model_PorpoiseEvent_VesselPresence_Season_MonthlySmoother_ToD_interaction)

summary(gam_model_PorpoiseEvent_VesselPresence_HourSmoother_MonthlySmoother)
summary(gam_model_PorpoiseEvent_VesselPresence_HourSmoother_MonthlySmoother_interaction)



###################################################################################################################################
####################### MODELING WITH A GAM #######################
###################################################################################################################################

## each gam is two phases, the first is creating a usable table with event counts and the necessary variable, 
# second phase is running the actual gam - then run summary to look as results

library(mgcv)

## establishing intercepts - this allows you to control what the intercepts are and make interpretation easier 
Buzz_Master$Time_of_day <- relevel(factor(Buzz_Master$Time_of_day), ref = "NIGHT")


##################### SINGLE VARIABLE MODELS WITH NO INTERACTIONS OR SMOOTHERS #####################

## 1.1 Basic GAM - Season only 

Model_table_EvCounts_Season <- Buzz_Master %>%
  group_by(Season) %>%
  summarise(Event_Count = n())

gam_poisson_Season <- gam(Event_Count ~ factor(Season),
                        data = Model_table_EvCounts_Season, 
                        family = poisson(link = "log"))

summary(gam_poisson_Season) ## all highly sig

## 1.2 Basic GAM - ToD only 
Model_table_EvCounts_ToD <- Buzz_Master %>%
  group_by(Time_of_day) %>%
  summarise(Event_Count = n())

gam_poisson_ToD <- gam(Event_Count ~ factor(Time_of_day),
                     data = Model_table_EvCounts_ToD, 
                     family = poisson(link = "log"))

summary(gam_poisson_ToD) 

## 1.3 GAM with hour instead of ToD 
Model_table_EvCounts_Hour<- Buzz_Master %>%
  group_by(Hour) %>%
  summarise(Event_Count = n())

gam_poisson_Hour<- gam(Event_Count ~ factor(Hour),
                     data = Model_table_EvCounts_Hour, 
                     family = poisson(link = "log"))

summary(gam_poisson_Hour)

##################### MULTI-VARIABLE MODELS WITH NO INTERACTIONS OR SMOOTHERS #####################

## 2.1 Basic GAM with ToD and Season, no interaction
Model_table_EvCounts_ToD_Season <- Buzz_Master %>%
  group_by(Time_of_day, Season) %>%
  summarise(Event_Count = n())


gam_poisson_ToD_Season <- gam(Event_Count ~ factor(Time_of_day) + factor(Season),
                 data = Model_table_EvCounts_ToD_Season, 
                 family = poisson(link = "log"))

summary(gam_poisson_ToD_Season)

## 2.2 Basic GAM with Hour (instead of ToD) and Season, no interaction 
Model_table_EvCounts_Hour_Season <- Buzz_Master %>%
  group_by(Hour, Season) %>%
  summarise(Event_Count = n())

gam_poisson_Hour_Season <- gam(Event_Count ~ factor(Hour) + factor(Season),
                            data = Model_table_EvCounts_Hour_Season, 
                            family = poisson(link = "log"))

summary(gam_poisson_Hour_Season)

## "Compared to daytime, morning and evening show significant differences 
# in porpoise click events, with lower counts in the morning and slightly higher counts in the evening. 
# Nighttime counts do not differ significantly from daytime."

##################### MODELS WITH HOUR SMOOTHER #####################

## TABLES 
# Table with Time of Day, Season, and Hour 
Model_table_EvCounts_Hour_ToD_Season <- Buzz_Master %>%
  group_by(Hour, Time_of_day, Season) %>%
  summarise(Event_Count = n())

# Table with Time of Day and Season (already created above)
Model_table_EvCounts_ToD_Season <- Buzz_Master %>%
  group_by(Time_of_day, Season) %>%
  summarise(Event_Count = n())


## MODELS 

## 2.1 GAM with Time of Day, Season, and an hourly smoother but NO INTERACTION
gam_poisson_ToD_Season_HourSmoother <- gam(Event_Count ~ s(Hour, bs = "cs") + factor(Time_of_day) + factor(Season),
                                         data = Model_table_EvCounts_Hour_ToD_Season , 
                                         family = poisson(link = "log"))

summary(gam_poisson_ToD_Season_HourSmoother)

## 2.2 GAM same as above but with AN INTERACTION 
gam_poisson_ToD_Season_HourSmoother_interaction <- gam(Event_Count ~ s(Hour, bs = "cs") + 
                   factor(Time_of_day) * factor(Season),
                 data = Model_table_EvCounts_Hour_ToD_Season, 
                 family = poisson(link = "log"))

summary(gam_poisson_ToD_Season_HourSmoother_interaction)

## 2.3 GAM with ToD and Season plus INTERACTION, no smoother !!!!!!!!!!!
gam_poisson_ToD_Season_interaction <- gam(Event_Count ~ factor(Season) * factor(Time_of_day),
                                                     data = Model_table_EvCounts_ToD_Season, 
                                                     family = poisson(link = "log"))

summary(gam_poisson_ToD_Season_interaction) # NOT A VIABLE MODEL - categories with interaction are overfitted 

################ GAMS WITH HOUR AND MONTHLY SMOOTHERS - instead of aggregated categories #############

### TABLES FOR THIS SECTION - EVENT COUNTS 
# Hour and Monthly Table
Model_table_EvCounts_Hour_Month <- Buzz_Master %>% 
  group_by(Hour, Month) %>%
  summarise(Event_Count = n())

# ToD and Month 
Model_table_EvCounts_ToD_Month <- Buzz_Master %>% 
  group_by(Time_of_day, Month) %>%
  summarise(Event_Count = n())

# ToD, Season, and Month
Model_table_EvCounts_ToD_Season_Month <- Buzz_Master %>% 
  group_by(Time_of_day, Season, Month) %>%
  summarise(Event_Count = n())

# Hour and Monthly Table
Model_table_EvCounts_Hour_Month <- Buzz_Master %>% 
  group_by(Hour, Month) %>%
  summarise(Event_Count = n())

View(Model_table_EvCount_ToD_Month)
View(Model_table_EvCount_ToD_Season_Month)
View(Model_table_EvCount_Hour_Month)


# 2.4 Hourly and Monthly smoothers instead of aggregated categories 
gam_poisson_HourSmoother_MonthlySmoother <- gam(Event_Count ~ s(Hour, k=10) + s(Month, k=12),
                                                         family=poisson(link = 'log'),
                                                         data=Model_table_EvCounts_Hour_Month)

summary(gam_poisson_HourSmoother_MonthlySmoother)
plot(gam_poisson_HourSmoother_MonthlySmoother)

## 2.5 Hour and Monthly smoothers + interaction 
gam_poisson_HourSmoother_MonthlySmoother_interaction <- gam(Event_Count ~ s(Hour, k=10) + s(Month, k=12) + 
                                                          te(Hour, Month, k=c(10, 12)), 
                                                          family=poisson(link = 'log'),
                                                          data=Model_table_EvCounts_Hour_Month)

summary(gam_poisson_HourSmoother_MonthlySmoother_interaction)
plot(gam_poisson_HourSmoother_MonthlySmoother)


# 2.6 ToD, Monthly smoother
gam_poisson_ToD_MonthlySmoother <- gam(Event_Count ~ factor(Time_of_day) + s(Month, k=12), 
                                                         family=poisson(link = 'log'),
                                                         data=Model_table_EvCounts_ToD_Month)
summary(gam_poisson_ToD_MonthlySmoother)
plot(gam_poisson_ToD_MonthlySmoother)


# 2.7 ToD, Season, Monthly smoother
gam_poisson_ToD_Season_MonthlySmoother <- gam(Event_Count ~ factor(Time_of_day) + factor(Season) + s(Month, k=12), 
                                    family=poisson(link = 'log'),
                                    data=Model_table_EvCount_ToD_Season_Month)

summary(gam_poisson_ToD_Season_MonthlySmoother)
plot(gam_poisson_ToD_Season_MonthlySmoother)

# ToD, Season, interaction, Monthly smoother
gam_poisson_ToD_Season_interaction_MonthlySmoother <- gam(Event_Count ~ factor(Time_of_day) * factor(Season) + s(Month, k=12), 
                                           family=poisson(link = 'log'),
                                           data=Model_table_EvCount_ToD_Season_Month)

summary(gam_poisson_ToD_Season_interaction_MonthlySmoother)
plot(gam_poisson_ToD_Season_interaction_MonthlySmoother)

# dispersion check 
dispersiontest(gam_poisson_ToD_Season_interaction_MonthlySmoother)

###############################################################################################################################
##################### MODELS WITH RECORDING EFFORT WEIGHT #####################

# SEASONAL RECORDING HOURS 
26788 # TOTAL 
7015/26788 # autumn (26.18%) 
6369/26788 # spring (23.77%)
6624/26788 # summer (24.72%)
6780/26788 # winter (25.31%)


# TABLES WITH EVENT COUNTS AND RECORDING HOURS PLUS TEMPORAL VARIABLES 

# table ToD and Season
Model_table_RecEffort_ToD_Season <- Recording_Effort %>%
  group_by(Time_of_day, Season) %>%
  summarise(Recording_Effort_Hours = sum(recording_effort))

# table ToD, Hour, Season
Model_table_RecEffort_ToD_Hour_Season <- Recording_Effort %>%
  group_by(Time_of_day, Hour, Season) %>%
  summarise(Recording_Effort_Hours = sum(recording_effort))

# table Hour, ToD, Month, Season
Model_table_RecEffort_Hour_ToD_Month_Season <- Recording_Effort %>%
  group_by(Hour, Time_of_day, Month, Season) %>%
  summarise(Recording_Effort_Hours = sum(recording_effort))

# table Hour and Month
Model_table_RecEffort_Hour_Month<- Recording_Effort %>%
  group_by(Hour, Month) %>%
  summarise(Recording_Effort_Hours = sum(recording_effort))

# table ToD and Month
Model_table_RecEffort_ToD_Month<- Recording_Effort %>%
  group_by(Time_of_day, Month) %>%
  summarise(Recording_Effort_Hours = sum(recording_effort))

# table ToD, Season, and Month
Model_table_RecEffort_ToD_Season_Month<- Recording_Effort %>%
  group_by(Time_of_day, Season, Month) %>%
  summarise(Recording_Effort_Hours = sum(recording_effort))

View(Model_table_RecEffort_Hour_Month)
View(Model_table_RecEffort_ToD_Season)
View(Model_table_RecEffort_ToD_Hour_Season)
View(Model_table_RecEffort_Hour_ToD_Month_Season)
View(Model_table_RecEffort_ToD_Month)
View(Model_table_RecEffort_ToD_Season_Month)

# JOIN THE VESSEL PRESENCE AND EVENT COUNT TABLES BASED ON THE TEMPORAL VARIABLES 
# Join for ToD and Season
Model_table_EvCounts_RecEffort_ToD_Season <- Model_EvCounts_ToD_Season %>%
  left_join(Model_table_RecEffort_ToD_Season, by = c("Time_of_day", "Season"))

View(Model_table_EvCounts_RecEffort_ToD_Season)

View(Model_table_EvCounts_Hour_ToD_Season)
View(Model_table_RecEffort_ToD_Hour_Season)
# Join for ToD, Hour, and Season 
Model_table_EvCounts_RecEffort_ToD_Hour_Season <- Model_table_EvCounts_Hour_ToD_Season %>%
  left_join(Model_table_RecEffort_ToD_Hour_Season, by = c("Time_of_day", "Hour", "Season"))
  
View(Model_table_EvCounts_RecEffort_ToD_Hour_Season)

# Join for Hour and Month 
Model_table_EvCounts_RecEffort_Hour_Month <- Model_table_EvCount_Hour_Month %>%
  left_join(Model_table_RecEffort_Hour_Month, by = c( "Hour", "Month"))

# Join for ToD and Month
Model_table_EvCounts_RecEffort_ToD_Month <- Model_table_EvCount_ToD_Month %>%
  left_join(Model_table_RecEffort_ToD_Month, by = c( "Time_of_day", "Month"))
View(Model_table_EvCounts_RecEffort_ToD_Month)

# Join for ToD, Season, and Month
Model_table_EvCounts_RecEffort_ToD_Season_Month <- Model_table_EvCount_ToD_Season_Month %>%
  left_join(Model_table_RecEffort_ToD_Season_Month, by = c( "Time_of_day", "Season", "Month"))
View(Model_table_EvCounts_RecEffort_ToD_Season_Month)


#################### NOW RUN THE WEIGHTED MODELS ####################

# 3.1 ToD and Season
gam_poisson_WEIGHTED_ToD_Season <- gam(Event_Count ~ factor(Season) + factor(Time_of_day),
      family = poisson(link = "log"), 
      data = Model_table_EvCounts_RecEffort_ToD_Season,
      weights = Recording_Effort_Hours)

summary(gam_poisson_WEIGHTED_ToD_Season) # dev est basically the same, but UBRE muuuuuch higher than non-weighted 
summary(gam_poisson_ToD_Season)

# 3.2 ToD, Season, interaction, no smoother 
gam_poisson_WEIGHTED_ToD_Season_interaction <- gam(Event_Count ~ factor(Season) * factor(Time_of_day),
                                                 family = poisson(link = "log"), 
                                                 data = Model_table_EvCounts_RecEffort_ToD_Season,
                                                 weights = Recording_Effort_Hours)

summary(gam_poisson_WEIGHTED_ToD_Season_interaction) ## THESE MODELS ARE OVERFITTED (weighted and non-weighted)

# 3.3 ToD, Season, Hour smoother, no interaction 
gam_poisson_WEIGHTED_ToD_Season_HourSmoother <- gam(Event_Count ~ s(Hour, bs = "cs") +
                                                    factor(Season) + factor(Time_of_day) ,
                                                  family = poisson(link = "log"), 
                                                  data = Model_table_EvCounts_RecEffort_ToD_Hour_Season,
                                                  weights = Recording_Effort_Hours)

summary(gam_poisson_WEIGHTED_ToD_Season_HourSmoother)
summary(gam_poisson_ToD_Season_HourSmoother)

# 3.4 ToD, Season, Interaction, Hour smoother 
gam_poisson_WEIGHTED_ToD_Season_HourSmoother_interaction <- gam(Event_Count ~ s(Hour, bs = "cs") +
                                                                factor(Time_of_day) * factor(Season) ,
                                                              family = poisson(link = "log"), 
                                                              data = Model_table_EvCounts_RecEffort_ToD_Hour_Season,
                                                              weights = Recording_Effort_Hours)

summary(gam_poisson_WEIGHTED_ToD_Season_HourSmoother_interaction)
summary(gam_poisson_ToD_Season_HourSmoother_interaction)
plot(gam_poisson_WEIGHTED_ToD_Season_HourSmoother_interaction)

# check dispersion 
dispersiontest(gam_poisson_WEIGHTED_ToD_Season_HourSmoother_interaction, trafo = 1)
plot(residuals(gam_poisson_WEIGHTED_ToD_Season_HourSmoother_interaction, type = "response"))

# 3.5 Hour and monthly smoothers AND INTERACTION
View(Model_table_EvCounts_RecEffort_Hour_Month)
gam_poisson_WEIGHTED_HourSmoother_MonthlySmoother_interaction <- gam(Event_Count ~ s(Hour, k=10) + s(Month, k=12) + 
                                                         te(Hour, Month, k=c(10, 12)), 
                                                       family=poisson(link = 'log'),
                                                       data= Model_table_EvCounts_RecEffort_Hour_Month, 
                                                       weights = Recording_Effort_Hours)


summary(gam_poisson_WEIGHTED_HourSmoother_MonthlySmoother_interaction)
plot(gam_poisson_WEIGHTED_HourSmoother_MonthlySmoother_interaction)
## the plot for these are a mess - this makes sense why the dispersion is really bad as well 

gam.check(gam_poisson_WEIGHTED_HourSmoother_MonthlySmoother_interaction) # this looks really good for the smoothers values 

dispersiontest(gam_poisson_WEIGHTED_HourSmoother_MonthlySmoother_interaction) # REALLY UNDERDISPERSED 

# 3.6 Hour and monthly smoother, no interaction 

gam_poisson_WEIGHTED_HourSmoother_MonthlySmoother<- gam(Event_Count ~ s(Hour, k=10) + s(Month, k=12),
                                                                   family=poisson(link = 'log'),
                                                                   data= Model_table_EvCounts_RecEffort_Hour_Month, 
                                                                   weights = Recording_Effort_Hours)

summary(gam_poisson_WEIGHTED_HourSmoother_MonthlySmoother)
summary(gam_poisson_HourSmoother_MontlySmoother)
plot(gam_poisson_WEIGHTED_HourSmoother_MonthlySmoother) # better than the plot with an interaction

dispersiontest(gam_poisson_WEIGHTED_HourSmoother_MonthlySmoother) # overdispersed 

# 3.7 ToD with monthly smoother, no interaction 

gam_poisson_WEIGHTED_ToD_MonthlySmoother <- gam(Event_Count ~ factor(Time_of_day) + s(Month, k=12), 
                                                                   family=poisson(link = 'log'),
                                                                   data= Model_table_EvCounts_RecEffort_ToD_Month, 
                                                                   weights = Recording_Effort_Hours)
summary(gam_poisson_WEIGHTED_ToD_MonthlySmoother)
plot(gam_poisson_WEIGHTED_ToD_MonthlySmoother)
dispersiontest(gam_poisson_WEIGHTED_ToD_MonthlySmoother) # overdispersed 

# 3.8 ToD, Season, monthly smoother

View(Model_table_EvCounts_RecEffort_ToD_Season_Month)

gam_poisson_WEIGHTED_ToD_Season_interaction_MonthlySmoother  <- gam(Event_Count ~ factor(Time_of_day) * factor(Season) + s(Month, k=12), 
                                              family=poisson(link = 'log'),
                                              data= Model_table_EvCounts_RecEffort_ToD_Season_Month, 
                                              weights = Recording_Effort_Hours)

summary(gam_poisson_WEIGHTED_ToD_Season_interaction_MonthlySmoother) # BEST FIT BY FAR 
plot(gam_poisson_WEIGHTED_ToD_Season_interaction_MonthlySmoother)
plot(residuals(gam_poisson_WEIGHTED_ToD_Season_interaction_MonthlySmoother, type = "response")) # visual check for aggregations
dispersiontest(gam_poisson_WEIGHTED_ToD_Season_interaction_MonthlySmoother) # slightly overdispersed but not significant 
gam.check(gam_poisson_WEIGHTED_ToD_Season_interaction_MonthlySmoother)

# check weighted against unweighted

pred_weighted <- predict(gam_poisson_WEIGHTED_ToD_Season_interaction_MonthlySmoother, type = "response")
pred_unweighted <- predict(gam_poisson_ToD_Season_interaction_MonthlySmoother, type = "response")
cor(pred_weighted, pred_unweighted)  # High correlation indicates consistency - 0.99 is a very good correlation between models 


###################################################################################################################################
############################# MODELS FOR VESSEL PRESENCE #################################

# FIRST MAKE SURE EVERYTHING IS A FACTOR - in the dataframe 
Vessel_Presence$Season <- factor(Vessel_Presence$Season)
Vessel_Presence$Time_of_day <- factor(Vessel_Presence$Time_of_day)
Vessel_Presence$Vessel_3k <- as.factor(Vessel_Presence$Vessel_3k)

str(Vessel_Presence)

## CREATE A BINARY COLUMN FOR PORPOISE EVENT ID 
Vessel_Presence$Porpoise_Event <- ifelse(Vessel_Presence$Event_ID > 0, 1, 0)

################### CREATE THE TABLES ###########################

## NOTE THE DIFFERENCE WITH THE SUM IN THIS TABLE COMPARED TO THE N =() IN THE OTHER 

# Table with Vessel Presence, ToD, and Season ONLY 
Model_table_VesselPresence_ToD_Season <- Vessel_Presence %>%
  group_by(Time_of_day, Season) %>%
  summarise(
    Total_Count = n(),
    Vessel_3k_Present = sum(Vessel_3k)
  )

# Table with Porpoise presence (porpoise positive hours) and vessel presence 
Model_table_PorpoisePresence_VesselPresence <- Vessel_Presence %>%
  group_by( Vessel_3k) %>%
  summarise(
    Total_Count = n(),
    Porpoise_Present = sum(Porpoise_Event)
  )

### Porpoise presence, Vessel presence, ToD, Season 
Model_table_PorpoisePresence_VesselPresence_ToD_Season <- Vessel_Presence %>%
  group_by(Time_of_day, Season, Vessel_3k) %>%
  summarise(
    Total_Count = n(),
    Porpoise_Present = sum(Porpoise_Event)
  )

# Table with Porpoise Presence, Vessel Presence, ToD, Season, and Hour 
Model_table_PorpoisePresence_VesselPresence_ToD_Season_Hour <- Vessel_Presence %>%
  group_by(Hour, Time_of_day, Season, Vessel_3k) %>%
  summarise(
    Total_Count = n(),
    Porpoise_Present = sum(Porpoise_Event)
  )

# Porpoise Presence, Vessel Presence, Hour, Month 
Model_table_PorpoisePresence_VesselPresence_Hour_Month <- Vessel_Presence %>%
  group_by(Hour, Month, Vessel_3k) %>%
  summarise(
    Total_Count = n(),
    Porpoise_Present = sum(Porpoise_Event)
  )

# Porpoise Presence, Vessel Presence, ToD, Season, Month 
Model_table_PorpoisePresence_VesselPresence_ToD_Season_Month <- Vessel_Presence %>%
  group_by(Time_of_day, Season, Month, Vessel_3k) %>%
  summarise(
    Total_Count = n(),
    Porpoise_Present = sum(Porpoise_Event)
  )

# Porpoise Presence, Vessel Presence, ToD, Month 
Model_table_PorpoisePresence_VesselPresence_ToD_Month <- Vessel_Presence %>%
  group_by(Time_of_day, Month, Vessel_3k) %>%
  summarise(
    Total_Count = n(),
    Porpoise_Present = sum(Porpoise_Event)
  )

View(Model_table_VesselPresence_ToD_Season) 
View(Model_table_PorpoisePresence_VesselPresence)
View(Model_table_PorpoisePresence_VesselPresence_ToD_Season)
View(Model_table_PorpoisePresence_VesselPresence_ToD_Season_Hour)
View(Model_table_PorpoisePresence_VesselPresence_Hour_Month)
View(Model_table_PorpoisePresence_VesselPresence_ToD_Season_Month)
View(Model_table_PorpoisePresence_VesselPresence_ToD_Month)

#############################################################################################################################
######### GAMS WITH POISSON DISTRIBUTION 

## 4.1 GAM just looking at vessel presence impacted by temporal patters 
gam_poisson_VesselPresence_ToD_Season <- gam(Vessel_3k_Present ~ Season + Time_of_day,
                           family = poisson(link = "log"),
                           data = Model_table_VesselPresence_ToD_Season)

summary(gam_poisson_VesselPresence_ToD_Season)
dispersiontest(gam_poisson_VesselPresence_ToD_Season) # significantly overdispersed 
  ##  ***shows that vessel presence in the 3k exposure zone was highly significant based on temporal variation 


## 4.2 GAM now looking at porpoise events with vessel presence alone 
gam_poisson_PorpoiseEvent_VesselPresence <- gam(Porpoise_Present ~ Vessel_3k,
                                              family = poisson(link = "log"),
                                              data = Model_table_PorpoisePresence_VesselPresence)


summary(gam_poisson_PorpoiseEvent_VesselPresence) # !!!! overfitted model - something not right, might be linear relationship, n=2

## 4.3 Now full GAM with Porpoise events, vessel presence, and seasonal patterns
gam_poisson_PorpoiseEvent_VesselPresence_Season_ToD <- gam(Porpoise_Present ~ Vessel_3k + 
                                                factor(Season) + factor(Time_of_day),
                                              family = poisson(link = "log"),
                                              data = Model_table_PorpoisePresence_VesselPresence_ToD_Season)

summary(gam_poisson_PorpoiseEvent_VesselPresence_Season_ToD) 


## 4.4 Same as above but + INTERACTION
gam_poisson_PorpoiseEvent_VesselPresence_Season_ToD_interaction <- gam(Porpoise_Present ~ Vessel_3k * factor(Time_of_day) + 
                                                         factor(Season),
                                                         family = poisson(link = "log"),
                                                         data = Model_table_PorpoisePresence_VesselPresence_ToD_Season)

summary(gam_poisson_PorpoiseEvent_VesselPresence_Season_ToD_interaction) # BEST DEVIANCE EXPLAINED! 


## 4.5 ToD, Season, Hourly smoother
gam_poisson_PorpoiseEvent_VesselPresence_Season_ToD_HourlySmoother<- gam(Porpoise_Present ~ Vessel_3k + factor(Time_of_day) + 
                                                                       factor(Season) + s(Hour, k=10),
                                                                     family = poisson(link = "log"),
                                                                     data = Model_table_PorpoisePresence_VesselPresence_ToD_Season_Hour)

summary(gam_poisson_PorpoiseEvent_VesselPresence_Season_ToD_HourlySmoother)
plot(gam_poisson_PorpoiseEvent_VesselPresence_Season_ToD_HourlySmoother)

## 4.6 ToD, Season, Hourly smoother, interaction 
gam_poisson_PorpoiseEvent_VesselPresence_Season_ToD_HourlySmoother_interaction <- gam(Porpoise_Present ~ Vessel_3k * factor(Time_of_day) + 
                                                                                      factor(Season) + s(Hour, k=10),
                                                                                    family = poisson(link = "log"),
                                                                                    data = Model_table_PorpoisePresence_VesselPresence_ToD_Season_Hour)

summary(gam_poisson_PorpoiseEvent_VesselPresence_Season_ToD_HourlySmoother_interaction)
plot(gam_poisson_PorpoiseEvent_VesselPresence_Season_ToD_HourlySmoother_interaction)

## 4.7 Hourly and Monthly smoothers instead of ToD and Seasonal aggregates
gam_poisson_PorpoiseEvent_VesselPresence_HourSmoother_MonthlySmoother <- gam(Porpoise_Present ~ Vessel_3k + s(Hour, k=10) + s(Month, k=12),
                                                                           family=poisson(link = 'log'),
                                                                           data= Model_table_PorpoisePresence_VesselPresence_Hour_Month)

summary(gam_poisson_PorpoiseEvent_VesselPresence_HourSmoother_MonthlySmoother)
plot(gam_poisson_PorpoiseEvent_VesselPresence_HourSmoother_MonthlySmoother)

# 4.8 hourly and monthly smoothers + interaction 

gam_poisson_PorpoiseEvent_VesselPresence_HourSmoother_MonthlySmoother_interaction <- gam(Porpoise_Present ~ s(Hour, k=10) + s(Month, k=12) +
                                                                                       te(Hour, Month, k=c(10, 12)) + Vessel_3k,
                                                                           family=poisson(link = 'log'),
                                                                           data= Model_table_PorpoisePresence_VesselPresence_Hour_Month)

summary(gam_poisson_PorpoiseEvent_VesselPresence_HourSmoother_MonthlySmoother_interaction)
plot(gam_poisson_PorpoiseEvent_VesselPresence_HourSmoother_MonthlySmoother_interaction)

# 4.9 ToD, Season, Monthly smoother 
gam_poisson_PorpoiseEvent_VesselPresence_Season_MonthlySmoother_ToD <- gam(Porpoise_Present ~ Vessel_3k + factor(Time_of_day) + 
                                                                                      factor(Season) + s(Month, k=12),
                                                                                    family = poisson(link = "log"),
                                                                                    data = Model_table_PorpoisePresence_VesselPresence_ToD_Season_Month)

summary(gam_poisson_PorpoiseEvent_VesselPresence_Season_MonthlySmoother_ToD)
dispersiontest(gam_poisson_PorpoiseEvent_VesselPresence_Season_MonthlySmoother_ToD)

# 4.10 ToD, Season, Monthly smoother, interaction
gam_poisson_PorpoiseEvent_VesselPresence_Season_MonthlySmoother_ToD_interaction <- gam(Porpoise_Present ~ Vessel_3k * factor(Time_of_day) *  
                                                                         factor(Season) + s(Month, k=12),
                                                                         family = poisson(link = "log"),
                                                                         data = Model_table_PorpoisePresence_VesselPresence_ToD_Season_Month)

summary(gam_poisson_PorpoiseEvent_VesselPresence_Season_MonthlySmoother_ToD_interaction) # BEST MODEL 
dispersiontest(gam_poisson_PorpoiseEvent_VesselPresence_Season_MonthlySmoother_ToD_interaction) # VERY OVERDISPERSED 
plot(gam_poisson_PorpoiseEvent_VesselPresence_Season_MonthlySmoother_ToD_interaction)

# 4.11 ToD, Monthly smoother (NO SEASON)
gam_poisson_PorpoiseEvent_VesselPresence_ToD_MonthlySmoother_interaction <- gam(Porpoise_Present ~ Vessel_3k * factor(Time_of_day) + s(Month, k=12),
                                                                              family = poisson(link = "log"),
                                                                              data = Model_table_PorpoisePresence_VesselPresence_ToD_Month)
summary(gam_poisson_PorpoiseEvent_VesselPresence_ToD_MonthlySmoother_interaction)
dispersiontest(gam_poisson_PorpoiseEvent_VesselPresence_ToD_MonthlySmoother_interaction) # everything is overdispersed 


###################################################################################################################################################
################ VESSEL PRESENCE - PROPORTIONAL DISTRIBUTION INSTEAD OF COUNT #################

### TABLES
# Table with PROPORTION, Vessel Presece, ToD, Hour, Season, Month 
Model_table_PorpoiseProp_VesselPresence_ToD_Hour_Season_Month <- Vessel_Presence %>%
  group_by(Time_of_day, Hour, Season, Month, Vessel_3k) %>%
  summarise(
    Total_Count = n(),
    Porpoise_Present = sum(Porpoise_Event),
    .groups = "drop"  # Ensures the result is an ungrouped dataframe
  ) %>%
  mutate(
    Proportion_Porpoise_Event = Porpoise_Present / Total_Count
  )

# Table with PROPORTION, Vessel Presence, ToD, Month 
Model_table_PorpoiseProp_VesselPresence_ToD_Month <- Vessel_Presence %>%
  group_by(Time_of_day, Month, Vessel_3k) %>%
  summarise(
    Total_Count = n(),
    Porpoise_Present = sum(Porpoise_Event),
    .groups = "drop"  # Ensures the result is an ungrouped dataframe
  ) %>%
  mutate(
    Proportion_Porpoise_Event = Porpoise_Present / Total_Count
  )

# Table with PROPORTION, Vessel Presece, ToD, Season, Month 
Model_table_PorpoiseProp_VesselPresence_ToD_Season_Month <- Vessel_Presence %>%
  group_by(Time_of_day, Season, Month, Vessel_3k) %>%
  summarise(
    Total_Count = n(),
    Porpoise_Present = sum(Porpoise_Event),
    .groups = "drop"  # Ensures the result is an ungrouped dataframe
  ) %>%
  mutate(
    Proportion_Porpoise_Event = Porpoise_Present / Total_Count
  )

View(Model_table_PorpoiseProp_VesselPresence_ToD_Hour_Season_Month)
View(Model_table_PorpoiseProp_VesselPresence_ToD_Month)
View(Model_table_PorpoiseProp_VesselPresence_ToD_Season_Month)

# 5.1 BIG GAM WITH ALL TEMPORAL DATA TO LOOK AT HOW WIGGLY EVERYTHING IS 
gam_beta_big_test_model <- gam(Proportion_Porpoise_Event ~ Vessel_3k + factor(Time_of_day) + s(Hour, k=12) + factor(Season) + s(Month, k=12),
                               family = betar(link = "logit"),
                               data = Model_table_PorpoiseProp_VesselPresence_ToD_Hour_Season_Month)
summary(gam_beta_big_test_model)
plot(gam_beta_big_test_model) 

## CONCLUSIONS: 
  # the hourly variable is generally U shaped and likely can be summarized with a ToD category 
  # the monthly variable is highly variable (wiggly) and should not be categorized into seasons


# 5.2 beta distributed model to test - ToD, Monthly smoother, interaction
gam_beta_PorpoiseProp_VesselPresence_ToD_MonthlySmoother_interaction <- gam(Proportion_Porpoise_Event ~ Vessel_3k * factor(Time_of_day) + s(Month, k=12),
                                                                              family = betar(link = "logit"),
                                                                              data = Model_table_PorpoiseProp_VesselPresence_ToD_Month)
summary(gam_beta_PorpoiseProp_VesselPresence_ToD_MonthlySmoother_interaction)

## 5.2 DIAGNOSTICS 
  # plot to check residuals (want to see no pattern)
  plot(residuals(gam_beta_PorpoiseProp_VesselPresence_ToD_MonthlySmoother_interaction, type = "pearson"), main = "Residuals (Beta Model)", 
      xlab = "Index", ylab = "Pearson Residuals", pch = 20)
  abline(h = 0, col = "red")

  # plot Q-Q - want all residuals to be close to the line 
  qqnorm(residuals(gam_beta_PorpoiseProp_VesselPresence_ToD_MonthlySmoother_interaction, type = "pearson"))
  qqline(residuals(gam_beta_PorpoiseProp_VesselPresence_ToD_MonthlySmoother_interaction, type = "pearson"), col = "red")

  plot(fitted(gam_beta_PorpoiseProp_VesselPresence_ToD_MonthlySmoother_interaction), 
       residuals(gam_beta_PorpoiseProp_VesselPresence_ToD_MonthlySmoother_interaction, type = "pearson"), 
     xlab = "Fitted Values", ylab = "Pearson Residuals",
     main = "Residuals vs Fitted Values", pch = 20)
  abline(h = 0, col = "red")

  # check for multi-collinearity 
  library(car)
  vif(gam_beta_PorpoiseProp_VesselPresence_ToD_MonthlySmoother_interaction) # THIS DOESNT REALLY WORK WITH THIS MODEL TYPE?
  
# 5.3 Beta with ToD, Season, Monthly smoother to compare 
gam_beta_PorpoiseProp_VesselPresence_ToD_Season_MonthlySmoother <- gam(Proportion_Porpoise_Event ~ Vessel_3k + 
                                                                          factor(Time_of_day) * factor(Season) + s(Month, k=12),
                                                                          family = betar(link = "logit"),
                                                                          data = Model_table_PorpoiseProp_VesselPresence_ToD_Season_Month)
summary(gam_beta_PorpoiseProp_VesselPresence_ToD_Season_MonthlySmoother)
plot(gam_beta_PorpoiseProp_VesselPresence_ToD_Season_MonthlySmoother)

  # 5.3 DIAGNOSTICS
  # plot to check residuals (want to see no pattern)
  plot(residuals(gam_beta_PorpoiseProp_VesselPresence_ToD_Season_MonthlySmoother, type = "pearson"), main = "Residuals (Beta Model)", 
     xlab = "Index", ylab = "Pearson Residuals", pch = 20)
  abline(h = 0, col = "red")
  
  # plot Q-Q - want all residuals to be close to the line 
  qqnorm(residuals(gam_beta_PorpoiseProp_VesselPresence_ToD_Season_MonthlySmoother, type = "pearson"))
  qqline(residuals(gam_beta_PorpoiseProp_VesselPresence_ToD_Season_MonthlySmoother, type = "pearson"), col = "red")
  
  ## HONESLTY REALLY SIMILAR TO THE MODEL ABOVE WITHOUT SEASON - not sure what to use here 


############################################################## trying quasi-binomial instead but NOT GOOD ###############
# quasi-binomial model - ToD, Monthly smoother, interaction
gam_quasi_binom_model <- gam(
  cbind(Porpoise_Present, Total_Count - Porpoise_Present) ~ 
    Vessel_3k * factor(Time_of_day) + s(Month, k = 12),
  family = quasibinomial,
  data = Model_table_PorpoisePresence_VesselPresence_ToD_Month
)
summary(gam_quasi_binom_model) # OVERALL THIS IS NOT A GOOD FIT COMPARED TO BETA 


# check for dispersion
deviance_residuals <- residuals(quasi_binom_model, type = "deviance")
dispersion <- sum(deviance_residuals^2) / quasi_binom_model$df.residual
dispersion # EXTREMELY OVERDISPERSED

## look at the adjusted r2 and explained deviance to compare the models, is one much better than the other? 

## IN THIS CASE THE QUASI IS EXTREMEMLY OVERDISPERSED AND NOT A GOOD FIT FOR THESE TYPES OF DATA - 
  # maybe just because they do not have 0 or 1 naturally, all some sort of proportion between those values 

###################################################################################################################################
################################ MODELS WITH VESSEL IMPACT ##################################

## need to make simpler tables for each model to be concise with the data used 

#  BIG table with Event Counts, Vessel Exposure, and all temporal variables 
Model_table_EvCounts_Hour_ToD_Month_Season_VesselExposure <- Buzz_Master %>% 
  group_by(Hour, Time_of_day, Month, Season, Vessel_Exposure) %>%
  summarise(Event_Count = n())
View(Model_table_EvCounts_Hour_ToD_Month_Season_VesselExposure)

## 5.1 Basic GAM with Vessel Exposure, ToD, Season, and no interactions
gam_poisson_ToD_Season_VesselExposure <- gam(Event_Count ~ factor(Time_of_day) + 
                                             factor(Season) + factor(Vessel_Exposure),
                                           data = gam_even_counts_ToD_Hour_Season_VesselExposure, 
                                           family = poisson(link = "log"))

summary(gam_poisson_ToD_Season_VesselExposure) ## Adjusted R-Sq and Deviance Explained drop substantially - model complexity 
          
## 5.2 same variable as above plus an INTERACTION                               
gam_poisson_ToD_Season_VesselExposure_interaction <- gam(Event_Count ~ factor(Time_of_day) * factor(Vessel_Exposure) + 
                                             factor(Season),
                                           data = Model_table_EvCounts_Hour_ToD_Month_Season_VesselExposure, 
                                           family = poisson(link = "log"))                                          

summary(gam_poisson_ToD_Season_VesselExposure_interaction) ## INTERACTION HELPS A LOT!!    

######################################################################################################################
## BUZZ CLICK TRAINS VS VESSEL EXPOSURE 

library(mgcv)
gam_Buzz_Train <- gam(Buzz_Train ~ Vessel_Exposure + factor(Time_of_day) * factor(Season) + s(Hour, bs = "cs") , family = binomial(), data = Buzz_Master)
summary(gam_Buzz_Train)

glm_Buzz_Train <- glm(Buzz_Train ~ Vessel_Exposure, family = binomial(), data = Buzz_Master)
summary(glm_Buzz_Train)

# + s(Season) + s(Time_of_day) - get error when there are smoothers - too few categories (k =4)
str(Buzz_Master)
Buzz_Master$Buzz_Train <- as.numeric(Buzz_Master$Buzz_Train)

######################################################################################################################
##############################################   - DATA SUBSET -  ###################################################


## making a new column to get the median of all Third Octave values less than 2000Hz 
Buzz_Noise_Monitor_Oct2018 <- Buzz_Noise_Monitor_Oct2018 %>%
  mutate(Median_ThirdOctave_2000Hz = apply(select(., ThirdOctave_447_561_median, 
                                                  ThirdOctave_561_709_median, 
                                                  ThirdOctave_709_894_median, 
                                                  ThirdOctave_894_1118_median, 
                                                  ThirdOctave_1118_1414_median, 
                                                  ThirdOctave_1414_1788_median, 
                                                  ThirdOctave_1788_2236_median), 
                                           1, median, na.rm = TRUE))

noise_lm <- lm(Median_ThirdOctave_2000Hz ~ Noise_Exposure * Average_Speed + Vessel_Count, data = Buzz_Noise_Monitor_Oct2018)
summary(noise_lm)

noise_lm <- lm(Median_ThirdOctave_2000Hz ~ Noise_Exposure * Average_Speed, data = Buzz_Noise_Monitor_Oct2018)
summary(noise_lm)

  # diagnostics 
residuals <- residuals(noise_lm)

plot(fitted(noise_lm), residuals, 
     xlab = "Fitted Values", 
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0

qqnorm(residuals, 
       main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red") 

## shows a heavy skew to the left of what is otherwise a normal distribution (long tail to the right)
hist(Buzz_Noise_Monitor_Oct2018$Median_ThirdOctave_2000Hz, breaks = 50, main = "Histogram of Noise Levels")

# RUNNING WITH A LOG TRANSFORMATION (and setting the minimum of 50 back to 1 so that it does not skew the log)
Buzz_Noise_Monitor_Oct2018$logMedian_ThirdOctave_2000Hz <- log(Buzz_Noise_Monitor_Oct2018$Median_ThirdOctave_2000Hz - 49)

lm_log_noise <- lm(logMedian_ThirdOctave_2000Hz ~ Noise_Exposure * Average_Speed + Vessel_Count, 
                data = Buzz_Noise_Monitor_Oct2018)
summary(lm_log_noise)

residuals_log <- residuals(lm_log_noise)
plot(fitted(lm_log_noise), residuals_log, 
     xlab = "Fitted Values", 
     ylab = "Residuals",
     main = "Residuals vs Fitted Values - lm w/ log")
abline(h = 0, col = "red")  # Add a horizontal line at 0

qqnorm(residuals_log, 
       main = "Q-Q Plot of Residuals - lm w/ log")
qqline(residuals_log, col = "red") # the log transformation helps slightly but there is still something fundamental in this data 
## NOT LINEAR OR NORMAL DISTRIBUTION !!!

## NOW TRYING A GAMMA DISTRIBUTION 

glm_gamma <- glm(Median_ThirdOctave_2000Hz ~ Noise_Exposure * Average_Speed, 
                   family = Gamma(link = "log"), 
                   data = Buzz_Noise_Monitor_Oct2018)

summary(glm_gamma) # this is a better fit ??

  ## now diagnostics for the gamma model 
plot(model_gamma$residuals ~ model_gamma$fitted.values, 
     main = "Residuals vs. Fitted (Gamma Model)", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(model_gamma$residuals)
qqline(model_gamma$residuals, col = "red")

## GAM with gamma distribution

gam_gamma_noise <- gam(Median_ThirdOctave_2000Hz ~ Noise_Exposure * Average_Speed * Vessel_Count, 
                       family = Gamma(link = "log"), 
                       data = Buzz_Noise_Monitor_Oct2018)
summary(gam_gamma_noise)
 ## this model explains the most deviance when all variables and interactions are included 
    # should we include vessel types in here? Some way to do a hierarchical vessel type? 



##################################

glm_binary_test <- glm(Buzz_Train ~ Vessel_Exposure + Vessel_Count + Average_Speed,
                       family = binomial(link = "logit"),
                       data = Buzz_Oct2018_subset)
summary(glm_binary_test)  

# Extract residual deviance and degrees of freedom
residual_deviance <- glm_binary_test$deviance
residual_df <- glm_binary_test$df.residual

# Calculate the dispersion ratio
dispersion_ratio <- residual_deviance / residual_df
print(dispersion_ratio)
       

       
       
       




