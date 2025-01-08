#######################################################################################################################
################################## FINAL MODELS ##################################

# load packages: 
library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)

#   Data: temporal_df (daydf) 
#         Vessel_Presence_20241212 (daydf2)
#         (daydf3)
#         (buzzdf4)

## WHAT VARIABLES AND CHANGES TO DF WERE DONE FOR EACH MODEL (only using events <= 60 mins, removing zeros, etc)

########################################################################################################################
##### TEMPORAL VARIABLES 

## QUESTIONS:
# 1. When are porpoises most active in a day - does that vary by season? and 
# 2. When are porpoises most active within a year and does that vary between years?
# 3. Does daylight activity change by season?

## DESCRIPTION:
#   Porpoise_Event is binary based on MINUTES with porpoise event presence 

# model3 - NOT SURE IF THIS IS THE CORRECT FINAL MODEL 
ModelTemporal_1 <- glm(Proportion_Porpoise_Event ~ factor(Month)*Daylight + (1|Year), data = daydf)
summary(ModelTemporal_1)

plot(ModelTemporal_1)

# model4 - temporal models 
ModelTemporal_2 <- glm(Porpoise_Event ~ factor(Month)*Daylight + (1|Year), data = daydf2,
              family = binomial(link = "logit"),
              weights = Recording_Effort)
summary(ModelTemporal_2)
# Diagnostics 
plot(ModelTemporal_2)

# SUMMARY OF RESULTS:
#   Vocalise more at night 
#   Consistent across all months but most prominent between May October 
# small exception for June at night with is not significantly different than daytime 

## VISUALS: 
PLOT_PPM_VesselPresence_Temporal

########################################################################################################################
#####   TEMPORAL VARIABLES + VESSEL PRESENCE
#         daydf2

## QUESTIONS: 
# 1. when are vessels most present (minutes) during a day?
# 2. Is there any seasonal variation in vessel activity we need to be aware of? 
# 3. Does daylight activity change by season?

## Description 
#   Porpoise_Event is binary based on MINUTES with porpoise event presence AND Vessel_3k binary MINUTE 

# modelv1 - temporal models
ModelVesselPresence_Temporal <- glm(Vessel_3k ~ factor(Month)*Daylight + (1|Year), data = daydf2,
               family = binomial(link = "logit"),
               weights = Recording_Effort)
summary(ModelVesselPresence_Temporal)
# Diagnostics
plot(ModelVesselPresence_Temporal)

# SUMMARY OF RESULTS:
## Vessel are present less in the night than the day, this is true across season and year. 
## They are most present from July - September. 

## VISUALS: 
PLOT_PPM_VesselPresence_Temporal

########################################################################################################################
######  TEMPORAL VARIABLES + VESSEL OVERLAP 
#         daydf3

## QUESTIONS:
# 1. When do vessels overlap with porpoise events? Does this vary diurnally? 
# 2. Is this prominent across months/seasons?
# 3. Do these results compare to patterns of vessel presence above?

## DESCRIPTION: 
#   Overlap is a binary response to if BOTH porpoise event and vessel_3k occurred for each minute - looking ONLY when both happen 

# moodelvo - temporal models
ModelVesselOverlap_Temporal <- glm(Overlap ~ factor(Month)*Daylight + (1|Year), data = daydf3, 
                family = binomial(link="logit"), 
                weights = Recording_Effort)

summary(ModelVesselOverlap_Temporal)
# Diagnostics 
plot(ModelVesselOverlap_Temporal)

## SUMMARY OF RESULTS: (these results are in log-odds because of binary response variable)
  # There is no significant difference in overlap ALONE diurnally across all months
  # However, significantly more overlap occurrence at nighttime compared to daytime
  # this effect is amplified from late spring to late summer (roughly May to September)
    # with MORE overlap during these months at night
    # and LESS overlap during these months during the day 


## VISUALS:
PLOT_PPM_VesselPresence_Temporal


########################################################################################################################
#####   BUZZ RATE + VESSEL PRESENCE + TEMPORAL (Daylight and Month) 
#         buzzdf4

## QUESTIONS:
# 1. Does the presence of a vessel impact the proportion of a porpoise event that is made up of buzz clicks?
# 2. Does this vary across diurnal patterns
# 3. Does this change across months/seasons?
# 4. How does this relate to the results from porpoise presence?

## Description: 
#     Uses Buzz Rate which is number of buzz clicks/total clicks from porpoise events (not in minute bins) and binary Exposure_3k
#     Only used events when buzz rate was > 0.0 (only looked at buzz events) AND when buzz clicks >5

# model2 - vessel impact model 
ModelBuzz_VesselPresence_Temporal <- glm(Buzz_Rate ~ Exposure_3k*Daylight + (1|Month), data = buzzdf4, family = quasibinomial)
summary(ModelBuzz_VesselPresence_Temporal)
# Diagnostics
plot(ModelBuzz_VesselPresence_Temporal)
# quasibinomial already accounts for overdispersion 


## SUMMARY OF RESULTS:
  # buzz rate lowest when no vessels during daytime
  # lower when vessels present 
  # lower at night 
  # buzz rate increased significantly at night when vessels were present 

  # vessel presence has a strong negative effect on buzz rate, with a 44% reduction in buzz rate when vessels present
  # buzz rate is 22% lower at night compared to day 
  # these combined have strong implications on buzz rate, with an increased buzz rate at night when vessels are present,
  # and significantly reduced buzz rate during the day when vessels are not present 

## VISUALS: 


########################################################################################################################
#####   BUZZ RATE + VESSEL OVERLAP + TEMPORAL (Daylight)
#         buzzdf4

## QUESTIONS:
# 1. Does the proportion of vessel exposure time impact the proportion of an event which contains buzz clicks?
# 2. Does this change based on diurnal patterns? Do these patterns match natural diurnal variation?
# 3. Is there any seasonal variation? -- not included in model so no?

## DESRIPTION:
#   Buzz Rate from porpoise event (no minute bins) and Vessel_Overlap as a function of Minutes vessel present/total event minutes
#   Vessel_Overlap contains 0.0 if no vessels overlapped
#   Only used events when buzz rate was > 0.0 AND when buzz clicks >5
#   MAXIMUM EVENT TIME WAS 60 MINUTES - show this in visual to justify?


# model5 - vessel impact model
ModelBuzz_VesselOverlap_Temporal <- glm(Buzz_Rate ~ Vessel_Overlap*Daylight, data = buzzdf4, family = quasibinomial)
summary(ModelBuzz_VesselOverlap_Temporal)
# Diagnostics
plot(ModelBuzz_VesselOverlap_Temporal)

## SUMMARY OF RESULTS:

## VISUALS:
ggplot(buzzdf4, aes(x = Vessel_Overlap, y = Buzz_Rate)) +
  geom_smooth(method = "lm") +
  labs(x = "Vessel Overlap", y = "Proportional Buzz Rate")

########################################################################################################################
## NOISE + 

## QUESTIONS:
# 1. 

## DESCRIPTION: 
# 

model # former model name 

# DIAGNOSTICS

## SUMMARY OF RESULTS: 

## VISUALS: 









