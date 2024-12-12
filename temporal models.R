 ## Temporal & diel pattern trends ##  -----

################################################
## Load packages
library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)

## Load df 
daydf <- temporal_df
daydf2 <- Vessel_Presence_20241203

## Clean variable type 
daydf2$Daylight <- as.factor(daydf2$Daylight)
daydf2$Month <- as.numeric(daydf2$Month)
daydf2$Year <- as.numeric(daydf2$Year)
daydf2$Vessel_3k <- as.factor(daydf2$Vessel_3k)


## Temporal trends #######################################

#QUESTION#
## When are porpoises most active in a day - does that vary by season? and 
## when are porpoises most active within a year and does that vary between years?

## Proportion of porpoise events:
# By daylight
model1 <- glm(Proportion_Porpoise_Event ~ Daylight,data = daydf)
summary(model1) 

## By season
model2 <- glm(Proportion_Porpoise_Event ~ factor(Month),data = daydf)
summary(model2) 

## Does daylight activity change by season?
model3 <- glm(Proportion_Porpoise_Event ~ factor(Month)*Daylight + (1|Year), data = daydf)
summary(model3)

## Weighted porpoise minutes by recorder effort: - more data retained in model
## GAM of season by year
modelgam <- gam(Porpoise_Event ~ s(Month, bs = "cc", by = Daylight) + Daylight, 
                data = daydf2, weights = Recording_Effort)
summary(modelgam)
plot(modelgam)

## GLM of season and year with a random effect of year 
model4 <- glm(Porpoise_Event ~ factor(Month)*Daylight + (1|Year), data = daydf2,
              family = binomial(link = "logit"),
              weights = Recording_Effort)
summary(model4)
## They vocalise more at night (stderror = 0.02, z = 46.82, p < 0.001), this is 
## consistent across seasons and throughout years. They vocalise the most between 
## spring - autumn, but less so in the winter. 


## Vessel temporal trends ####################################

## GLM of season and year with a random effect of year 
modelv1 <- glm(Vessel_3k ~ factor(Month)*Daylight + (1|Year), data = daydf2,
              family = binomial(link = "logit"),
              weights = Recording_Effort)
summary(modelv1)

## Vessel are present less in the night than the day, this is true across season and year. 
## They are most present from July - September. 

## Vessel overlap #######################################

#QUESTION#
## When do vessels overlap with porpoise presence - in a day, - in a season? Does this 
## change across years?

## Create an overlap column
daydf2 <- Vessel_Presence_20241203
daydf2$Overlap <- ifelse(daydf2$Porpoise_Event > 0 & daydf2$Vessel_3k > 0, 1, 0)
daydf3 <- filter(daydf2, Porpoise_Event == 1)
daydf3$Overlap <- as.factor(daydf3$Overlap)
daydf3$Year <- as.numeric(daydf3$Year)

## GLM of season and year with a random effect of year 
moodelvo <- glm(Overlap ~ factor(Month)*Daylight + (1|Year), data = daydf3, 
                family = binomial(link="logit"), 
                weights = Recording_Effort)

summary(moodelvo)

#####################################################

