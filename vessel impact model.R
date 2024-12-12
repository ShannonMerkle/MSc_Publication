## Vessel Impacts ##  -----

################################################
## Set up ----

# Load packages
library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)
library(DHARMa)
library(car)

## Clean vessel presence of 0/1 effort 
vp <- Vessel_Presence_20241203

vp2 <- filter(vp, vp$Recording_Effort == 0)
vp2 <- filter(vp2, vp2$Porpoise_Event == 1)

unique(vp2$Event_ID)

# Clear vessel presence
Vessel_Presence_20241212 <- anti_join(vp, vp2, by = "Event_ID")
saveRDS(Vessel_Presence_20241212, "Vessel_Presence_20241212.rds")

# Clear buzz_df
bdf <- Buzz_Master_20241204

Buzz_Master_20241212 <- anti_join(bdf, vp2, by = "Event_ID")
saveRDS(Buzz_Master_20241212, "Buzz_Master_20241212.rds")


## Load df 
buzzdf <- Buzz_Master_20241212

# Clean variable type 
buzzdf$Daylight <- as.factor(buzzdf$Daylight)
buzzdf$Month <- as.numeric(buzzdf$Month)
buzzdf$Exposure_3k <- as.factor(buzzdf$Exposure_3k)

################################################
## Model vessel impacts
buzzdf2 <- filter(buzzdf, buzzdf$Buzz_Rate != 0.0)
buzzdf3 <- filter(buzzdf2, buzzdf2$Buzz_Clicks > 5)
hist(buzzdf2$Buzz_Rate)buzzdf2$Buzz_Clicks

## 1hr long events 
buzzdf4 <- filter(buzzdf3, buzzdf3$Total_Minutes <61)
hist(buzzdf4$Total_Minutes)

## Binomial model of proportions
model <- glm(cbind(Buzz_Clicks, Total_Clicks - Buzz_Clicks) ~ Exposure_3k, data = buzzdf4, family = binomial)
summary(model)

#Diagnostics
# Simulate residuals
sim_residuals <- simulateResiduals(fittedModel = model)
plot(sim_residuals)

#Overdispersion check
residual_deviance <- deviance(model)
residual_df <- df.residual(model)
dispersion_stat <- residual_deviance / residual_df

## Quasibinomial for overdispersed and non-normally distributed data
model1 <- glm(Buzz_Rate ~ Exposure_3k, data = buzzdf4, family = quasibinomial)
summary(model1)

# Diagnostics
plot(model1)

# residuals
crPlots(model1)

################################################

## Quasibinomial with environmental variables 
# BEST MODEL
model2 <- glm(Buzz_Rate ~ Exposure_3k*Daylight + (1|Month), data = buzzdf4, family = quasibinomial)
summary(model2)

# Diagnostics
plot(model2)


## without month 
model3 <- glm(Buzz_Rate ~ Exposure_3k*Daylight, data = buzzdf4, family = quasibinomial)
summary(model3)

#Diagnostics
plot(model3)


## Month as a fixed effect
model4 <- glm(Buzz_Rate ~ Exposure_3k*Daylight + Month, data = buzzdf4, family = quasibinomial)
summary(model4) ## No effect



# Visualise
ggplot(buzzdf4, aes(x = Exposure_3k, y = Buzz_Rate, fill = Daylight)) +
  geom_boxplot() +
  labs(x = "Vessel Presence", y = "Proportional Buzz Rate")

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
