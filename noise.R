## Noiseband impact ##  -----

################################################
## Load packages
library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)

## Load df 
noisedf <- Buzz_Master_Subset_Oct2018_20250119
noisedf <- filter(noisedf, noisedf$Total_Clicks > 6)
noisedf <- filter(noisedf, noisedf$Total_Minutes <61)

################################################
## Vessels and noise -----

## Noise and presence 
modelp <- lm(Normalized_AUC ~ Exposure_3k, data = noisedf)
summary(modelp)

#Overdispersion check
residual_deviance <- deviance(modelp)
residual_df <- df.residual(modelp)
dispersion_stat <- residual_deviance / residual_df
# Overdispersed 

# Negative binomial model
#BEST MODEL
modelp2 <- glm(Normalized_AUC ~ Exposure_3k * Daylight, data = noisedf, family = negative.binomial(theta = 1))
summary(modelp2)

# Diagnostics 
plot(modelp2)

# Visualise
ggplot(noisedf, aes(x = factor(Exposure_3k), y = Normalized_AUC)) +
  geom_boxplot() +
  labs(x = "Vessel Presence", y = "Noise") +
  facet_wrap(~Daylight)
# Gets louder when a vessel is present.


## Noise and count
model <- lm(Normalized_AUC ~ Vessel_Count , data = noisedf)
summary (model)

#Overdispersion check
residual_deviance <- deviance(model)
residual_df <- df.residual(model)
dispersion_stat <- residual_deviance / residual_df
# Overdispersed 

# Negative binomial model
# BEST MODEL
model2 <- glm(Normalized_AUC ~ Vessel_Count, data = noisedf, family = negative.binomial(theta = 1))
summary(model2)

# Diagnostics 
plot(model2)

# Visualise
ggplot(noisedf, aes(x = Vessel_Count, y = Normalized_AUC)) +
  geom_smooth(method = "lm") +
  labs(x = "Vessel Count", y = "Noise")
# With more vessels present, the noise gets louder.


## Noise and speed 
noisedf2 <- filter(noisedf, noisedf$Vessel_Count == 1)
model3 <- lm(Normalized_AUC ~ Average_Speed , data = noisedf2)
summary (model3)

#Overdispersion check
residual_deviance <- deviance(model3)
residual_df <- df.residual(model3)
dispersion_stat <- residual_deviance / residual_df
# Overdispersed 

# Negative binomial model

# BEST MODEL
model4 <- glm(Normalized_AUC ~ Average_Speed, data = noisedf2, family = negative.binomial(theta = 1))
summary(model4)

#Diagnostics 
plot(model4)

# Visualise
ggplot(noisedf3, aes(x = Average_Speed, y = Normalized_AUC)) +
  geom_smooth() +
  #geom_point()+
  labs(x = "Vessel Speed", y = "Noise")
# Negative relationship, quicker vessels, noise decreases

## Noise and overlap 
noiseoverlap <- lm(Vessel_Overlap ~ Normalized_AUC, data = noisedf)
summary(noiseoverlap)

ggplot(noisedf, aes(x = Vessel_Overlap, y = Normalized_AUC)) +
  geom_smooth() +
  #geom_point()+
  labs(x = "Vessel overlap", y = "Normalised AUC")


################################################
## Model noise impacts
# AUC (cumulative noise)
## Binomial model of proportions
model <- glm(cbind(Buzz_Clicks, Total_Clicks - Buzz_Clicks) ~ Normalized_AUC, data = noisedf, family = binomial)
summary(model)

#Overdispersion check
residual_deviance <- deviance(model)
residual_df <- df.residual(model)
dispersion_stat <- residual_deviance / residual_df
#overdispersed

## Quasibinomial for overdispersed and non-normally distributed data
model1 <- glm(Buzz_Rate ~ Normalized_AUC, data = noisedf, family = quasibinomial)
summary(model1)

# Diagnostics
plot(model1)

# residuals
crPlots(model1)

# visualise
ggplot(noisedf, aes(x = Vessel_Overlap, y = Buzz_Rate, fill = factor(Exposure_3k))) +
  geom_smooth() +
  #geom_point()+
  labs(x = "Vessel overlap", y = "Buzz Rate")

## Vessel Type
model1 <- glm(Buzz_Rate ~ Vessel_Type , data = noisedf, family = quasibinomial)
summary(model1)

## Vessel type & AUC
vt_auc <- lm(Normalized_AUC ~ Vessel_Type, data = noisedf4)
summary(vt_auc)

###################################################################################################
## High & low percentile 
