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

################################################
## Vessels and noise -----
## Noise and presence 
noisedf$present <- ifelse(noisedf$Vessel_Count > 1, 1, 0)
noisedf$present <- as.factor(noisedf$present)

modelp <- lm(Normalized_AUC ~ present , data = noisedf)
summary(modelp)

#Overdispersion check
residual_deviance <- deviance(modelp)
residual_df <- df.residual(modelp)
dispersion_stat <- residual_deviance / residual_df
# Overdispersed 

# Negative binomial model
#BEST MODEL
modelp2 <- glm(Normalized_AUC ~ present, data = noisedf, family = negative.binomial(theta = 1))
summary(modelp2)

0.66935     + 0.73460    

exp(1.40395)

# Diagnostics 
plot(modelp2)

# Visualise
ggplot(noisedf, aes(x = present, y = Normalized_AUC)) +
  geom_boxplot() +
  labs(x = "Vessel Presence", y = "Noise")
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
noisedf3 <- filter(noisedf, noisedf$Average_Speed < 40)

# BEST MODEL
model4 <- glm(Normalized_AUC ~ Average_Speed + (1|Event_ID), data = noisedf3, family = negative.binomial(theta = 1))
summary(model4)

#Diagnostics 
plot(model4)

# Visualise
ggplot(noisedf3, aes(x = Average_Speed, y = Normalized_AUC)) +
  geom_smooth() +
  #geom_point()+
  labs(x = "Vessel Speed", y = "Noise")
# Negative relationship, quicker vessels, noise decreases

################################################
## Model noise impacts
noisedf4 <- filter(noisedf3, noisedf3$Buzz_Rate != 0.0)
noisedf4 <- filter(noisedf4, noisedf4$Buzz_Clicks > 5)
hist(noisedf3$Buzz_Rate) # need to remove longer events 

## 1hr long events 
noisedf4 <- filter(noisedf4, noisedf4$Total_Minutes <61)
hist(noisedf3$Total_Minutes)

## Binomial model of proportions
model <- glm(cbind(Buzz_Clicks, Total_Clicks - Buzz_Clicks) ~ Normalized_AUC, data = noisedf4, family = binomial)
summary(model)

#Overdispersion check
residual_deviance <- deviance(model)
residual_df <- df.residual(model)
dispersion_stat <- residual_deviance / residual_df
#overdispersed

## Quasibinomial for overdispersed and non-normally distributed data
model1 <- glm(Buzz_Rate ~ Normalized_AUC, data = noisedf4, family = quasibinomial)
summary(model1)

# Diagnostics
plot(model1)

# residuals
crPlots(model1)

# visualise
ggplot(noisedf3, aes(x = Normalized_AUC, y = Buzz_Rate)) +
  geom_smooth(method = "lm") +
  #geom_point()+
  labs(x = "Noise", y = "Buzz Rate")

## Vessel Type

model1 <- glm(Buzz_Rate ~ Vessel_Type , data = noisedf4, family = quasibinomial)
summary(model1)
