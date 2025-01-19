## Noiseband impact ##  -----

################################################
## Load packages
library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)

## Load df 
noisedf <- noisedf

################################################
## Vessels and noise -----
## Noise and presence 
noisedf$present <- ifelse(noisedf$Vessel_Count > 1, 1, 0)
noisedf$present <- as.factor(noisedf$present)

modelp <- lm(Median_2000Hz ~ present , data = noisedf)
summary(modelp)

#Overdispersion check
residual_deviance <- deviance(modelp)
residual_df <- df.residual(modelp)
dispersion_stat <- residual_deviance / residual_df
# Overdispersed 

# Negative binomial model
#BEST MODEL
modelp2 <- glm(Median_2000Hz ~ present, data = noisedf, family = negative.binomial(theta = 1))
summary(modelp2)

4.1229216 + 0.0473186

exp(0.0473186)

# Diagnostics 
plot(modelp2)

# Visualise
ggplot(noisedf, aes(x = present, y = Median_2000Hz)) +
  geom_boxplot() +
  labs(x = "Vessel Presence", y = "Noise")
# Gets louder when a vessel is present.


## Noise and count
model <- lm(Median_2000Hz ~ Vessel_Count , data = noisedf)
summary (model)

#Overdispersion check
residual_deviance <- deviance(model)
residual_df <- df.residual(model)
dispersion_stat <- residual_deviance / residual_df
# Overdispersed 

# Negative binomial model
# BEST MODEL
model2 <- glm(Median_2000Hz ~ Vessel_Count, data = noisedf, family = negative.binomial(theta = 1))
summary(model2)

# Diagnostics 
plot(model2)

# Visualise
ggplot(noisedf, aes(x = Vessel_Count, y = Median_2000Hz)) +
  geom_smooth(method = "lm") +
  labs(x = "Vessel Count", y = "Noise")

# With more vessels present, the noise gets louder.


## Noise and speed 
noisedf2 <- filter(noisedf, noisedf$Vessel_Count == 1)

model3 <- lm(Median_2000Hz ~ Average_Speed , data = noisedf2)
summary (model3)

#Overdispersion check
residual_deviance <- deviance(model3)
residual_df <- df.residual(model3)
dispersion_stat <- residual_deviance / residual_df
# Overdispersed 

# Negative binomial model
noisedf3 <- filter(noisedf, noisedf$Average_Speed < 40)

# BEST MODEL
model4 <- glm(Median_2000Hz ~ Average_Speed + (1|Event_ID), data = noisedf3, family = negative.binomial(theta = 1))
summary(model4)

#Diagnostics 
plot(model4)

# Visualise
ggplot(noisedf3, aes(x = Average_Speed, y = Median_2000Hz)) +
  geom_smooth() +
  #geom_point()+
  labs(x = "Vessel Speed", y = "Noise")
# Negative relationship, quicker vessels, noise decreases

################################################


