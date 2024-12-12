## Noisband impact ##  -----

################################################
## Load packages
library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)

## Load df 
noisedf <- Buzz_Noise_Monitor_Oct2018

################################################
## Vessels and noise -----
## Noise and presence 
noisedf$present <- ifelse(noisedf$Vessel_Count > 1, 1, 0)
noisedf$present <- as.factor(noisedf$present)

modelp <- lm(ThirdOctave_894_1118_median ~ present , data = noisedf)
summary(modelp)

#Overdispersion check
residual_deviance <- deviance(modelp)
residual_df <- df.residual(modelp)
dispersion_stat <- residual_deviance / residual_df
# Overdispersed 

# Negative binomial model
modelp2 <- glm(ThirdOctave_894_1118_median ~ present, data = noisedf, family = negative.binomial(theta = 1))
summary(modelp2)

# Diagnostics 
plot(modelp2)

# Visualise
ggplot(noisedf, aes(x = present, y = ThirdOctave_894_1118_median)) +
  geom_boxplot() +
  labs(x = "Vessel Presence", y = "Noise")



## Noise and count
model <- lm(ThirdOctave_894_1118_median ~ Vessel_Count , data = noisedf)
summary (model)

#Overdispersion check
residual_deviance <- deviance(model)
residual_df <- df.residual(model)
dispersion_stat <- residual_deviance / residual_df
# Overdispersed 

# Negative binomial model
model2 <- glm(ThirdOctave_894_1118_median ~ Vessel_Count, data = noisedf, family = negative.binomial(theta = 1))
summary(model2)

# Diagnostics 
plot(model2)

# Visualise
ggplot(noisedf, aes(x = Vessel_Count, y = ThirdOctave_894_1118_median)) +
  geom_smooth(method = "lm") +
  labs(x = "Vessel Count", y = "Noise")


## Noise and speed 
noisedf2 <- filter(noisedf, noisedf$Vessel_Count == 1)

model3 <- lm(ThirdOctave_894_1118_median ~ Average_Speed , data = noisedf2)
summary (model3)

#Overdispersion check
residual_deviance <- deviance(model3)
residual_df <- df.residual(model3)
dispersion_stat <- residual_deviance / residual_df
# Overdispersed 

# Negative binomial model
noisedf3 <- filter(noisedf, noisedf$Average_Speed < 40)

model4 <- glm(ThirdOctave_894_1118_median ~ Average_Speed, data = noisedf3, family = negative.binomial(theta = 1))
summary(model4)

#Diagnostics 
plot(model4)

# Visualise
ggplot(noisedf3, aes(x = Average_Speed, y = ThirdOctave_894_1118_median)) +
  geom_smooth() +
  #geom_point()+
  labs(x = "Vessel Speed", y = "Noise")

################################################

