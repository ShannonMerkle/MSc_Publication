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
Vessel_Presence_20241212 <- readRDS("C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc/MSc_Publication/3. Data/Vessel_Presence_20241212.rds")
vp <- Vessel_Presence_20241212

vp2 <- filter(vp, vp$Recording_Effort == 0)
vp2 <- filter(vp2, vp2$Porpoise_Event == 1)

# Clear vessel presence
Vessel_Presence_20241212 <- anti_join(vp, vp2, by = "Event_ID")
saveRDS(Vessel_Presence_20241212, "Vessel_Presence_20241212.rds")

# Clear buzz_df
bdf <- Buzz_Master

Buzz_Master_20241212 <- anti_join(bdf, vp2, by = "Event_ID")
saveRDS(Buzz_Master_20241212, "Buzz_Master_20241212.rds")


## Load df 
Buzz_Master_20241212 <- readRDS("C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc/MSc_Publication/3. Data/Buzz_Master_20241212.rds")
buzzdf <- Buzz_Master_20241212

# Clean variable type 
buzzdf$Daylight <- as.factor(buzzdf$Daylight)
buzzdf$Month <- as.numeric(buzzdf$Month)
buzzdf$Exposure_3k <- as.factor(buzzdf$Exposure_3k)

################################################
## Model vessel impacts
buzzdf2 <- filter(buzzdf, buzzdf$Buzz_Rate != 0.0)
buzzdf3 <- filter(buzzdf2, buzzdf2$Buzz_Clicks > 5)
hist(buzzdf2$Buzz_Rate) # need to remove longer events 

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

## 28% reduction in buzz rate

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


#Log odds
intercept = -1.62254
exposure_3k1 = -0.57368
daylight_night = -0.25153
interaction = 0.41027

# Calculate log-odds for each scenario
# 1. Daylight, Exposure_3k1 = 0
log_odds_daylight_exposure0 = intercept

# 2. Daylight, Exposure_3k1 = 1
log_odds_daylight_exposure1 = intercept + exposure_3k1

# 3. Night, Exposure_3k1 = 0
log_odds_night_exposure0 = intercept + daylight_night

# 4. Night, Exposure_3k1 = 1
log_odds_night_exposure1 = intercept + daylight_night + exposure_3k1 + interaction

# Convert log-odds to odds
odds_daylight_exposure0 = exp(log_odds_daylight_exposure0)
odds_daylight_exposure1 = exp(log_odds_daylight_exposure1)
odds_night_exposure0 = exp(log_odds_night_exposure0)
odds_night_exposure1 = exp(log_odds_night_exposure1)


reduction_daylight = 1 - (odds_daylight_exposure1 / odds_daylight_exposure0)
reduction_night = 1 - (odds_night_exposure1 / odds_night_exposure0)

# Visualise
plot <- ggplot(buzzdf4, aes(x = Exposure_3k, y = Buzz_Rate, fill = Daylight)) +
  geom_boxplot() +
  labs(x = "Vessel Presence", y = "Proportional Buzz Rate") +
  scale_fill_manual(values = c("Day" = "navy", "Night" = "pink")) +  # Customize fill colors
  theme_minimal()

tiff('vessel impact.tiff', units="in", width=5, height=4, res=1000)
plot

dev.off()

## Month as a fixed effect
model4 <- glm(Buzz_Rate ~ Exposure_3k*factor(Month), data = buzzdf4, family = quasibinomial)
summary(model4) ## No effect


# Visualise
ggplot(buzzdf4, aes(x = Exposure_3k, y = Buzz_Rate, fill = factor(Month))) +
  geom_boxplot() +
  labs(x = "Vessel Presence", y = "Proportional Buzz Rate")


exp(-0.57368)

#####################################################################################

overlap_buzz <- glm(Buzz_Rate + Vessel_Overlap )
