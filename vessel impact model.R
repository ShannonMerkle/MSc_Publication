## Vessel Impacts ##  -----

################################################
## Set up ----

# Load packages
library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)

# Load df 
buzzdf <- Buzz_Master_20241204

# Clean variable type 
buzzdf$Daylight <- as.factor(buzzdf$Daylight)
buzzdf$Month <- as.numeric(buzzdf$Month)
buzzdf$Year <- as.numeric(buzzdf$Year)
buzzdf$Exposure_3k <- as.factor(buzzdf$Exposure_3k)

# Clean vessel presence of 0/1 effort 
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

################################################
## Model vessel impacts

model1 <- glm(Buzz_Rate ~ )

