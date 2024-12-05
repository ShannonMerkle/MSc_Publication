## Temporal & diel pattern trends ##  -----

################################################
## Load packages
library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)

## Load df 
daydf <- Vessel_Presence_20241203

## Clean variable type 
daydf$Porpoise_Event <- as.factor(daydf$Porpoise_Event)
daydf$Daylight <- as.factor(daydf$Daylight)
daydf$Month <- as.numeric(daydf$Month)
daydf$Year <- as.numeric(daydf$Year)


#####################################################
## Full model #######################################

## When are porpoises most active in a day - does that vary by season? and 
## when are porpoises most active within a year and does that vary between years?

## When are porpoises most active in a day?
model1 <- glm(Porpoise_Event ~ Daylight*factor(Month) + (1|Year) ,data = daydf, 
              family = binomial(link="logit"))

summary(model1) 
# Porpoises more active at night, true across all season and between years
# except May where they are more active in the day?

## Visualise
ggplot(daydf, aes(x = Daylight, y = Porpoise_Event, color = factor(Month))) +
  labs(
    x = "Daylight",
    y = "Porpoise Event (0/1)",
    color = "Month"
  ) +
  theme_minimal()


# binomial events over month and between times with a correlation structure by year
model1 <- gam(Porpoise_Event ~ s(Month, bs = "cc", by = Daylight) + Daylight, 
               data = daydf, family = binomial(link = "logit"))
summary(model1) ## more vocal at night than day 
plot(model1)

# same as model1 but with a correlation structure and year variable 
model2 <- gamm(Porpoise_Event ~ s(Month, bs = "cc", by = Daylight) + Daylight
                         + year, 
                         data = daydf, family = binomial(link = "logit"), 
                         correlation = corARMA(form = ~ 1|Year, p = 1))
                         

## When do porpoises overlap with vessel the most a) in a day?, b) across a year?



