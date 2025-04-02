## Load packages
library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)
library(ggeffects)

## Load dataframe
Vessel_Presence_20241212 <- readRDS("C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc/MSc_Publication/3. Data/Vessel_Presence_20241212.rds")
daydf <- Vessel_Presence_20241212

## Set variable type
daydf$Vessel_3k <- as.numeric(daydf$Vessel_3k)
daydf$Year <- as.numeric(daydf$Year)
daydf$Daylight <- as.factor(daydf$Daylight)
daydf$Month <- as.numeric(daydf$Month)
daydf$Year <- as.numeric(daydf$Year)
daydf$Vessel_3k <- as.factor(daydf$Vessel_3k)

## Create an overlap column in daydf
daydf$Overlap <- ifelse(daydf$Porpoise_Event == 1 & daydf$Vessel_3k == 1, 1, 0) # run this instead 
daydf$Overlap <- as.numeric(daydf$Overlap)

## overdispersion gam function
overdispersion_gam <- function(gam_model) {
  # Calculate the residual deviance
  residual_deviance <- gam_model$null.deviance - gam_model$deviance
  
  # Calculate the residual degrees of freedom
  residual_degrees_of_freedom <- gam_model$df.residual
  
  # Calculate the residual scaled deviance to residual degrees of freedom ratio
  ratio <- residual_deviance / residual_degrees_of_freedom
  
  return(ratio)
}

## Temporal trends ----
## Porpoises

## Full model
model1 <- glm(Porpoise_Event ~ factor(Month)*Daylight + (1|Year), data = daydf,
              family = binomial(link = "logit"),
              weights = Recording_Effort)
summary(model1)

## without year 
model2 <- glm(Porpoise_Event ~ factor(Month)*Daylight, data = daydf,
              family = binomial(link = "logit"),
              weights = Recording_Effort)
summary(model2)

# compare
AIC(model1, model2)
## same so we don't need year

## without effort weight
model3 <- glm(Porpoise_Event ~ factor(Month)*Daylight, data = daydf,
              family = binomial(link = "logit"))
summary(model3)

# compare
AIC(model2, model3) 

## GAM of season by year
modelgam <- gam(Porpoise_Event ~ s(Month, bs = "cc", by = Daylight) + Daylight, 
                data = daydf, weights = Recording_Effort, family = binomial)
summary(modelgam)
plot(modelgam)

# compare 
AIC(model3, modelgam)
gam.check(modelgam)

#Model 3 is best. 
## They vocalise more at night (stderror = 0.02, z = 46.82, p < 0.001), this is 
## consistent across seasons and throughout years. They vocalise the most between 
## spring - autumn, but less so in the winter. 



## Visual of GAM
# Get predictions from the model
pred <- ggpredict(modelgam, terms = c("Month [all]", "Daylight"))

# Convert Month to a factor or circular scale if needed
pred$Month <- as.numeric(pred$x)  # Convert to numeric for proper plotting

a <- ggplot(pred, aes(x = Month, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +  # Month labels
  labs(x = " ", y = " ",
       title = "Porpoise positive minutes",
       color = "Daylight", fill = "Daylight") +
  scale_y_continuous(limits = c(0, 0.75
              )) +
  theme(legend.position = "none") 


## Vessel temporal trends ####################################

## GLM of season and year with a random effect of year 
# BEST MODEL
modelv1 <- glm(Vessel_3k ~ factor(Month)*Daylight + (1|Year), data = daydf2,
              family = binomial(link = "logit"),
              weights = Recording_Effort)
summary(modelv1)


## GAM model
modelgam2 <- gam(Vessel_3k ~ s(Month, bs = "cc", by = Daylight) + Daylight, 
                data = daydf2, weights = Recording_Effort, family = binomial)

summary(modelgam2)
plot(modelgam2)

gam.check(modelgam2)

## Visual of GAM
# Get predictions from the model
pred2 <- ggpredict(modelgam2, terms = c("Month [all]", "Daylight"))

# Convert Month to a factor or circular scale if needed
pred2$Month <- as.numeric(pred$x)  # Convert to numeric for proper plotting

group <- c("black", "navy")

b <- ggplot(pred2, aes(x = Month, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +  
  scale_y_continuous(limits = c(0, 0.75
  )) +# Month labels
  labs(x = " ", y = " ",
       title = "Vessel Positive Minutes", 
       color = "Daylight", fill = "Daylight") 
## Vessel are present less in the night than the day, this is true across season and year. 
## They are most present from July - September. 


tiff(filename = "season_diel.tiff", units="in", width=8, height=5, res=1000)

grid.arrange(
  arrangeGrob(
    grobs = list(a, b),  # List of plots
    ncol = 2                                   # Arrange plots in a 2x2 grid
  ),
  left = textGrob("Predicted Presence", rot = 90, gp = gpar(fontsize = 14)), 
  bottom = textGrob("Month")
) 

dev.off()
## Vessel overlap #######################################

#QUESTION#
## When do vessels overlap with porpoise presence - in a day, - in a season? Does this 
## change across years?

## Create new dataframe daydf2 - and an overlap column
daydf2 <- Vessel_Presence_20241203
daydf2$Vessel_3k <- as.numeric(daydf2$Vessel_3k)
#daydf2$Overlap <- ifelse(daydf2$Porpoise_Event > 0 & daydf2$Vessel_3k > 0, 1, 0) this one did not work correctly 
daydf2$Overlap <- ifelse(daydf2$Porpoise_Event == 1 & daydf2$Vessel_3k == 1, 1, 0) # run this instead 

# Create dataframe daydf3 - only when porpoise are present 
daydf3 <- filter(daydf2, Porpoise_Event == 1) 
daydf3$Overlap <- as.factor(daydf3$Overlap)
daydf3$Year <- as.numeric(daydf3$Year)

## GLM of season and year with a random effect of year 
# BEST MODEL 
moodelvo <- glm(Overlap ~ factor(Month)*Daylight + (1|Year), data = daydf3, 
                family = binomial(link="logit"), 
                weights = Recording_Effort)

summary(moodelvo)

