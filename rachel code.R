## Trends of Harbour Porpoise (Phocoena phocoena) 
## Presence and the Disruptive Effects of Vessel 
## Noise on Foraging in a High-Traffic Coastal Habitat 

## Statistical Analysis Script ##

## Set up ## ----
## Load packages
library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)
library(ggeffects)

## Load dataframe & rename
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
daydf$Overlap <- ifelse(daydf$Porpoise_Event == 1 & daydf$Vessel_3k == 1, 1, 0) 
daydf$Overlap <- as.numeric(daydf$Overlap)

## Overdispersion gam function
overdispersion_gam <- function(gam_model) {
  # Calculate the residual deviance
  residual_deviance <- gam_model$null.deviance - gam_model$deviance
  
  # Calculate the residual degrees of freedom
  residual_degrees_of_freedom <- gam_model$df.residual
  
  # Calculate the residual scaled deviance to residual degrees of freedom ratio
  ratio <- residual_deviance / residual_degrees_of_freedom
  
  return(ratio)
}

## Seasonal and diel patterns in harbour porpoise presence ## ----

## (1) Full GAM model: month with daylight as interaction and year 
modelgam1 <- gam(Porpoise_Event ~ s(Month, bs = "cc", by = Daylight) + Daylight + Year, 
                data = daydf, weights = Recording_Effort, family = binomial)

## (2) With smooth of year for comparison of model capturing
modelgam2 <- gam(Porpoise_Event ~ s(Month, bs = "cc", by = Daylight) + s(year) + Daylight, 
                 data = daydf, weights = Recording_Effort, family = binomial)

## (3) With re of year
modelgam3 <- gam(Porpoise_Event ~ s(Month, bs = "cc", by = Daylight) + s(year, bs = "re") + Daylight, 
                 data = daydf, weights = Recording_Effort, family = binomial)

## (4) Without year
modelgam4 <- gam(Porpoise_Event ~ s(Month, bs = "cc", by = Daylight) + Daylight, 
                 data = daydf, weights = Recording_Effort, family = binomial)

## (5) Without interaction of daylight
modelgam5 <- gam(Porpoise_Event ~ s(Month, bs = "cc") + Daylight, 
                 data = daydf, weights = Recording_Effort, family = binomial)

## (6) Without daylight
modelgam6 <- gam(Porpoise_Event ~ s(Month, bs = "cc"), 
                 data = daydf, weights = Recording_Effort, family = binomial)

## (7) With  month interaction of year
modelgam7 <- gam(Porpoise_Event ~ s(Month, bs = "cc", by = year) + year, 
                 data = daydf, weights = Recording_Effort, family = binomial)

## (8) With  month interaction of year and daylight
modelgam8 <- gam(Porpoise_Event ~ s(Month, bs = "cc", by = year) + year + daylight, 
                 data = daydf, weights = Recording_Effort, family = binomial)

## (9) With  month interaction of year and daylight
modelgam9 <- gam(Porpoise_Event ~ s(Month, bs = "cc", by = daylight) + (1|year) + daylight, 
                 data = daydf, weights = Recording_Effort, family = binomial)

AIC(
    modelgam1, 
    modelgam2, 
    modelgam3, 
    modelgam4, 
    modelgam5, 
    modelgam6, 
    modelgam7, 
    modelgam8)
## Model 4

# diagnostics
gam.check(modelgam4)
overdispersion_gam(modelgam4, daydf)
# test for residual autocorrelation
layout(matrix(1:2, ncol = 2))
res <- resid(modelgam4) ## extract Pearson's residuals
acf(res, lag.max = 36, main = "ACF - AR(1) errors") ## run acf and partial acf plots 
pacf(res, lag.max = 36, main = "pACF- AR(1) errors")
layout(1) 

# output
summary(modelgam4)
plot(modelgam4)

## They vocalise more at night (stderror = 0.02, z = 46.82, p < 0.001), this is 
## consistent across seasons and throughout years. They vocalise the most between 
## spring - autumn, but less so in the winter. 


## Visual of GAM
# Get predictions from the model
pred <- ggpredict(modelgam4, terms = c("Month [all]", "Daylight"))

# Convert Month to numeric for plotting
pred$Month <- as.numeric(pred$x)  

porptemp<- ggplot(pred, aes(x = Month, y = predicted, color = group)) +
  #geom_line(size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +  # Month labels
  labs(x = "Month", y = "Predicted Porpoise Presence",
       title = "",
       color = "Daylight", fill = "Daylight") +
  scale_y_continuous(limits = c(0, 0.75)) +
  scale_color_manual(values = c("Day" = "grey80", "Night" = "grey20")) +  # Change line colors
  scale_fill_manual(values = c("Day" = "grey80", "Night" = "grey20")) +  # Change ribbon fill colors
  theme_minimal()

tiff('porp temporal plot.tiff', units="in", width=5, height=4, res=1000)

porptemp
dev.off()

## Vessel and porpoise overlap ## ----
## Set up so only looking at when porpoises ARE present
daydf2 <- filter(daydf, Porpoise_Event == 1) 
daydf3 <- daydf2
daydf3$Overlap <- as.factor(daydf3$Overlap)
daydf3$Year <- as.numeric(daydf3$Year)

## (1) Full GAM model: month with daylight as interaction and year 
overlap1 <- gam(Overlap ~ s(Month, bs = "cc", by = Daylight) + Daylight, 
                 data = daydf3, weights = Recording_Effort, family = binomial)

## (2) Without interaction of daylight
overlap2 <- gam(Overlap ~ s(Month, bs = "cc") + Daylight, 
                 data = daydf3, weights = Recording_Effort, family = binomial)

## (3) Without daylight
overlap3 <- gam(Overlap ~ s(Month, bs = "cc"), 
                   data = daydf3, weights = Recording_Effort, family = binomial)

## (4) As a GLM
overlap4 <- glm(Overlap ~ factor(Month)*Daylight, data = daydf2, 
                   family = binomial(link="logit"), 
                   weights = Recording_Effort)


AIC(overlap1, overlap2, overlap3, overlap4) #GLM is best.  Continue:

## (5) Without month interaction
overlap5 <- glm(Overlap ~ factor(Month) + Daylight, 
                data = daydf3, weights = Recording_Effort, family = binomial)

## (6) Without daylight 
overlap6 <- glm(Overlap ~ factor(Month), 
                 data = daydf3, weights = Recording_Effort, family = binomial)

## (7) Without month 
overlap7 <- glm(Overlap ~ Daylight, 
                 data = daydf3, weights = Recording_Effort, family = binomial)

AIC(overlap4, overlap5, overlap6, overlap7)


# diagnostics
plot(overlap5)
# Overdispersion check
residual_deviance <- deviance(overlap5)
residual_df <- df.residual(overlap5)
dispersion_stat <- residual_deviance / residual_df # OK

# output
summary(overlap5)

## Visual
# Summarize to calculate proportion of events with overlap
summary_data <- daydf2 %>%
  group_by(Year, Month, Daylight, Vessel_3k, Porpoise_Event) %>%
  summarise(Proportion_Overlap = mean(Overlap),  # Proportion of 1s in each group
            Count = n(), .groups = "drop")  # Count of events per group

# Create the bar plot with dodged bars
vessel_overlap <- ggplot(summary_data, aes(x = as.factor(Month), y = Proportion_Overlap, fill = Daylight)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) + 
  scale_fill_manual(values = c("Day" = "navy", "Night" = "pink")) +
  labs(x = "Month", y = "Mean Vessel Overlap", fill = "Daylight") +
  theme_minimal() +
  theme(text = element_text(size = 14))

tiff('porp temporal plot.tiff', units="in", width=5, height=4, res=1000)

vessel_overlap

dev.off()

## Vessel Impact ----
## Load df 
Buzz_Master_20241212 <- readRDS("C:/Users/Rachel Lennon/OneDrive - University of Glasgow/MSc/MSc_Publication/3. Data/Buzz_Master_20241212.rds")
buzzdf <- Buzz_Master_20241212
buzzdf$year <- as.numeric(format(buzzdf$Start_Time, "%Y"))
buzzdf$Normal_Clicks <- buzzdf$Total_Clicks - buzzdf$Buzz_Rate
buzzdf$Normal_Clicks <- as.integer(buzzdf$Normal_Clicks)

# Clean variable type 
buzzdf$Daylight <- as.factor(buzzdf$Daylight)
buzzdf$Month <- as.numeric(buzzdf$Month)
buzzdf$Exposure_3k <- as.factor(buzzdf$Exposure_3k) 
buzzdf2 <- filter(buzzdf, buzzdf$Buzz_Rate != 0.0) #Only events with buzz i.e. foraging events. 
buzzdf3 <- filter(buzzdf2, buzzdf2$Buzz_Clicks > 5) # Need > 6 buzz calls to qualify
buzzdf3 <- filter(buzzdf3, buzzdf3$Total_Minutes <61) # Only 1 hr events

# look at distribution
hist(buzzdf3$Exposure_3k)
count(buzzdf3, Exposure_3k, Month)

# Models
## (1) Binomial model of proportions and exposure only
buzz1 <- glm(cbind(Buzz_Clicks, Normal_Clicks) ~ Exposure_3k, data = buzzdf3, family = binomial)
summary(buzz1)

#Overdispersion check
residual_deviance <- deviance(model)
residual_df <- df.residual(model)
dispersion_stat <- residual_deviance / residual_df

## (2) Quasibinomial for overdispersed and non-normally distributed data
buzz2 <- glm(Buzz_Rate ~ Exposure_3k, data = buzzdf3, family = quasibinomial)
summary(buzz2)

## Backtransform 
odds_ratio <- 1 - exp(coef(buzz2)[2])

## 28% reduction in odds of buzz rate

################################################

## Quasibinomial with environmental variables 
## (3) Full model
buzz3 <- glm(cbind(Buzz_Clicks, Normal_Clicks) ~ Exposure_3k:factor(Month) + Exposure_3k:Daylight + Exposure_3k + Daylight + Exposure_3k + factor(Month), data = buzzdf3, family = binomial)

## (4) without month interaction
buzz4 <- glm(cbind(Buzz_Clicks, Normal_Clicks) ~  Exposure_3k:Daylight + Daylight + Exposure_3k + factor(Month), data = buzzdf3, family = binomial)

AIC(buzz3, buzz4) # better with year

## (5) without daylight interaction
buzz5 <- glm(cbind(Buzz_Clicks, Normal_Clicks) ~ Exposure_3k:factor(Month) + Daylight + factor(Month), data = buzzdf3, family = binomial)

AIC(buzz3, buzz5) #better with month interaction

## (5) without interaction
buzz5 <- glm(cbind(Buzz_Clicks, Normal_Clicks) ~ Exposure_3k+ Daylight +  + factor(Month), data = buzzdf3, family = binomial)

AIC(buzz3, buzz5) #better with daylight interaction

# Diagnostics
plot(buzz4)
plot(buzz3)
summary(buzz4)

#Overdispersion check
residual_deviance <- deviance(buzz4)
residual_df <- df.residual(buzz4)
dispersion_stat <- residual_deviance / residual_df

## (6) overdispersed so quasibinomial
buzz6 <- glm(Buzz_Rate ~ Exposure_3k:Daylight + Daylight + Exposure_3k + factor(Month), data = buzzdf3, family = quasibinomial)

# Diagnostics# 
plot(buzz6)
summary(buzz6)

# odds ratio
intercept = -1.744334
exposure_3k1 = -0.604190
daylight_night = -0.242391 
interaction =0.418552

OR_day <- exp(exposure_3k1)
1-OR_day
OR_night <- exp(exposure_3k1 + daylight_night + interaction)
1-OR_night

# Visualise
plot <- ggplot(buzzdf3, aes(x = Exposure_3k, y = Buzz_Rate, by = Daylight, fill = Daylight)) +
  geom_boxplot() +
  labs(x = "Vessel Presence", y = "Buzz Rate") +
  scale_fill_manual(values = c("Day" = "navy", "Night" = "pink")) +  # Customize fill colors
  theme_minimal() 

tiff('vessel impact.tiff', units="in", width=5, height=4, res=1000)
plot

dev.off()

#####################################
## Overlap 
buzzdf6 <- filter(buzzdf3, Vessel_Overlap >0.00)

overlap_buzz <- glm(Buzz_Rate ~ Vessel_Overlap, data = buzzdf6, family = quasibinomial())
summary(overlap_buzz)

ggplot(buzzdf6, aes(Vessel_Overlap, Buzz_Rate)) +
  geom_smooth(method = "lm") +
  labs(x = "Vessel Exposure", y = "Buzz Rate") +
  theme_minimal() 

odds_ratio <- 1 - exp(coef(overlap_buzz)[2])

