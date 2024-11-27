## Impacts of  vessel on vocalisation ##

## Packages to load
library(dplyr)
library(tibble)
library(ggplot2)
library(DBI)
library(RSQLite)
library(dbplyr)
library(tidyverse)



## get master df
db_master <- read.csv("Buzz_Master_UPDATED.csv")
db_master$Exposure_3k <- as.factor(db_master$Exposure_3k)
db_master$Exposure_500m <- as.factor(db_master$Exposure_500m)

db_exposure <- count(db_master, Exposure_500m)
db_exposure2 <- count(db_master, Exposure_3k)


## Models 
# Look at how the buzz rate (i.e. proportion of a Porpoise positive event that are
# buzz calls) and how that changes when vessels are or are not present across 
# various exposure distances. 

# Both exposure distances
buzz_model1 <- lm(Buzz_Rate ~ Exposure_500m + Exposure_3k, data = db_master)
summary(buzz_model1) ## not overdispersed

# 3k exposure distance
buzz_model2 <- lm(Buzz_Rate ~ Exposure_3k, data = db_master)
summary(buzz_model2) # significantly fewer buzz calls

# 500m exposure distance
buzz_model3 <- lm(Buzz_Rate ~ Exposure_500m, data = db_master)
summary(buzz_model3) # almost significant but not quite

## Compare models
AIC(buzz_model1, buzz_model2, buzz_model3) # buzz model 2?

## Diagnostics
plot(buzz_model2) ## looks okay 

## Visualise
db_3k_plot <- ggplot(db_master, aes(x = Exposure_3k, y = Buzz_Rate)) +
  geom_boxplot() +
  labs(x = "Vessel Presence (within 500m)", y = "Buzz Rate") +
  theme_minimal()

db_3k_plot

## Print to working directory:
tiff('db_buzz_test_signif_500m.tiff', units="in", width=6, height=4, res=1000, compression = 'lzw')

db_500m_plot

dev.off()


## Testing changes 