

###################################################################################################################################
################ ISOLATING ALL CLICK EVENTS TEMPORALY ##########

##### RUNNING A CHI SQUARED TO LOOK AT EACH CATEGORICAL VARIABLE'S SIGNIFICANCE 

# Time of Day 
Chisq_Diurnal <- chisq.test(Click_Event_Time_of_Day_Table$`Count of Events`)
print(Chisq_Diurnal)
## VERY small p-value -- Time of Day SIGNIFICANT 

# Seasonal 
Chisq_Season <- chisq.test(Click_Event_Seasonal_Table$`Count of Events`)
print(Chisq_Season)
## VERY small p-value -- Season SIGNIFICANT 

####### NOW RUNNING A POST-HOC PAIRWISE CHI-SQUARED TO LOOK AT EACH CATEGORY INDIVIDUALLY 

event_table <- with(Click_Event_Time_of_Day_Table, table(`Time of Day`, `Count of Events`))

# Perform pairwise chi-squared tests with Bonferroni correction
pairwise_results_DIURNAL <- pairwise.prop.test(
  x = Click_Event_Time_of_Day_Table$`Count of Events`,
  n = rep(sum(Click_Event_Time_of_Day_Table$`Count of Events`), length(Click_Event_Time_of_Day_Table$`Count of Events`)),
  p.adjust.method = "bonferroni"
)

print(pairwise_results_DIURNAL)

# RESULTS: 1. Day, 2. Evening, 3. Morning, 4. Night 
# 1 vs 2 - Day vs Evening: 0.078 - NOT sig 
# 1 vs 3 - Day vs Morning: SIGNIFICANT 
# 2 vs 3 - Evening vs Morning: SIGNIFICNAT 
# 1 vs 4 - Day vs Night: 1.0 - NOT sig
# 2 vs 4 - Evening vs Night: 0.093 - NOT sig 
# 3 vs 4 - Morning vs Night: SIGNIFICANT 

## SEASONAL PAIRWISE 

# not sure why we do this?
event_table_season <- with(Click_Event_Seasonal_Table, table(Season, `Count of Events`))

# Perform pairwise chi-squared tests with Bonferroni correction
pairwise_results_SEASONAL <- pairwise.prop.test(
  x = Click_Event_Seasonal_Table$`Count of Events`,
  n = rep(sum(Click_Event_Seasonal_Table$`Count of Events`), length(Click_Event_Seasonal_Table$`Count of Events`)),
  p.adjust.method = "bonferroni"
)

print(pairwise_results_SEASONAL)


# RESULTS: 1. Autumn, 2. Spring, 3. Summer, 4. Winter
# 1 vs 2 - autumn vs spring - SIG
# 1 vs 3 - autumn vs summer - SIG
# 2 vs 3 - spring vs summer - SIG 
# 1 vs 4 - autumn vs winter - SIG 
# 2 vs 4 - spring vs winter - NOT SIG 0.5
# 3 vs 4 - summer vs winter - SIG  

######### TEMPORAL WITH CLICK TRAIN TYPE AS A VARIABLE 


### CATEGORICAL MODEL 

library(nnet)
Click_Train_test_model <- multinom(Click_Train_Type ~ Vessel_Exposure, data = Buzz_Master)
summary(Click_Train_test_model)


library(nnet)
str(Buzz_Master)

# Convert Click_Type to a factor if it isn't already
Buzz_Master$Click_Train_Type <- as.factor(Buzz_Master$Click_Train_Type)
Buzz_Master$Time_of_day <- as.factor(Buzz_Master$Time_of_day)

Multinomial_Diurnal <- multinom(Click_Train_Type ~ Time_of_day, data = Buzz_Master)
summary(Multinomial_Diurnal)

## this alone does not give you significance, need to look at a z-score and then convert to p-value

# Displaying z-scores and p-values in the model summary
Multinomial_Diurnal_Results <- summary(Multinomial_Diurnal)
z_scores <- Multinomial_Diurnal_Results$coefficients / Multinomial_Diurnal_Results$standard.errors
p_values <- (2 * (1 - pnorm(abs(z_scores))))

print(p_values)

# Combine into a table for easy interpretation
Multinomial_Diurnal_Results_Table <- cbind(Multinomial_Diurnal_Results$coefficients, Multinomial_Diurnal_Results$standard.errors, z_scores, p_values)
colnames(Multinomial_Diurnal_Results_Table) <- c("Coefficient", "Std. Error", "z value", "Pr(>|z|)")
print(Multinomial_Diurnal_Results_Table)



  





