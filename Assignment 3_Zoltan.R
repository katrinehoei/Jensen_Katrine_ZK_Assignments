##########Assignment 3##########

#Running packages
library(psych) 
library(tidyverse) 
library(r2glmm) 
library(cAIC4) 
library(lme4) 
library(lmerTest)
library(MuMIn)
library(optimx)
library(dplyr)

#Running custom code for calculating the standardized betas for each predictor 
stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

#Running dataset

data_sample_3 = read.csv("https://tinyurl.com/ha-dataset3")

View(data_sample_3)

#Checking for outliers
data_sample_3 %>% summary()

#Changing sex and hospital to factor
data_sample_3 <- data_sample_3 %>% 
  mutate(sex = factor(sex)) %>% 
  mutate(hospital = factor(hospital))

#Cleaning up errors in the dataset 

str(data_sample_3$sex) #Discovering three levels of sex (one spelled "femlae")
data_sample_3$sex[data_sample_3$sex == "femlae"] = "female" #changing to correct spelling
data_sample_3$sex <- droplevels(data_sample_3$sex) #dropping unused levels
str(data_sample_3$sex)

describe(data_sample_3$household_income) #another participant with negative household income, a bit unusual but not invalid-- not removed from the dataset 

#Building a random intercept model 

mod_rnd_intercept = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_sample_3)

#Evaluating the model

mod_rnd_intercept
summary(mod_rnd_intercept) #For getting the predictors
confint(mod_rnd_intercept) #For retrieving the confident intervals
stdCoef.merMod(mod_rnd_intercept) #Standardized beta for each predictor 

r2beta(mod_rnd_intercept, method = "nsj", data = data_sample_3) #marginal R2 with CI

r.squaredGLMM(mod_rnd_intercept) #marginal and conditional R2                     

#Running dataset 
data_sample_4 = read.csv("https://tinyurl.com/ha-dataset4")

data_sample_4 %>% summary()

View(data_sample_4)

#One participant with mindfulness >6 

data_sample_4 %>% slice(c(80)) #Outside range, excluded from the dataset 

data_sample_4 = data_sample_4 %>% slice(-c(80))

data_sample_4 %>% summary() #rechecking

#Changing variables into factor levels 

data_sample_4 <- data_sample_4 %>% 
  mutate(sex = factor(sex)) %>% 
  mutate(hospital = factor(hospital))

#Using model to make predictions on data_sample_4

predicted_pain_model_rnd_intercept = predict(mod_rnd_intercept, newdata = data_sample_4, allow.new.levels = TRUE) #Running predictions on new dataset using the theory-based-model
predicted_pain_model_rnd_intercept

data_sample_4 <- cbind(data_sample_4, predicted_pain_model_rnd_intercept) #Adding predicted values as a new column to data_sample_2

#To calculate TSS, I first have to compute the "null model"
mod_null <- lmer(pain ~ 1 + (1|hospital), data= data_sample_3)

predicted_pain_mod_null = predict(mod_null, newdata = data_sample_4, allow.new.levels = TRUE) #Making predictions using the null model
data_sample_4 <- cbind(data_sample_4, predicted_pain_mod_null) #Adding to the set 

#Calculating TSS & RSS

TSS = sum((data_sample_4$pain - predicted_pain_mod_null)^2)
RSS = sum((data_sample_4$pain - predicted_pain_model_rnd_intercept)^2)

R2_new_data = (1 - (RSS/TSS)) #Calculating R^2 
R2_new_data

#Building the random slope model and random intercept model with only one predictor

mod_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_sample_3)

#Evaluating the model
mod_slope
summary(mod_slope) #For getting the predictors
confint(mod_slope) #For retrieving the confident intervals
stdCoef.merMod(mod_slope) #Standardized beta for each predictor 

r2beta(mod_slope, method = "nsj", data = data_sample_3) #marginal R2 with CI

r.squaredGLMM(mod_slope) #marginal and conditional R2  

#Saving predictions of the slope model into the dataset in order to plot the regression lines 
data_sample_3 = data_sample_3 %>% mutate(pred_slope = predict(mod_slope))

#Cleaning up the headings and releveling factors to prepare for graphs
levels(data_sample_3$hospital)[levels(data_sample_3$hospital)=="hospital_1"] <- "Hospital 1" 
levels(data_sample_3$hospital)[levels(data_sample_3$hospital)=="hospital_2"] <- "Hospital 2" 
levels(data_sample_3$hospital)[levels(data_sample_3$hospital)=="hospital_3"] <- "Hospital 3" 
levels(data_sample_3$hospital)[levels(data_sample_3$hospital)=="hospital_4"] <- "Hospital 4" 
levels(data_sample_3$hospital)[levels(data_sample_3$hospital)=="hospital_5"] <- "Hospital 5" 
levels(data_sample_3$hospital)[levels(data_sample_3$hospital)=="hospital_6"] <- "Hospital 6" 
levels(data_sample_3$hospital)[levels(data_sample_3$hospital)=="hospital_7"] <- "Hospital 7" 
levels(data_sample_3$hospital)[levels(data_sample_3$hospital)=="hospital_8"] <- "Hospital 8" 
levels(data_sample_3$hospital)[levels(data_sample_3$hospital)=="hospital_9"] <- "Hospital 9" 
levels(data_sample_3$hospital)[levels(data_sample_3$hospital)=="hospital_10"] <- "Hospital 10"

levels(data_sample_3$hospital) <- c("Hospital 1", "Hospital 2", "Hospital 3", "Hospital 4", "Hospital 5", "Hospital 6",
                          "Hospital 7", "Hospital 8", "Hospital 9", "Hospital 10")

#Plotting the regression lines for each hospital

plot_slope = data_sample_3 %>%
  ggplot() +
  aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + 
  geom_line(color='red', aes(y=pred_slope, x=cortisol_serum)) +
  labs(x= "Serum Cortisol", y = "Reported Postoperative Pain") +
  facet_wrap( ~ hospital, ncol = 3)
plot_slope


