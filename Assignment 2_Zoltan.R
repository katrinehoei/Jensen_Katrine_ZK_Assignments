##########Assignment 2###########

#Running packages
library(tidyverse)
library(psych)
library(dplyr)
library(car)
library(lmtest)

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

#Excluding the same variables as in Assignment 1

data_sample_1_new <- data_sample_1[-c(150, 93),]

View(data_sample_1_new)

#Running custom function for retrieving coefficients table 

 coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

#Building the researcher's model 

mod3 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1_new)

#Running model diagnostics 

#Visual inspection of potential outliers

mod3 %>% plot(which = 5) #Residuals vs leverage plot

mod3 %>% plot(which = 4) #Cook's distance 

#Although none of the three outliers have distances>1, they are all above 4/n. I will therefore check the outlying individual variables to see if it may be a result of a coding error 

data_sample_1_new %>% 
  slice(c(3, 103, 114))

#They look okay. Moving on to check the model assumptions. If none of the assumptions are violated, I will not remove the outliers. 

#Assumption 1: Normality

mod3 %>% plot(which = 2) #QQ-plot - visual inspection

residuals_mod3 = enframe(residuals(mod3)) #2nd visual inspection - histogram of the residuals
residuals_mod3 %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(mod3)) #Checking for skew and kurtosis 

#All is within normal range - the assumption of normality holds 

#Assumption 2: Linearity 

mod3 %>% residualPlots() #Visual inspection and tests

#Though slight curvature in some variables, all tests are non-significant. Assumption therefore is not violated. 

#Assumption 3: homoscedasticity

mod3 %>% plot(which = 3) #Visual inspection

mod3 %>% ncvTest() #Non-constant variance score test

mod3 %>% bptest() #Breush-Pagan test

#All non-significant. No indication of heteroscedasticity. 

#Assumption 4: multicollinearity

mod3 %>% vif()

#All variables scoring below critical threshold of 3. No violation. 

#Summary of the researchers model
mod3 %>% summary()
AIC(mod3)

#All assumptions in model 3 hold --- now moving on the backwards regression 

backward_mod3 = step(object = mod3, direction = "backward") #Running the backwards regression
backward_mod3 #3 variables: "state trait anxiety", "weight" and "IQ" are excluded from the model as a result of running the backwards regression

#Running model diagnostics on the retained backward model

#Visual inspection of potential outliers

backward_mod3 %>% plot(which = 5) #Residuals vs leverage plot

backward_mod3 %>% plot(which = 4) #Cook's distance 

#Although none of the three outliers have distances>1, they are all above 4/n. I will therefore check the outlying individual variables to see if it may be a result of a coding error 

data_sample_1_new %>% 
  slice(c(148, 103, 114))

#They look okay. Moving on to check the model assumptions. If none of the assumptions are violated, I will not remove the outliers. 

#Assumption 1: Normality

backward_mod3 %>% plot(which = 2) #QQ-plot - visual inspection

residuals_backward_mod3 = enframe(residuals(backward_mod3)) #2nd visual inspection - histogram of the residuals
residuals_backward_mod3 %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(mod3)) #Checking for skew and kurtosis 

#All is within normal range - the assumption of normality holds 

#Assumption 2: Linearity 

backward_mod3 %>% residualPlots() #Visual inspection and tests

#Though slight curvature in some variables, all tests are non-significant. Assumption therefore is not violated. 

#Assumption 3: homoscedasticity

backward_mod3 %>% plot(which = 3) #Visual inspection

backward_mod3 %>% ncvTest() #Non-constant variance score test

backward_mod3 %>% bptest() #Breush-Pagan test

#All non-significant. No indication of heteroscedasticity. 

#Assumption 4: multicollinearity

backward_mod3 %>% vif() #no indication of multicollinearity 

#None of the assumptions were violated - I will keep the three outlying variables in the dataset. 

#Comparing the backward model with the researchers model 
AIC(mod3, backward_mod3)

summary(mod3)$adj.r.squared*100
summary(backward_mod3)$adj.r.squared*100

#Comparing the two models
mod2_final =  lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1_new) #re-running final model from assignment 1

backward_mod3 %>% summary() #Summary of backward model
summary(backward_mod3)$adj.r.squared * 100 #Adjusted R2 values
confint(backward_mod3) #confidence intervals
coef_table(backward_mod3)

mod2_final %>% summary()
summary(mod2_final)$adj.r.squared * 100
confint(mod2_final)
coef_table(mod2_final)

AIC(mod2_final, backward_mod3) #Cannot use an ANOVA approach, because models are not nested (STAI_trait was excluded). Therefore only comparing using AIC. 

#Loading second dataset

data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")

#Inspecting data_sample_2
View(data_sample_2)

summary(data_sample_2)
data_sample_2 %>% describe()

#One participants scored outside mindfullness scale -- removing participants
data_sample_2_final = data_sample_2 %>% slice(-c(8))

summary(data_sample_2_final)

#Making predictions

predicted_pain_theory = predict(mod2_final, newdata = data_sample_2_final) #Running predictions on new dataset using the theory-based-model
data_sample_2_final <- cbind(data_sample_2_final, predicted_pain_theory) #Adding predicted values as a new column to data_sample_2

predicted_pain_backward = predict(backward_mod3, newdata = data_sample_2_final) #Running predictions on new dataset using the backward model
data_sample_2_final <- cbind(data_sample_2_final, predicted_pain_backward) #Adding predicted values as a new column to data_sample_2

View(data_sample_2_final) #Inspecting the data and checking to see if predicted values were added to the dataset

#Visual inspection of prediction accuracy 
backward_prediction_scatter = data_sample_2_final %>% 
  ggplot() +
  aes(x = predicted_pain_backward, y = pain) + geom_point() 
backward_prediction_scatter

theory_prediction_scatter = data_sample_2_final %>%
  ggplot() +
  aes(x = predicted_pain_theory, y = pain) + geom_point() 
theory_prediction_scatter

#Comparing predicting accuracy using the residual squared sum of differences 
RSS_theory = sum((data_sample_2_final$pain - data_sample_2_final$predicted_pain_theory)^2) 
RSS_backward = sum((data_sample_2_final$pain - data_sample_2_final$predicted_pain_backward)^2) 

RSS_theory
RSS_backward




