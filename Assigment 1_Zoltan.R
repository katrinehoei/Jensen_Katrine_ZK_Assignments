##########Assignment 1##########

#Running packages
library(tidyverse)
library(psych)
library(dplyr)
library(car)
library(lmtest)

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

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

#Checking the data 

data_sample_1

View(data_sample_1)

data_sample_1 %>%
  summary()

#Checking possible coding error 
data_sample_1$age %>% describe() #Participant with age of 444 
data_sample_1$household_income %>% describe() #negative household income 
data_sample_1$STAI_trait %>% describe() #participant with a score outside the range (20-80)

#Taking a closer look at the affected variables
data_sample_1 %>%  slice(c(150, 109, 93))

#Removing two variables  

data_sample_1_new <- data_sample_1[-c(150, 93),]

View(data_sample_1_new)

#Visualizing and re-checking the affected variables 

age_bar_chart = 
  data_sample_1_new %>%
  ggplot() +
  aes(x = age) + geom_bar()
age_bar_chart

STAI_bar_chart = 
  data_sample_1_new %>%
  ggplot() +
  aes(x = STAI_trait) + geom_bar()
STAI_bar_chart 

#Creating model 1 

mod1 = lm(pain ~ age + sex, data = data_sample_1_new)

#Checking assumptions 

#Visual inspection of potential outliers 

mod1 %>% plot(which = 5) #Residuals vs leverage plot 

mod1 %>% plot(which = 4) #Cook's distance, 

#Although none of the three outliers have distances>1, they are all above 4/n. I will therefore check the outlying individual variables to see if it may be a result of a coding error 

data_sample_1_new %>% 
  slice(c(100, 128, 141))

#They look okay. I will now continue to check the assumptions, in order to decide what to do with these outlying case. 

#Assumption 1: Normality

mod1 %>% plot(which = 2) #QQ-plot - visual inspection

residuals_mod1 = enframe(residuals(mod1))
residuals_mod1 %>% 
  ggplot() + aes(x = value) + geom_histogram() #2nd visual inspection - histogram of the residuals 

describe(residuals(mod1)) #Checking for skew and kursosis 

#All is within normal range - the assumption of normality holds 

#Assumption 2: Linearity 

mod1 %>% residualPlots() #Visual inspection and tests 

#Although there is slight curvature, the tests are both non-significant. Linearity assumption holds true.

#Assumption 3: Homoscedasticity  

mod1 %>% plot(which = 3) #Visual inspection

mod1 %>% ncvTest() #Non-constant variance score test

mod1 %>% bptest() #Breush-Pagan test

#All tests are insignificant. No indication of heteroscedasticity. 

#Assumption 4: Multicollinearity 

mod1 %>% vif() #Variance inflation factor 

#Both below threshold of 3. This indicates that the assumption of multicollinearity holds.
#Because all four assumptions of linear regression held true. I will keep the outliers identified in Cook's distance in my model. 

#Creating model 2 

mod2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1_new)

#Visual inspection of potential outliers 

mod2 %>% plot(which = 5) #Residuals vs leverage plot 

mod2 %>% plot(which = 4) #Cook's distance 

#Three outliers stand out - checking these rows to see if it is a result of a coding error 

data_sample_1_new %>% 
  slice(c(68, 100, 114))

#They look valid. I will now continue to check the assumptions, in order to decide what to do with these outlying case. 

#Assumption 1: Normality

mod2 %>% plot(which = 2) #QQ-plot - visual inspection

residuals_mod2 = enframe(residuals(mod2))
residuals_mod2 %>% 
  ggplot() + aes(x = value) + geom_histogram() #2nd visual inspection - histogram of the residuals 

describe(residuals(mod2)) #Checking for skew and kursosis 

#All is within normal range - the assumption of normality holds 

#Assumption 2: Linearity 

mod2 %>% residualPlots() #Visual inspection and tests 

#Although there is very slight curvature in some of the variables, the tests are all non-significant. Linearity assumption holds true.

#Assumption 3: Homoscedasticity  

mod2 %>% plot(which = 3) #Visual inspection

mod2 %>% ncvTest() #Non-constant variance score test

mod2 %>% bptest() #Breush-Pagan test

#All tests are insignificant. No indication of heteroscedasticity. 

#Assumption 4: Multicollinearity 

mod2 %>% vif() #Variance inflation factor 

#Indication of multicollineartiy (Both cortisal predictors score > 3)

#Checking the correlation matrix

data_sample_1_new %>% select(age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva) %>% pairs.panels(col = "red", lm = T)

#Cortisol serum and cortisol saliva are very highly correlated (0.89)

#Removing cortisol_saliva from model_2

mod2_final =  lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1_new)

#Because I have exluded a variable from my model, I will re-check all model assumptions

mod2_final %>% plot(which = 5) #Residuals vs leverage plot

mod2_final %>% plot(which = 4) #Cook's distance  

#Checking to see if the outliers are valid

data_sample_1_new %>% 
  slice(c(96, 100, 114)) #They look fine - nothing seems to be an error 

#Assumption 1: Normality 

mod2_final %>% plot(which = 2) #QQ plot - visual inspection

residuals_mod2_final = enframe(residuals(mod2_final)) #Visual inspection, histogram of residuals
residuals_mod2_final %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(mod2_final)) #Checking for skew and kurtosis

#All within normal range - No violation of normality

#Assumption 2: Linearity 

mod2_final %>% residualPlots() 

#All tests are non-significant - no violation of linearity

#Assumption 3: homoscedasticity 

mod2_final %>% plot(which = 3) #Visual inspection

mod2_final %>% ncvTest() #Non-constant variance score test

mod2_final %>% bptest() #Breush-Pagan test

#All non-significant. No indication of a violation. 

#Assumption 4: multicollinearity 

mod2_final %>% vif() #No variables scoring >3, no longer a violation of the assumption

#Model comparison

mod1
mod1 %>% summary() #Checking model 1 
summary(mod1)$adj.r.squared * 100
confint(mod1)
coef_table(mod1)
AIC(mod1)

mod2_final
mod2_final %>% summary() #Checking model 2
summary(mod2_final)$adj.r.squared * 100
confint(mod2_final)
coef_table(mod2_final)
AIC(mod2_final)

anova(mod1, mod2_final)
AIC(mod1, mod2_final)

