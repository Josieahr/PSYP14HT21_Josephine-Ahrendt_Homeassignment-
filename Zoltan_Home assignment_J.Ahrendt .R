library(tidyverse)
library(gridExtra)
library(psych)
library(lm.beta)
library(car) 
library(broom)
library(lmtest) 
library(sandwich) 
library(r2glmm) 
library(lme4) 
library(lmerTest) 
library(MuMIn)
library(cAIC4)
library(plyr)

#load data
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
view (data_sample_1)

#getting an overview over the data
str(data_sample_1)
describe(data_sample_1)

#data screening --> min and max values for each variable
describe(data_sample_1)

#making sex a factor variable 
data_sample_1 <- data_sample_1 %>% 
  mutate (sex = factor(sex))
table (data_sample_1$sex)

#filtering out the cases that have non-valid values for STAI-trait and pain 
data_sample_1 %>% 
  filter(STAI_trait < 20) 
# ID_34

data_sample_1 %>% 
  filter(pain > 10) 
#ID_88

#mutating the values (STAI_trait 4.2 = 42, pain 55 = 5.5) that did not seem to fit due to spelling errors
# someone pressed 5 twice instead of once 
data_sample_1_mutated <- data_sample_1 %>% 
  mutate (STAI_trait = replace (STAI_trait, STAI_trait == "4.2" , 42)) %>% 
  mutate (pain = replace (pain, pain == "55", 5))

#check whether mutating variables worked
view(data_sample_1_mutated)
str(data_sample_1_mutated)
describe(data_sample_1_mutated)

#checking mutated data through histograms 
old_STAI_trait_histogram<- ggplot(data_sample_1, aes(x = STAI_trait))+
  geom_histogram()
old_STAI_trait_histogram

new_STAI_trait_histogram <- ggplot(data_sample_1_mutated, aes(x = STAI_trait))+
  geom_histogram()

grid.arrange(old_STAI_trait_histogram, new_STAI_trait_histogram, ncol=2)

old_pain_histogram <- ggplot(data_sample_1, aes(pain))+
  geom_histogram()

new_pain_histogram <- ggplot(data_sample_1_mutated, aes(pain))+
  geom_histogram()

grid.arrange(old_pain_histogram, new_pain_histogram, ncol=2)

#creating linear regression model 
model_1 <- lm(pain ~ sex + age, data = data_sample_1_mutated)

#looking for outliers

# looking for extreme cases through visualisation with added case numbers --> changing x variable to all predictor variables but nothing really changes 
data_sample_1_mutated %>% 	
  mutate(rownum = row.names(data_sample_1_mutated)) %>%  	
  ggplot() +	
  aes(x = age, y = pain, label = rownum) +	
  geom_point() +	
  geom_text()	

# Cook's distance for model 1
model_1%>% 
  plot(which = 4)

model_1 %>% 
  plot(which = 5)

#Rule of thumb Cooks distance 4/160 = 0.025
#point 8, 23, 47 over 0.025
data_sample_1_mutated %>% 
  slice(c(8, 23, 47))
# cases seem to be realistic when looking at them in particular

#Checking assumptions for model_1
#Normality 
model_1 %>% 
  plot (which = 2)

residuals_model_1 = enframe(residuals(model_1))
plot_normality_mod1 <- residuals_model_1 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

#skew and kurtosis
describe(residuals(model_1))
#skew and kurtosis between -1 and 1 so normality is not violated

#Linearity
model_1 %>%
  residualPlots()
# none of the tests are significant which means that the linearity assumption is not violated

#Homoscedasticty 
model_1 %>%
  plot(which = 3)

model_1 %>%
  ncvTest()

model_1 %>%
  bptest()
#no violation of homoscedasticty because thep-values for the NCV and the Breush-Pagan test are non significant 

#Multicollinearity 
model_1 %>%
  vif()

#Builing model 2
model_2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1_mutated)

#Checking assumptions for model_2
#Cooks distance for model 2
model_2%>% 
  plot(which = 4)

model_2 %>% 
  plot(which = 5)
#looking whether there is anything wrong with the data, if there are any errors in the data

data_sample_1_mutated %>% 
  slice(c(47,74,86))
#data points look normal when looking at the data individually 

#Normality 
model_2 %>% 
  plot (which = 2)

residuals_model_2 = enframe(residuals(model_2))
plot_normality_mod2 <- residuals_model_2 %>%
  ggplot() + aes(x = value) + geom_histogram()
#skew and kurtosis
describe(residuals(model_2)) 
#skew and kurtosis lay both within -1 and 1 so the normality assumption is not violated

#Linearity
model_2 %>%
  residualPlots()
# none of the tests are significant which means that the linearity assumption is not violated

#Homoscedacity
model_2 %>%
  plot(which = 3)

model_2 %>%
  ncvTest()

model_2 %>%
  bptest()

#Multicollinearity --> cortisol_serum and cortisol_saliva will correlate = take cortisol_saliva out since serum is more scientifically backed up by research
#data multicollinearity, because cortisol measures are naturally collinear 
model_2 %>%
  vif()

#treat multicollinearity if VIF is above 3

#Rechecking assumptions for corrected model 
model_2_corrected <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1_mutated)
summary(model_2_corrected)

#Cooks distance for model_2_corrected
model_2_corrected%>% 
  plot(which = 4)

model_2_corrected %>% 
  plot(which = 5)

#Normality 
model_2_corrected %>% 
  plot (which = 2)

residuals_model_2_corrected = enframe(residuals(model_2_corrected))
residuals_model_2_corrected %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model_2_corrected))

#Linearity
model_2_corrected %>%
  residualPlots()

#Homoscedacity
model_2_corrected %>%
  plot(which = 3)

model_2_corrected %>%
  ncvTest()

model_2_corrected %>%
  bptest()

#Multicolinearity --> cortisol_serum and cortisol_saliva will correlate = take cortisol_saliva out since serum is more scientifically backed up by research
model_2_corrected %>%
  vif()

#in model_2_corrected now all assumptions are met and none are violated anymore

#model comparison also called hierarchical regression
#since model 1 is a subset of model 2 we can use the anova() function

#look at adjusted R squared function to see how much variance is explained by the old and the new model 
summary(model_1)$adj.r.squared
#0.072 --> 7% of vairance is explained by model 1
summary(model_2_corrected)$adj.r.squared
#0.5039 --> 50% of variance is explained by model 2
# there is a substantial increase of how much we can predict the pain level after surgery with model 2

#checking if difference between models is statistically significant --> making AIC model

AIC(model_1)	
#579.55
AIC(model_2_corrected)	
#483.25

#rule of thumb: AIC difference must be larger than 2, this is the case with model_1 and model_2_corrected
#the model with the lower AIC is the better fit model 

#anova F test
anova(model_1, model_2_corrected)
#there is a significant difference between the two models --> p-value is significant = 2.2e-16
#model with smallest RSS (residual sum of squares) is the better fit model 

#Zoltans custom code for coefficient table
coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}
confint(model_1)
lm.beta(model_1)
coef_table(model_1)
coef_table(model_2_corrected)

#Part 2
#data screening for the newly added variables: weight, IQ, household_income
data_sample_1_mutated %>% 
  select(weight) %>% 
  summary()
#Min value: 39.7, max value = 100.1

data_sample_1_mutated %>% 	
  ggplot() +	
  aes(x = weight) +	
  geom_histogram()

data_sample_1_mutated %>% 
  select(IQ) %>% 
  summary()
#Min value: 53, max value = 146

data_sample_1_mutated %>% 	
  ggplot() +	
  aes(x = IQ) +	
  geom_histogram()

data_sample_1_mutated %>% 
  select(household_income) %>% 
  summary()
#Min value: 3628.0, max value = 126667.0

data_sample_1_mutated %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_histogram()

#nothing out of the ordinary with the variables weight, IQ, household income

#Build linear model with all variables 

model_3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1_mutated)
summary(model_3)

#model diagnostics for model_3
#Cooks distance

model_3%>% 
  plot(which = 4)

model_3 %>% 
  plot(which = 5)

#Normality 
model_3 %>% 
  plot (which = 2)

residuals_model_3 = enframe(residuals(model_3))
residuals_model_3 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model_3))
#skew and kurtosis between -1 and 1 so no violation of normality

#Linearity
model_3 %>%
  residualPlots()
#no significant values for any of the variables so no violation of linearity

#Homoscedacity
model_3 %>%
  plot(which = 3)

model_3 %>%
  ncvTest()

model_3%>%
  bptest()

#Multicollinearity 
model_3 %>%
  vif()

#run a backward regression 
back_model = step(model_3, direction = "backward")
summary(back_model)
coef(back_model)

#final model after backwards regression 
lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, data = data_sample_1_mutated)

# run model diagnostics for back_model again because some variables were excluded
#Cooks distance

back_model%>% 
  plot(which = 4)

back_model %>% 
  plot(which = 5)

#Normality 
back_model %>% 
  plot (which = 2)

residuals_back_model = enframe(residuals(back_model))
residuals_back_model %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(back_model))
#skew and kurtosis between -1 and 1 so no violation of normality

#Linearity
back_model %>%
  residualPlots()
#no significant values for any of the variables so no violation of linearity

#Homoscedacity
back_model %>%
  plot(which = 3)

back_model %>%
  ncvTest()

back_model%>%
  bptest()

#Multicollinearity 
back_model %>%
  vif()

#save final model from part 1 of assignment as theory-based model
theory_based_model <- model_2_corrected

#comparison of backward_model and theory_based_model 

#look at adjusted R squared function to see how much variance is explained by the old and the new model 
summary(theory_based_model)$adj.r.squared
#0.5039 --> 50% of vairance is explained by theory_based_model
summary(back_model)$adj.r.squared
#0.5067 --> 51% of variance is explained by backward_model

#checking if difference between models is statistically significant --> making AIC model
AIC(model_3)
#487.92
AIC(theory_based_model)	
#483.2503
AIC(back_model)	
#480.407

#anova between model_3 and backward model 
anova(model_3, back_model)

#anova backward model and theory_based_model
anova(back_model, theory_based_model)

coef_table(back_model)

#load data set 2

data_sample_2 = read.csv("https://tinyurl.com/87v6emky")
view(data_sample_2)
describe(data_sample_2)
#there seems to be nothing wrong with the data from data_sample_2

#use models from data set one with the actual data from data set to to make predictions about how much the models from data set one are able to predict pain 
pred_theory_based_model <- predict (theory_based_model, data_sample_2)
pred_backward_model <-predict(back_model, data_sample_2)
  
#calculate sum of squares
RSS_theory_model = sum((data_sample_2[, "pain"] - pred_theory_based_model)^2)
RSS_theory_model
#243.7498

RSS_backward_model = sum((data_sample_2[, "pain"] - pred_backward_model)^2)
RSS_backward_model
#249.6759  

#backward model has more error 

confint(back_model)
lm.beta(back_model)
coef_table(back_model)

#Assignment 3 
#load data sets
data_sample_3 = read.csv("https://tinyurl.com/b385chpu")
view(data_sample_3)

data_sample_4 = read.csv("https://tinyurl.com/4f8thztv")
view(data_sample_4)

#screen data for errors 
summary(data_sample_3)
describe(data_sample_3)

data_sample_3 %>% 
  select (household_income) %>% 
  summary()
#Min value: -7884, Max value: 132361
#minus value for income unrealistic

data_sample_3 %>% 
  filter (household_income == -7884)
#ID_2 is the one with the negative household income --> exclude 

data_sample_3 <- data_sample_3 %>% 
  mutate (hospital = factor(hospital))
table (data_sample_3$hospital)

data_sample_3 <- data_sample_3 %>% 
  mutate (sex = factor(sex))
table (data_sample_3$sex)
#weird that there is one woman --> change to female!

data_sample_3 %>% 
  filter(sex == "woman")
#ID_25 is the one with woman for sex instead of female

#exclude and mutate data to change errors in the dataset 
data_sample_3_mutated <- data_sample_3 %>% 
  mutate (sex = replace (sex, sex == "woman" , "female")) %>% 
  slice(-2)

view(data_sample_3_mutated)

#build random intercept model
random_intercept_model <- lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1|hospital), data = data_sample_3_mutated)
random_intercept_model

#note the model coefficients and confidence intervals of coefficients for all fixed effect predictors 
summary(random_intercept_model)
coef(random_intercept_model)
confint(random_intercept_model)

#variance eplained by fixed effect predictor using marginal R^2
#marginal R^2 shows a proportion of variance explained by the fixed factors alone, significance can be interpreted from confidence intervals (no p-value)

r2beta(random_intercept_model, method = "nsj", data = data_sample_3_mutated)
#if the 95% CI does NOT contain 0, it means that he fixed effect term explains a significant portion of the variance of the outcome compared to the mean (the null model)
#in this random_intercept_model, cortisol_serum, pain_cat and age are significant
# mindfulness, sexmale, STAI_trait contain zero in 95% CI

#variance explained by fixed and random effect combined using conditional R^2
r.squaredGLMM(random_intercept_model)

#use regression equation obtained on data_sample_3_mutated to predict pain in data_sample_4
str(data_sample_4)
describe(data_sample_4)

#there seems to be nothing wrong with the data_sample_4

pred_model_4 <- predict (random_intercept_model, data_sample_4, allow.new.level= TRUE)
pred_model_4

#calculate sum of squares
RSS_random_intercept_model = sum((data_sample_4[, "pain"] - pred_model_4)^2)
RSS_random_intercept_model
#306.87

#Total sum of squares for model without any predictors
mod_mean <- lm(pain ~ 1, data = data_sample_4)
mod_mean

TSS = sum((data_sample_4$pain - predict(mod_mean))^2)
TSS
#495.50

R2 = 1 - (RSS_random_intercept_model/TSS)
R2
#0.38
#by using the random intercept model we are able to eyplain 38% of the variability of the outcome

#compare this R2 to the marginal and conditional R^2 computed for the model on data_sample_3_mutated

#Zoltan custom code
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

coef_table(theory_based_model)
summary(random_intercept_model)
stdCoef.merMod(random_intercept_model)
#build new linear mixed effect model on data_sample_3_mutated with pain as predictor variable
#only include most influential predictors from the previous model 
#allow for both random intercept and random slope 

#cortisol_serum is the most influential predictor since it has the highest standardized beta coefficient (35%) and therefore explains most of the variance of the predictor

mod_rnd_slope = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital),
                     data = data_sample_3_mutated)
mod_rnd_slope
?isSingular
#warning message singular fit 
#model migth be overfitted

#visualize fitted regression lines for each hospital separately
data3_mutated_slope = data_sample_3_mutated %>%
  mutate(pred_int = predict(random_intercept_model), pred_slope = predict(mod_rnd_slope))

#regression line for random slope model

neworder <- c("hospital_1", "hospital_2", "hospital_3", "hospital_4", "hospital_5", "hospital_6", "hospital_7", "hospital_8", "hospital_9", "hospital_10")
data3_mutated_slope_neworder <- arrange(mutate(data3_mutated_slope,
                                               hospital=factor(hospital,levels=neworder)),hospital)
data3_mutated_slope_neworder %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 1) + geom_line(color = "red",
                                                       aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)
#the regression lines all look quite similar to each other for all the hospitals 

#what to report: discuss whether random intercept or random slope model is a better fit for the data
sum(residuals(theory_based_model)^2)
#173.75
sum(residuals(random_intercept_model)^2)
#222.13
sum(residuals(mod_rnd_slope)^2)
#281.93

#in the case of random effect models it is more appropriate to look at the conditional AIC (cAIC)
cAIC(random_intercept_model)$caic
#617.60
cAIC(mod_rnd_slope)$caic
#671.18

#the cAIC of the random_intercept_model is smaller than 2 , indicating that the random_intercept_model seems to be a better fit 

#Likelihood ratio test
anova(random_intercept_model, mod_rnd_slope)

#R^2
r2beta(mod_rnd_slope, method = "nsj", data = data_sample_3_mutated)
r2beta(random_intercept_model, method = "nsj", data = data_sample_3_mutated)