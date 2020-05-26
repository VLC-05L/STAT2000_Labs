#--------------------------------------------------------------------------------------
#
# STAT2000: Lab 10 - Week 11
#
# Glen Livingston Jr
#
# 26/05/2020
#
#--------------------------------------------------------------------------------------


rm(list = ls())
library("car")


#--------------------------------------------------------------------------------------
# VIF and Cook's
#--------------------------------------------------------------------------------------


data = haven::read_sav("Lab_09_Surgical.sav")
data = as.data.frame(data)


head(data)

lm_1 = lm(Survival1~BloodClot+Liver1, data = data)
Anova(lm_1,type = 2)
summary(lm_1)

# Liver function is a statistically significant predictor (p>0.001). 
# Unable to identify Bloodclot score as a statistically significant predictor 
# (p=0.931). 

vif(lm_1)

# VIF < 5, therefore we do not identify multicollinearity.  

data$LiverC = data$Liver1 - mean(data$Liver1)
data$EnzymeC = data$Enzyme1 - mean(data$Enzyme1)
lm_2 = lm(Survival1 ~ Liver1 + Enzyme1 + LiverC:EnzymeC, data)
cooks_2 = cooks.distance(lm_2)

hist(cooks_2)
abline(v = 1, col = "red")

plot(cooks_2, ylim = c(0,1))
abline(h = 1, col = "red")

# Data point 5 is close to 1 but below it. 

dataWithout5 = data[-5,]

lm_3 = lm(Survival1 ~ Liver1 + Enzyme1 + LiverC:EnzymeC, dataWithout5)

summary(lm_2)$coefficients[,1]
summary(lm_3)$coefficients[,1]

# Coefficients have changed a little. 

summary(lm_2)$adj.r.squared
summary(lm_3)$adj.r.squared

# Adjusted R Squared has reduced though implying the original model is better. 


#--------------------------------------------------------------------------------------
# ANOVA as Regression
#--------------------------------------------------------------------------------------

# ANOVA model: Y_i = \mu + \alpha_i + \varepsilon_i

# For group i = 1, fertiliser A, expected Y is: Yhat = \mu + \alpha_1
# For group i = 2, fertiliser B, expected Y is: Yhat = \mu + \alpha_2
# For group i = 3, fertiliser C, expected Y is: Yhat = \mu + \alpha_3

# \alpha's here are positive and negative with \mu sitting somewhere in the 
# middle of them. 

# For regression:
# Group i = 1: Yhat = \beta_0 + \beta_1 * 0 + \beta_2 * 0 = \beta_0
# Group i = 2: Yhat = \beta_0 + \beta_1 * 1 + \beta_2 * 0 = \beta_0 + \beta_1
# Group i = 3: Yhat = \beta_0 + \beta_1 * 0 + \beta_2 * 1 = \beta_0 + \beta_2

# for both of these models the Yhat values for each group will be the same. 
# That is, \mu + \alpha_1 = \beta_0.

# The ANOVA model measures the distance from the global mean, where as the 
# regression model measures the distance from a reference level, in this case
# group 1. 


data <- haven::read_sav('Lab_03_Fertilizer.sav')
data$Type <- as.factor(data$Type)
data <- as.data.frame(data)

data$X1 <- NA
data$X2 <- NA

data$X1[data$Type == 'A'] <-  0 # this is checking if Type is A, make X1 equal 0
data$X2[data$Type == 'A'] <-  0 # this is checking if Type is A, make X2 equal 0
data$X1[data$Type == 'B'] <-  10 # this is checking if Type is B, make X1 equal 1
data$X2[data$Type == 'B'] <-  0 # this is checking if Type is B, make X2 equal 0
data$X1[data$Type == 'C'] <-  0
data$X2[data$Type == 'C'] <-  20

lm_4 = lm(Tonnes~Type, data = data)
Anova(lm_4,type=2)
summary(lm_4)
# p-values are 0.921 and 0.491 in the summary object

lm_5 = lm(Tonnes~X1 + X2, data = data)
Anova(lm_5,type=2)
summary(lm_5)
# p-values are 0.921 and 0.491 in the summary object
# Even after changing the indicators to 10 and 20, the p-values are still 
# the same. The values of the parameter estimates has changed, reduced, 
# but after substituting in the larger indicator values the fitted values will
# be the same as the original model. 

Anova(lm_4,type=2)
summary(lm_4)


#--------------------------------------------------------------------------------------
# Teaching Language
#--------------------------------------------------------------------------------------

data <- haven::read_sav('Lab_10_Language.sav')
data <- as.data.frame(data)
data$METHOD = as.factor(data$METHOD)
head(data)

boxplot(LANGUAGE~METHOD, data = data)
# Method 2 appears to be higher than 1 and 3. 

ggplot(data = data, aes(x=METHOD, y=LANGUAGE)) + geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.1))

lm_6 = lm(LANGUAGE~METHOD, data = data)
Anova(lm_6,type=2)
# We are unable to identify a statistically significant effect of method on 
# language scores (p = 0.1092).

colMeans(data[data$METHOD == 1,2:3])
colMeans(data[data$METHOD == 2,2:3])
colMeans(data[data$METHOD == 3,2:3])

# IQ scores appear to be different in each group. 

plot(LANGUAGE~IQ, data = data)
# Does appear to be a positive relationship between IQ and Language score. 

ggplot(data = data, aes(x=IQ, y=LANGUAGE, colour = METHOD)) + 
  geom_point(size = 2) 


# The model with IQ included

# Y_i = \beta_0 + \beta_1 X1_i + \beta_2 X2_i + \beta_3 X3_i + \varepsilon_i
# Here X1 and X2 are indicator variables used to define the level of the 
# categorical predictor variable METHOD. X3 is the numerical predictor of IQ.
# \beta_0 is the population intercept term, \beta_1 represents the populaiton 
# difference between the fitted value for level 1 and level 2 of the 
# categorical variable. \beta_2 ...

lm_7 = lm(LANGUAGE~METHOD+IQ, data = data)
Anova(lm_7,type=2)
summary(lm_7)

# For method 1, the fitted model is:
#     Yhat = 42.10 + 4.91*0 - 2.16*0 + 0.316*IQ
#     Yhat = 42.10 + 0.316*IQ

# For method 2, the fitted model is:
#     Yhat = 42.10 + 4.91*1 - 2.16*0 + 0.316*IQ
#     Yhat = 47.01 + 0.316*IQ

# For method 3, the fitted model is:
#     Yhat = 42.10 + 4.91*0 - 2.16*1 + 0.316*IQ
#     Yhat = 39.94 + 0.316*IQ

# These are three different regression lines with the same slope, 
# but different intercept. 

data = cbind(data,predict(lm_7,interval="confidence"))

ggplot(data = data, aes(x=IQ, y=LANGUAGE, colour = METHOD)) + 
  geom_point(size = 2) + 
  geom_line(aes(y=fit)) # + geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.05)


Anova(lm_7,type=2)

# Both method and IQ are statistically significant predictors at the 5% 
# significance level.


Anova(lm_7)
Anova(lm_6)

# Mean square of the residuals is the "average" variation of the residuals. 
# It is calculated as the Sum Sq / Df. For the model with IQ as a predictor,
# This is: 
998.98/26
# = 38.42.

# For the model without IQ this is:
1712.60/27
# = 63.43.

# If the residuals represent the "left over" variation in our response 
# variable after we use the predictor variables, then we want the residual
# mean square to be low. This will mean that we have explained more of the 
# variation in our response variable with the model. 


lm_8 = lm(LANGUAGE~METHOD+IQ+METHOD:IQ, data = data)
Anova(lm_8,type=2)
summary(lm_8)
# Interaction effect is not statistically significant (p=0.37)


data = cbind(data,predict(lm_8,interval="confidence"))
colnames(data)[7:9] = c("fit2", "lwr2", "upr2")

ggplot(data = data, aes(x=IQ, y=LANGUAGE, colour = METHOD)) + 
  geom_point(size = 2) + 
  geom_line(aes(y=fit2)) #+ geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.05)


