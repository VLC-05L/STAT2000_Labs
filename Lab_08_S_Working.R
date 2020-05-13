#---------------------------------------------------------------------------------------
#
# STAT2000 - Lab 8 - Simple Linear Regression
#
# Glen Livingston
#
# 12/05/2020
#
#---------------------------------------------------------------------------------------

rm(list = ls())
library("car")


#---------------------------------------------------------------------------------------
# Stress and Health
#---------------------------------------------------------------------------------------

data <- haven::read_sav('Lab_08_Stress.sav') # Reads the data
data <- as.data.frame(data) # Converts it to a data frame. 

summary(data)

# Symptoms are indicators of psychological health. 
# We want to predict these based on the stress measure.

plot(Symptoms~Stress, data = data)

# Positive relationship. As stress score increased we expect an increase in symptoms score, 
# on average. Appears to be linear. Even without looking at a residual plot, it appears that the 
# variation around a line of best fit increases for larger values of the predictor. This may be 
# an issue. 


lm_1 = lm(Symptoms~Stress, data = data)
summary(lm_1)

# y_hat = 73.89 + 0.78*x
# predicted symptoms score = 73.89 + 0.78 * (stress score)


summary(lm_1)


# Variance * (n-1) = Total sum of squares
var(data$Symptoms) * (length(data$Symptoms)-1) 

anova(lm_1)
# From the anova() table
SS_R = 11148
SS_E = 32386

# Therefore, R squared is SS_R/SS_T
SS_R/(SS_E+SS_R)

summary(lm_1)$r.squared

# The R squared value indicates that 25.6% of the variation in symptom scores 
# is explained by the linear regtression with stress score as the predictor. 

data$res_1 = residuals(lm_1)

plot(res_1~Stress, data = data)
# As expected, the residuals fan out as stress increases. There is non-constant 
# variance, heteroskedasticity.

data$pred_1 <- predict(lm_1)

plot(res_1~pred_1, data = data)
# Non random pattern - bad. 


qqPlot(data$res_1)
shapiro.test(data$res_1)
# For the QQ plot there are many points in the right hand tail that are outside the 
# printed confidence region. This gives evidence of non-normaility. This is confirmed
# by the small p-value in the Shapiro-Wilk test.  

# Testing for a linear relationship
# This can be done in two ways, testing for a statistically significant correlation, 
# or by testing the population slope coefficient. 
# Given that in this case there is an obvious response and predictor relationship, 
# this should lead you to test the population slope coefficient. That said, both 
# approaches will be consistent. 

# correlation
cor.test(data$Stress,data$Symptoms)$p.value
# Very low p-value, reject the null and conclude statistically significant linear
# relationship. 

# regression
# H_0: \beta_1 = 0
# H_A: \beta_1 =/= 0

summary(lm_1)$coefficients[2,]
# t = 6.01
# p = 2.69e-08

anova(lm_1)
# F = 36.1 which is equal to the t test statistic squared
# p = 2.69e-08

# Very low p-value (p < 0.001) provides very strong evidence of a statistically significant 
# linear relationship between stress score and symptoms score. 


confint(lm_1)
# 95% CI: (0.52, 1.04)

# Interpreation: We are 95% confident that for a unit increase in stress score, 
# we expect symptom score to increase by somewhere between 0.52 and 1.04, on average. 

# Since zero is not contained in this interval, we can be 95% confident that there 
# exists a statistically significant linear relationship between stress score and
# symptom score. 

# Disadvantage? - I cant think of any... Maybe you dont get a conditional probability 
# of your test statistic assuming the null is true - ie, a p-value. 
# Advantages? - You get an indication of the uncertainty surrounding the parameter
# estimate. You also get an indication of what the p-value would be. 
# Eg. Say a 95% interval is (0.01, 0.35) and testing for zero inside the interval. 
# Zero is almost inside the interval, but just not, so the p-value would be a little 
# under 0.05. If the interval was (1.01, 1.35) and testing for zero, the p-value 
# would be tiny. 


new_data = data.frame(Stress=35)
predict(lm_1, new_data, interval='confidence')

predict(lm_1, new_data, interval='prediction')
# this interval is wider because it has two sources of variation or uncertainty. The
# confidence interval diplays our uncertainty around the true value of the populaiton
# mean for a given value of our predictor. The prediction interval includes this
# same uncertainty, but also has the additional variation or uncertainty added due 
# to the natural variation present in the data. 


# As the asessment of the model showed that the residuals had non-constant variance,
# we should try to tranform the variables and fit a model that meets the assumptions. 
# Transforming the data
plot(Symptoms~Stress, data = data)
plot(log(data$Symptoms)~sqrt(data$Stress))

lm_2 = lm(log(data$Symptoms)~sqrt(data$Stress))
summary(lm_2)
plot(lm_2)
qqPlot(residuals(lm_2))
shapiro.test(residuals(lm_2))

# This was a first attempt, other transformations may yield better results. 
