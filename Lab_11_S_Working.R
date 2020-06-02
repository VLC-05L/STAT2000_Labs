#--------------------------------------------------------------------------------------
#
# STAT2000: Lab 11 - Week 12
#
# Glen Livingston Jr
#
# 02/06/2020
#
#--------------------------------------------------------------------------------------


rm(list = ls())
library("car")


#--------------------------------------------------------------------------------------
# Another look at the AFL Data
#--------------------------------------------------------------------------------------

data = haven::read_sav(file = "Lab_11_AFL.sav")
data = as.data.frame(data)

head(data)

hist(data$MCG)

plot(MCG~(Members), data = data)
boxplot(MCG~(Away), data = data)

table(data$Away)

boxplot(Members~(Away), data = data)
boxplot(MCG~(Home), data = data)

table(data$Home)

Anova(lm(MCG~Home, data = data),type=2)
Anova(lm(MCG~Away, data = data),type=2)

head(data)

lm_1 = lm(MCG~Members + Interstate, data = data)
Anova(lm_1, type = 2)
summary(lm_1)

lm_2 = lm(MCG~Members + Interstate + Members:Interstate, data = data)
Anova(lm_2, type = 2) # OOOPS!!! This model has interaction, therefore should use type 3 sums of squares. 
Anova(lm_2, type = 3)
summary(lm_2)

lm_3 = lm(MCG~Members + Interstate + Members:Interstate + Other, data = data)
Anova(lm_3, type = 3)
summary(lm_3) # Adjusted R Square 0.7137

res_3 = residuals(lm_3)
qqPlot(res_3)
shapiro.test(res_3)
plot(res_3~Other, data = data)
plot(res_3~Members, data = data)
boxplot(res_3~Interstate, data = data) # differing variance for the two groups?
leveneTest(res_3,data$Interstate) # Appears as though there may be different variance for the two groups. 


lm_4 = lm(LogMCG~Members + Interstate + Members:Interstate + Other, data = data)
Anova(lm_4, type = 3) # Other is not significant now
summary(lm_4) # Adjusted R Square 0.7247

res_4 = residuals(lm_4)
qqPlot(res_4)
shapiro.test(res_4)
plot(res_4~Other, data = data)
plot(res_4~Members, data = data) # Variance of the residuals gets smaller as members increases? 
boxplot(res_4~Interstate, data = data)

lm_5 = lm(LogMCG~Members + Interstate + Members:Interstate, data = data)
Anova(lm_5, type = 3) # Final model in the lecture
summary(lm_5) # Adjusted R Square 0.7152

res_5 = residuals(lm_5)
qqPlot(res_5)
shapiro.test(res_5)
plot(res_5~Members, data = data) # Variance of the residuals gets smaller as members increases?
boxplot(res_5~Interstate, data = data)


data$rootMCG = sqrt(data$MCG)

lm_6 = lm(rootMCG~Members + Interstate + Members:Interstate + Other, data = data)
Anova(lm_6, type = 3)
summary(lm_6) # Adjusted R Square 0.7256

res_6 = residuals(lm_6)
qqPlot(res_6)
shapiro.test(res_6)
plot(res_6~Other, data = data)
plot(res_6~Members, data = data)
boxplot(res_6~Interstate, data = data) # This has improved
leveneTest(res_6,data$Interstate) # Fine now. 

# When you have a positive response variable, two simple options for transformation are square root and log. Log is 
# more aggressive at removing non-constant variance as it makes a large number x smaller than what taking the square
# root of x does. It looks like in the above, using log was too aggressive. Tried square root and it worked. 

# Other was not significant at the 5% significance level, try removing it. 

lm_7 = lm(rootMCG~Members + Interstate + Members:Interstate, data = data)
Anova(lm_7, type = 3)
summary(lm_7) # Adjusted R Square 0.7068

res_7 = residuals(lm_7)
qqPlot(res_7)
shapiro.test(res_7)
plot(res_7~Other, data = data)
plot(res_7~Members, data = data)
boxplot(res_7~Interstate, data = data) # This has improved
leveneTest(res_7,data$Interstate) # Fine now. 

# I think that lm_6 was better. 


# The Other variable is interesting. It's not particularly useful for prediction as we will not know the 
# number before the saturday of the games. 

# Let's look at the value of the parameter estimate though. 

summary(lm_6) # Adjusted R Square 0.7256

# The parameter estimate is positive. This is saying that all other variables remaining constant, we expect 
# the square root of the attendance at the MCG to INCREASE by 13.17 people for every 1000 additional people
# attending other games in Melbourne? 

# An INCREASE? Is that what you would expect? How do you justify that? 

# Is an odd result. Possibly weather related? Temp is not significant but maybe whether it is raining has an effect?
# Something seasonal? 


plot(data$MCG~data$Date)

plot(data$MCG[1:19]~data$Date[1:19])  
anova(lm(data$MCG[1:19]~as.numeric(data$Date[1:19]))) # Not significant

plot(data$MCG[20:41]~data$Date[20:41])
anova(lm(data$MCG[20:41]~as.numeric(data$Date[20:41]))) # Not significant

# Lets add the two years together though for greater POWER!!!!

diff(as.numeric(data$Date))

one_year = c(as.numeric(data$Date[1:19]),
             as.numeric(data$Date[20:41]) - 365)

plot(data$MCG~one_year)
anova(lm(data$MCG~one_year)) # This is a significant predictor. Appears as though as the year goes on, more people 
# attend games. May not be significant when used in the other model though. 


lm_8 = lm(rootMCG~Members + Interstate + Members:Interstate + Other + one_year, data = data)
Anova(lm_8, type = 3) 
summary(lm_8) # Adjusted R Square 0.7256

# Not enough power to detect one_year as significant with all the other predictors in the model. The other predictors 
# do a better job at explaining the variation in attendance at the MCG. 

head(data)



