#------------------------------------------------------------------------------------------
#
# STAT2000: Lab 6 - Random Effects ANOVA
#
# Glen Livingston Jr
#
# 07/04/2020
#
#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
# Exercise 6.1 - Fixed or Random Effects
#------------------------------------------------------------------------------------------

# 1. 
# They are likely interested in making some statement about the entire population of  
# production lines, not just the ones that happened to randomly sample. Therefore the 
# production line would be considered a random effect. 

# 2. 
# Here the scientist has specific states (drink, sleepy, normal) that they are interested 
# in making some statement about. Therefore the effect should be considered fixed. 

# 3.
# The car manufacturer has randomly selected the drivers. This indicates that they are not
# likely interested in the effect of each individual driver, but on whether driver has an  
# effect on lap times in general.  
# 
# For the car variable, there are many "levels" that this variable can have. The company 
# did not randomly choose other cars, in fact it appears that they chose two of their 
# competitors cars. This indicates that they have a particular interest in those cars, and 
# therefore the factor should be considered fixed. 
#
# Not asked for in the question, but consider an interaction effect. An interaction effect 
# between a fixed and a random effect should be considered random. This might be an  
# important term to include in the model. Maybe some driver's driving styles suit 
# particular cars more so than other driver's styles. Especially for the cars that we are
# looking at here. 


#------------------------------------------------------------------------------------------
# Exercise 6.2 - Bean Counters
#------------------------------------------------------------------------------------------

# 1. 
# The accounting firm is not interested in the partners that they just happened to randomly 
# slect. They are interested in making a statement about whether partner has some effect on 
# write offs in general. They are interested in making a statement about the population of 
# partners. 

# 2.
# This is excellently presented in the lecture slides. 

# 3.

library("lme4")
library("lmerTest")

data = read.csv(file = "./Lab 06 - Accounting.csv")

fit_1 = lmer(write_off~(1|partner), data = data)
fit_1
summary(fit_1)

anova(fit_1)
# There are no fixed effects in the model. 
# Note the use of type III sums of squares, even though normally the anova() function uses
# type I sums of squares. 

rand(fit_1)
rand(fit_1)$`Pr(>Chisq)`[2] # Extracting the p-value

# Null and Alternative: 
# H_0: \sigma^2_A = 0
# H_A: \sigma^2_A =/= 0  -  OR  -  H_A: \sigma^2_A > 0

# Since the p-value (p = 0.004) is less than our significance level (assumed) of 5%, we 
# will reject the null hypothesis and conclude that the variance of our random effect is 
# not equal to zero. This indicates that the factor of partner has an affect on write offs.
# 
# If the factor of "partner" has an affect on write offs, this means that there is 
# variation in write offs that is due to there being different partners working on jobs. In
# order to remove this source of variation, perhaps the accounting firm could introduce 
# standard methods for quoting jobs to ensure jobs are not under quoted or over quoted. 
# Perhaps partners and their managers could undergo training so that across the firm, write
# offs are more homogenous. 

# 4. 
#
# Assumptions 
#
# Random Effect: We are told that the levels of the random effect are randomly chosen.  
#
# Randomness and Independence: The clients were randomly selected from the population of 
# clients a partner has. This should ensure independence across partners clients. There 
# will be some dependence within partners, but this is expected and ok with a random 
# effect. 
#
# Normality

shapiro.test(fit_1) # Note that this doesn't work. They type of object created by the 
                    # lmer() function does not play well with these functions. Have to 
                    # do this more explicitly by extracting the residuals.  

resid_1 = residuals(fit_1)
shapiro.test(resid_1)

# Shapiro Wilk test results in a p-value of 0.1817. The null hypothesis is that the data 
# (residuals in this case) are normally distributed. Therefore we are unable to reject 
# the null at the 5% significance level. No evidence that this assumption has been 
# violated. 

# Constant Variance

library(car)
leveneTest(fit_1) # Note that this doesn't work. They type of object created by the 
                  # lmer() function does not play well with these functions. Have to 
                  # do this more explicitly by extracting the residuals. 

leveneTest(resid_1) # This wont work, we need to tell it what residuals relate to what 
                    # partner

leveneTest(resid_1~data$partner) # This didn't work either, lets look at the variable 
                                 # partner

data$partner # Ahh... this is not a factor. R thinks that this is a vector of numerical 
             # variables. Ok, lets tell R this is a factor. 

data$partner = as.factor(data$partner)

data$partner # Now you can see that the vector has "Levels: 1 2 3 4" under it, indicating 
             # R knows its a factor. 

is.factor(data$partner)

leveneTest(resid_1~data$partner)

# Ok, p-value is 0.6516, therefore we do not have enough evidence to reject the null of 
# constant variance across the groups. Assumption is satisfied. 

# outliers
boxplot(write_off~partner, data = data)

# Potential outliers for partner 1, however R tends to claim things as outliers when they 
# are likely not. Will look at better methods for outlier detection later in the course. 


# 5. 
# Doesn't appear that there has been random selection of levels from the entire population 
# of levels. These levels, small medium and large are likely the levels that we actually 
# care about. 


# 6. 

fit_2 = lmer(write_off~(1|partner)+size, data = data)
summary(fit_2)
anova(fit_2)
rand(fit_2)

# Null and alternative for the random effect is the same as above. For the fixed effect it 
# is:
#
# H_0: \alpha_i = 0 for all i in 1,2,3
# H_A: \alpha_i =/= 0 for at least one i in 1,2,3

# The p-value for the fixed effect (p = 4.675e-07) provides very strong evidence that the 
# size of the client has a significant effect on the write offs. 
#
# The p-value for testing the random effect of partner is now 0.82, therefore we are 
# unable to identify a statistically significant effect on write offs due to partner. 
#

# 7.
# See the solutions







