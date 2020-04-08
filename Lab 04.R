#---------------------------------------------------------------------------------------------------------------
#
# Some Code from Lab 4
#
# Glen Livingston Jr
# 24/03/2020
#
#---------------------------------------------------------------------------------------------------------------

# Make sure the working directory is set

#install.packages("haven") # This installs the package to open SPSS files
library("haven") # This loads the package

rm(list = ls()) # remove all the items in the workspace

dat <- haven::read_sav('Lab 04 - Activity.sav') # Reads the data
dat <- as.data.frame(dat) # Converts it to a data frame.

dat


lm_1 = lm(Y ~ TrmntGrp, data = dat) # Can define the model object like this... or
anova(lm_1)

lm_2 = lm(data$Y ~ data$TrmntGrp) # This method works but does not play well with  the Levene's test function.
anova(lm_2)                       # Best not to use this method
leveneTest(lm_2)

attach(dat) # This is another option. This adds the items of the data frame into the normal workspace

lm_1 = lm(Y ~ TrmntGrp)
anova(lm_1)

resids <- residuals(lm_1)
shapiro.test(resids) # p = 0.8442, no evidence of a departure from normality

library(car)
qqPlot(resids) # plot looks pretty good. points follow the line fairly well. 
               # No evidence of a departure from normality

leveneTest(lm_1) # p value is less than 5%, therefore reject the null of equal variance and conclude that 
                 # not all population variances are equal

lm_1
resids <- residuals(lm_1)
preds  <- predict(lm_1)
plot(preds, resids) # Looks as though the variance increases for the groups withe larger predicted values. 
                    # There is fanning out of the residuals. Log may be helpful here.  

# Transforming the data

dat$LogY <- log(dat$Y) # This adds a new variable to the data frame called dat
                       # This is fine, but we used the attach command so we can just make a new variable in the 
                       # workspace if we like.

LogY = log(Y)

lm_2 = lm(LogY~TrmntGrp)
anova(lm_2)

resids_2 <- residuals(lm_2)
shapiro.test(resids_2) # p = 0.05887, at the 5% significance level, will not reject. But normality has 
                       # been affected.

qqPlot(resids_2) # Bit of an S shape around the line, supports the low p-value. 

leveneTest(lm_2) # p value is now 0.2245, therefore no evidence that the populaiton varinances are unequal

lm_2
preds_2  <- predict(lm_2)
plot(preds_2, resids_2) # No fanning out of the residuals now. 

# Tukey HSD
aovObject <- aov(lm_1)
TukeyHSD(aovObject)

LOGaovObject <- aov(lm_2)
TukeyHSD(LOGaovObject)


# Orthogonal Contrasts

rm(list = ls())

dat <- haven::read_sav('Lab 04 - Learning.sav')
dat <- as.data.frame(dat)

is.factor(dat$Trtmnt_Grp) # Is the variable a factor? We want R to know this is a categorical variable.

dat$Trtmnt_Grp = as.factor(dat$Trtmnt_Grp) # Re-save the variable as a factor

# Contrasts

c1 = c(0.5,  0.5, -1/3, -1/3, -1/3) # Constants (c_i) for the first contrast
c2 = c(0.5, -0.5,  0.0,  0.0,  0.0) # Constants (c_i) for the second contrast
c3 = c(0.0,  0.0,  0.5,  0.5, -1.0) # Third
c4 = c(0.0,  0.0,  1.0, -1.0,  0.0) # Fourth

mat = cbind(c1, c2, c3, c4) # cbind = column bind, so make a matrix by taking the vectors as 
                            # column vectors and adding them together. rbind() is also a function

summary(lm(Trials ~ Trtmnt_Grp, data=dat, contrasts=list(Trtmnt_Grp = mat))) 
# Here the lm function is nested in the summary function. Could also do the following
lm_3 = lm(Trials ~ Trtmnt_Grp, data=dat, contrasts=list(Trtmnt_Grp = mat))
summary(lm_3)





