#---------------------------------------------------------------------------------------
#
# STAT2000 - Lab 7 - Non-parametric methods
#
# Glen Livingston
#
# 05/05/2020
#
#---------------------------------------------------------------------------------------

rm(list = ls())

#---------------------------------------------------------------------------------------
# Parkinson’s Disease
#---------------------------------------------------------------------------------------

data <- haven::read_sav('Lab_07_Parkinsons.sav') # Reads the data
data <- as.data.frame(data) # Converts it to a data frame. 

boxplot(data$BeforeSurgery,data$AfterSurgery)

# What is the null? 
# H_0: The population median of the difference in severity of the disease is equal to zero. 
# H_A: The population median of the difference in severity of the disease is greater than zero.

test_1 = wilcox.test(data$BeforeSurgery,data$AfterSurgery,paired = T,alternative = "greater")
test_1$p.value

# Since the p-value is low, we reject the null and conclude that the severity of the disease
# is lower after the treatment. 


# What is the null? 
# H_0: \mu_d = 0 
# H_A: \mu_d > 0
test_2 = t.test(data$BeforeSurgery,data$AfterSurgery,paired = T,alternative = "greater")
test_2$p.value
# Again, very low p-value. Therefore there is strong evidence that the the severity of 
# the disease has reduced following the treatment.

# Using both methods we were able to conclude that the treatment had a positve effect on 
# the patients. The null and alternatives are slightly different though with the 
# non-parametric null being about the population median of the difference, whereas the 
# parametric version is about the population mean of the difference. The p-value of the
# parametric test was lower as expected (though not guaranteed).


#---------------------------------------------------------------------------------------
# Pregnancy
#---------------------------------------------------------------------------------------

data <- haven::read_sav('Lab_07_Pregnancy.sav') # Reads the data
data <- as.data.frame(data) # Converts it to a data frame. 

test_1 = wilcox.test(Pregnancies~Group, data = data)


# Another way to perform the test. What way you choose will depend on the format of the 
# data.
x = data$Pregnancies[data$Group == "Pregnant"]
y = data$Pregnancies[data$Group != "Pregnant"]
wilcox.test(x,y, data = data)

# Pearson Chi-squared test

chisq.test(table(data))
# Chi square test does not take into account the ordinal nature of the number of 
# pregnancies. 

# Why were the researchers interested in making these comparisons between the two groups? 
# If this difference is a source of variation in the response variable and it is not 
# included in the model, we may make an error in our conclusion. A difference in the 
# number of pregnacies could be a confounding variable. This was explored in Lab 6 where 
# without the effect of the size of the clients, we made an incorrect conclusion. 



#---------------------------------------------------------------------------------------
# Colour
#---------------------------------------------------------------------------------------

data <- haven::read_sav('Lab_07_Colour.sav') # Reads the data
data <- as.data.frame(data) # Converts it to a data frame. 

data

boxplot(CountTrapped~ColourGrp, data = data)


# H_0: alpha_blue = alpha_green = alpha_lemon = alpha_white = 0
# H_A: at least one treatment effect not equal to 0

lm_1 = lm(CountTrapped~ColourGrp, data = data)
anova(lm_1)

# p-value is low, strong evidence to reject, therefore at least one treatment 
# effect not equal to 0.


car::leveneTest(lm_1)
shapiro.test(residuals(lm_1))
shapiro.test(residuals(lm_1)[data$ColourGrp == "Lemon"])
shapiro.test(residuals(lm_1)[data$ColourGrp == "White"])
shapiro.test(residuals(lm_1)[data$ColourGrp == "Green"])
shapiro.test(residuals(lm_1)[data$ColourGrp == "Blue"])
# Thought one of the assumption above would fail, but apparently not. ANOVA appears fine
# to use assuming independence, randomness and no outliers.  


# Non-parametric
# H_0: The data comes from the same population distribution.
# H_A: At least one of the colours has a different population distribution.


kruskal.test(CountTrapped~ColourGrp, data = data)

# There is strong evidence (p=0.0007) that at least one of the population distribtutions
# differs.

library("NSM3")
pSDCFlig(x = data$CountTrapped,g = data$ColourNum, method = "Asymptotic")
# What a silly function, doesnt allow a factor in the group vector.

# 1 - Lemon
# 2 - White
# 3 - Green
# 4 - Blue

# Diffs - 1-2; 1-4; 
# Diffs - Lemmon-White; Lemon-Blue; 

TukeyHSD(aov(lm_1))
# Can identify all differences except between blue and white. 

# ANOVA is more powerful, therefore we are able it identify more differences
# than we did by using the non-parametric Kruskal Wallace. If the assumption for the 
# parametric test are met, we should always use the parametric test. 








