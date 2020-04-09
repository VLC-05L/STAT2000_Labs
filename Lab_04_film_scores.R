#--------------------------------------------------------------------------------------------
#
# Ordinal Contrasts - film scores and box office
#
# Glen Livingston Jr
# Date: 13/03/2020
#
#--------------------------------------------------------------------------------------------

# rm(list = ls())

# Set the seed
set.seed(2844462)

#--------------------------------------------------------------------------------------------
# First data set
#--------------------------------------------------------------------------------------------

# Set the sample size
N = 3*5

# Simulate some film ratings
film_ratings = rep(c(1:5), each = N/5)

# Simulate box office figures based on a linear and quadratic relationship
box_office = 40 + (film_ratings)^2 + rnorm(N,sd=5)

# Set the ratings as an ordered factor 
film_ratings = ordered(film_ratings)

# Create the data frame and write
dat = data.frame(box_office = box_office,film_ratings = film_ratings)
#write.csv(x = dat, file = "Lab 04 - Box_Office_1.csv", row.names = F)

# Run the linear model with the polynomial contrasts
lm_contrast = lm(box_office~film_ratings, data=dat, contrasts=list(film_ratings = contr.poly(length(levels(film_ratings)))))
summary(lm_contrast)

boxplot(box_office~film_ratings, data=dat)


#--------------------------------------------------------------------------------------------
# Second data set
#--------------------------------------------------------------------------------------------

set.seed(2844463)

# Set the sample size
N = 10*5

# Simulate some film ratings
film_ratings = rep(c(1:5), each = N/5)

# Simulate box office figures based on a linear and quadratic relationship
box_office = 100-5*(3-film_ratings)^2 + rnorm(N,sd=5)

# Set the ratings as an ordered factor 
film_ratings = ordered(film_ratings)

# Create the data frame
dat = data.frame(box_office = box_office,film_ratings = film_ratings)
#write.csv(x = dat, file = "Lab 04 - Box_Office_2.csv")

# Run the linear model with the polynomial contrasts
lm_contrast = lm(box_office~film_ratings, data=dat, contrasts=list(film_ratings = contr.poly(5)))
summary(lm_contrast)

boxplot(box_office~film_ratings, data=dat)


#--------------------------------------------------------------------------------------------
# Third data set - Same as first but more data, more power!
#--------------------------------------------------------------------------------------------

set.seed(2844463)

# Set the sample size
N = 100*5

# Simulate some film ratings
film_ratings = rep(c(1:5), each = N/5)

# Simulate box office figures based on a linear and quadratic relationship
box_office = 40 + (film_ratings)^2 + rnorm(N,sd=5)

# Set the ratings as an ordered factor 
film_ratings = ordered(film_ratings)

# Create the data frame and write
dat = data.frame(box_office = box_office,film_ratings = film_ratings)
#write.csv(x = dat, file = "Lab 04 - Box_Office_3.csv", row.names = F)

# Run the linear model with the polynomial contrasts
lm_contrast = lm(box_office~film_ratings, data=dat, contrasts=list(film_ratings = contr.poly(length(levels(film_ratings)))))
summary(lm_contrast)

boxplot(box_office~film_ratings, data=dat)


