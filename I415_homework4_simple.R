# Elizabeth Mendoza
# Lab 4: Regression Part 1
# Simple Linear Regression

# This script will be incorporating the simple linear regression questions only!
# Another script will be created for the multiple linear regression questions.

# Load in the dataset:
library(tidyverse)
install.packages("ISwR")
library(ISwR)
data(rmr)

# Q1: Use lm() to create a linear fit of body weight by resting metabolic rate. Show
# a scatterplot of the data including the regression line from the fit.
# Y is porportional to X, so metabolic rate goes first (acts as Y)
fit_rmr <- lm(rmr$metabolic.rate ~ rmr$body.weight, data = rmr)
# Plot a scatterplot of the data
plot(rmr$body.weight, rmr$metabolic.rate,
     main = "Body Weight by Metabolic Rate",
     xlab = "Body Weight", ylab = "Metabolic Rate")
# Add a regression line
abline(lm(rmr$metabolic.rate ~ rmr$body.weight, data = rmr), col = "red")


# Q2: Is there a relationship between body weight and resting metabolic rate?
# Be sure to explain your reasoning and quantify the strength of the relationship,
# if any. 
summary(fit_rmr)
# F-statistic is greater than 1, indicating there is a relationship between the
# two. The p-value is also significantly different than 0, indicating there is
# a strong correlation between the two features.Intercept is also a large number.


# Q3: What are the two coefficents in this model, including the 95% confidence 
# interval for each? 
# The two coefficents in this model are body weight and metabolic rate.
# To get the 95% confidence interval for each, we look at the fit:
summary(fit_rmr)
# In this we see Metabolic Rate = 811.2267 + 7.0595*(Body Weight)

# Calculate the confidence interval for each coefficient:
confint(fit_rmr, "rmr$body.weight", level = 0.95)
# The 95% confidence interval for body weight is [5.086656, 9.0324]

confint(fit_rmr, "(Intercept)", level = 0.95)
# The 95% confidence interval for metabolic rate is [655.8838, 966.5695]


# Q4: What is the predicted body weight for a resting metabolic rate of 1400? 
# What are the confidence and prediction intervals for this prediction?
# Create a new data frame for body weight and metabolic rate at 1400
predicted = data.frame(metabolic.rate = 1400)
predicted1 <- select(filter(rmr, metabolic.rate == 1400), body.weight, metabolic.rate)

predict(fit_rmr, newdata = predicted1, interval = "prediction")
predict(fit_rmr, newdata = predicted, interval = "prediction")
# Prediction interval is [1077.2867, 1722.696]

predict(fit_rmr, newdata = predicted, interval = "confidence")
# Confidence interval is [1349.095, 1450.888]

# Q5: In your own words, what's the difference between the two intervals you
# found in Q4. Why is the confidence interval always smaller than the 
# prediction interval?
# The confidence interval predicts the mean value and in terms above, the
# confidence interval is smaller than the prediction interval. The prediction
# interval predicts an individual number, and has a bigger interval between the
# lower and higher. 
# Prediction intervals account for the uncertainty in estimating and also
# the random variation of the values, so there's more for the prediction
# interval to process/display.