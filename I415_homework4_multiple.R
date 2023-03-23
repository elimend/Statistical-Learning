# Elizabeth Mendoza
# Lab 4: Regression Part 1
# Multiple Linear Regression

# This script will be incorporating the multiple linear regression questions only!


# Q1: Read in the dataset, and assign descriptive names to the columns:
housing <- read.table("cadata.dat", header = F,
                  strip.white = T)
names(housing) <- c("med_houseval", "med_income", "house_medage", "total_rooms",
                    "total_beds", "population", "households", "latitude",
                    "longitude")


# Q2: Create a scatterplot matrix for the dataset. Which predictors appear to be
# most highly correlated with the response? Which predictors appear to most highly
# correlated with each other? Is there any visual evidence of outliers?
pairs(housing)
# Median house value and median income, the graph appears to be straight with
# lots of clumping in one area of the graph, however it's a big area of the graph that's clumped. 
# Total rooms and total beds appear to be very correlated according to the straight line,
# the data looks to be very clumped together similar to how a regression line would fit.
# Total rooms and population appear to look correlated, although the graph shows
# small curving with less data points shown. Total beds and population look similar
# to the total rooms and population, more "outliers" shown though. 
# Households and population shown very similar to population and total beds graph.
# Longitude and latitude showcase a straight negative line.
# Graphs incorporating population appear to have outliers as they are away from 
# the main clump of data. 



# Q3: Show the correlation matrix. Does its entries confirm your assumptions from
# the scatterplot matrix? Are there any other pairs of features with significant
# correlations? 
cor(housing)
# The features which appear to be highly correlated with each other: 
# median house value and median income; total rooms and total bedrooms; total rooms
# and households; total rooms and population; total bedrooms and population;
# total bedrooms and households; population and households; latitude and longitude.
# All of these are 0.7 correlation and above. 
# Many of the graphs have data clustered together with a few data points outside of
# the clumping, however many of these graphs don't have "true" outliers, as they
# are still close to the data.
# Comparing my assumptions from the scatterplot matrix, I did find almost all
# correlations, however the correlation matrix showcases how significant these
# relationships are better.



# Q4: Create a linear fit of median house price against all other features.
fit_housing <- lm(med_houseval ~., data = housing)


# Q5: Is there a relationship between at least one predictor and the response?
summary(fit_housing)
# Yes, there is a relationship with all the predictors, although some are
# more significant or stronger than others. All predictors are shown to be
# highly significant (***), with almost all being the same p-value except for one,
# which has a lower p-value than the rest. 


# Q6: Which predictors appear to have a statistically significant relationship
# to the reponse. Explain your reasoning and quanitfy the relationship(s).
# Median income, house median age, latitude, and longitude each have
# significant relationships to the response. Each predictor are at least 2 away
# in terms of Standard error, their t-values are significantly different than 0 
# (high above 0), and all have p-values which are significantly different than 0.
# Other predictors weren't chosen due to not enough significance with either
# standard error, t-value, or p-value.


# Q7: What fraction of the total variance in the response is accounted for by this
# linear model?
# R-squared in the linear model is used to account for the fraction of variance
# In the fit_housing linear model, (median housing value against all other features)
# is 0.6371.


# Q8: What median housing price is predicted for the instance below? What
# are the confidence intervals and prediction intervals for the predicted response?
# I wasn't too sure what to do to with median house price/value and the 
# instances/decimals listed below so I used $89,400 listed with the question:
predicted_housing = subset(housing, med_houseval == 89400)

predict(fit_housing, newdata = predicted_housing, interval = "confidence")
# There are 10 instances where median house value is 89000, depending on 
# the fit, there are different lowers and uppers.
predict(fit_housing, newdata = predicted_housing, interval = "prediction")
# There are 10 instances where median house value is 89000, depending on 
# the fit, there are different lowers and uppers.
