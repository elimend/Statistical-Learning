# Elizabeth Mendoza
# Lab 2: Data Exploration

# The next few questions will be over the wine quality data:
# Q1: Provide a summary of each feature in the dataset
summary(winequality.white)
# The summary function allows a user to see the minimum, Q1, median, mean,
# Q3, and maximum for each column within the dataframe.
# Includes features such as fixed acidity, volatile acidity, citric acid, residual sugar,
# chlorides, free sulfur dioxide, total sulfur dioxide, density, pH, sulphates,
# alcohol, and quality.

# Q2: Pass the dataset to cor() to view the correlation matrix
cor(winequality.white)
# Passing the cor function allows the user to see correlation between features
# Correlation will be regarded as highly correlated when the r value is larger
# than 0.7 between functions

# The most highly-correlated functions are: 
# Residual sugar against density (positive), density against alcohol (negative)
# Many features had higher correlations - between 4-6 but didn't quite 
# make the highly correlated qualifactions - moderate correlations

# Q3: For the most highly-correlated pair of features, use ggplot to create a
# scatterplot that also containS a smoothing line

# Scatterplot with smoothing line for correlated pairs Residual sugar x density
ggplot(data = winequality.white) + 
  geom_point(mapping = aes(x = residual.sugar, y = density)) +
  geom_smooth(mapping = aes(x = residual.sugar, y = density))
# This scatterplot shows most of the data clumped together, mostly between 0.00 - 1.00
# density, and 0-20 for residual sugar, however there are a few points outside this area
# The smoothing line is positive, with the data points being close to the line

# Scatterplot with smoothing line for correlated pairs density against alcohol
ggplot(data = winequality.white) + 
  geom_point(mapping = aes(x = density, y = alcohol)) +
  geom_smooth(mapping = aes(x = density, y = alcohol))

ggplot(data = winequality.white) + 
  geom_point(mapping = aes(x = alcohol, y = density)) +
  geom_smooth(mapping = aes(x = alcohol, y = density))

# I did two separate scatterplots, the first with density on x and alcohol on y -
# This results in points clumping in one area but also looks like the points "merge"
# together making it look like alcohol has a range. The smoothing line goes through
# the data downwards but makes a curve different once outside of the data points

# The second scatterplot has density on y and alcohol and x, and is easier to read
# The points are laid out in a more uniform pattern as alcohol is on the x axis,
# The line is also quite different from the first scatterplot, with the line
# going through all the data with no curve this time - doesn't show as much the
# negative correlation that was shown with the r-value on Q2, however does
# go down slightly


# The next few questions will be over the Cars93 dataset: 
install.packages("MASS")
library(MASS)
data(Cars93)

# Q1: A statistical summary of the dataset
summary(Cars93)
# In this summary instead of minimum, Q1, median, etc. for some functions there are 
# the number of different types and how many there are within the set, i.e. manufacturer
# This is because they are nominal or use non-numbers so when looking at a summary, 
# that function or column shows the amount of that factor
# Otherwise numerical features like price, mpg, engine size, etc. have the minimum,
# Q1, median, mean, Q3, and maximum. 

# Q2: Using ggplot, create a visualization that gives you qualitative information
# about whether the price depends on the type of car. 
ggplot(data = Cars93) + 
  geom_boxplot(mapping = aes(x = Type, y = Price))

ggplot(data = Cars93) +
  geom_point(mapping = aes(x = Type, y = Price))
# Comparing a boxplot and scatterplot, a scatterplot is easier to read and points 
# are easier to see and evaluate - shows van types usually of a price clumped within
# 20k-25k; Sporty type is more distributed with price ranged from 10k-39k, most of
# the data is clumped in the 15k-20k range; Small type if very clumped in the before 20k range;
# Midsize type is very distributed where points clumped in multiple spots; 
# Large type doesn't have a lot of points and not a lot of clumping; Compact type
# has more data points than large but there is no clumping 
# Tried using the ggpairs on Cars93 to see correlation with different scatterplots,
# however some of the columns have more levels than the allowed threshold.

# Q3: Using ggplot, create a visualization that gives you qualitative information
# about whether the highway mpg depends on the type of car. 
ggplot(data = Cars93) + 
  geom_boxplot(mapping = aes(x = Type, y = MPG.highway))

ggplot(data = Cars93) +
  geom_point(mapping = aes(x = Type, y = MPG.highway))
# In the scatterplot for all types their data points are distributed without a lot
# of clumping in one area. Some types have overlapping with others, but for the most
# part each type has their own price range you can see - Compact is 25-33mpg;
# Large is very small, not a lot of points 25-28mpg; Midsize is 22-32mpg; 
# Small has the most data points and is 28-50mpg; Sporty also has some more data points
# with distribution over 23-36mpg; and van has not a lot of data points as well 
# with most of the data congregated in the 20-24mpg.
# In the boxplot, Large and Small type don't have a median or line in the middle, 
# although the bold lines looks to be at the bottom box line indicating most of the
# data is situated in Q3. Other types have some distribution - whether data situates
# more towards Q1 or Q3. 

# Q4: Using ggplot, create a visualization that gives you qualitative information
# about whether highway mpg depends on the price of car. 
ggplot(data = Cars93) + 
  geom_boxplot(mapping = aes(x = Price, y = MPG.highway))

ggplot(data = Cars93) +
  geom_point(mapping = aes(x = Price, y = MPG.highway))

ggplot(data = Cars93) +
  geom_violin(mapping = aes(x = Price, y = MPG.highway))
# The boxplot and violin was easier for me see and understand the data in this case
# vs. the scatterplot. With the scatterplot there is a clear definition where the data
# points are more clumped together, however there is distribution. Looking at violin,
# we can see that most of the data is skewed towards one end rather than more uniform.
# The violin is expanded within the 20-30mpg area and in this price is represented 
# with all price points 10-60k. The data looks like a funnel (less data) going  upwards in mpg. 
# Looking at the boxplot the box, which hosts Q1, median, and Q3 information, is situated
# at the 26-31 range on mpg but hosts all the price points. The data within the box
# is slightly skewed towards the Q3 line, with more data being above the median line
# of around 27mpg. The boxplot shows more clearly outliers as shown by the dots outside
# of the box and whiskers. 

# Q5: Using ggplot, create a visualization that gives you qualitative information about
# how the highway mpg depends on the type and origin of the car. 
ggplot(data = Cars93) +
  geom_point(mapping = aes(x = Type, y = MPG.highway, color = Origin))

ggplot(data = Cars93) +
  geom_point(mapping = aes(x = Origin, y = MPG.highway, color = Type))
# The first scatterplot shows the type of car with mpg highway with different
# colors for the origin of the car. This shows the user in each category of car type
# which are USA or non-USA origin. It's also an easier way to see type against mpg
# in regards to how much mpg car type uses. Regarding the data shown, non-USA types
# tend to have higher mpg in small and sporty types of cars. For van, midsize, and 
# compact it's harder to analyze the data since not as many data points exist and
# the data is skewed towards one origin. For compact, midsize, and van types non-USA
# origin has more data so it seems where they lie means that's the mpg of the type
# by origin. In compact type, it can't be determined which origin has better mpg
# although USA has better for the three data points it has. For midsize, non-USA
# origin types appear to have lower mpg compared to USA origin types. For van, non-USA
# origin types appear to have better mpg, however data is small for that car type.
# For large cars, USA has the better mpg.
# For the second scatterplot, this is just another way for the user to see where each
# car type falls in regards to whether they're USA or non-USA made and how much mpg
# that car type has. 


# The next few questions will be over the Housing dataset:
data(housing)
# Q1: A statistical summary of the dataset
summary(housing)
# This dataset is much smaller than the Cars93 dataset, in that it only has five
# functions or columns. 4/5 functions are factors in which they are non-numerical.
# The only numerical feature is the frequency. One thing to note about this dataset
# from just looking at the summary that is that with the nominal data for each column
# they are all even, so there is a balanced distribution.

# Q2: Using ggplot, create a visualization that gives you qualitative information
# about the satisfaction of house dwellers based on the type of housing and the
# contact residents have with other residents
ggplot(data = housing) +
  geom_bar(mapping = aes(x = Type, fill = Sat), position = "dodge") +
  xlab("Housing Type") + ylab("Cont")
# Using a grouped bar graph to show the different housing types and bars for 
# satisfaction. Contact was "turned" into a numerical value since it is only
# defined by low or high, and in the summary of the dataset we learned that each
# had the same amount: 36. We also know that each satisfaction degree has the same
# number as well: 24. Housing type also had the same for each one as well: 18. 
# With this we can plot Housing Type on the bar with satisfaction as colors. 