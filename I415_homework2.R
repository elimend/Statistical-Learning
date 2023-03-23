# Elizabeth Mendoza
# Homework 2: Data Wrangling

# The next few questions will deal with the mammal's sleeping habits dataset:
library(tidyverse)
data(msleep)

# Q1: Add some R code describing the dataset. Include some code showing the number
# of instances that contain missing data.
dim(msleep) # 83 rows and 11 columns
summary(msleep)
sum(is.na(msleep)) # Count of NA's in dataset
sum(is.na(msleep$vore)) #7 NA's in this feature
sum(is.na(msleep$conservation)) #29 Na's in this feature
sum(is.na(msleep$sleep_rem)) #22 NA's in this feature
sum(is.na(msleep$sleep_cycle)) #51 NA's in this feature
sum(is.na(msleep$brainwt)) #27 NA's in this feature

# Q2: Remove the instances with missing data to carry out the tasks that follow
# No information about replacing the data, so we will just completely remove 
# all unknowns within the dataset
msleep_no_na <- drop_na(msleep)
summary(msleep_no_na)
dim(msleep_no_na)
# Went from 83 rows to 20 rows - more than half the data has been removed
# due to NAs

# Q3: Print the correlation matrix of the numeric features
# Create a dataset containing only numeric features: sleep_total, sleep_rem, 
# sleep_cycle, awake, brainwt, and bodywt
numeric_cols <- select(msleep_no_na, sleep_total, sleep_rem, sleep_cycle, awake,
                       brainwt, bodywt)

# Simplest way is to use the cor function
numeric_cols.cor = cor(numeric_cols)
# This shows the correlation of each numeric column in a table (numeric_cols.cor)
view(numeric_cols.cor)

# Q4: For each of these, store the median value in a variable and use dplyr to
# summarize the requested results 
# Q4. A - How many animals' total sleep is over 10 hours?
over_10_sleep <- filter(numeric_cols, sleep_total > 10)
# Filter animals with more than 10 total sleep hours
summarise(over_10_sleep, count = n()) # Counts how many anaimals sleep over 10 hours: 12
median(over_10_sleep$sleep_total) # Median is 13.4 

# Q4. B - How many animals' brain weight is below the median value?
median(numeric_cols$brainwt) # Median brainwt is 0.0059
below_median_brainwt <- filter(numeric_cols, brainwt < median(numeric_cols$brainwt))
# Filters all the rows with brainwt below the median 0.0059
summarise(below_median_brainwt, count = n())
# Counts the number of animals' with brain weight below the median
summarise(below_median_brainwt, count = n(), below_median_brainwt$brainwt)
# Shows each value of brain weights under the median

# Q4. C - How many animals' brain weight and body weight is below the median 
# value for each respective feature?
median(numeric_cols$brainwt) # Median brainwt is 0.0059
median(numeric_cols$bodywt) # Median bodywt is 0.749

# The data together in one dataframe:
below_median_weight <- filter(numeric_cols, brainwt < median(numeric_cols$brainwt)
                                                             & bodywt < median(numeric_cols$bodywt))
summarise(below_median_weight, count = n()) # Counts the number of both body weight
# and brain weight below the median: 9

# Q5: Create a dataframe that shows all the geni in the dataframe. 
# For each of these geni, indicate how many instances belong in there and the
# median value of each's total sleep.
df_geni <- select(msleep, genus)
# 83 instances belong in this dataframe, geni for the animals (some share the same genus)
mutate(df_geni, median_sleep = median(msleep$sleep_total))
# Adds a row to the table showing the median sleep of each genus
# Only shows the table in the console?



# The next few questions will deal with the statlog heart dataset:
# Q1: Load the raw data directly into an R object from the UCI repo
heart <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/heart/heart.dat", header = F,
                  strip.white = T)

# Q2: Add a header row to the dataset with the feature names
names(heart) <- c("age", "sex", "cpt", "rbp", "serum", "fbs", "rer", "mhr", "eia",
                  "oldpeak", "slope", "nmaj", "thal", "class")

# Q3: Print a statistical summary of the dataset
summary(heart)
# Shows the minimum, Q1, median, mean, Q3, and maximum for each numeric feature.
# Some features include codes their values, i.e. sex 0 or 1 for each sex instead
# of using categorical data. Only some features utilize the full summary - i.e.
# maximum heart rate achieved or mhr, which includes data more than just codes. 


# Q4: Convert features 2, 3, 6, 7, 9, and 13 to factors with meaningful, human-readable
# labels. Have to use a codebook indicating what labels correspond to codes.
# First convert the feature 2 which includes 0 for female and 1 for male:
heart <- heart %>%
  mutate(sex = replace(sex, sex == 0, "female"))
heart <- heart %>%
  mutate(sex = replace(sex, sex == 1, "male"))
# Convert the labels in 3 about chest pain - which includes typical and 
# atypical angina, non-anginal pain, and asymptomatic
heart <- heart %>%
  mutate(cpt = replace(cpt, cpt == 1, "typ_angina"))
heart <- heart %>%
  mutate(cpt = replace(cpt, cpt == 2, "atyp_angina"))
heart <- heart %>%
  mutate(cpt = replace(cpt, cpt == 3, "non_angina_p"))
heart <- heart %>%
  mutate(cpt = replace(cpt, cpt == 4, "asympto"))
# Convert the labels in 6 which includes blood sugar being higher than 120mg/dl,
# 0 being false and 1 being true
heart <- heart %>%
  mutate(fbs = replace(fbs, fbs == 1, "true"))
heart <- heart %>%
  mutate(fbs = replace(fbs, fbs == 0, "false"))
# Convert the labels in 7 with results on electrocardiographic results - which
# includes 0 for normal, 1 for having wave abnormality, and 2 for ventricular
# hypertrophy
heart <- heart %>%
  mutate(rer = replace(rer, rer == 0, "normal"))
heart <- heart %>%
  mutate(rer = replace(rer, rer == 1, "wave_abnorm"))
heart <- heart %>%
  mutate(rer = replace(rer, rer == 2, "hypertrophy"))
# Convert the labels in 9 with results on exercise-induced angina - which includes
# 0 for no and 1 for yes
heart <- heart %>%
  mutate(eia = replace(eia, eia == 0, "no"))
heart <- heart %>%
  mutate(eia = replace(eia, eia == 1, "yes"))
# Convert the labels in 13 with results on the heart rate of the patient - which
# includes 3 for normal, 6 for fixed defect, and 7 for reversable defect
heart <- heart %>%
  mutate(thal = replace(thal, thal == 3, "normal"))
heart <- heart %>%
  mutate(thal = replace(thal, thal == 6, "fix_defect"))
heart <- heart %>%
  mutate(thal = replace(thal, thal == 7, "revers_defect"))

# Q5: For the remaining real-valued features, include a scatterplot matrix
# with the points colored by class.
# Real-attribute types: 1, 4, 5, 8, 10, and 12. Will work with these to find the
# scatterplot matrix
scatterplotm_heart <- select(heart, age, rbp, serum, mhr, oldpeak, nmaj)
pairs(scatterplotm_heart, col = heart$class)


# Q6: Include a correlation matrix for the real-valued features.
cor(scatterplotm_heart)


# Q7: Create a boxplot of rbp vs the chest pain types.
ggplot(data = heart) +
  geom_boxplot(mapping = aes(x = rbp, y = cpt))


# Q8: Create a visualization showing the relationship between rbp, a numerical
# feature, and sex and chest pain types, two nominal features. Comment
# on any observations. 
ggplot(data = heart) + 
  geom_point(mapping = aes(x = rbp, y = cpt, color = sex))
# Observation: For typical angina pain in female vs male patients according to
# rbp, more male patients are noted - x6 as many to the female patients. 
# Because there's many more males for this pain type, they overtake this category.
# Clumping takes place around 150 rbp. 
# For non anginal pain, clumping takes place around 120-135 rbp part. In this clumping
# part male patients are here more often, Although there are 2x as more male patients
# they present a clumping area, while the female patients are more spread out. 
# For atypical angina, clumping occurs around the 127-135 rbp with more male
# patients in this clumping area. In general there are almost the same amount of 
# male to female patients (slightly more male patients). 
# For asymptomatic, clumping occurs in a larger area that the other pains, 120-140 rbp
# with more male patients in this range and overall.
# Overall, male patients more likely were asymptomatic, and if not were more likely to
# have non-anginal pain. Female patients are less likely to experience typical anginal
# pain compared to other pain types. 


# Q9: Add two other visualizations that seem interesting for you to explore
# the dataset. Briefly describe why you arrived at creating them. Comment on
# any observations. 
# I wanted to look at age with blood pressure and fasting blood sugar, so I decided
# to create a dataframe to look at this with the ages grouped in ranges
heart_age <- select(heart, age , rbp, fbs)
heart_age <- filter(heart, age > 35 & age < 50)
# I decided to go with the age range 35-50, since we are more likely to see 
# heart problems to related to the heart start to develop in this range.
ggplot(data = heart_age) + 
  geom_point(mapping = aes(x = rbp, y = fbs, color = age))
# In this visualization, fbs > 120 is shown as true and below 120 is shown as false
# against rbp. Ages are shown in different point sizes. Many patients are shown
# with fasting blood sugar below 120, with the highest resting blood pressure being
# around 155. There are only 5 patients with fasting blood sugar above 120, with
# most of the data being clumped near 120-132rbp. Of these 5 patients, they range
# in age from 40.0-47.5, with 2 being on the 40.0 age and 2 being 47.5 age.
# There are not distinct conclusions to make from the ones who have higher than
# 120fbs since they all range in different ages, as other features mentioned in the
# heart dataset maybe factors on their higher fbs/rbp. 

# I also wanted to look at serum, class, and rer. I wanted to look at any possible 
# correlations between people with heart disease and how their information
# is either higher/lower/about the same with people without heart disease. I also
# found that doctors looked at electrocardiographic results first when they
# suspect heart diseases.
ggplot(data = heart) + 
  geom_point(mapping = aes(x = serum, y = rer, color = class))
# With this observation, we see that not a lot of patients had wave abnormalities
# with their ecg result, and out of the two, one had heart disease and the other didn't.
# For normal ecg results, we see variation of those with and without heart disease,
# however there seems to be more of those without heart disease (dark blue). 
# The dark blue (absence of heart disease) also seems to clump around the 190-270
# serum region more, compared to those with heart disease (light blue) being
# more towards 250+ serum range. 
# For hypertrophy  there seems to be light blue (presence of heart disease) rather
# than those without heart disease (dark blue). There is a general clumping of
# every color point in the 200-310 serum range, which is higher than the normal ecg
# result. Even those without heart disease have higher serum results than those
# with normal ecg results. Hypertrophy result also has more "outliers" than the
# normal result. 

