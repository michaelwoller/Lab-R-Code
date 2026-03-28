# ====================== Prediction ======================

# Author: Michael Woller

# ====================== Load Packages ======================

#install.packages("dplyr")    # needed for data management
#install.packages("ggplot2")  # needed for plotting
#install.packages("leaps")    # needed for model selection
#install.packages("glmnet")   # needed for LASSO and Ridge regression

library(dplyr)
library(ggplot2)
library(leaps) 
library(glmnet)

# Remember to load in PROCESS. 

# ---------------------- Specify File Path ----------------------

filelocation <- "/Users/woller/Documents/250c 2026/Lab 5/Big5data.csv" # wherever on your computer
big5 <- read.csv(filelocation, header = TRUE)

# ====================== cross-validation and Shrunken R ======================

# 1.1 cross-validation and Shrunken R 

# For predictive models, we are often less concerned with how well the model fits
# the data we already have, and more concerned with how well it will perform on
# new, unseen data. A model can fit the current dataset very well simply by
# capturing noise (overfitting), but this does not guarantee it will generalize 
# to future datasets.

# Cross-validation is a method for evaluating how well a model is likely to perform
# in predicting new data. It involves splitting the data into two parts:
# 1) a training set, used to fit the model
# 2) a testing (or hold-out) set, used to evaluate the model's predictive accuracy

# The model is fit on the training data, and then used to predict outcomes in the
# testing data. The relationship between the predicted and actual values in the
# testing set provides a more realistic estimate of model performance.

# This typically results in a R^2 compared to the original model,
# because the model is no longer being evaluated on the same data it was trained on.
# This reduced value is often referred to as the "shrunken" R or R^2. It gives
# the relationship between the observed and predicted data. 

# For the purpose of this lab, I want to see how well the Big 5 items can 
# predict scores of existential well being, and what model of the Big 5 items
# is the most optimal for prediction. 

# 1.2 Splitting Training/Testing Data

# The first thing you need to do is set your seed.
# This is a VERY important step, as you NEED to be able to replicate with the 
# exact data splits. Your choice of seed doesn't matter.
set.seed(8675309)

# Next is to split the data. Typically, its common to leave 10-20% of the data
# as "hold out" and 80-90% of the data as training data. However, for the sake
# of this example (and for the sake of demonstrating double cross calidation),
# I will split the data into halves.

n <- nrow(big5)
# create a random index of which rows will be in the training data
train_index <- sample(1:n, size = floor(0.5 * n))

train_data <- big5[train_index, ] # subsets big5 training data
test_data  <- big5[-train_index, ] # subsets big5 testing data 
# (Note for new R users, '-train_index' means all rows that DONT include 
# these values)

# Note, the results of your validation and shrunken R's will differ for 
# simple, double, and k-fold cross-validation depending on your seed.

# Leave-one-out cross-validation will be insensitive to the seed
# (as you will see).

# ---------------------- Simple cross-validation ----------------------

# 1.3 Simple cross-validation

# This method simply trains a model with the training data, then tries to predict
# the testing data by funneling the testing data into the trained model. 
# The closer the model's predictions are to the true testing data, the better
# the model is. We will use shrunken R to determine this. 

# First fit your training model with the training data 
training_fit <- lm(ExistentialWellBeing ~ OpenMindedness + Conscientiousness 
                  + Extraversion + Agreeableness + NegativeEmotionality,
                   data = train_data)

# Predict the trained model on the testing data and save the predictions.
# Use "newdata" to plug in the testing data.
tested <- predict(training_fit, newdata = test_data)

# Next is to get Shrunken R, Shrunken R^2, out-of-sample R^2, and RMSE. 

# We get the Shrunken R by finding the correlation of the original left-out 
# testing data and the predicted scores from the testing data.
shrunkenR <- cor(test_data$ExistentialWellBeing, tested, use="complete.obs")
shrunkenR

# The interpretation is that our model’s predictions have a moderate positive 
# correlation (~0.60) with actual existential well being in new data.

# To get Shrunken R^2, simply square the shrunken R. It is a measure of how 
# strongly predictions are with their actual values. This statistic asks
# "Are my predictions actually close to the true values?"
shrunkenR2 <- shrunkenR^2
shrunkenR2

# The interpretation is that about 36% of the variance in existential well being 
# is associated with the model’s predictions in new data.

# Out-of-sample R^2 
# This statistic shows what happens you took this model to a new sample from 
# the same population, how much  variance in Y would it explain? This is another 
# R^2 statistic for prediction, and is generally preferred. It is calculated via 
# the sum of squares as opposed to the correlation like shrunken-R^2.
# It asks "Are my predictions actually close to the true values?"
SSE <- sum((test_data$ExistentialWellBeing - tested)^2)
SST <- sum((test_data$ExistentialWellBeing - mean(test_data$ExistentialWellBeing))^2)
R2_oos <- 1 - SSE/SST
R2_oos

# The interpretation is the model explains about 36% of the variance in 
# existential well being in the test dataset.

# RMSE
# This is a measure of how much variance this model minimized). It is very common
# in predictive statistics and upper level statistics, though we won't talk about
# it this course.
rmse <- sqrt(mean((test_data$ExistentialWellBeing - tested)^2))
rmse

# The interpretation is that, on average, the model’s predictions are off by
# about 0.59 units of existential well being.

# ---------------------- Double cross-validation ----------------------

# 1.3 Double cross-validation

# Double cross-validation is the same thing as single cross-validation, but you
# train two models and test them on the data used to train the other model.

# Step 1 is to keep the results from the 50/50 split above 
# (so you don't have to repeat what we've done)

# Step 2 is to repeat the above process, but now swap what half of the data 
# you train and test on

# First fit your training model with the training data 
training_fit <- lm(ExistentialWellBeing ~ OpenMindedness + Conscientiousness 
                   + Extraversion + Agreeableness + NegativeEmotionality,
                   data = train_data)

# Fit the second model with the the testing data
second_fit <- lm(ExistentialWellBeing ~ OpenMindedness + Conscientiousness + Extraversion + 
                   Agreeableness + NegativeEmotionality,
                 data = test_data)

# You now have two trained models. 

# Predict the first trained model on the testing data and save the predictions.
tested <- predict(training_fit, newdata = test_data)

# Predict the second trained model on the original training data and save predictions.
tested2 <- predict(second_fit, newdata = train_data)

# Get the Shrunken R by finding the correlations by correlating the
# predictions of the models to the data they were trying to predict.

# Shrunken R for model 1
shrunkenR <- cor(test_data$ExistentialWellBeing, tested, use="complete.obs")

# Shrunken R for model 2
shrunkenR2 <- cor(train_data$ExistentialWellBeing, tested2, use="complete.obs")

# Get average shrunken R between two models. This is the final Shrunken R
shrunkenR_dcv <- mean(shrunken, shrunken2)
shrunkenR_dcv

# Shrunken R^2
shrunkenR_dcv^2

# Interpretations follow what was shown in Section 1.3.

# I will skip out on the LOO-R^2 and the RMSE. But basically, you need to calculate
# two of them and then take the average, similar to what you did with shrunken R.

# ---------------------- K-Fold cross-validation ----------------------

# 1.4 K-Fold  cross-validation

# K-fold cross-validation is double the same thing as double cross-validation, 
# but instead of doing it twice, you do it a "k" number of times, where k is some
# arbitrary numbers. Typically k = 10, as there is diminishing returns of validation
# when it gets much higher. This is probably the most common form of validation
# for regression.

# Define how many folds (we'll just use k = 5 for simplicity).
k <- 5 
N <- nrow(big5) # How large the sample is
nk <- N/k # How many participants are in each fold

# Insert fold indices into data.
big5$fold <- rep(1:k, times = nk+1)[1:N] 
# If N is not divisible by k, "times = nk+1" fixes that

# The next step is to run a 'for' loop that will solve for each fold and 
# calculate the Shrunken R for each fold. Basically, it is doing the steps we did
# in double cross-validation, but it will do it k number of times. The loop
# just does it automatically.

# Makes an empty list to be filled in
shrunkenRCV_kfold <- vector(mode = "numeric", length = k) 

set.seed(8675309) # Sets seed for train vs test split

# If you want to understand this more, you should learn more about how to write
# loops in R. It is a bit too complex for now, so I am going to hand wave it away.

for(i in 1:k){
  train <- big5[big5$fold != i,]                # training data without fold i
  test <- big5[big5$fold == i,]                 # testing data of fold i
  modeltest <- lm(ExistentialWellBeing ~ OpenMindedness 
                  + Conscientiousness + Extraversion 
                  + Agreeableness + NegativeEmotionality,
                  data = train)                  # trains model
  Ypredcv <- predict(modeltest, newdata = test)  # fits test data
  # calculate and save shrunken R
  shrunkenRCV_kfold[i] <- cor(Ypredcv, test$ExistentialWellBeing) 
}
# Find the average shrunken for k number of folds
shrunkenR_kfold <- mean(shrunkenRCV_kfold) 
shrunkenR_kfold

# Shrunken R^2
shrunkenR_kfold^2

# Interpretations follow what was shown in Section 1.3.

# I will skip out on the LOO-R^2 and the RMSE. But basically, you need to calculate
# k of them and then take the average, similar to what you did with shrunken R.
# This would just go in the for loop too.

# ---------------------- Leave-One-Out Validation ----------------------

# 1.5 Leave-One-Out Validation

# Leave-one-out (LOO) validation evaluates a model by using all but one observation
# to fit the model, then testing it on the single left-out observation, repeating
# this process for every data point in the dataset.

# For each iteration:
# - we leave out one data point
# - fit the model on the remaining N - 1 observations
# - use the model to predict the left-out observation
# - record how accurate that prediction is

# This process is repeated for every observation in the dataset, so each data
# point gets a turn as the "test case."

# The results are then averaged to give an overall measure of how well the model
# is expected to perform on new data.

# In simple terms:
# “Train on almost all the data, test on one point, and repeat this for every point.”

# Since we are testing on every single point, we don't need to run a seed.
# So we start by estimating the model on all data.

full_model <- lm(ExistentialWellBeing ~ OpenMindedness 
                 + Conscientiousness + Extraversion  
                +  Agreeableness + NegativeEmotionality,
                 data = big5) # fit model with all the data

# There's a few ways of doing the next steps. The easiest way is to use the
# residuals and hat values to calculate the leave one out predictions manually.

# Save the residuals
e <- resid(full_model) 

# Save the hat values (these are a measure of leverage that is somewhat 
# complex to explain in this lab)
h <- hatvalues(full_model) 

# Get LOO predicted value for each case i
yhat_loo <- big5$ExistentialWellBeing - e / (1 - h) 

# This is how much the model’s prediction for case i changes when case 
# i is removed from model fitting.

# Finally, we calculate the LOO Shrunken R by correlating the full data
# to these leave-one-out predicted Y's
shrunkenR_LOO <- cor(big5$ExistentialWellBeing, yhat_loo, use="complete.obs")
shrunkenR_LOO

# This is how much the model’s prediction for case i changes when case 
# i is removed from model fitting

# Get the LOO Shrunken R^2
shrunkenR_LOO^2

# Interpretations follow what was shown in Section 1.3.

# ---------------------- PROCESS LOO and Browne Equation ----------------------

# 1.6 Validation in PROCESS

# PROCESS comes built in with a couple of validation methods. All you need to 
# do is input "crossv = 1" into the model options.

process(data = big5, x = c("OpenMindedness", "Conscientiousness", "Extraversion",
                           "Agreeableness", "NegativeEmotionality"),
        y = c("ExistentialWellBeing"), 
        crossv = 1) # add this to get cross-validation statistics

# Look at the table "Shrunken R estimates" near the top. Here you can see three
# shrunken R estimates. The "Browne" equation estimate (see next subsection),
# "LvOut1" is the LOO estimate that we found in Section 1.5, and "LvOut2" is
# The leave-two-out shrunken R. This isn't really that commonly used, but it
# follows the same thought process as leave-one-out, except you are testing 
# on every two data values instead of one. 

# If you want the shrunken R^2 or other validity estimates, you will have to
# find those manually.

# 1.7 Browne Equation

# The Browne equation is an analytically method of calculating shrunken R. Basically,
# instead of calculating shrunken R by correlating predicted scores to observed
# scores, it finds it via a mathematical equation. However, the Browne equation
# is never used, so we won't go over it in detail. It's mainly a relic of a 
# bygone era. It becomes more inaccurate when sample size becomes lower,
# the number of predictors is high, and there is a lot of multicolinearity. 
# But since the number of predictors and multicolinearity is often high when 
# doing predictive statistics, it is usually not very helpful.

# If you want to get this easily, you can take it from the Srunken R estimate
# table given as an output in PROCESS. 

# ====================== Variable Selection ======================

# 2.1 Variable Selection

# Since the purpose of prediction is to have the most accurate predictions, the
# models need to focus on minimizing error variance amongst predictions 
# rather than getting unbiased estimates. 

# Unbiased estimates -> Valid inference of the population
# Minimized error variance -> More accurate and consistent predictions of samples

# Unfortunately, when doing statistics, you have to focus on either biasness
# or minimized error variance. If you want to know why, I would recommend
# reading more about the infamous "variance/bias trade off" in statistics.

# Since prediction focuses on minimizing error variance, we want to construct our
# model to be most optimal for this issue. This requires a different process than
# constructing a model with inference for the population in mind.

# The process of minimizing error variance requires techniques called variable
# selection, which involves step-by-step algorithmic techniques to best select 
# from a list of variables, which ones are best suited for minimizing error 
# variance.

# To those unfamiliar, all an "algorithm" is is just a step-by-step guide to 
# get a specific outcome. So following an algorithm will always get you the end 
# result you want. You can do algorithms by hand (which we will shortly), but
# once algorithms start getting more complicated, it gets harder to understand
# the intermediate steps within them. Machine learning are just automated
# algorithms to figure out how to minimize error variance. Some machine learning
# algorithms are more "black box" than others, which means that the intermediate
# steps that the algorithms use are unknown to us.

# An example of a non-black box automated algorithm is Ridge or LASSO regression,
# which we will discuss more later.

# An example of a more black boxy algorithm would be gradient tree boosting, which
# is a more complicated machine learning technique. If you want to learn more about 
# that, learn how to use XGBoost or CatBoost algorithms. 

# For now, we will focus on the simplest algorithms of variable selection. The 
# following methods are very out of date and should not really be used. Though,
# you may find that they surprisingly are still used in many fields when they 
# shouldn't be. So just keep in mind better options exist. 

# In psychology, you could most likely get away with using Ridge regression in
# most cases, unless you have a ton of data and complicated variables. But more
# complicated machine learning algorithms aren't as useful in psychology as other
# fields due to our limited data and our focus on inferential models rather
# than predictive models. But this is changing every year.

# ---------------------- Forward Selection ----------------------

# 2.2 Forward Selection

# Forward selection is the process of starting with no predictors in the model
# and adding them one at a time based on how much they improve model fit. At each
# step, the predictor that provides the largest improvement (e.g., biggest increase
# in R^2 or most significant p-value) is added, and the process continues until no
# remaining variables meaningfully improve the model. This approach builds the model
# gradually, but it can miss important combinations of predictors and may capitalize
# on chance, especially in smaller samples.

# You can do this with many types of fit indices (like AIC or adjusted-R^2), but
# we will be using regular R^2.

# We will be using the add1() function.

# First specify the empty model.

modelnone <- lm(ExistentialWellBeing ~ 1, data = big5) 
# NOTE: ~1 means only the intercept is in the model

# Add all the variables to the model to see which ones have a significance 
# delta-R^2 F test. Include the "test = F" option to make sure the 
# delta-R^2 F test is done.

add1(modelnone, scope = ~OpenMindedness + Conscientiousness + Extraversion + 
       Agreeableness + NegativeEmotionality, test = "F") 

# Select the variable with the highest F/lowest p based on the previous output.
# In this case, it is NegativeEmotionality. To add it to the model, the next
# section we will use the update() function. MAKE SURE that the variables you add
# in update() have a + infront of them (e.g. +NegativeEmotionality). This lets
# the function know that we are updating the empty model by adding a new predictor.

# Include every variable in the "scope =" argument.

add1(update(modelnone, ~.+NegativeEmotionality), 
     scope = ~OpenMindedness + Conscientiousness + Extraversion
            +  Agreeableness + NegativeEmotionality, test = "F")
# This output will calculate the delta-R^2 for adding in the other variables to 
# a model with just NegativeEmotionality. We can see that the the next most 
# significant value is Conscientiousness. So we will add this variable using the
# same steps as above (make sure to add a + infront of it)

add1(update(modelnone, ~.+NegativeEmotionality+Conscientiousness), 
     scope = ~OpenMindedness + Conscientiousness + Extraversion 
     + Agreeableness + NegativeEmotionality, test = "F")


# Repeat the process of adding the variable with highest F/lowest p until 
# no predictors left to be added are significant.

add1(update(modelnone, ~.+NegativeEmotionality+Conscientiousness), 
     scope = ~OpenMindedness + Conscientiousness + Extraversion
     + Agreeableness + NegativeEmotionality, test = "F")
# Aggreableness is significant so we will add that.

add1(update(modelnone, ~.+NegativeEmotionality+Conscientiousness+Agreeableness), 
     scope = ~OpenMindedness + Conscientiousness + Extraversion 
     + Agreeableness + NegativeEmotionality, test = "F")

# No significant variables are left to add, so the end model from 
# forward selection is:
# ExistentialWellBeing ~ NegativeEmotionality + Conscientiousness + Agreeableness

# ---------------------- Backwards Selection ----------------------

# 2.3 Backwards Selection

# Backwards selection is a variable selection technique where you start with the
# full model, including all predictors of interest, and then remove predictors one
# at a time. At each step, the least useful variable (often the one with the largest
# p-value or weakest contribution to model fit) is dropped, and the model is refit.
# This process continues until all remaining predictors meaningfully contribute to
# the model based on a chosen criterion (e.g., R^2, AIC, or adjusted-R^2). The
# goal is to create a simpler, more interpretable model that still explains the
# outcome well without including unnecessary predictors.

# This is probably the "best" of the simple variable selection methods. However,
# it is still not recommended to use this method these days. 

# We will be using R^2 and doing delta-R^2 tests for our selection criteria.

# First specify the full model
full_model <- lm(ExistentialWellBeing ~ OpenMindedness + Conscientiousness 
                 + Extraversion + Agreeableness + NegativeEmotionality,
                 data = big5) 

# Then we drop each variable and find which ones with the lowest F/highest p
# using the drop1() function. Use the "type = F" argument to make sure we are
# doing a delta-R^2 test.

drop1(full_model, test = "F")

# Based on this output, we can see that OpenMindedness has the highest p value, 
# so it will be dropped from the model. We will use the update() function within
# the drop1() function to drop OpenMindedness. MAKE SURE to add a "-" in front of
# the variables you want to drop. The - sign lets the update() function know we
# are updating the function by removing OpenMindedness.

drop1(update(full_model, ~ . -OpenMindedness), test = "F")

# Based on the p values here, we should drop Extraversion, and continue on.

drop1(update(full_model, ~ . -OpenMindedness -Extraversion), test = "F")

# Now all variables are "significant" so we stop and end with:
# ExistentialWellBeing ~ NegativeEmotionality + Conscientiousness + Agreeableness

# ---------------------- Stepwise Selection ----------------------

# 2.4 Stepwise Selection

# Stepwise selection, or more specifically bidirectional (both-direction) stepwise
# selection, is a variable selection procedure that combines forward selection and
# backward selection The process begins similarly to forward selection by adding
# predictors to the model one at a time based on their contribution to model fit.
# However, after each addition, the procedure also checks whether any predictors
# already in the model have become unnecessary and should be removed. This means
# variables can both enter and leave the model at different steps. The process
# continues iteratively until no predictors meet the criteria for entry or removal
# (often based on p-values, AIC, or another fit statistic). The goal is to balance
# model fit and simplicity by allowing more flexibility than strictly forward or
# backward approaches.

# First start with an empty model.

modelnone <- lm(ExistentialWellBeing ~ 1, data = big5) 

# Then add every predictor using add1() (this is the same second step as forward
# selection).

add1(modelnone, scope = ~OpenMindedness + Conscientiousness + Extraversion + 
       Agreeableness + NegativeEmotionality, test = "F") 

# Select the variable with highest F/lowest p, which is NegativeEmotionality,
# and use update() with a "+" sign to update the model with this predictor.
# (This matches forward selection so far). 

add1(update(modelnone, ~.+NegativeEmotionality), 
     scope = ~OpenMindedness + Conscientiousness + Extraversion 
     +  Agreeableness + NegativeEmotionality, test = "F")

# Conscientiousness has the highest F so we select this to add to the model.
# However, after adding to the model, we need to see if NegativeEmotionality
# stays significant. So we use drop1() with update() of our added predictors.
# This will let us see if any variable should be dropped.

drop1(update(modelnone, ~.+NegativeEmotionality +Conscientiousness), test = "F")

# Both predictors are significant, so don't drop either. So we continue the 
# selection steps by using add1() and add the predictor with highest F/lowest p.

add1(update(modelnone, ~.+NegativeEmotionality +Conscientiousness), 
     scope = ~OpenMindedness + Conscientiousness + Extraversion 
     +  Agreeableness + NegativeEmotionality, test = "F")

# Agreeableness is the most significant so we add this to the model. Then we need
# to see if NegativeEmotionality and Conscientiousness both remain significant
# after adding Agreeableness to the model using drop1().

drop1(update(modelnone, ~.+NegativeEmotionality +Conscientiousness 
             +Agreeableness), test = "F")

# They are still significant so we don't drop any predictor. Then we need to
# see if any new variables can be added by checking significance using add1().

add1(update(modelnone, ~.+NegativeEmotionality +Conscientiousness +Agreeableness), 
     scope = ~OpenMindedness + Conscientiousness 
     + Extraversion +  Agreeableness + NegativeEmotionality, test = "F")

# None are significant, so no further predictors will be added and we are done
# with variable selection.

# The final model is 
# ExistentialWellBeing ~ NegativeEmotionality + Conscientiousness + Agreeableness

# ---------------------- All Subsets Regression ----------------------

# 2.5 All Subsets Regression

# All subsets regression is a form of variable selection that evaluates every
# possible combination of predictors to identify the best-fitting model. Instead
# of adding or removing variables step-by-step, this approach fits all candidate
# models and compares them using criteria such as R^2, adjusted-R^2, AIC, BIC, or
# other methods. For this example, we will use adjusted-R^2 as that's typical.

# This method allows you to directly compare models of
# different sizes and choose the one that provides the best balance between fit
# and complexity. While this method is comprehensive and can find the optimal
# model under the chosen criterion, it can become computationally intensive when
# the number of predictors is large, since the number of possible models grows
# rapidly.

# 2.6 All Subsets Regression with regsubsets()

# The best method to do this with R packages I found is to use the regsubsets()
# function from the 'leaps' package.

# The first step is to subset your dataset to just have the predictors of interest.
# You can do this any way, but I will just use select() and %>% 
# from the 'dplyr' package.

big5small <- big5 %>% select(ExistentialWellBeing, OpenMindedness, 
                                    Conscientiousness, Extraversion,
                                    Agreeableness, NegativeEmotionality)

# Plug this dataset into an empty model described in regsubsets(). Note, in this
# case, you don't need to include a 1 for the intercept. 
# What this function will do is test every possible liner regression combination
# with the predictors in your dataset and save that as an object.

subsetmodels <- regsubsets(ExistentialWellBeing~., data = big5small)

# To get the output of the all subsets regression, get the summary of this object.

summary(subsetmodels)

# Look at the bottom table of the output here. This shows the best model with 
# a certain number of predictors, based from the adjusted R^2. The number furthest 
# to the left is how many number of predictors are in the model, and the variable
# with * means that this variable is included for the best model. For example, for a 
# model with just one predictor, including NegativeEmotionality gives you 
# the best model. For a model with just two predictors, including 
# Conscientiousness and  NegativeEmotionality gives you the best model and so forth.

# If you want the adjusted-R^2's of each best model, you need to extract it
# from the summary object like so.

summary(subsetmodels)$adjr2

# This gives the adjusted-R^2 per model. Look at the models from the first
# output and compare them to these adjusted-R^2's Your selected models should be
# based on the best fitted model for each number of predictors that have the
# highest adjusted-R^2. Based on this, the model with four predictors had the
# highest adjusted-R^2 so the final model is

# ExistentialWellBeing ~ NegativeEmotionality + Conscientiousness 
#                       + Agreeableness + Extraversion

# 2.6 All Subsets Regression with PROCESS

# PROCESS also can do all subsets regression if you include "subsets = 1" as an
# option. However there are a couple of caveats. One, the number of predictors
# must be ≤ 15. Two, the outcome must be continuous. With that said, let's run 
# the model.

process(data = big5, x = c("OpenMindedness", "Conscientiousness", "Extraversion",
                           "Agreeableness", "NegativeEmotionality"),
        y = c("ExistentialWellBeing"), 
        subsets = 1)

# As you can notice, there is a very big table under "All subsets and regression
# results". This represents every possible combination of the predictors in a
# linear regression. For each line, you will see that each predictor either has
# a 1 to denote it is in that model, or a 0 to denote it is not in that model. 
# As an example, the very first row displays the effect sizes for a model that
# only has OpenMindedness, and has R^2 = .0113 and an adjusted-R^2 = .0107. 

# The models are listed in terms of the size of the R^2's, with the smallest R^2's
# at the top and the largest R^'s at the bottom. Here you are basically looking 
# at which row has the least amount of predictors but maximizes the adjusted-R^2.
# To do this, you need to look at the bottom row, which will show you the row
# with the highest adjusted-R^2. 

# Looking at the bottom, you can see the selected model with the highest 
# adjusted-R^2 is

# ExistentialWellBeing ~ NegativeEmotionality + Conscientiousness 
#                       + Agreeableness + Extraversion

# There is an issue that has popped up before for when you have a lot of predictors
# in your process model. The number of possible model combinations becomes too 
# much for the PROCESS output to display, so you won't actually be able to 
# see the bottom, highest adjusted-R^2 model. To fix this, you need to change
# the output options for your R display.

# To do this, use the options() function and change the "max.print" argument to 
# a relatively high number. The default option is 1000 before R starts
# cutting off output (this includes all the output from the general PROCESS 
# syntax), so you might want to try setting it to a large number like 10,000.

options(max.print = 10000)

# Keep in mind, you may want to change this back to default after unless you want
# to get a bajillion obnoxious data rows everytime you run a line of code. 
# R studio will automatically do this everytime you restart (and don't save
# your session). But you can also just run this line.

options(max.print = 1000)

# ======================  Prediction Intervals ======================

# 3.1 Prediction Intervals
 
# Prediction intervals are used when we want to estimate not just the average
# outcome, but the value of a *new individual observation* given a set of predictor
# values. In regression, we often compute a predicted value (y-hat) for a specific
# combination of predictors, but that prediction is not exact. There is always
# uncertainty and natural variability in the data. A prediction interval provides
# a range of values within which we expect a single future observation to fall,
# with a certain level of confidence (typically 95%).

# It is important to distinguish prediction intervals from confidence intervals.
# A confidence interval gives a range for the *mean* outcome at a given set of
# predictors, whereas a prediction interval gives a range for an *individual*
# outcome. This goes back to the difference between predictive and inferential
# statistics. Because individual observations vary more than averages, prediction
# intervals are always wider than confidence intervals.

# Conceptually, a prediction interval accounts for two sources of uncertainty:
# (1) uncertainty in estimating the regression line itself, and (2) the random
# error variance around that line. This is why prediction intervals
# can sometimes be quite wide, especially when the model has a lot of unexplained
# variance.

# In practice, prediction intervals are especially useful when making real-world
# predictions (e.g., predicting a person's score, a house price, or a future
# measurement), where we care about the likely range of a single outcome rather
# than just the average trend.

# If most of what we learned this lab is out of date, prediction intervals are 
# the main thing you should take away. These are used everywhere and you will
# always need to generate some kind of interval estimate when doing anything
# that involves prediction. Since it is important to know what the uncertainty
# of your prediction is.

# 3.2 Getting Prediction Intervals

# To get prediction intervals, first lets fit the full model again (Note, you 
# can use any model, we are just interested in the full model for this example).

full_model <- lm(ExistentialWellBeing ~ OpenMindedness + Conscientiousness 
                 + Extraversion + Agreeableness + NegativeEmotionality,
                 data = big5) 

# Then you need to use the predict() function, but this time you include
# the dataset and the option "interval = prediction" as well as the model object.

predict(full_model, big5, interval = "prediction") 

# These are the 95% prediction intervals for each predicted outcome. The "fit" 
# column is the predicted existential well being for each individual, and
# "lwr" and "upr" represent the lower and upper bounds of the 95% prediction
# interval respectively. 

# If you want to change the width of the intervals, change "level = " which is 
# .95 by default. 

# 90% prediction interval
predict(full_model, big5, interval = "prediction", level = .9) 

# If you want a confidence interval for each point you can change the interval
# type (this is just a bonus, but not needed for prediction).

# 95% confidence interval per predited score.
predict(full_model, big5, interval = "confidence") 

# 3.2 Specific Prediction Intervals

# Prediction intervals also let you measure the uncertainty of specific 
# predictions. To do this, you need to save a specific combination of predictor
# scores in a new data frame with the appropriate names. 

# Let's find the prediction interval for an indivdual who scored the following
# on the Big 5 items.

newdata <- data.frame(OpenMindedness = 1, Conscientiousness = 2,
                      Extraversion = 3, Agreeableness = 0, 
                      NegativeEmotionality= 2)

# Now plug this new dataset into the predict() function with our full model.

predict(full_model, newdata, interval = "prediction")

# So we predict this individual has an existential well being score of about 3,
# with a 95% prediction interval of [1.86, 4.19]. So there's a small ammont of 
# uncertainty around this.

# If you want to do multiple specific predictions at a time, you just need
# to add more hypothetical data points into the new dataset. 

newdata <- data.frame(OpenMindedness = c(1,0), Conscientiousness = c(2,1), 
                      Extraversion = c(3,2), Agreeableness = c(0,1), 
                      NegativeEmotionality= c(3,2))
predict(full_model, newdata, interval = "prediction")

# ======================  LASSO Regression ======================

# 4.1 Ridge vs LASSO vs Elastic Net Regression

# These two methods are more widely used automated selection methods for predictive
# models, however they are a bit more technical to explain, so bare with my 
# descriptions.

# Ridge and LASSO regression are both methods for deciding which predictors
# should be included in a model, similar in spirit to techniques like backward
# or stepwise selection. However, instead of removing variables one at a time,
# these approaches keep all predictors in the model but reduce their influence
# based on how useful they are for prediction.

# Ridge regression shrinks this influence by shrinking all coefficients toward zero.
# Predictors that are less useful will have smaller coefficients, but with Ridge
# regression, they are never fully removed from the model. This means Ridge keeps 
# everything, but gives more weight to the most important variables and less 
# weight to weaker ones. This contrasts the above methods that remove variables
# completely from the model. 

# LASSO regression works a bit differently. Like Ridge, it shrinks coefficients,
# that aren't as useful for prediction, but unlike Ridge, it can shrink some of 
# the predictors all the way to zero as opposed to very small numbers. When this
# happens, those  predictors are effectively removed from the model. In this way, 
# LASSO performs variable selection automatically, similar to backward selection, 
# but in a more gradual and data-driven way.

# A helpful way to think about this is that backward selection makes “yes or no”
# decisions about each variable (keep it or drop it), whereas Ridge and LASSO
# instead adjust how much each variable matters. LASSO can still make “drop”
# decisions by setting coefficients to zero, while Ridge only reduces their
# importance without fully removing them.

# There is another method called "Elastic Net", which is a mix of LASSO and Ridge.
# This method can both shrink coefficients (like Ridge) and remove unimportant 
# variables (like LASSO) at the same time. This gives you a balance between 
# keeping useful information and simplifying the model. A key advantage for Elastic
# Net regression shows up when predictors are highly correlated (which can be very 
# common in psychology data). LASSO tends to pick one variable and drop the others
# somewhat arbitrarily, while Ridge keeps them all but doesn’t simplify the model.
# Elastic Net, however, can keep groups of related variables together while still 
# shrinking or removing less useful ones, leading to more stable and interpretable 
# results. Elastic Net’s main drawback is that it is more complicated to use 
# because you have to make additional choices about how the model balances 
# shrinking coefficients versus removing variables, which makes it harder to
# understand and apply compared to simpler methods like LASSO.

# In this lab, we will primarily focus on LASSO regression. The reason is that 
# LASSO directly helps with variable selection by reducing some predictors’ 
# effects to 0, effectively removing them from the model, which aligns with 
# our main goal when building models. This makes the final model easier to
# interpret and more similar to the selection methods you have already learned,
# like backward or stepwise selection. Since many real-world datasets
# (especially in psychology) include predictors that may not meaningfully 
# contribute to the outcome, LASSO provides a practical and efficient starting 
# point for identifying a smaller, more useful set of predictors while still 
# maintaining strong predictive performance. If ayone is more interested in 
# learning about techniques like Elastic Net, I encourage you to do so. 

# 4.2 LASSO Regression

# LASSO Regression is a regularization technique used to improve prediction
# accuracy and perform variable selection at the same time. It works by adding
# a penalty to the regression model that shrinks the size of the coefficients
# toward 0. Unlike standard multiple regression, which only minimizes the
# sum of squared errors, LASSO also penalizes the absolute size of the
# coefficients (this is called an L1 penalty).

# NOTE: "Regularization" refers to ais a method used in regression that
# intentionally reduces the size of coefficients to prevent the model from 
# overfitting and to improve how well it predicts new data.
# "Penalty" is an extra constraint added to the model that discourages large
# coefficients by making them “cost” more, which pushes the model to keep 
# coefficients smaller.

# Essentially, the less predictive a certain variable is, the more its coefficient
# will be shrunk towards 0. Mind you, if the predictor's coefficient is at 0,
# then the predictor is not useful in actually predicting. 

# As a result, some coefficients can be shrunk exactly to zero, effectively
# removing those predictors from the model. This makes LASSO especially useful
# when you have many predictors or when some predictors are not very important,
# because it helps create a simpler, more interpretable model while reducing
# overfitting.

# The strength of the penalty is controlled by a tuning parameter (often called
# lambda, λ). This is called a "tuning" parameter because the goal of LASSO is to
# find the value of λ that leads to the best predictive performance (we are 
# "tuning" the model like an instrument). When λ is small, the model behaves more
# like ordinary least squares (less shrinkage, more complex model). As λ increases,
# more shrinkage occurs and more coefficients may be driven to zero, resulting in
# a simpler model.
#
# The goal is to choose a value of λ that gives the lowest prediction error, while
# avoiding unnecessary complexity. In practice, this means finding a balance where
# the model predicts well without including predictors that do not meaningfully
# improve performance.Choosing the optimal value of λ is typically done using 
# cross-validation. As a result, LASSO will find many different λ values, and the
# final model will be based on the value that corresponds to the simplest model
# among all the models that predited within an acceptable level of accuracy.

# Do note that interpretation of the coefficients isn't the most useful for LASSO
# because LASSO just seeks to get the most optimal model in terms of minimizing 
# error variance. The coefficients it ends up with might be wildly different than
# what you might have fit by hand. Thus, if you care about interpretable slopes,
# LASSO might not be for you.

# 4.2 LASSO with cv.glmnet()

# To do LASSO in R, you can use the cv.glmnet() function from the 'glmnet' package.
# The "cv" stands for "cross-validation" for a glmnet function. This cross 
# validation is the process for choosing λ before the LASSO is ran. 

# All you need to do is plug in your predictors as a matrix. I'm going to reuse
# the smaller dataset in the big5small object I created on Line 599. However, this
# needs to be in a matrix. So I can use as.matrix() to do so.

predictors <- as.matrix(big5small)

# Next is to plug the predictors and outcome into the model, and put the 
# 'family' argument to equal "gaussian" and the 'alpha' argument to equal 1.
# We are still doing ordinary regression, so "gaussian" represents the normal
# distribution (which is our assumption of the residuals, see the "Diagnostics"
# lab), and the "alpha = 1" tells the function to fit a LASSO regression.

# NOTE: "alpha = 2' fits a Ridge regression, and "alpha = " some number between
# 1 and 2 fits an Elastic Net, where the number between 1 and 2 helps dictate the
# additional tuning parameter required for that method.

# Run the model
lasso_model <- cv.glmnet(x = predictors,
                         y = big5$ExistentialWellBeing, 
                         family = "gaussian", 
                         alpha = 1)

# If we want to see what the algorithm tuned λ to, extract lambda.min from it.

lasso_model$lambda.min

# So the optimal λ was .02. This doesn't really have an interpretation nor does the
# size of this value mean anything concrete (.02 could be small in one dataset 
# and large in another).

# If you want to see how the algorithm arived at this value, you can plot the 
# the model, which displays a cross-validation curve. 

plot(lasso_model)

# When looking at this, the Y-axis represents the prediction error (mean-squared 
# error), where lower values indicate better predictive performance. The X-axis 
# shows −log(λ), so moving to the right corresponds to smaller values of λ 
# (less shrinking and a more complex model), while moving to the left corresponds 
# to larger λ (more shrinking and a simpler model). The numbers along the top
# indicate how many predictors are included in the model at each value of λ that
# was tested. The vertical dotted line marks the λ value that produced the lowest
# prediction error based on cross-validation.

# As you can see, the prediction error decreases quickly at first as we allow more
# predictors into the model, indicating that adding variables improves prediction.
# However, after a certain point, the curve begins to flatten, meaning that
# additional complexity provides little improvement. In this case, the optimal λ 
# is located far to the right, suggesting that very little shrinking is needed 
# and that the model is close to a standard regression. Overall, this tells us
# that a relatively simple model with only a small number of meaningful 
# predictors—performs best for predicting the outcome.

# If you want the exact values for this plot's solution path, you can 
# extract them with the following.

lasso_model$glmnet.fit

# The "DF" column tells you the number of coefficients per the point in the plot,
# the %Dev is the percent of deviance explained (this tells you how much of the 
# variation in the outcome is explained by the model, similar to R^2), and the 
# "Lambda" column is just the λ value. 

# Here are some other potentially useful info you can extract.

lasso_model$lambda  # all lambda values tested
lasso_model$cvm     # mean cross-validation error per lambda tested
lasso_model$cvsd    # standard error of cross-validation error per lambda tested
lasso_model$nzero   # number of non-zero coefficients per lambda tested

# If you want your model coefficients, to see what variables were actually left
# in the model and what the coefficient values were, you need to use the 
# coef() function.

# You can use it with either the argument s = "lambda.min" to see the model 
# with the best predictive performance, or the argument s = "lambda.1se" 
# (which is the default) to see a simpler model that performs nearly as well.
# Recall, a smaller λ means less shrinking (more complex model), and a larger 
# λ means more shrinking (simpler model).

coef(lasso_model, s = "lambda.min")
coef(lasso_model, s = "lambda.1se")

# In this case, they are the same, so we can say that both the “best” model 
# and the “simpler” model ended up being identical. This tells us that allowing 
# for more complexity (adding more variables or reducing shrinkage) does not 
# meaningfully improve prediction. As a result, a single predictor captures 
# most of the useful information in the data.

# So we can see that the LASSO removed everything from the model except 
# ExistentialWellBeing. This is interesting as this differs from the previous
# methods that tended to keep a couple of more predictors. LASSO didn't see
# any use for anything besides ExistentialWellBeing. Though, I suppose this is 
# not that surprising, since it was the only one with a somewhat large effect 
# (recall the Variable Importance lab). Of course, the specific coefficient here
# should not be interpreted. All we know is that NegativeEmotionality plays an
# important roll in predicting ExistentialWellBeing, but we can't tell the exact
# relationship from this model.

# If you want predicted scores, you can use predict() while
# specifying the predictors

predict(lasso_model, newx = predictors, s = "lambda.min")
