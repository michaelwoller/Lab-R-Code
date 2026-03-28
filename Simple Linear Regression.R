# ====================== Simple Linear Regression ======================

# Author: Michael Woller

# ====================== Load Packages ======================

#install.packages("ggformula")
#install.packages("dplyr")
library(ggformula)  # used for gf_point() and gf_lm()
library(dplyr)      # used for the pipe operator %>%

# Make sure you load in PROCESS

# ====================== Correlations in R ======================

# 1.1 Correlations with cor()

# Before fitting a regression model, it is often useful to look at the
# correlation between the predictor and the outcome.

# You can use the cor() function in R to find a correlation between two variables

cor(faithful$eruptions, faithful$waiting)

# This computes the Pearson correlation between eruptions and waiting.

# For those who are new to R, the $ operator lets you select specific v
# ariables from a datset

# Example:
faithful$eruptions

# If your dataset contains missing values, cor() can fail
# unless you tell R how to handle them.

cor(faithful$eruptions, faithful$waiting, use = "complete.obs")

# The option use = "complete.obs" tells R to use only cases
# that have non-missing values on both variables.

# You can interpret this correlation value as a standardized difference.
# Old Faithful eruption length are highly correlated with its waiting 
# time between eruptions with cor = .9.

# Between two eruptions of Old Faithful that differ in one standard deviation
# of eruption length, we expect the longer eruption length to also have a .9
# standard deviation increase in wait time for the next eruption. 

# ====================== Simple Linear Regression in R ======================

# 2.1 Simple Linear Regression

# In this example, we will use the built-in faithful dataset.
# We will predict waiting time from eruption length.

# A simple linear regression asks whether one variable (the predictor)
# helps us predict another variable (the outcome).

# Here:
#   Outcome (Y)   = waiting
#   Predictor (X) = eruptions

# We will first run the regression using base R, 
# and then run the same analysis using PROCESS.

# ---------------------- Linear Regression with Base R ----------------------

# 2.2 Fitting Simple Linear Regression with lm()

# Use the lm() to fit a linear model ("lm" stands for "linear model").
# The following code does a linear regression predicting wait time from eruptions.

faithfulModel <- lm(waiting ~ eruptions, data = faithful)

# The ~ can be viewed as an = sign for the purpose of regression.
# The formula waiting ~ eruptions means:
# predict waiting from eruptions

# The "faithfulModel" object contains much information about the regression.
# However, to extract the information, you need to do different things.
# Simply entering the model will only give you the coefficients (intercept and slope).

faithfulModel

# Typically, people use the summary() function to get a display of the majority
# of relevant model output.

summary(faithfulModel)

# The summary() output gives useful model information, including:
#   regression coefficient values (under Estimate)
#   standard errors (under Std. Error)
#   t tests statistics (under t value)
#   p values (under Pr(>|t|))

# For ease, significance coding (for a criteria of .05) is given in * asterisks.
# At the top of the output, you can also see the general range of residual values.
# At the bottom, you can see other model fit statistics (which we will discuss in the future).
  # Residual Standard Error (RSE) gives the typical size of prediction error
  # Multiple R^2 is the R^2 for the model
  # Adjusted R^2 is the adjusted-R^2 for the model
  # F-statistic and the values in that line is the F test for the model R^2

# If you want confidence intervals, you need to use the confint() function.
# You can specify different confidence levels, but we will cover this in future
# lectures.

confint(faithfulModel)

# 2.3 Interpretations of Coefficients

# Recall that Amanda is particular about the interpretations of coefficients. 
# That is, avoid within-subject causal language like "change" or "increase."
# This is because a regression model like above is comparing two separate data points.
# It is not changing the same point of data. So interpretations shoudl reference
# comparing two seperate things.

# Intercept:
#   For an Old Faithful eruption that lasted 0 minutes, the wait time is
#   expected to be 33.47 minutes. Obviously this doesn't make sense given the context.

# Slope: 
#   For two different Old Faithful eruptions that differ in their length by one minute,
#   their is expected to be a 10.73 minute longer wait time for the longer eruption.

# Remember to also mention "expected/predicted" differences and directionality of effect.

# 2.4 What Is Stored in the Model Object?

# As I eluded to before, when you fit a model in R, 
# the result is stored as an "lm" object.
# To check you can use class(), which tells you the class of any R object.

class(faithfulModel)

# lm objects are widely used throughout many functions in R as they contain
# many pieces of information about whichever regression model they represent.

# The attributes() function gives a list of everything within an object.

attributes(faithfulModel)

# Here, under $names, we can see everything that R has automatically calculated. 

# You do not need to memorize everything stored inside the object,
# but it is useful to know that the model contains much more than
# just the printed summary output.

# Some especially important pieces are:

faithfulModel$coefficients   # regression coefficients 
faithfulModel$residuals      # residuals
faithfulModel$fitted.values  # predicted values

# coefficients:
# the estimated intercept and slope

# residuals:
# the difference between observed and predicted scores

# fitted.values:
# the predicted Y values from the regression line

# Recall:
#  A predicted score is the model's estimated value of Y for each case.
#  A residual is the difference between the observed Y value
#  and the predicted Y value.

# In other words:
# residual = observed score - predicted score

# If the residual is large, the model predicted that case poorly.
# If the residual is small, the model predicted that case well.

# Since these two values are so universally used, 
# there's actually short hand for them

faithfulModel$coef  
faithfulModel$resid   
faithfulModel$fit 

# There are many more things to extract from the lm object, but we will 
# cover those when relevant.

# 2.5 Alternative Functions for Extracting Model Information

# There are also built-in functions that extract the same information.

coef(faithfulModel)      # coefficients
resid(faithfulModel)     # residuals
predict(faithfulModel)   # predicted values

# These functions are often preferred because they work across
# many different model objects, not just lm() models.
# You can also use them in more complicated functions, but that is for
# more advanced R purposes.

# 2.6 Save residuals and predicted values with transform()

# One way to save new variables into the dataset is with transform().
# This function mutates your inputted dataset with whatever new variables you specify.

faithful <- transform(faithful, 
                      faithfulResid = resid(faithfulModel),
                      faithfulPred = predict(faithfulModel))

# This adds two new variables to the faithful dataset:
#   faithfulResid = residual scores
#   faithfulPred  = predicted scores

# You can choose different variable names if you want.

# 2.7 Save Them with the $ Operator

# Another way to save new variables is with the $ operator.
# This method is very useful since it is quick.

faithful$res <- resid(faithfulModel)
faithful$pred <- predict(faithfulModel)

# This does the same thing as transform(), but on separate lines.

# The general pattern is:
# dataset$newVariable <- values

# You, can save these to any dataset that has the same number of rows technically.

# ---------------------- Linear Regression with PROCESS ----------------------

# 3.1  Fit the Regression Model with PROCESS

# PROCESS is an exterior macro that does many things automatically
# The reason we are learning it is that it becomes useful for more complicated purposes
# that we will do in the future, even though it may seem unnecessary now.

process(data = faithful, y = "waiting", x = "eruptions", model = 0)

# This fits a simple linear regression model predicting waiting from eruptions.

# In PROCESS:
#   y = the outcome variable being predicted
#   x = the predictor variable being used to predict Y
#     These MUST be in quotations "". 
#   model = the type of regression model we want to do
#     model = 0 tells PROCESS we are doing multiple regression
#     this is the default, so you don't have to include this argument if you dont
#     want to.


process(data = faithful, y = "waiting", x = "eruptions") 

# Note that this output is equivalent to the one that include model = 0
# This "model = " option won't come back up until we discuss moderation, so don't 
# worry about it until then. 

# Note that unlike lm(), PROCESS gives confidence intervals by default.
# We will go over how to change the confidence level in the future.

# 3.2 Save Predicted Scores and Residuals with PROCESS

# If you want PROCESS to save predicted scores and residuals,
# use the save = 4 option and assign the output to a new object.

diagnostics <- process(data = faithful, y = "waiting", x = "eruptions", save = 4)

# This creates a new object containing the original data, but it
# will still automatically give you the model output.

# View the first several rows with the head() function. This
# function is just useful for minimizing output clutter.

head(diagnostics)

# As you can see, there's a lot of things saved in this object.
# We will bring each of these up when they become relevant, but for now
# we will focus on the pred and resid columns.

# Predicted scores
diagnostics$pred

# Residual scores
diagnostics$resid

# ====================== Plotting the Data and the Model ======================

# 4.1 Plotting Regressions

# Graphs are extremely useful in regression because they help us
# see the relationship between X and Y directly.
# There's many types of plots that will be useful, but for now, we will focus
# on basic scatterplots.

# There are MANY methods of plotting. We will mainly be using the 'ggformula' or
# 'ggplot2' packages, depending on the lecture. For now, we will use 'ggforumla'.
# But I will show the default R function and PROCESS too.

# A scatterplot shows the relationship between two quantitative variables.

# Here:
#   X-axis = eruptions
#   Y-axis = waiting

# Each point represents one observation in the dataset.

# If the points generally move upward from left to right,
# that suggests a positive relationship.
# If they move downward from left to right,
# that suggests a negative relationship.

# For plotting regression lines, recall: 
# that the regression line shows the predicted value of waiting 
# for each value of eruptions.

# In other words, the line is the model's best-fitting straight line
# through the data.

# For Predicted Score plots, recall:
# This plot shows the predicted values from the model
# as a function of the predictor.

# Because this is a simple linear regression, the predicted scores
# fall exactly on a straight line.

# For Residual plots, recall:
# This type of plot shows the residuals for each case.

# A residual tells us how far each observed score is from
# the model's predicted score.

# Residual plots are important because they help us check
# whether the model is fitting the data appropriately.

# For a well-behaved linear regression model, we generally want
# residuals to be scattered around 0 without a strong pattern.

# If the residuals show a curve, funnel shape, or clustering pattern,
# that can suggest problems with model assumptions.

# ---------------------- Using ggformula ----------------------

# 4.2 Using gf_point() for raw data

# The gf_point function from the 'ggformula' package gives a large variety of 
# options for visualization. The benefit of this function is that you can conveniently
# specify your model within the function instead of pluggin in variables manually.

gf_point(waiting ~ eruptions, data = faithful)

# You will need to look up or ask AI for options because there are many things you can
# do to change the look.

# 4.3 Using gf_lm() to fit a line

# In 'ggformula', you can add the fitted regression line with gf_lm().
# You have to first add a "piping" operator from the 'dplyr' package at the
# end of the gf_point() function.

gf_point(waiting ~ eruptions, data = faithful) %>%
  gf_lm()

# This line matches the predicted scores from the regression.

# 4.4 Using gf_point() for predicted scores

# Since gf_point() works on a selected dataset, you need your dataset to have the
# predicted scores saved within it. 

gf_point(faithfulPred ~ eruptions, data = faithful)

# 4.5 Using gf_point() for residuals 

# Same stipulations for the predicted scores using gf_point() matter here.

gf_point(faithfulResid ~ eruptions, data = faithful)

# ---------------------- Using base R ----------------------

# 4.6 Using plot() for raw data

# Base R has the plot() function that can give basic scatterplots.
# There are options to customize the look, but it is more limited than other packages.
# I mainly use plot() for very quick visualizations to just get the idea of what 
# data looks like.

# There's two methods for this. Manually specifying the variables using the $ operator.

plot(faithful$eruptions, faithful$waiting)

# You could also do this using the with() function, where you basically say do plot() 
# with faithful. Which ever method is up to you.

with(faithful, plot(eruptions, waiting))

# 4.7 Fitting regression lines on plot() 

# If you want to add a fitted line with plot(), use abline() with your model object
# in the next line. This will add the line on top of the previous generated plot.

with(faithful, plot(eruptions, waiting))
abline(faithfulModel)

# 4.8 Plotting Predicted Scores and Residuals with plot()

# There's nothing special about this. Just take wherever you saved the 
# predicted scores and residuals and put them in plot()

plot(faithful$faithfulPred, faithful$waiting)
plot(faithful$faithfulResid, faithful$waiting)

# ---------------------- Using PROCESS ----------------------

# 4.4 Using PROCESS for plotting

# PROCESS doesn't have many useful built in plotting options, so it is recommended
# to use a different package. 

# One convenient thing about the diagnostics you saved is that the regular variables
# are in there too, so you can plug them into either gf_point() or plot().

gf_point(resid ~ eruptions, data = diagnostics)
plot(diagnostics$resid, diagnostics$eruptions)

