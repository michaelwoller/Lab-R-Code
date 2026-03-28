# ====================== Regression Inference and Multiple Correlations ======================

# Author: Michael Woller
# Data courtesy of Dr. Egamaria Alacam

# ====================== Load Packages ======================

# Remember to load in PROCESS. 

# ---------------------- Specify File Path ----------------------

filelocation <- "/Users/woller/Documents/250c 2026/Lab 4/rauw.csv" # wherever on your computer
rauw <- read.csv(filelocation, header = TRUE)

# ---------------------- Data Cleaning ----------------------

# remove missing values
rauw <- rauw[complete.cases(rauw$Age) & complete.cases(rauw$RL),]
# N = 1549

# This isn't necessary with PROCESS, since it removes missing values automatically,
# but this is necessary if you are using base R.


# ====================== Coefficient Significance ======================

# 1.1 Coefficient t Tests

# All regression output will give you significance tests for the slopes by default.
# These will be t tests with degrees of freedom equal to 
#   df = N - k - 1
# Where N is your sample size and k is the number of predictors in the model.

# Let's fit a basic model in lm() and PROCESS just to demonstrate.

# We will be predicting how much an individual Enjoys the music of 
# Rauw Alejandro (Enjoy) by their age (Age) and how much they like reggaeton (RL).

# With lm()
model1 <- lm(Enjoy ~ RL + Age, data = rauw)
summary(model1)

# With PROCESS
process(data = rauw, x = c("RL", "Age"), y = "Enjoy")

# Here we can see that the intercept and slope of RL are significance at the 
# alpha = .05 rejection level, however Age is not significant. 

# 1.2 Inferential Statements

# Previously, you learned about interpretations of what we observe. 
# "Inference" has to do with how what we observe can apply to the population.
# These are two separate concepts, so keep this in mind. 

# Inference involves a null hypothesis about the population. "Null" coming from the
# fact that it assumes a baseline fact about the population, which for a coefficient
# t test is that the coefficient equals 0. 

# You are most likely familiar with the concept of p values and rejection of the 
# null, but it is important to note semantics here when making inferential conclusions.

# When addressing inference, "significance" applies to your sample statistic 
# (i.e., the coefficient you estimates from the sample). The null hypothesis is
# about the population. 

# It is NOT appropriate to say you reject/fail to reject the null that your slope
# equals 0. 
#   The hypothesis is that the population slope is 0.

# It is NOT appropriate to say that the slope is significantly different than 
# in the population.
#   Significance applies to what you observe from your regression model.

# It IS appropriate to say you reject/fail to reject the null that the 
# POPULATION slope equals 0. 

# It IS appropriate to say that your model's coefficient is significantly different
# than 0.

# An correct inferential statement also ALWAYS includes the test you used (t test),
# the proper degrees of freedom, and the p value (unless it is below .001, in 
# which you just say. p < .001 (note, different fields and journals have different
# standards of how to report p values). You should also report confidence intervals
# and their width (95%).

# A proper inferential statement for each model coefficient is as follows. 
 
# Intercept:
#   The intercept was significant t(1546) = 5.78, p < .001, 95% CI [.91, 1.85]. 
#   This means that the model intercept was significant, so we reject the null that
#   the intercept is 0 in the population.

# RL Slope:
#   The slope of RL was significant t(1546) = 22.94, p < .001, 95% CI [.30, .36]. 
#   This means that the slope of RL was significant, sowe  reject the null that
#   how much an individual enjoys reggaeton does not predict how much they enjoy
#   Rauw Alejandro in the population.

# Age Slope:
#   The slope of RL was not significant t(1546) = 1.36, p < .001, 
#   95% CI [-.01, .03].  This means that the slope of Age was not significant,
#   so we fail to reject the null that and individual's age does not predict 
#   how much an individual enjoys Rauq Alejandro in the population. 

# 1.3 Inference with Confidence Intervals

# It's possible to do inference with confidence intervals. To do this, you need
# to see if the confidence interval contains your null hypothesis. Since our null
# for the coefficients is 0, and we are using the 95% confidence level (determined
# by your alpha rejection rate), we will see if the 95% confidence intervals
# for each coefficient contain 0. 

# This is given by default in PROCESS, but to get this with base R, you need
# to use the confint() function, which gives 95% confidence intervals by default.

confint(model1)

# Looking at the output, we see that the confidence intervals for intercept 
# and slope of RL do not contain 0, while the interval of Age does contain 0. 
# So we would conclude that our intercept and slope of RL are significant, but 
# the slope of Age is not significant.

# The resulting interpretations would follow what they are in section 1.2. 

# If you are using a different alpha level than .05, you will need to manually 
# change the confidence interval width.

# Example: Alpha = .01 and a 99% Confidence Interval

# With confint()
confint(model1, level = .99)

# With PROCESS
process(data = rauw, x = c("RL", "Age"), y = "Enjoy", 
        conf = 99)         # the CI size needs to be whole number, not decimals

# Even with the new alpha level, we would still reject the null of the intercept
# and RL. 

# ====================== R^2 F tests ======================

# 2.1 R^2 F tests

# The R^2 coefficient also has a significance test associated with it. It tests
# whether or not the model explains a significant amount of the variation in 
# the outcome or not. 

# Inference on R^2 is about the "population" R^2. This can be thought
# of as the "true" relationship your predictors have on the outcome. Your model
# only estimates this true relationship, but we want to make direct ifnerences 
# on it as well.

# Both base R and PROCESS will give you the R^2 F test as default output.

# It is at the bottom of the summary() output.

summary(model1)

# The F-statistic line gives the F test statistic, the degrees of freedom, and
# p value.

# For PROCESS, it is located at the top under model summary.

process(data = rauw, x = c("RL", "Age"), y = "Enjoy")

# The F and p columns are for the "R-sq" statistic.

# Note, the Adjusted-R^2  value does NOT have a significance test associated 
# to it. All R^2 tests are for unadjusted R^2s.

# The degrees of freedom for this F test follow the function
#   df1 = k 
#   df2 = N - k - 1
# Where N is the sample size and k is the number of predictors.

# 2.2 R^2 Inferential Statements

# Inferential statements about R^2 follow the same idea that significance is 
# for the statistic from the model, and the null hypothesis conclusions is for the 
# the population relationship. 

# A correct inferential statement needs to include both. It also needs to include
# the F statistic, both degrees of freedom, and the p value.

# For our model, the inference test for the R^2 can be reported as the following.

# The R^2 = .25 coefficient was significant F(2, 1546) = 263.7, p < .001. Thus,
# we reject the null hypothesis that how much an individual loves reggeaton and 
# the individual's age does not explain variation of how much individuals like
# Rauw Alejandro in the population.

# ====================== Model Comparison Tests ======================

# 3.1 Model Comparisons

# t tests of model coefficients allow you to test single parameter slopes.
# However, it is often desirable to test multiple slopes *combined*.
# To do this, you need to do a model comparison test (or multiple parameter test).
# A common one is to test the change in R^2, or delta-R^2.

# If a slope t test determines if a single variable is significant in 
# explaining Y, a delta-R^2 F test can determine if multiple variables at a time  
# are significant in explaining Y.

# The default model R^2 F test compares all the variables in the model to an "empty"
# model with no predictors. It is possible to do this type of comparison, but to 
# just test a select subset of variables rather than every variable.

# The main model we are going to consider is to predict how much an individual
# enjoys the music of Rauw Alejandro (Enjoy) predicted by their age (Age),
# how much they like regaeton music (RL), and a set of predictors I am calling
# "Latin American cultural experience."

# This set of variables consists of self-report competency in Spanish (Span),
# the amount of hours per week an individual engages with Latin American
# media (LatMed), and the ammount of hours per week the individual spends in a
# Latin American based club (LatClub).

# We will be looking at a few models, but the focal question is whether or not
# the Latin American cultural experience variable set explains a significant 
# amount of variance of Enjoy, after controlling for Age and RL. 

# ---------------------- Model Comparison with Base R ----------------------

# 3.2 Model comparison using anova()

# In order to do a delta-R^2 F test, you need to first save two different models,
# one being "nested" within the other. "Nested" means that one model is just 
# a reduced version of the other, with a subset of the predictors.

# First fit a "full" model that includes all relevant variables.

modelFull <- lm(Enjoy ~ RL + Age + Span + LatMed + LatClub, data = rauw)

# Then fit a "reduced" model that includes the variables you DON'T want to test.
# Aka, remove the variables you are testing.

modelReduced <- lm(Enjoy ~ RL + Age, data = rauw)

# The idea here is that we are going to check how good the reduced model is already, and 
# then see if the extra Latin American cultural variables in the full model significantly
# increase the R^2 or not. If so, then they, as a group, are useful. If not, 
# then they, as a group, are not helpful. Note that we are checking the group,
# not individual predictors. 

# To test this, we can use the anova() function with the model objects.
# The order doesn't really matter, but you can put the reduced model first.

anova(modelReduced, modelFull)

# This F test compares the R^2 between the two models, and as you can see from the
# p = .56 value, our predictor set does not significantly increase the R^2.

# Therefore, we can conclude that the set of variables that make up 
# Latin American cultural experience doesn't increase our ability to explain
# enjoyment of Rauw Alejandro. It turns out Rauw is for everybody.

# 3.3 Manually Checking Model R^2's

# You can double check the F test above manually.

# First get the R^2s from the summary of the model objects.

R2Full <- summary(modelFull)$r.squared
R2Reduced <- summary(modelReduced)$r.squared

# Manually find the difference in the two models (full - reduced)

deltaR2 <- R2Full - R2Reduced
deltaR2

# This .0009 (very tiny) is the change in R^2 that the anova() above is testing. 
# You can also test the delta-R^2 manually.
# I'll demonstrate, but you don't need to know how to do this.

n <- nrow(rauw)                                       # Sample Size
dfFull <- length(coef(modelFull)) - 1                 # df for Full model
dfReduced <- length(coef(modelReduced)) - 1           # df for Reduced model

# Calculate F statistic
Fstat <- ((R2Full - R2Reduced)/(dfFull - dfReduced)) / ((1 - R2Full)/(n - dfFull - 1))
Fstat

# This is the same as the anova() output!
# We can also check the p value using the probability F distribution function pf().

pf(Fstat, df1 = dfFull - dfReduced, df2 = n - dfFull - 1, lower.tail = FALSE)

# This is the same p value!

# ---------------------- Model Comparison with PROCESS ----------------------

# 3.4 Model Comparisons in PROCESS

# PROCESS is a bit weird in how you input your model. Before, you learned to add
# predictors into the "x = " argument. For the purpose of model comparison, you 
# should only include the variables you want to test in the "x = " line. 

# The variables you want to keep consistent across the two models you are comparing
# (i.e., the variables you are NOT testing) should go in the "cov = " argument.

# "cov" stands for covariate. You can think of this by writing out the two models
# we are comparing:

# Full Model:
#   Enjoy = RL + Age + Span + LatMed + LatClub

# Reduced Model:
#   Enjoy = RL + Age 

# If you notice, RL and Age are covariates in both models. So we are NOT testing 
# these predictors. Hence why they go in the "cov = " line. 

# The resulting process() function should be the following.

process(data = rauw, x = c("Span",
                           "LatMed", "LatClub"), 
        cov = c("RL", "Age"),
        y = c("Enjoy"))

# The only new output you will notice here is at the bottom under
# "Hypothesis test for variabels in X set:"
#   The "X set" literally means the variables you included in "x = c()" above.
#   R2-chng is the delta-R^2 between the full and reduced model. 
#   F is the F test statistic for the delta-R^2
#   df1 is the numerator degrees of freedom for the F test
#   df2 is the denominator degrees of freedom for the F test
#   p is the p value for the F test

# The p is .56, which is much larger than .05. 
# Therefore, we can conclude that the set of variables that make up 
# Latin American cultural experience doesn't increase our ability to explain
# enjoyment of Rauw Alejandro. It turns out Rauw is for everybody.

# ---------------------- Delta-R^2 Inference Statements ----------------------

# 3.5 Delta-R^2 Inference

# Inferential statements for delta-R^2 tests follow the same idea as other tests.
# You should report all relevant statistics, make a conclusion based on the p value
# if the found delta-R^2 is significant or not, then make a conclusion about the
# change in R^2 at the population level.

# The degrees of freedom for the delta-R^2 F tests are the following:
#   df1 = k_full - k_reduced
#   df2 = N - k_full - 1
# Where N is the sample size, k_full is the number of predictors in the full model,
# and k_reduced is the number of predictors in the reduced model.

# As an example using our model. 

# The delta-R^2 =.01 statistic was not significant F(3,1543) = .79, p = .56. Therefore,
# we fail to reject that an individual's Latin American cultural experience does not
# explain a significant more amount of variation of their enjoyment of Rauw Alejeandro
# in the population after accounting for their age and how much they like reggaeton.

# ---------------------- Single Variable Model Comparison ----------------------

# 3.6 Model Comparisons of one Predictor

# Something important to mention that will occasionally come back up is doing a
# delta-R^2 model comparison test for a single variable. That is, the reduced 
# model only differs from the full model by one variable.

# Example: 

# Full Model:
#   Enjoy = RL + Age + Span + LetMed + LatClub

# Reduced Model:
#   Enjoy = Age + Span + LetMed + LatClub

# In this case, the reduced model does not have RL as a predictor.

# If you were to calculate a delta-R^2 F test for the difference of R^2 between
# these two models, this will be analytically equivalent to the t test for the 
# slope of RL in the full model.

# Let me briefly demonstrate.

modelFull2 <- lm(Enjoy ~ RL + Age + Span + LatMed + LatClub, data = rauw)
modelReduced2 <- lm(Enjoy ~ Age + Span + LatMed + LatClub, data = rauw)

# Calculate F test wiwth anova()
anova(modelReduced2, modelFull2)

# The F statistic if 528.16 with a p value of 2.02 * 10^-7.
# Now we will look at the t test for RL in the full model.

summary(modelFull2)

# The t statistic is 22.982 with a p value of 2.02 * 10^-7.
# If we square this t statistic...
22.982^2

# We get the same F statistic as the anova() output! 
# Thus, an F test for a model comparison for a single variable is identical to the
# t test of the coefficient in the full model. And the F statistic is just the 
# t statistic squared! 

# Conceptually, you can think of the model comparison test above analyzing 
# whether a model that adds RL significantly explains Enjoy any more variance 
# than  a model without  RL. It turns out that this is exactly what the slope
# test  of RL is doing, it checks whether or not RL is significant for 
# explaining Enjoy.

# Technically speaking, since you are only comparing one variable, the numerator
# degrees of freedom become 1, which keeps the numerator constant, and thus the 
# entire equation of the F statistic reduces down to a t statistic for that 
# single predictor.

# You can also check this with PROCESS by adding only RL in "x = "

process(data = rauw, x = "RL", 
        cov = c("Age", "Span",
                "LatMed", "LatClub"),
        y = c("Enjoy"))

# we can see under R2-chng that adding RL increases the R^2 value by .25, which
# is a lot. So it seems like how much a person likes reggaeton is very significant
# for explaining if people enjoy Rauw Alejandro.

# ====================== Squared Multiple Correlations ======================

# 4.1 Squared Multiple Correlations

# Previously, we have learned correlations between two variables. 
# E.g. Correlation between Y and X

# However, it is possible to get a "correlation" between one variable 
# and a set of of predictors
# E.g. Correlation between Y and (X1, X2, X3)

# Since there are multiple predictors in this desired correlation, the only way
# to format it is by making it a "squared" correlation, akin to a R^2 statistic.

# 4.2 Partialling Out Multiple Squared Correlations

# Previously, we learned how to partial out variable from Y and another predictor.
# E.g. Partialling out X2 from X1 and/or Y.
#    (Partial Y for partial correlations and don't partial Y for semi-partial)

# However, it is possible to partial multiple variables at a time to get a 
# partial/semi-partial squared multiple correlation
# E.g. Partialling out out X33 from X1, X2, and/or Y.
#    (Partial Y for partial squared correlations and don't partial Y for semi-partial)

# The result is to get a statistic that explains the relationship between a set
# of variables and the outcome while removing out the influence of other variables.

# ---------------------- Squared Semi-Partial Multiple Correlations ----------------------

# 4.3 Calculating Semi-Partial Multiple Correlations

# To calculate multiple squared correlations, there is no easy built in method in
# either R or PROCESS. It is doubtful that there is a package out there that does 
# it, but feel free to check.

# Instead, we will have to manually calculate these by saving partialed residuals,
# which is what was taught in Section 2.1 in the Multiple Linear Regression code.

# Basically, you add the variables you want to partail from as outcomes in many
# regression models, and the variable(s) you want to partial out as the predictor.

# As a running example, we want to partial out Age from each other variable 
# besides the outcome of Enjoy (since this is "semi-partial").

# 4.4 Calculating semi-partials with base R

# To do this with base R, you need to run multiple models and save the residuals.

residRL <- lm(RL ~ Age,data = rauw)$residuals
residSpan <- lm(Span ~ Age,data = rauw)$residuals
residLatMed <- lm(LatMed ~ Age,data = rauw)$residuals
residLatClub <- lm(LatClub ~ Age,data = rauw)$residuals

# Then you need to add these residuals as the predictors in a model that 
# predicts Enjoy.

semipartModel <- lm(Enjoy ~ residRL + residSpan + residLatMed + residLatClub,
                    data = rauw)

# All you need to do is find the R^2 of this model (everything else is not 
# useful from this model).

summary(semipartModel)$r.squared

# Thus, the semi-partial squared multiple correlation is .25. 

# 4.5 Calculating semi-partials with PROCESS

# Using PROCESS follows what was done in Section 2.1 of the Multiple Linear 
# Regression code. 

# First save the diagnostics from each model that partials age.

dgSpan <- process(data = rauw, x = "Age", y = "Span", save= 4)
dgLatMed <- process(data = rauw, x = "Age", y = "LatMed", save= 4)
dgLatClub <- process(data = rauw, x = "Age", y = "LatClub", save= 4)
dgRL <- process(data = rauw, x = "Age", y = "RL", save= 4)

# Then save the residuals in one data set (one dataset is 
# necessary for PROCESS).

rauw$SpanResid <- dgSpan$resid
rauw$LatMedResid <- dgLatMed$resid
rauw$LatClubResid <- dgLatClub$resid
rauw$RLResid <- dgRL$resid

# Then run a model the predicts Enjoy from the partialled predictors. 

process(data = rauw, x = c("SpanResid", "LatMedResid", 
                           "LatClubResid", "RLResid"), 
                      y = "Enjoy")

# Look at model summary R-sq, and see that the semi-partial squared multiple
# correlation is .255. 

# 4.7 Semi-Partial Squared Correlations Interpretation

# The interpretation of semi-partial squared correlations follows a R^2.

# The proportion of variance explained in Rauw Alejandro enjoyment that is 
# uniquely attributed reggaeton enjoyment and Latin American cultural experience. 
#   AKA, we accounted for Age already. 

# So for our example, Reggaton enjoyment and Latin American cultural exerience
# uniquely explain 22.5% of the variation in Rauw Alejandro enjoyment, after
# accounting for age.

# 4.8 Semi-Partial Squared Correlations Equal Delta-R^2

# An interesting fact about the semi-partial squared multiple correlations is that
# they actually equal a delta-R^2 test for whatever you are partialling. 

# Consider the following full and reduced models:

# Full Model:
#   Enjoy = RL + Age + Span + LatMed + LatClub

# Reduced Model:
#   Enjoy = Age

# The reduced model only has Age. When we compare these two models, we are 
# essentially comparing how much the other set of variables (RL, Span, Latmed, 
# and Latclub) can explain Enjoy *after* already accounting for Age.

# To demonstrate, here is a delta-R^2 from PROCESS holding Age constant.

process(data = rauw, x = c("Span",
                           "LatMed", "LatClub","RL"),
        cov = c("Age"),
        y = c("Enjoy"))

# The delta-R^2 is .255. 

# As you can see this is identical to using the residuals earlier.

# ---------------------- Squared Partial Multiple Correlations ----------------------

# 4.9 Squared Partial Multiple Correlations

# To get the squared *partial* multiple correlations, there are two methods.
# One method is to use the residuals as before, but now we must also partial out
# Age from the Enjoy outcome along with the predictors. 

# With R
residEnjoy <- lm(Enjoy ~ Age,data = rauw)$residuals

# With PROCESS
dgEnjoy <- process(data = rauw, x = "Age", y = "Enjoy", save= 4)
rauw$EnjoyResid <- dgEnjoy$resid

# Then just run the models with the residuals for each of the partialled 
# variables, and get the R^2. 

# With R
partModel <- lm(residEnjoy ~ residRL + residSpan + residLatMed + residLatClub,
                    data = rauw)
summary(partModel)$r.squared

# With PROCESS
process(data = rauw, x = c("SpanResid", "LatMedResid", 
                           "LatClubResid", "RLResid"), 
        y = "EnjoyResid")

# From both of these, we can see that the partial squared correlation is .2551.
# You should ignore all other output.

# 4.10 Partial Correlations from Semi-Partial Correlations

# It's possible to get the partial squared multiple correlation directly from
# the semi-partial squared correlations.

# First, you need is the semi-partial multiple correlation (delta-R^2). 
# Then you need the R^2 from the reduced model.
#   I.e., the model where we partialled out Age from Enjoy
#   (Note, if you are following along with calculations, this is a different 
#   R^2 reduced than what we calculated above)

# To get that, we can just use
R2Age  <- summary(lm(Enjoy ~ Age, data = rauw))$r.squared

# Also take the delta-R^2 semi-partial multiple correlation

dR2 <- summary(semipartModel)$r.squared

# Then to get the partial squared multiple correlation, you need to use the
# following function:

# delta-R^2 / (1 - R^2_reduced)

# In this case we can plug in the values.

dR2 / (1 - R2Age)

# You can see this is identical to the partial squared multiple correlation 
# from Section 2.9!


# ====================== Tolerance and Variance Inflation Factor ======================

# 5.1 Tolerance

# Tolerance is the proportion of variance that is unexplained.
# In other words: Tolerance = 1 - R.j^2

# R.j^2 is the R^2 you get from predicting X.j in the model from all the other
# X's in the model. 

# Example: R.RL^2 = .002
R2.RL <- summary(lm(RL ~ Age + Span + LatMed + LatClub, data = rauw))$r.squared
R2.RL

# Essentially, this tells you how well can we predict this RL from the others
# variables?

# Tolerance thus is how much unique variance does this predictor still have?

# Example: Tolerance.RL = .998
1 - R2.RL

# Tolerance is inherently unrelated to the size of correlation between
# predictors and outcome. It's everything else.

# Since the tolerance is so high in RL's case, hardly any of the predictive ability
# for RL is due to the presence of other variables.

# Typically, this isn't a statistic of direct interest itself, but it is used
# as a transitionary step to get other statistics. 

# 5.2 VIF

# The Variance Inflation Factor (VIF) is the extent to which a slope 
# coefficient’s standard error is affected by the correlation to another variable.
# It acts as a measure of collinearity, and is measured as 

# VIF = 1 / Tolerance
# or
# VIF = 1 / (1 - R.j^2)

# In our above example, the VIF of RL is 1.002
1/(1-R2.RL)

# The larger the VIF for a specific slope, the more collinear that predictor is
# with the other predictors of the model. 

# To interpret the VIF of RL we can say that means that b1 is 1.002 times larger 
# than it would have been if RL wasn’t correlated with the other predictors. 
# However, obviously this is a tiny amount. This is because RL is pretty much not
# correlated with the other predictors at all.

# The traditional cut off for for non-ignorable collinearity is when a predictor's
# VIF ≥ 1.10. This means that the predictor's slope is 10% larger than it would 
# have been if the other predictors are in the model. 10% is a lot, so this has 
# been the arbitrary cut off used by statisticians. Like many thing in statistics
# however, there is no necessary hard and fast rule.

# Keep in mind that the higher the collinearity of the predictors, the less valid
# your inference is, since the inflation away from the null of 0 might be due to 
# the correlations rather than "true" deviations from 0. So it is important that
# the collinearity be as minimal as possible.

# 5.2 Getting VIF with vif()

# There are probably multiple packages in R that can get you these statistics,
# but the most common is to use the vif() function from the 'cars' package.

# All you need to get the VIF for each coefficient is to plug your model object
# into the function.

vif(modelFull)

# As you can see, the VIF for each variable is tiny. So none of the variables are
# collinear with each other. This can be explained by the fact that the data was
# simulated without any correlations in mind. Real data won't be this perfect.

# If you want the tolerance, you can manually calculate it.

1 / vif(modelFull)

# 5.3 Getting VIF with PROCESS

# If you want VIFs with PROCESS, you need to adjust the function to include a
# "diagnose = 4" option, which gives a lot of diagnostics, VIF and Tolerance being 
# two of the many.

process(data = rauw,
        x = c("Span", "LatMed", 
            "LatClub", "RL"), 
        y = "Enjoy",
        diagnose = 1)             # This needs to be equal to 1

# Find the table under "Variable tolerance and VIF" and you will find automatically
# calculated tolerances and VIFs for each slope.

