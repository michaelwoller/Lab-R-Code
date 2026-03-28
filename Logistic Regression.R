# ====================== Logistic Regression ======================

# Author: Michael Woller

# ====================== Load Packages ======================

#install.packages("ggformula")
#install.packages("mosaicCore")
#install.packages("lmtest")
library(ggformula) # used for gf_point
library(mosaicCore) # used for ilogit function
library(lmtest) # used for likelihood ratio test

# Make sure you load in PROCESS

# ---------------------- Specify File Path ----------------------

filelocation <- "/Users/woller/Documents/250c 2026/Lab 10/employee_data.csv" # wherever on your computer
employee <- read.csv(filelocation, header = TRUE)

# ====================== Logistic Models ======================

# We will predict the binary outcome employee turnover intention
# (0 = intend to stay, 1 = intend to leave).

# The goal of a logistic model is to rank individuals according to
# their risk of the outcome occurring.

# If the model works well:
#   Individuals where turnover actually occurred (1) should receive
#   higher predicted log-odds and probabilities.
#   Individuals where turnover did not occur (0) should receive
#   lower predicted log-odds and probabilities.

# A poor performing model is one in which our predictors cannot discriminate
# between employees who are or are not considering quitting. 
# We want to be able to appropriately sort individuals by predicting their 
# group membership, so we know which individual is higher in "risk"
  # "Risk" is a specific term in statistics, which is just the probability of 
  # an event occurring
# A bad model cannot accurately predict the individual's risk. 

# ---------------------- Logistic Mdoels With Base R ----------------------

# 1.1 Fit logistic regression with glm()

# In base R, the lm() function cannot perform logistic regression.
# Instead we use glm() (generalized linear models).

model1 <- glm(turnoverint ~ male + lmx,
              data = employee,
              family = "binomial")  # binomial family = logistic regression

# glm() can estimate many model types. The argument
# family = "binomial" specifies that we want a logistic model.
# Recall that we are predicting the probability of a binary outcome, 
# which is represented with a binomial function

# Find model output:
summary(model1)

# The output looks similar to a linear regression summary, but several parts are different:

# Coefficient tests
# The coefficient tests in logistic regression are Wald tests, which use a Z statistic
# instead of the t statistic used in ordinary least squares (OLS) regression.
# If you report these results, be sure to refer to them as Wald tests.

# Deviance
# Logistic models do not have a traditional R^2. Instead, the output reports
# deviance values for both the null model and the fitted model.

#   Null Deviance: deviance of a model with no predictors
#   Residual Deviance: deviance of the model we specified

# The Residual Deviance is equal to -2 log-likelihood (-2LL).
# These deviance values will later be used to calculate a pseudo-R^2
# and to conduct a likelihood ratio test.

# AIC
# AIC (Akaike Information Criterion) is used to compare competing models.
# Lower AIC values generally indicate a better-fitting model.
# But we won't be using these for this class.

# Fisher Scoring Iterations
# "Number of Fisher Scoring iterations" tells us how many steps were needed
# for the algorithm to estimate the model. Logistic regression does not use
# the OLS estimation procedure used in standard linear regression.
# This output just helps diagnosing issues with model estimation.

# ---------------------- Logistic Models with PROCESS ----------------------

# 1.2  Fit logistic regression with PROCESS

# PROCESS automatically detects when the outcome variable is binary.
# If Y only contains 0s and 1s, PROCESS will automatically run a
# logistic regression model.

process(data = employee, y = "turnoverint", x = c("male", "lmx"))

# Coding Scheme
# At the top of the output, PROCESS shows how the outcome variable was coded.
# This only matters if your binary outcome uses values other than 0 and 1
# (for example, 1 and 2). The coding scheme tells you which value was treated
# as the event being predicted.

# Model Summary
# The Model Summary section reports the residual deviance (-2LL) and the
# likelihood ratio test for the overall model (ModelLL, df, and p-value).
# We will discuss likelihood ratio tests later.

# Pseudo-R^2 Measures
# McFadden, Cox-Snell, and Nagelkerke are all types of pseudo-R^2 measures
# used to describe model fit in logistic regression. 
# We will discuss pseudo-R^2's later.

# Remaining Output
# The rest of the output (coefficients, standard errors, Wald tests, etc.)
# follows the same structure as glm() regression output above.

# ====================== Coefficient Interpretations, Odds Ratios, and Probabilities ======================

# 2.1 Interpreting the log-odds coefficients

# By default, logistic regression coefficients are reported in log-odds units.

# Binary predictor (male: 0 = female, 1 = male):
#    Holding LMX constant, males are expected to differ from females by -.144 in the
#    log-odds of turnover intention = 1. Because the coefficient is negative here,
#    males have lower log-odds than females.

# Continuous predictor (lmx):
#    Between two females that differ in one unit of LMX, the log-odds of the outcome 
#    is expected to be .12 less for the female with higher lmx

# Intercept:
#    When all predictors equal 0, the intercept gives the predicted log-odds of
#    turnover intention = 1. In this example, that is the predicted log-odds for
#    females with LMX = 0.

# The problem with log-odds is that they are not very intuitive in practice.
# For that reason, we often convert them into odds ratios or probabilities.

# As far as I know, PROCESS does not directly report the quantities in the
# following sections, so you may need to calculate them manually.

# ---------------------- Odds Ratios ----------------------

# 2.2 Calculating and Interpreting Odds Ratios

# We can convert log-odds coefficients into odds ratios by exponentiating them with exp().
exp(model1$coef)

# Odds ratios tell us the multiplicative change in the odds of turnoverint = 1
# relative to turnoverint = 0.

# Interpretation rule:
# Values greater than 1 indicate higher odds of turnoverint = 1.
# Values less than 1 indicate lower odds of turnoverint = 1.
# Because this is a ratio, the interpretation is always with respect to the
# outcome in the numerator.

# Example interpretations:

# Binary predictor:
#   Holding LMX constant, males are predicted to have .86 times 
#   the odds of turnoverint = 1, which means their odds are
#   predicted to be about 14% lower than females.

# Continuous predictor:
#   For two females who differ in an LMX value of 1, the individual 
#   with the larger LMX is predicted to be .88 times lower in odds than the other,
#   which means they are predicted to be about 12% lower in odds.

# Intercept:
#   Exponentiating the intercept gives the baseline odds of turnoverint = 1 when
#   all predictors equal 0. For example, if exp(b0) = 1.57, then the baseline
#   odds are 1.57 to 1. You can understand this as the ratio being (exp(b0) / 1).
#   This is why the odds ratio of the intercept is usually not interpreted much in practice.

# 2.3 Odds Ratios in the Opposite Direction

# If you want to interpret the odds ratio in the opposite direction
# (e.g., comparing males to females instead of females to males),
# you can take the inverse of the odds ratio.

# Example:
1 / 0.86
# Equals 1.162791

# Interpretation:
#   For a male and female with equal LMX values, being male increases
#   the odds of turnoverint = 1 by a factor of 1.16 compared to being female.
#   In other words, the odds are about 16% higher.

# Reversing the odds ratio does not change the p-value.
# The confidence interval conveys the same information as well,
# although its endpoints will be inverted.


# 2.3 Odds Ratio Confidence Intervals 

# Significance tests for odds ratios follow the same results as the
# log-odds coefficients. However, if you want confidence intervals
# for the odds ratios, you can exponentiate the confidence intervals
# from the log-odds model.

exp(confint(model1))

# For odds ratios, statistical significance is evaluated against 1
# (not 0, as in linear regression). If the confidence interval
# contains 1, the effect is not statistically significant.

# In this example, only LMX has a confidence interval that does not
# contain 1, indicating it is statistically significant.

# ---------------------- Converting Log-Odds to Probabilities ----------------------

# 2.4 Probabilities 

# Sometimes, it might be preferable to give concrete probabilities rather than more abstract odds.
# You can convert predicted log-odds into probabilities manually.
# To do this, plug specific predictor values into the logistic regression equation
# to get the predicted log-odds (sometimes called eta), then convert eta to a probability.

# In other words:
# 1. Compute the predicted log-odds
# 2. Convert the log-odds to probability with:
#    p = 1 / (1 + exp(-eta))

# Example 1:
# What is the predicted probability that a MALE with LMX = 0 has turnoverint = 1?

eta1 <- model1$coef[1] + (model1$coef[2] * 1) + (model1$coef[3] * 0)
p1 <- 1 / (1 + exp(-eta1))
p1

# Note that the first line of code is just the estimated logistic regression 
# equation with the specific values:
0.4479383 + (-0.1447437 * 1) + (-0.1260464 * 0)

# The probability is .57. So there is about a .57 predicted probability that a male with LMX = 0
# has turnoverint = 1.

# Example 2:
# What is the predicted probability that a FEMALE with LMX = 3 has turnoverint = 1?

eta2 <- model1$coef[1] + (model1$coef[2] * 0) + (model1$coef[3] * 3)
p2 <- 1 / (1 + exp(-eta2))
p2

# So there is about a .51 predicted probability that a female with LMX = 3
# has turnoverint = 1.

# If you do not want to convert these manually, you can use the ilogit()
# function from the 'mosaicCore' package. ilogit() converts log-odds to probabilities.

ilogit(eta1)
ilogit(eta2)

# Alternatively, if you want to convert probabilities back to log-odds,
# you can use the logit() function from 'mosaicCore'.

logit(p1)
logit(p2)

# These values should match the respective eta values above.

# ====================== Pseudo-R^2 ======================

# 3.1 Pseudo R^2's

# Logistic models do not have residuals in the same way OLS regression does,
# so the usual R^2 is not available. Instead, we use pseudo-R^2 measures.

# Pseudo-R^2 measures quantify how much the model improves prediction
# relative to a null model (a model with only an intercept and no predictors).
# In other words, they measure how much better our model fits the data
# compared to a model that assumes the same probability for every observation.

# Some pseudo-R^2 measures are scaled so that their values roughly resemble
# the 0–1 interpretation of R^2 in linear regression.
  # Examples include Cox–Snell and Nagelkerke pseudo-R^2.

# However, the McFadden pseudo-R^2 is often considered the most "honest"
# because it does not attempt to mimic the variance-explained interpretation
# of OLS R^2. Instead, it directly measures the proportional improvement
# in model likelihood relative to the null model.
# Rough intuition:
#    larger pseudo-R^2 values indicate that the predictors help the model
#    predict the outcome better than the null model
#    (aka assuming no variables can predict the outcome).

# This is often what is desired from the pseudo-R^2. 
# Thus the McFadden statistic will suffice, and we will focus on it for the class

# ---------------------- With Base R ----------------------

# 3.2 Calculating Pseudo-R^2 in Base R

# McFadden pseudo-R^2 calculation:
dev.null <- model1$null.deviance      # deviance of the null model
dev.proposed <- model1$deviance       # deviance of the fitted model

(dev.null - dev.proposed) / dev.null # Calculated McFadden pseudo-R^2

# Interpretation is broadly similar to R^2 in linear regression.
# For example, if this value is about .02, then the predictors are estimated
# to explain about 2% of the variation in employee turnover intention.

# However, keep in mind that this is a pseudo-R^2, so it should not be treated
# as exactly the same thing as the usual OLS R^2. 
# Hence why I use "estimated" in my wording.  

# ---------------------- With PROCESS ----------------------

# 3.3 Calcualting Pseudo-R^2 with PROCESS

# PROCESS reports multiple pseudo-R^2 measures by default.
process(data = employee, y = "turnoverint", x = c("male", "lmx"))

# Look under "Model Summary" to find McFadden's pseudo-R^2
# along with Cox-Snell and Nagelkerke pseudo-R^2 values.
# However, we will only focus on the McFadden output for this class.

# ====================== Likelihood Ratio Test ======================

# 4.1 Likelihood Ratio Tests

# The likelihood ratio test (LRT) is a model comparison test.
# By default, it compares your fitted model to the null model
# (a model with only an intercept and no predictors).

# Conceptually, this asks:
# Does the model with predictors fit significantly better than the null model?

# This is similar to the overall model F test in linear regression.

# You can also use an LRT to compare two nested models.
# "Nested" means that one model is just a reduced version of the other,
# with a subset of the predictors.

# This is similar to a delta-R^2 test in linear regression.

# ---------------------- With Base R ----------------------

# 4.2 Calculating Likelihood Ratio Tests with Deviance Values

# One way to do this in base R is to work with deviance values.

# Deviance is equal to -2 log-likelihood (-2LL), so differences in deviance
# can be used to compute the likelihood ratio test.

dev.null <- model1$null.deviance      # deviance of the null model
dev.proposed <- model1$deviance       # deviance of the fitted model

# Manually calculate the p-value for the likelihood ratio test.
# The test statistic is the difference in deviance values and follows
# a chi-square distribution.

1 - pchisq(dev.null - dev.proposed,    # Difference of deviances
           df = length(model1$coefficients) - 1) # df = # of coefficients - 1

# The output is the p-value for the overall model test.
# If p < .001, the fitted model predicts the outcome significantly better
# than the null model. So since our value is significant, our model predicts the better
# significantly better than the empty null model

# 4.3 Calculating Likelihood Ratio Tests with R Functions

# Another approach is to fit the null model directly and compare the two models.

model0 <- glm(turnoverint ~ 1,        # intercept-only model (denoted with ~ 1)
              data = employee,
              family = binomial)

# Use anova() to calculate LRT between fitted model 1 and null model 0:

anova(model0, model1, test = "Chisq")

# This gives the same likelihood ratio test in a more standard format.
# The "Deviance" is the LRT test statistic
# Make sure test = "Chisq" is specified.

# You can also use lrtest() from the 'lmtest' package.

lrtest(model0, model1)

# 4.4 Comparing Nested Predictor Sets with R Functions

# If you want to compare predictor sets, fit two nested models and then
# compare them with an LRT. 
# This method is akin to delta-R^2 test of regular OLS regression.

model2 <- glm(turnoverint ~ jobsat,   # model with job satisfaction only
              data = employee,
              family = binomial)
summary(model2)

model3 <- glm(turnoverint ~ male + lmx + jobsat,   # add male and lmx
              data = employee,
              family = binomial)
summary(model3)

# To run the LRT, compare the two models.
# Make sure one model is nested within the other (Model 2 is nested in Model 3.

anova(model2, model3, test = "Chisq")

# If this test is not significant, then adding male and lmx does not
# improve prediction beyond the model that already contains jobsat.

# In other words, male and lmx are not providing much additional predictive
# value once job satisfaction is already in the model.

# This matches the model summaries above, where jobsat is significant
# but male and lmx are not.

# In this dataset, job satisfaction is a much stronger predictor of turnover
# intention than gender or leader-member exchange.

# ---------------------- With PROCESS ----------------------

# 4.5 Likelihood Ratio Tests in PROCESS

# PROCESS automatically reports the LRT for the full fitted model versus the null model.
# If you want to test whether a specific set of predictors adds explanatory value,
# put that predictor set in "x =" and the predictors you want to control for in "cov =".

process(data = employee,
        y = "turnoverint",
        x = c("male", "lmx"),   # The variables to be tested
        cov = c("jobsat"))      # The variable(s) consistent across the models

# At the bottom of the output, look for:
# "Likelihood ratio test for variables in X set"

# This tests whether male and lmx improve prediction beyond the model
# that already includes jobsat, which is the same logic as the nested
# model comparison above.

# This test is not significant, then adding male and lmx does not
# improve prediction beyond the model that already contains jobsat.

# In other words, male and lmx are not providing much additional predictive
# value once job satisfaction is already in the model.


# ====================== Plotting Logistic Regression ======================

# 5.1 Plotting Log-Odds and Probabilities

# Since the outcome variable is binary and logistic models do not produce
# traditional residuals, we are somewhat limited in the usual regression plots.
# However, we can still visualize how well the model separates the two outcome
# groups using predicted values.

# 5.2 Computing and Ranking Probabilities 

# First compute the predicted probabilities of turnover for each individual
employee$pred <- predict(model1, type = "response")

# Next rank individuals based on the size of thei predicted probability
employee$predrank <- rank(employee$pred)

# rank() assigns each individual a rank from the lowest predicted probability
# of turnover to the highest predicted probability.

# Example:
# If individual 1 has a rank of 158, this means they have the
# 158th lowest predicted probability of turnover in the dataset.

# If you see a decimal rank (e.g., 417.5), it means that observation
# is tied with another individual who has the same predicted probability.

# 5.3 Plot Predicted Probabilities

gf_point(pred ~ predrank, data = employee,
         color = ~as.factor(turnoverint))

# This plot allows you to visually inspect how well the model separates
# the two turnover groups.

# X-axis: rank of individuals based on predicted probability
# Y-axis: predicted probability of turnover
# Color: actual observed outcome of individual (0 = stay, 1 = turnover intention)

# 5.4 Interpreting the Plot

# Ideally we would like to see individuals with turnoverint = 1 appearing
# mostly on the right side of the plot (higher predicted probabilities),
# while individuals with turnoverint = 0 appear mostly on the left side.

# If this pattern occurs, it means the model is effectively ranking
# higher-risk individuals above lower-risk individuals.

# In this example, the model does push some higher-risk employees
# toward the right side of the plot, but the separation between
# employees with and without turnover intention is modest.

# 5.5 Model Goals from Plots

# Look back at 1.1 where I discuss the goals of a logistic regression. 
# Recall that the goal of a logistic model is to rank individuals according to
# their risk of the outcome occurring (probability of turnoverint = 1).

# If the model works well:
#   Individuals where turnover actually occurred (1) should receive
#   higher predicted probabilities.
#   Individuals where turnover did not occur (0) should receive
#   lower predicted probabilities.

# Thus our plot should show all the turnoverint = 1 dots on the right, since that 
# would mean they have a higher predicted probability. 
# We WANT them to have a higher *predicted* probability, since we KNOW 
# that they are in turnoverint = 1.

# Since our model isn't the best at doing that 
# (it only slightly discriminates forturnoverint = 1),
# we can't say our model is well performing.
# This matches up with what we've seen from the model summary, 
# in which the coefficients weren't very effective at predicting the outcome.
#    Pseudo-R^2 of .02
