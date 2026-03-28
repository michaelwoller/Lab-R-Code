# ====================== Regression Diagnostics ======================

# Author: Michael Woller

# ====================== Load Packages ======================

#install.packages("ggplot2") # For plotting diagnostics
#install.packages("sandwich") # For robust standard errors
#install.packages("lmtest") # For robust standard errors
#install.packages("parameters") # For boostrapped standard errors
library(ggplot2)
library(sandwich)
library(lmtest)
library(parameters)

# Remember to load in PROCESS. 

# ---------------------- Specify File Path ----------------------

filelocation <- "/Users/woller/Documents/250c 2026/Lab 5/Big5data.csv" # wherever on your computer
big5 <- read.csv(filelocation, header = TRUE)

# ====================== Assumptions of Regression ======================

# 1.1 Assumptions 

# Linear regression is a great tool of providing inference into the world from
# observations of data. However, it requires certain conditions to be true about
# the data in order for the estimates and inference you have to be optimal and 
# valid.

# These conditions are "assumed" to be true whenever you use regression. If it 
# turns out they are not true, then there will be flaws in the model and its 
# ability to provide sound interpretations and inference. Sometimes the issues 
# with assumption violations are negligible. Sometimes they completely ruin the
# model.

# In order of the "most important" to "least important", the main assumptions
# of linear regression are 

# 1) Linearity: Linear relationship between X’s and Y.
#     Necessary for meaningful coefficients that describe a linear relationship.
# 2) Homoskedasticity: Consistent variance of the Y residuals.
#     Needed for valid standard errors to get valud inference.
# 3) Normality: Y residuals are normally distributed.
#    Needed for inference to be valid. This is arguably not important as regression
#    is relatively robust against this violation (meaning the negative effect it 
#    has is minor).
# 4) Independence: Each case is independent from other cases.
#    Not really necessary most of the time.

# A golden rule is that violations of these assumptions, if they exist, are usually
# pretty obvious. Small deviations of each are likely to occur, but if true
# violations are present, they will be pretty evident.

# 1.2 Diagnostic Material 

# Lets say for this lab, we are interested in the degree to which existential 
# well being can be predicted by the Big 5 personality traits. 

# In order to make sure the inference and interpretations of this model are valid,
# we will have to check the diagnostics for each of the assumptions to see if they
# hold or not. 

# The majority of diagnostics checks comes from using residuals and/or predicted
# scores along with the predictors. So you will need to save these somehow.

# Save in R
model1 <- lm(ExistentialWellBeing ~ OpenMindedness + Conscientiousness + 
               Extraversion + Agreeableness + NegativeEmotionality, data = big5)
big5$resid <- resid(model1)
big5$pred <- predict(model1)

# Save in PROCESS
dg <- process(data = big5, x = c("OpenMindedness", "Conscientiousness", "Extraversion",
                                 "Agreeableness", "NegativeEmotionality"),
              y = c("ExistentialWellBeing"), 
              save = 4) # save diagnostic material

# Either will work, but I will be using the diagnostics in the dg save object
# for the following plots. You can use whatever plotting package you want, but 
# I will be using the 'ggplot2' package.

# ====================== Linearity Assumption ======================

# 2.1 Linearity Assumption

# To check this assumption, one needs to plot the predicted scores of the model
# against the residuals.

# What you want to see is if the distribution of the residuals has a mean of 0. 
# If this doesn't seem the case, that means that the linear line didn't fit
# the data particularly well. The closer the data can be fit with a straight linem
# the more the residuals will be centered around 0. So the less they are centered
# around 0, the more a linear line is a poor fit, which means a non-linear curve 
# would probably be better.

ggplot(data = dg, mapping = aes(x = pred, y =  resid))+
  geom_point()

# From this plot, there is no clear non linearity as the residuals are generally
# centered around 0. 

# You can do this check with the predictors too, however this is largely to determine
# where non-linearity is coming from if there is true non-linearity. If there IS
# non-linearity, it might stem from one or more of the predictors (but this 
# doesn't have to be the case).

# Plotting each  predictor against the residuals
ggplot(data = dg, mapping = aes(x = OpenMindedness, y =  resid))+
  geom_point()
ggplot(data = dg, mapping = aes(x = Conscientiousness, y =  resid))+
  geom_point()
ggplot(data = dg, mapping = aes(x = Extraversion, y =  resid))+
  geom_point()
ggplot(data = dg, mapping = aes(x = Agreeableness, y =  resid))+
  geom_point()
ggplot(data = dg, mapping = aes(x = NegativeEmotionality, y =  resid))+
  geom_point()

# None of the predictors really have the residuals deviating away form 0. This is
# to be expected, since there was no non-linearity in the residual vs predicted 
# score plot. 

# 2.2 Linearity Violation

# If you do violate linearity, there's not much you can do with your model.
# You will have to fit some kind of non-linear model to it instead, of which
# there are a multiple options that we will cover in the future. 

# ====================== Homoskedasticity Assumption ======================

# 3.1 Homoskedasticity Assumption

# This assumption (sometimes called homogeneity) is about the residuals having a
# constant variance across all values. 

# Let's say you have a outcome that ranges from 1-10. Heteroskedasticity might mean
# the variation of your predicted scores could be very consistent on the upper
# end for scores 8, 9, and 10, but on the lower end of 1, 2, and 3, your model
# might become very inconstant with its predictors. You want the consistency
# to be constant for your inference from the model to be valid. If your predictions
# are NOT consistent across all scores, what good is your model at inferring about
# the population?

# Heteroskedasticity distorts the standard errors of the coefficients,
# which will lead to distortions of anything that relies on the standard errors
# (test statistics, p values, confidence intervals, etc.). So heteroskedasticity
# can become problematic for inferential purposes.

# There's many causes of heteroskedasticity including cluster-dependency, 
# ceiling/flooring effects, or just having a poorly fit model in general.

# 3.2 Plotting homoskedasticity

# Inference on homoskedasticity is most commonly done in residual vs predicted score
# plots. What you need to look for is if there is any deviations from constant
# variance of the points across the X axis.

ggplot(data = dg, mapping = aes(x = pred, y =  resid))+
  geom_point()

# There seems to be a bit of heteroskedasticity, as the left side of the plot is much more
# tight than the right side of the plot. The variance on the left is tighter than
# the variance on the right as the points on the right are further spread out.

# However, this is probably very minor and negligeable. I would say than that the
# assumption upholds. Heteroskedasticty tends to be very obvious when it manifests.
# I would look up heteroskedastic plot examples if you are curious, as there are 
# many. 

# If you violate homoskedasticity, there's a few options. You can use robust
# standard errors, bootstrapped standard errors, or weighted least squares (WLS)
# regression. We will cover the first two options. 

# WLS is usually out of grasp for psychological research since it
# requires you to be able to describe the exact form of the error variance
# (i.e., how the variability in the outcome changes as a function of predictors).
# In practice, this variance structure is rarely known and is difficult to 
# estimate correctly, so misspecification can actually make results worse. 
# Robust and bootstrapped standard errors are more commonly used because they do
# not require specifying a variance model and still provide valid inference 
# under heteroskedasticity.

# ---------------------- Robust Standard Errors ----------------------

# 3.3 Robust Standard Errors

# A lot of my dissertation is on robust standard errors, so bare with the length
# of my exposition of this section. I'll try to keep it breif.

# Robust standard errors are adjustments to the standard error formulas of the 
# coefficients that accounts for heteroskedasticity. Basically, it solves the 
# distortions that heteroskedasticity causes and fixes inference issues.

# Robust standard errors are so easy to apply and have virtually no downsides 
# if applied to any model, that many researchers actually suggest to apply them
# universally. The only downside is if the data is truly homoskedastic, your
# coefficient t tests will be less powerful. 

# But practically speaking, you could most likely apply these without even looking
# at the heteroskedastic plot. Only if the heteroskedasricity is very extreme
# or sample sizes were very small would robust standard errors falter and could
# not be used as a band aid solution.

# There are many types of robust standard errors: HC0, HC1, HC2, HC3
#   "HC" comes from "heteroskedastic consistent" because the standard errors
#   are consistent under heteroskedasticity

# Each has its uses, but generally, for regular linear regression, its typical to
# use HC2 standard errors. The others have their use cases, but HC2 is the most 
# widely applied, and unless you are doing specific things, you don't need to really
# worry about them. 

# There are even other forms of robust standard errors that tackle different issues
# than just heteroskedasticity, such as "cluster robust" standard errors, which
# adjust for non-independence issues. But that's besides the content of this lab.

# Note, robust standard errors will NOT change the coefficients, only their 
# standard errors. So applying robust standard errors will change *inference*
# but it will not change *interpretations*. 

# 3.3 Robust Standard Errors with vcovHC() and coeftest()

# There are probably many methods to get these standard errors in R, but I will
# use the hvcovHC() function from the 'sandwich' package. This can give you 
# many types of robust standard errors. This has to be used in conjunction with
# the coeftest() function from the 'lmtest' package, which just lets you do 
# readjusted coefficient tests.

# You can use the coeftest() function in this way, specify that you are using
# "HC2" robust standard errors in the "vcov =" line.

coeftest(model1, vcov = vcovHC(model1, type = "HC2"))

# As you can see, the estimates did not change, but the standard errors changed, 
# along with the t statistics and p values.

# The standard errors with the robust standard error adjustments will ALWAYS be
# larger than non-robust standard errors. Thus the resulting p values with robust
# standard errors will be larger. This is what makes robust standard error
# tests less powerful. Since if there was a true effect, it would have more 
# difficulty detecting it. However, if there was heteroskedasticity in the data,
# the results using HC2 would be more accurate.

# The larger the change of the standard errors before and after applying 
# the HC2 correction, the more of a deal heteroskedasticity was for the model. 
# However, the changes we observe are minimal, and no changes to the inferential
# conclusions. So there probably wasn't any heteroskedasticity in the data. 
# This aligns with what we observed visually in the diagnostic plot. 

# If you want adjusted confidence intervals with the HC2 standard errors, use the
# coefci() function from 'lmtest'. 

coefci(model1, vcov. = vcovHC(model1, type = "HC2"))

# The resulting intervals are "robust" to heteroskedasticity.

# 3.4 Robust Standard Errors with PROCESS

# Getting robust standard errors in PROCESS is pretty easy. All you need to do
# is specify "hc = " to whatever type of robust standard error you would want.

process(data = big5, x = c("OpenMindedness", "Conscientiousness", "Extraversion",
                                  "Agreeableness", "NegativeEmotionality"),
               y = c("ExistentialWellBeing"), 
               hc   = 2)          # HC2 standard errors

# Note how in the output, the standard errors now say se(HC2). The resulting
# t statistics, p values, and confidence intervals have also changed to be
# more robust (larger p values, and larger confidence interval widths). 
# See section 3.3 for descriptions of the changes and why they changed.

# ---------------------- Bootstrapped Standard Errors ----------------------

# 3.5 Boostrapped Standard Errors

# Bare with me for a second, since bootstrapping might be a bit abstract for
# those it is new.

# Another form of standard error correction is the bootstrapped standard error.
# Unlike robust standard errors, which swap out the model standard errors with
# something completely new (after adjusting for heteroskedasticity), bootstrapped
# standard errors are built by repeatedly resampling your data (with replacement),
# refitting the model many times, and looking at how much the estimated coefficients
# vary across those samples. In other words, instead of relying on assumptions about
# the residual distribution, bootstrapping uses the data itself to approximate the
# sampling variability of the estimates. This makes it a flexible approach that can
# work well even when assumptions like normality or homoskedasticity are violated,
# though it can be more computationally intensive.

# The logic is as follows. You assume that your data is representative of 
# the population. Given this assumption, you can pretend that your sample IS the 
# population. Then you can take repeated sub-samples of the original sample to 
# make inferences about your original sample by generating a sampling distribution.

# Recall a few things, that a sampling distribution is a distribution of many SAMPLES.
# This distribution of many SAMPLES can make direct inferences about the POPULATION, 
# and the standard deviation of the sampling distribution is the standard error. 

# Now in the above paragraph, replace POPULATION with "your sample" and replace
# SAMPLES with "sub-samples," and that is essentially what you are doing with a
# bootstrap. You are simulating the population with your data.

# Aka:
#   Your Sample -> The Hypothetical Population
#   Sub-Samples -> Hypothetical Repeated Samples
#   Distribution of Hypothetical Repeated Samples -> Hypothetical Sampling Distribution

# Thus you get a standard error which turns out fo be very accurate to the shape
# of true sampling distributions from the actual population. If this doesn't make
# sense to you still, I would recommend finding a video on the topic because there
# is only so much of this that is easy to explain through pure written text. 

# There are many, many forms of bootstrapping techniques and tests with them, ask
# Amanda for many options. I will cover the most simplest version. 

# 3.6 Bootstrap Set Up

# Since with bootstraps, you will be generating random sub-samples, you need to 
# set a seed for replicability. The seed number is arbitrary.

set.seed(8675309)  

# The algorithm of the bootstrap follows these steps:
#   1. Take your original dataset of size N.
#   2. Draw a new sample of size N *with replacement*
#       (each case is put back after being selected, so it can be chosen multiple times)
#   3. Fit your regression model to this resampled dataset.
#   4. Save the coefficient estimates.
#   5. Repeat steps 2–4 many times (e.g., 1000+ samples).
#   6. Compute the standard deviation of the saved estimates 
#        - this is the bootstrapped standard error.

# 3.7 Bootstrap with model_parameters()

# There's many, many ways of implementing a bootstrap, but most of them require
# a decent bit of knowledge on how to write functions. If you want to, you could
# code the whole thing by hand (I have an example later on), but the most basic
# function I could find is model_parameters() from the 'parameters' package.

# What you need to do is insert your model object, into the model_parameters()
# function, specify that boostrap = TRUE, and specify the number of iterations
# (the amount of resamples). Generally for a regression model as simple as this,
# 1k iterations is fine. 

model_parameters(model1, bootstrap = TRUE, iterations = 1000)

# It will take a few seconds for the bootstrap to run and estimate since it has to
# essentally generate 1000 model estimates. The more complicated the model or the
# more iterations you have, the longer this wait will be. So keep that in mind.

# From the output, the "Coefficient" column has the coefficients of the regression
# output. The "95% CI" column has the bootstrapped confidence intervals, and the 
# "p" column has the bootstrapped p values. 

# Generally speaking, you should focus on the 95% CI's for inference. There's a lot
# of issues around p values of bootstrapped standard errors since there is no 
# universally agreed method on how to calculate them. So you just ignore them 
# and focus on the confidence intervals instead.

# Inference using these bootstrapped standard errors proceeds as normal.

# 3.8 Bootstrap with PROCESS

# Getting boostrapped standard errors with PROCESS involves fitting a model 
# and specifying "modelbt = 1". If you do "save = 1" you will also get the 
# the individual boostrapped estimates in the diagnostics output. This is not needed
# but can be helpful for visualization if you wanted to (as I will show in the
# next subsection). 

# By default, PROCESS will use 5,000 iterations. There might be a way to adjust 
# this, but I am not sure how. 

dg2 <- process(data = big5, x = c("OpenMindedness", "Conscientiousness", "Extraversion",
                                  "Agreeableness", "NegativeEmotionality"),
               y = c("ExistentialWellBeing"), 
               save = 1,         # save bootstrapped estimates
               modelbt = 1)     # specify bootstrap for 5k iterations SE

# The usual model output will pop up immediately, and at the bottom, you can see
# a loading bar for the bootstrap process. After it finishes, the bootstrapped
# estimates will appear in a table with the following:
# Coeff
#   The model coefficients (same as without bootstrapping).
# BootMean
#   The mean of the boostrapped sampling distributon (not important).
# BootSE
#   The bootstrapped standard error.
# BootLLCI/BootULCI
#   The upper and lower limits of the 95% bootstrapped confidence interval.

# There is no p value for the bootstrapped standard errors (see 3.7 subsection
# for the reason why). 

# Inference of the coefficients should come from the bootstrapped confidence 
# intervals, but other than that can proceed as normal.

# 3.9 Visualizing Bootstrapping

# If you want to visualize the bootstrap, you just need to save the bootstrapped
# estimates somehow and plot them in a histogram. 
# These are where the 95% bootstrapped confidence intervals are taken from.

hist(dg2$ExistentialWellBeing_OpenMindedness, breaks = 100) # OpenMindedness
hist(dg2$ExistentialWellBeing_Conscientiousness, breaks = 100) # Conscientiousness
hist(dg2$ExistentialWellBeing_Extraversion, breaks = 100) # Extraversion
hist(dg2$ExistentialWellBeing_Agreeableness, breaks = 100) # Agreeableness
hist(dg2$ExistentialWellBeing_NegativeEmotionality, breaks = 100) # NegativeEmotionality

# This is the "hypothetical sampling distribution" from the simulation process.
# Yours will look slightly different depending on your seed. 

# 3.10 Bootstrap by Hand

# Just as a quick tech demo, here is a bootstrap by hand. You don't need to know
# how to do this, but in case you are curious, you can just write it all in a 
# big 'for' loop. You can probably get AI to do this in a few seconds if you wished.

B <- 1000       # Iterations
N <- nrow(big5) # Bootstrap sample size

# Store bootstrapped coefficients in empty object
boot_coefs <- matrix(NA, nrow = B, ncol = 6)
colnames(boot_coefs) <- names(coef(
  lm(ExistentialWellBeing ~ OpenMindedness + Conscientiousness +
       Extraversion + Agreeableness + NegativeEmotionality, data = big5) 
))

# Bootstrap loop. This is the start of the bootstrap.
for (b in 1:B) {
  
  # Resample rows with replacement
  idx <- sample(1:N, size = N, replace = TRUE)
  boot_data <- big5[idx, ]
  
  # Fit our model
  model_boot <- lm(ExistentialWellBeing ~ OpenMindedness + Conscientiousness +
                     Extraversion + Agreeableness + NegativeEmotionality,
                   data = boot_data)
  
  # Store coefficients
  boot_coefs[b, ] <- coef(model_boot)
}
# End of bootstrap loop. This is the end of the bootstrap.
# The bootstrap is that simple. 
# If you boil it down by removing line breaks, the bootstrap is only four lines
# of code. Next is to get the actual standard errors and confidence intervals

# Bootstrap standard errors
boot_se <- apply(boot_coefs, 2, sd)

# Bootstrap 95% CI 
boot_ci <- apply(boot_coefs, 2, quantile, probs = c(.025, .975))

# Combine results in one data.frame
results <- data.frame(
  Estimate = coef(model1),
  Boot_SE = boot_se,
  CI_lower = boot_ci[1, ],
  CI_upper = boot_ci[2, ]
)

results

# ====================== Normality Assumption ======================

# 4.1 Normality Assumption

# The normality assumption assumes that the distribution of the residuals is normal.

# A violation of normality can lead to distortion of your coefficient inference.
# It could also lead to potential bias of your coefficients too, meaning that
# *both* the inference and interpretations would be off. However, the effects
# are pretty minor, especially on coefficient bias. This is unless your
# residuals are very non-normal.

# Normality of the predictors can be tested too as non-normality of the residuals
# might be stemming from here, but the assumption is from the residuals. The 
# predictors can be non-normal (in fact, you wouldn't be able to have categorical
# predictors if this wasn't the case), but they might just be cause of non-normal
# residuals. If you are so interested, you can do histogram diagnostics on them 
# too, but if they are non-normal, this is NOT a violation for regression. 

# 4.2 Normality Histograms

# If you want a general but not full-proof idea of if your residuals are normal, 
# you can make a histogram.

hist(dg$resid, breaks = 50)

# From this visualization, the distribution of the residuals seem decently 
# normal but has a clear thicker tails to the right (more points to the right).

# However, its very hard to tell sometimes from these types of visualizations, so
# we can turn to a much better form of visualization in a QQ-plot.

# 4.2 QQ-plots

# QQ-plots depict the distribution of residuals in terms of where they are in 
# a distribution (see histogram above) and where they *should* be if they were 
# perfectly normal.

# This lets us see a very objective measure of how non-normal the residuals are, 
# where non-normality occurs, and what type of non-normality is it.

# Use the qqnorm() and qqline() function to construct the QQ-plot of the residuals.

qqnorm(dg$resid)
qqline(dg$resid)

# In a perfect world, the points on the QQ-plot would lie directly on the line.
# Any deviation of the line means some amount of deviation from normality. 
# If the residual point is below the line, that means that the residual if further 
# to the left on the distribution than where it should be if it was perfectly normal.
# If the point is above the line, that means the residual is further to the right
# than where it should be. Based on this knowledge, you can infer the general
# description of the residual distribution.

# From the QQ-plot we found, if you look at the lowest residuals furthest to the
# left on the X-axis, we can see that they are below the line. Therefore, these
# residuals are further out to the left on the distribution, meaning that
# they are more extreme than we would like. Aka, the left tail is thick.
# Though not by a whole lot, since they aren't that far below the line. 
# Also, you can always expect the points at the  end of the QQ-plot to deviate 
# from the lines somewhat. It's pretty much  impossible to fill the tails of 
# the distribution perfectly, there is always going to be too much or too little.

# Turning to the right side of the distribution, since we see the points below
# the line, this means the right side of the distribution is further to the left
# than what we would like it to be. This means there is less volume in the 
# right tail, which makes it a thin tail.

# This brings up an issue I see a lot of students make. Don't confuse long tails
# in distributions with "thin" tails just because they look skinny. If you 
# go back to the histogram, you might be tempted to say the left side is thin
# because it looks skinny, but its actually fat because there are more points
# in it than what a true normal distribution should be.

# Going back to the qq plot, we can see that the center of the distribution is 
# very consistent with the line, so there aren't that many deviations from
# normality.

# In conclusion, I would say that our residual distribution is perfectly normal.
# In fact, the QQ-plot has to start to look pretty egregious for "non-normality" 
# that you see to start becoming an issue. If this happens, you WILL notice it
# and it will be obviously non-normal. If you are ever are on the "maybe it is
# maybe it is not ok" stance, the QQ-plot is most likely ok.

# You can also plot QQ-plots with ggplot() if you want more custom graphics
# using the stat_qq() and sat_qq_line() additions.

ggplot(data.frame(r = dg$resid), aes(sample = r)) +
  stat_qq() +
  stat_qq_line()

# 4.3 Normality Violations

# If you have violations of normality, you can consider a few options.

# One is a logarithmic transformation, where you take the natural log of your
# variable. This is very common for certain types of variables like income, 
# which are naturally non-normal. This can help the residual distribution.
# You can also log-transform predictors to help with interpretation, and potentially
# help with the residual distribution too, if that variable is a source of 
# residual non-normality. 

# To log-transform, simply use the log() function to get a new variable. 

# For lm(), you can use log() in the model if you don't want to save a new
# variable
model2 <- lm(log(ExistentialWellBeing) ~ OpenMindedness + Conscientiousness + 
               Extraversion + Agreeableness + NegativeEmotionality, data = big5)
summary(model2)

# For PROCESS, you need to save a new log variable
big5$logExist <- log(big5$ExistentialWellBeing)
process(data = big5, x = c("OpenMindedness", "Conscientiousness", "Extraversion",
                                 "Agreeableness", "NegativeEmotionality"),
              y = c("logExist"))

# Just keep in mind that all interpretations are then in LOG-units of the outcome.
# E.g., Between two individuals who differ in OpenMindedness by 1 point, the more
# open minded person is expected to be .01 lower in log-units of existential well
# being. These coefficients are essentially percentage changes of the outcome.

# If you want to transform the slope to ge tthe exact percent change of Y, 
# you can use the equation to get a percent change of the outcome
# %ΔY = (exp(b1)−1)×100

(exp(-.013)-1)*100

# So for two individuals who differ in OpenMindedness by 1 point, the more open 
# minded person is expected to be 1.29% lower in existential well being.

# If you log-transform a predictor, you cannot simply take the exponential of the 
# slope to back transform. That would be a bad thing to do as the slope now
# represents the change in Y for a one-unit change in log(X), not X itself.
# Because the relationship is now nonlinear, the effect of X depends on its
# current value, so there is no single “back-transformed” slope on the original
# scale. Instead, the coefficient should be interpreted in terms of proportional
# or percentage changes in X (e.g., a one-unit increase in log(X) corresponds to
# multiplying X by a constant factor).

# Just keep in mind that when you do log-transformations, you need to be more
# careful on interpretations because they can get slippery very quickly.

# There's other ways to handle non-normality depending on the severity. Robust
# standard errors work decently to handl non-normality too (because non-normal
# residuals are very correlated with heteroskedastic residuals). 

# Alternatively, non-normality is also just not a great concern and regression
# is very robust to it unless non-normality is very extreme or your sample size
# is small. So you might not have to worry about it at all.

# ====================== Independence Assumption ======================

# 5.1 Independence Assumption

# The last and often least emphasized assumption is that each observation is
# independent of the others. This means that the value of one data point should
# not systematically influence or be related to another. In other words, each
# row of data should represent a separate, unrelated piece of information.

# Violations of independence commonly occur in clustered or repeated-measures
# data (e.g., students within the same classroom, repeated responses from the
# same person, or participants from the same family). In these situations individuals
# tend to be more similar to each other than to those from different clusters.
# Other causes might be waiting room effect from participants observing each other.

# When independence is violated, standard errors are typically underestimated,
# which can inflate Type I error rates (i.e., finding effects that are not truly
# there). Importantly, the coefficient estimates themselves are still unbiased,
# but our confidence in them becomes overstated. Though these will probably be 
# less biased than other assumption violations.

# In general, this doesn't tend to be a huge issue unless there is very obvious
# non-independence (such as within-subject or natural hierarchical clustering
# structures). In these cases, there are many tools and methods you could employ
# depending on your analysis objective.

# 5.2 Independence Visualization

# To visualize independence, you need to fit each variable and the predicted scores
# to the residuals and see if there is any trend to the residuals across the 
# X-axis.

# Predicted Socres
ggplot(data = dg, mapping = aes(x = pred, y =  resid))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# OpenMindedness
ggplot(data = dg, mapping = aes(x = OpenMindedness, y =  resid))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# Conscientiousness
ggplot(data = dg, mapping = aes(x = Conscientiousness, y =  resid))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# Extraversion
ggplot(data = dg, mapping = aes(x = Extraversion, y =  resid))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# Agreeableness
ggplot(data = dg, mapping = aes(x = Agreeableness, y =  resid))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# NegativeEmotionality
ggplot(data = dg, mapping = aes(x = NegativeEmotionality, y =  resid))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# What you are looking here is for relationships between points close to each other
# on the X-axis. Aka, maybe points lower on Negative Emotionality trend further 
# downward, which would imply there's some dependency amongst low emotionality 
# traits.

# These trends, if they exist, would be very obvious. What you see in all of these
# plots are fine and not out of the ordinary. This is to be expected since this
# data set came from a between-subject design with no natural hierarchical structure.

# It's hard to find examples of violations in these types of plots, but you can
# look online for one. Most people diagnose non-independence using other tools 
# such as the ICC in multilevel modeling (which you might take in the future).

# 5.3 Independence Violation

# There are many solutions to independence violations. If non-independence does 
# matter, it is usually a substantive matter. 

# Some methods that psychologist use are multilevel models and cluster robust
# standard errors. The former of which is technical, and I recommend you learning.
# The latter of which pretty much works like the previous standard errors you 
# learned about. But you shouldn't apply these standard errors if 
# non-independence matters for you study (e.g., if there is time-dependency or
# a hierarchical structure you are interested).


# ====================== df Betas ======================

# 6.1 df Betas

# The last diagnostic you will learn about is dfbetas, which is a measure of
# influential points. 

# "Influence" is the degree to which a single data point has an effect on the 
# size of a coefficient. The more influential a point is, the more it influences
# the coefficients. 

# Influence is a sum of distance and leverage. Distance is how far the score is
# from the predicted regression line (i.e., a large residual), and leverage
# measures how far a data point’s predictor values are from the rest of the
# data, indicating how much potential that point has to influence 
# the fitted regression line.

# So a point far from a line that has a lot of leverage is very influential.

# These are the most important "outliers" you need to consider, as not every
# "outlier" is actually that important in the model. 

# A way to quantify the influence of each point is to calculate dfbetas. 
# These are the degree to which each model coefficient changes when you 
# remove a point from the dataset. 

# Aka, removing an influential point will have each coefficient change by a 
# lot, whereas removing a non-influential point will probably not change the
# coefficients that much.

# The point that has the largest differences in slopes when removed is the most
# influential point.

# 6.2 dfbeta with base R

# R has built in dfbeta funcitons. Specifically it has two, dfbeta() and dfbetas().
# To get the dfbetas of each coefficient, you need to use dfbeta() DO NOT USE
# THE dfbetas() function with the "s" afeter "dfbeta". That added "s" representes
# "standardized". So dfbeta() gets you the regular dfbetas, while dfbetas() gets
# you the standardized dfbetas. You want the former not the latter.

# Every year someone makes this mistake, so please keep this in mind. 

# To get the dfbetas with this function, you just need to insert the model object
# into it. 

dfb <- dfbeta(model1)
dfb

# This will give you the output of dfbetas per each individual row. If you want
# an easier view, use head().

head(dfb)

# As an example for interpretation, the df beta for OpenMindedness for the 
# first row is 1.72*10^-4, which means that the  that the slope of OpenMindness
# would increase by 1.72*10^-4 if the first individual was removed. 
# This is very tiny, which means individual 1 is  really not that influential.

# These values are not that helpful by themselves. You want to find the highest 
# in magnitude (regardless of sign) and figure out what individual is attached
# to them. You can do this anyway you want, just make
# sure you keep the sign because that is important for interpretation.

# Here's just something I had AI write me up really quickly.

dfb <- dfbetas(model1)
cases <- apply(dfb, 2, function(x) which.max(abs(x)))
values <- apply(dfb, 2, function(x) x[which.max(abs(x))])
data.frame(Predictor = names(cases), Case = cases, DFBETA = values)


# We can see that for OpenMindedness, individual 893 had the largest
# dfbeta magnitude with a value of -.0041. This means that the slope of the 
# OpenMindedness would decrease by .0041 if we removed individual 893. 

# This may seem small on the surface, but you need to keep in mind how many 
# standard errors of change this is. This is important because changes that seem
# tiny on the surface might be very big in magnitude.
# The standard error of the slope of OpenMindedness is .0265. 
# So we need to find how many standard errors of change this is

# This may seem small on the surface, but you need to keep in mind how many 
# standard errors of change this is. This is important because changes that seem
# tiny on the surface might be very big in magnitude.
# The standard error of the slope of OpenMindedness is .0265. 
# So we need to find how many standard errors of change this is

.0041/.0265

# So removing individual 893 would decrease the slope by about .15 standard errors,
# which is not that much. 

# There is no metric to how much influence is *too* influential. It is a subjective
# judgement. Amanda doesn't suggest removing points regardless of influence
# unless they are errors.

# 6.3 df Betas in PROCESS

# We are going back to the first model in PROCESS (replicated below). Use
# "diagnose = 1" to get the dfbeta diagnostic summary.
dg <- process(data = big5, x = c("OpenMindedness", "Conscientiousness", "Extraversion",
                                  "Agreeableness", "NegativeEmotionality"),
               y = c("ExistentialWellBeing"), 
               save = 4, # save the predicted/residuals/df betas
               diagnose = 1)  # gives you the most influential cases
dg

# First, lets start with the diagnostic output in the dg object.
# In the diagnostic output, you will find many columns with the dfbetas for each
# coefficient (dfb_0, dfb_1, etc.). These give you the values of how much each
# coefficient will change if that data row was removed. 

# If you want to know, the other diagnostic output columns are they are as follows:
# pred
#    predicted scores
# resid
#    residuals
# d_resid
#    changed in residual when the individual is removed
# stresid
#    standardized residuals
# tresid
#    studentized residual
# h
#    hat values, which is a measure of leverage
# mahal
#    Mahalanobis distance value, which are a measure of distance
# cook
#    Cook's distance values, a different measure of influence that Amanda doesn't recommend
# dmsreg
#     change in model sum of squares when an individual is removed
# drsq
#    change in R^2 when the individual is removed
# dskew
#    change in skew when the individual is removed (not sure why it is all 9999's)

# If you then go and look at the "Most influential observations" table, you can
# see a list of largest dfbeta for each coefficient and which casenumber they 
# are attached to. The interpretations follow what is given in 6.3.

# 6.4 dfbeta Plots

# These plots aren't necessary, but can give you some visual context of how many 
# notable influential points exist per coefficient. The futher from the center
# the point is on the histogram, te more influential the point is.

# Plotting the dfbetas for each coefficient
hist(dg$dfb_0, breaks = 50)        # intercept
hist(dg$dfb_1, breaks = 50)        # OpenMindedness
hist(dg$dfb_2, breaks = 50)        # Conscientiousness
hist(dg$dfb_3, breaks = 50)        # Extraversion
hist(dg$dfb_4, breaks = 50)        # Agreeableness
hist(dg$dfb_5, breaks = 50)        # NegativeEmotionality

# If you want, you can plot each point with the casenumber attached so you 
# can identify which individual is where on the scatter plots.
# This method is very messy, but can help identify
# influential points that might be more on the outskirts.

# OpenMindedness
ggplot(aes(x = OpenMindedness, y = ExistentialWellBeing), data = dg)+
  geom_text(aes(label = casenum), size = 3)
# Conscientiousness
ggplot(aes(x = Conscientiousness, y = ExistentialWellBeing), data = dg)+
  geom_text(aes(label = casenum), size = 3)
# Extraversion
ggplot(aes(x = Extraversion, y = ExistentialWellBeing), data = dg)+
  geom_text(aes(label = casenum), size = 3)
# Agreeableness
ggplot(aes(x = Agreeableness, y = ExistentialWellBeing), data = dg)+
  geom_text(aes(label = casenum), size = 3)
# NegativeEmotionality
ggplot(aes(x = NegativeEmotionality, y = ExistentialWellBeing), data = dg)+
  geom_text(aes(label = casenum), size = 3)


