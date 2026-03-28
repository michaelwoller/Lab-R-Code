# ====================== Variable Importance ======================

# Author: Michael Woller

# ====================== Load Packages ======================

#install.packages("dplyr")               # For %>% operators
#install.packages("ggplot2")             # For plotting diagnostics
#install.packages("effectsize")          # For effect size functions
#install.packages("car")                 # For Type III sum of squares and contrasts
#install.packages("dominanceanalysis")   # For dominance analysis
#install.packages("parameters")          # For standardized coefficients
library(dplyr)
library(ggplot2)
library(effectsize)
library(car)
library(dominanceanalysis)
library(parameters)

# Remember to load in PROCESS. 

# ---------------------- Specify File Path ----------------------

filelocation <- "/Users/woller/Documents/250c 2026/Lab 5/Big5data.csv" # wherever on your computer
big5 <- read.csv(filelocation, header = TRUE)

# ====================== Predictor Effect Sizes ======================

# 1.1 Predictor Effect Sizes 
# While R^2 is good at giving you an idea of how all variables together can predict
# the outcome, and significance tests can let you know whether a predictor's effect
# is different than 0, these do not let you know the relative importance of one
# predictor to the other in terms of how much variability each predictor explains
# and how important each predictor is relative to the others. Significance alone
# can't give you this.

# To do that, you can use other effect sizes. Specifically for this class,
# we will cover Eta^2, partial-Eta^2, and Cohen's f^2. 

# Previously, we've tested significance of the Big 5 in predicting existential
# well being, but now we want to look at the relative importance of the Big 5
# predictors in explaining existential well being instead.

# ---------------------- Effect Sizes with 'effectsize' Package ----------------------

# 1.2 Effect Sizes in R 

# The easiest method I have found in getting these effect sizes in R is to use the
# eta_squared() and cohens_f_squared() from the 'effectsize' package. Though the
# process is still annoying. I highly suggest you skip to section 1.4 to just
# learn how to do it with PROCESS. However, Section 1.3 gives a demo on how to 
# get some of the effect sizes manually if you are interested.

# The issue comes from that these functions use Type I1 sum of squares by default.
# This is what we need whereas, we need Type III sum of squares to get some of 
# these effect sizes. The difference between these two is that Type III
# sum of squares. Type I sums of squares test each predictor in the order 
# entered (sequentially), whereas Type III sums of squares test each predictor’s 
# effect after controlling for all other predictors in the model. Since with
# regression, we are controlling for all variables at the same time, we need this
# type of sum of squares.

# First we need to make the model.

model1 <- lm(ExistentialWellBeing ~ OpenMindedness + Conscientiousness + 
               Extraversion + Agreeableness + NegativeEmotionality, data = big5)

# You need to plug this model into the Anova() function from the 'car' package.
# NOTE, this is Anova() with a capital 'A', not anova() with a lower case 'a'. 
# These are two different functions. 
# We want to use Anova() since we can specify Type III sum of squares.
# Save the output into an object.

anova1 <- Anova(model1, type = 3)

# Side note, if you ever want to be specific about what function comes from what
# package (since some packages use the same function names), you can name the
# package and then put a :: after it to call a specific function from the package.
# You technically don't even need to run library('package') if you do this too.

# Example:

car::Anova(model1, type = 3)

# we need to use this object to get the partial-eta^2 and Cohen's f^2 from the
# 'effectsize' package.

# To get partial-eta^2, use ta_squared() and set "partial = T".

eta_squared(anova1, partial = T)

# To get Cohen's f^2, use cohens_f_squared() and set "partial = T".

cohens_f_squared(anova1, partial = T)

# If you want to get eta^2, you need to be careful. There are separate definitions
# of this effect size given the type of model you are running. Do NOT use
# eta_squared() with "partial = F". This will give you a eta^2 for an anova. This
# is a different definition that is the predictor specific effect of variance 
# explained of the outcome that we do in regression. 

# Note, don't worry about interpreting the confidence intervals. 

# In our context, the eta^2 is just the semi-partial squared multiple correlation
# of each predictor. So to ge this, you need to partial out each variable in the
# model, then find the squared multiple correlations for each individual predictor.
# To do this, you need to partial out every Big 5 measure from each of each other.
# Save the residuals. Then fit a model that predicts ExistentialWellBeing 
# from that individual partialed residuals. Get the R^2 and that is your eta^2
# effect size.

# eta^2 for OpenMindedness
residOpen <- lm(OpenMindedness ~ Conscientiousness + 
                Extraversion + Agreeableness + NegativeEmotionality,
                data = big5)$residuals
summary(lm(ExistentialWellBeing ~ residOpen, data = big5))$r.squared

# eta^2 for Conscientiousness
residCon <- lm(Conscientiousness ~ OpenMindedness + 
                  Extraversion + Agreeableness + NegativeEmotionality,
                data = big5)$residuals
summary(lm(ExistentialWellBeing ~ residCon, data = big5))$r.squared

# eta^2 for Extraversion
residExt <- lm(Extraversion ~ OpenMindedness + 
                 Conscientiousness + Agreeableness + NegativeEmotionality,
               data = big5)$residuals
summary(lm(ExistentialWellBeing ~ residExt, data = big5))$r.squared

# eta^2 for Agreeableness
residAgree <- lm(Agreeableness ~ OpenMindedness + 
                 Extraversion + Conscientiousness + NegativeEmotionality,
               data = big5)$residuals
summary(lm(ExistentialWellBeing ~ residAgree, data = big5))$r.squared

# eta^2 for NegativeEmotionality
residNeg <- lm(NegativeEmotionality ~ Conscientiousness + 
                  Extraversion + Agreeableness + OpenMindedness,
                data = big5)$residuals
summary(lm(ExistentialWellBeing ~ residNeg, data = big5))$r.squared

# Obviously, this all of this is obnoxious. There were better packages in the 
# past, but I don't think they've been updated and are not compatabile with
# modern R. But if you find anything better, feel free to use it. Just make 
# sure what it gives you is correct. 

# 1.3 Other Effect Sizes with Squared Multiple Correlations

# Technically, you can get partial-Eta^2 by getting the partial squared multipl
# correlations. To do this, you just need to get the partialed residuals of 
# ExistentialWellBeing while partialling out all but the other variable you
# are correlating.

# partial-eta^2 for OpenMindedness
resExOpen <- lm(ExistentialWellBeing ~ Conscientiousness + 
                  Extraversion + Agreeableness + NegativeEmotionality,
                data = big5)$residuals
summary(lm(resExOpen ~ residOpen, data = big5))$r.squared

# partial-eta^2 for Conscientiousness
resExCon <- lm(ExistentialWellBeing ~ OpenMindedness + 
                 Extraversion + Agreeableness + NegativeEmotionality,
               data = big5)$residuals
summary(lm(resExCon ~ residCon, data = big5))$r.squared

# partial-eta^2 for Extraversion
residExEx <- lm(ExistentialWellBeing ~ OpenMindedness + 
                 Conscientiousness + Agreeableness + NegativeEmotionality,
               data = big5)$residuals
summary(lm(residExEx ~ residExt, data = big5))$r.squared

# partial-ta^2 for Agreeableness
residExAg <- lm(ExistentialWellBeing ~ OpenMindedness + 
                   Extraversion + Conscientiousness + NegativeEmotionality,
                 data = big5)$residuals
summary(lm(residExAg ~ residAgree, data = big5))$r.squared

# partial-eta^2 for NegativeEmotionality
residExNeg <- lm(ExistentialWellBeing ~ Conscientiousness + 
                 Extraversion + Agreeableness + OpenMindedness,
               data = big5)$residuals
summary(lm(residExNeg ~ residNeg, data = big5))$r.squared

# Lastly, you can also find f^2 by dividing the semi-partial squared correlation
# (i.e., the eta^2 effect size) with the R^2 from the full model.

# Save the R^2
R2 <- summary(model1)$r.squared

# Cohen's f^2 for OpenMindedness
summary(lm(ExistentialWellBeing ~ residOpen, data = big5))$r.squared / (1 - R2)

# Cohen's f^2 for Conscientiousness
summary(lm(ExistentialWellBeing ~ residCon, data = big5))$r.squared / (1 - R2)

# Cohen's f^2 for Extraversion
summary(lm(ExistentialWellBeing ~ residExt, data = big5))$r.squared / (1 - R2)

# Cohen's f^2 for Agreeableness
summary(lm(ExistentialWellBeing ~ residAgree, data = big5))$r.squared / (1 - R2)

# Cohen's f^2 for NegativeEmotionality
summary(lm(ExistentialWellBeing ~ residNeg, data = big5))$r.squared / (1 - R2)

# I highly recommend just using PROCESS for to get the effect sizes
# It is much simpler. I will next go over PROCESS.

# ---------------------- Effect Sizes with PROCESS ----------------------

# 1.2 Effect Sizes with PROCESS

# It is very easy to get the effect sizes in PROCESS. To do this, you need to
# include the "stand = 1" option. This will give you effect sizes, correlations,
# and standardized coefficients.

process(data = big5, x = c("OpenMindedness", "Conscientiousness", "Extraversion",
                           "Agreeableness", "NegativeEmotionality"),
        y = c("ExistentialWellBeing"), 
        stand = 1) 

# Look at the very bottom table where you see "Scale-free and standardized measures
# of association". At the very bottom, you will see the eta-sq, p_eta-sq, and f-sq
# columns, for eta^2, partial-eta^2, and Cohen's f^2 respectively. 

# As you can see if you read Section 1.1, this is much easier than trying to get 
# the effect sizes through other R packages.

# ---------------------- Effect Sizes Interpretations ----------------------

# 1.3 Effect Sizes Interpretations

# It's not typical that you would intepret these effect sizes outright, but you
# can if you'd wish. I will give an example of how to interpret the effectiszes
# for NegativeEmotionality. 

# eta^2
#   The proportion of existential well being that is explained by negative 
#   emotionality is  about .17. So 17% of all variance of existential 
#   well being was explained by negative emotionality.

# partial-eta^2
#   The proportion of existential well being that is *uniquely* explained by
#   negative emotionality is about .21. So out of what's left over after 
#   other predictors, negative emotionality explains about 21% of existential
#   well being. 

# Cohen's f^2 
#   The amount of existential well being that negative emotionality explains
#   is about .27 the size of the unexplained variation of existential well being.
#   So negative emotionality explains about 27% of the variance that remains
#   unexplained after accounting for the other predictors in the model.

# 1.4 Relative Importance

# Another thing you might have to interpret is the relative importance of one 
# predictor to another. This gives you a one-to-one "which predictor is better" 
# interpretation. To do this, you just need to divide one effect size for one
# predictor by the same effect size for another predictor. The order you divide 
# doesn't matter, it will just change the interpretation. I would recommend
# doing the bigger effect over the smaller effect

# For example, I will give the relative importance for NegativeEmotionality when
# compared to Agreeableness.

# With eta^2:
.1672/.0035
#   According to eta^2, negative emotionality is about 47 times more important
#   in explaining existential well being than agreeableness.

# With partial-eta^2:
.2123 / .0056
#   According to partial-eta^2, negative emotionality is about 38 times more 
#   important in explaining existential well being than agreeableness.

# With f^2:
.2695/.0057
#   According to Cohen's f^2, negative emotionality is about 47 times more 
#   important in explaining existential well being than agreeableness.

# Note one thing, the magnitude of the relative importance for any two variables
# will ALWAYS be the same for eta^2 and f^2. This is by definition
# (see Amanda's slides on the pie diagram). Partial-eta^2 won't have the same
# relative importance magnitude, however. Keen eyes might have spot that, if you 
# round differently, eta^2 and f^2 actually have different relative importance
# magnitudes above. Well in this case, the discrepancies have to do with how the
# values were rounded previously (since I took them from PROCESS). They will
# equal each other when the full values are used.

# ---------------------- Rank Ordering from Effect Sizes ----------------------

# 1.5 Rank Ordering from Effect Sizes

# Rank ordering is simply to list the order of most important variable to least
# important variable based on the magnitude of effect sizes. The variable with
# the largest effect is most important, the variable with the second largest 
# effect is the second most important, and so on. 

# The rank ordering with the three above effect sizes will always be the same.
# This is due to the nature on how they are calculated. 

# Also by nature of their calculations, two variables cannot tie in rank of
# importance (e.g., two variables can't tie for "second most important").

# To rank order, just look at the magnitudes of the effect sizes and list
# the predictors in order of greatest to least effect size.

# Rank Order
# 1) NegativeEmotionality
# 2) Conscientiousness
# 3) Agreeableness
# 4) Extraversion
# 5) OpenMindedness

# So negative emotionality was the most important Big 5 measure in explaining
# existential well being, and open mindedness was the least important measure

# ====================== Dominance Analysis ======================

# 2.1 Dominance Analysis

# Dominance analysis is another somewhat niche method for comparing variable importance
# in terms of explaining the outcome. It seeks to find which variables are "dominant"
# over others.

# Instead of looking at a predictor in just one model, dominance analysis looks at
# that predictor across *all possible combinations of models*. For each predictor,
# it asks: "How much does R^2 increase when I add this variable to models of
# different sizes?"

# For example, it compares how much a variable improves prediction:
# - when it is added to an empty model
# - when it is added to models with 1 predictor
# - when it is added to models with 2 predictors, and so on

# It then averages these contributions to get an overall measure of importance.

# A variable is considered "dominant" if it consistently adds more explanatory power
# than another variable across all model comparisons.

# The key idea is that dominance analysis accounts for shared variance between 
# predictors, giving a more complete picture of importance than just looking 
# at one model or one coefficient.

# In simple terms:
# “Which variable helps prediction the most, no matter what other variables 
# are in the model?”

# 2.2 Dominance Analysis with dominanceAnalysis()

# In R, you can do dominance analysis using the dominanceAnalysis() function
# from the 'dominanceanalysis' package. Hopefully, this package keeps working
# after while. We've had issues of dominance analysis packages becoming out of 
# date in the past.

# To do this, you just need to put in your model object into the
# dominanceAnalysis() function and save it as a new object.

da <- dominanceAnalysis(model1)
da

# Then, all you need to do is put in this dominance analysis object into the
# dominanceMatrix() function with "type = 'complete' "

dominanceMatrix(da, type = 'complete')

# This matrix gives the proportions of all possible models and the percentage
# the variable on the row is dominant over the variable in the column.
# A 1 means that the variable is dominant over the other. A 0 means that the
# variable did not improve R^2 in any possible model, and thus is not 
# dominant over the other. A decimal means that one variable is not dominant
# over the other, though in some cases it did improve R^2 over the other variable.
# Also, variables on diagonal is .5 for variables with themselves 
# though this .5 is meaningless in this context.

# You ONLY care about 1's. You should ignore any other number. Note, decimals 
# do not count as "partial" dominance. A variable is either dominant (1) or not 
# (anything else). 

# As an example, if you see NegativeEmotionality in the bottom row, we can 
# see its dominant over all other variables. For Conscientiousness on the second
# row, we can see it is dominant over OpenMindedness and Agreeableness but not
# Extraversion or NegativeEmotionality.

# 2.2 Dominance Analysis with PROCESS

# To do dominance analysis with PROCESS, you just need to include the command
# "dominate = 1". You can also include "subsets = 1" if you want a list of 
# all of the possible models the domincance analysis is examining, but this 
# isn't neceessary. 

process(data = big5, x = c("OpenMindedness", "Conscientiousness", "Extraversion",
                           "Agreeableness", "NegativeEmotionality"),
        y = c("ExistentialWellBeing"), 
        dominate = 1, # specify dominance analysis
        subsets = 1)  # specify all subsets regression

# The big table that has "All subsets regression results" shows every possible
# combination of the variables. It also gives the R^2's and adjusted-R^2's of 
# each model, but you can ignore these (we will come back to this output for
# the prediction week).

# At the bottom, find the table under "Dominance matrix". 

# This matrix gives the proportions of all possible models and the percentage
# the variable on the row is dominant over the variable in the column.
# A 1 means that the variable is dominant over the other. A 0 means that the
# variable did not improve R^2 in any possible model, and thus is not 
# dominant over the other. A decimal means that one variable is not dominant
# over the other, though in some cases it did improve R^2 over the other variable.
# Also, the diagonal will all be 0's when a variable is matched against itself.

# You ONLY care about 1's. You should ignore any other number. Note, decimals 
# do not count as "partial" dominance. A variable is either dominant (1) or not 
# (anything else). 

# As an example, if you see NegativeEmotionality in the bottom row, we can 
# see its dominant over all other variables. For Conscientiousness on the second
# row, we can see it is dominant over OpenMindedness and Agreeableness but not
# Extraversion or NegativeEmotionality.

# 2.3 Rank Ordering Dominance Analysis

# Rank ordering will determine the order of most important to least important
# variable in terms of explaining variance of the outcome. 

# Rank ordering with dominance analysis involves seeing how much is each variable
# dominant over the other. You only count dominance (1's in the matrix). You do
# not count partial improvements (the decimals). This is not partial dominance!

# Counting up the dominance, we get
# OpenMindedness: 0
# Conscientiousness: 2
# Extraversion: 1
# Agreeableness: 1
# NegativeEmotionality: 4

# To rank order, we need to put them in order of most dominance over least 
# dominance. There CAN be ties in this case. Meaning, it's possible for two 
# variables to be the second most important. 

# Rank order of importance:
# 1) NegativeEmotionality
# 2) Conscientiousness
# 3) Agreeableness and Extraversion
# 4) OpenMindedness

# As you can see, this differs from the effect size rank importance.

# ====================== Direct Coefficient Comparisons ======================

# 3.1 Direct Comparisons of Coefficients

# Sometimes we don't care about effect sizes. Sometimes we just want to compare
# one slope directly to another. Well, there's two methods of doing this. 
# One that is more common is to use standardized predictors. Another is to do a
# specific unstandardized slope comparison that I will explain. But first, let's
# start with standardized coefficients. 

# ---------------------- Standardized Coefficients ----------------------

# 3.2 Standardized Coefficients with R

# There's many ways you can get standardized coefficients in R, but I will show
# you the model_parameters() function from the 'parameters' package.

# Insert your model object into the model_parameters() function and specify
# the argument "standardized = "refit" ".

model_parameters(model1, standardize = "refit")

# The output gives the standardized coefficients. 
# NOTE: Technically, there should be no intercept since the intercept of 
# a standardized model will alqays equal 0. (Recall what happens to the mean
# of a standardized variable, they all become 0).

# In the output, you now have new standardized coefficients. The significance of 
# coefficients won't change, so their inference won't change. The 
# interpretations will change, however. 

# You can manually calculate these too by standardizing each variable in the model.
# To do this, use the scale() function around each variable.

model1_std <- lm(scale(ExistentialWellBeing) ~ 
                scale(OpenMindedness) + scale(Conscientiousness) +
                scale(Extraversion) + scale(Agreeableness) +
                scale(NegativeEmotionality),
                data = big5
                )
summary(model1_std)

# You can use whichever method above you prefer. 

# 3.2 Standardized Coefficients with PROCESS

# To get the standardized coefficients with PROCESS, you just need to add the
# "stand = 1" option like what you did to get the effect sizes.

process(data = big5, x = c("OpenMindedness", "Conscientiousness", "Extraversion",
                           "Agreeableness", "NegativeEmotionality"),
        y = c("ExistentialWellBeing"), 
        stand = 1) 

# Under the "Scale-free and standardized measures of association" table, look at
# the "standYX" column. This is the standardized coefficients we are concerned 
# about (when both Y and X's are standardized). 

# The "standY" column gives the coefficients when only Y is standardized, and the
# "standX" gives the coefficients when only the X's are standardized. These are 
# usually not wanted, however. 

# Note that in this output, there is no intercept. As I said in Section 3.1, the 
# intercept of a standardized model will always be 0, so PROCESS just doesn't 
# include it. 

# 3.3 Interpretations of Standadized Coefficients.

# Standardized slopes have similar interpretations to non-standardized slopes,
# but now we are talking about differences in standard deviations rather than
# raw units on the original metric. Recall that the "standardized" metric
# is just standard deviations. It is "scale-free". 

# Example: Slope of NegativeEmotionality
#   Between two individuals who differ in one standard deviation in negative
#   emotionality, the individual with more negative feelings is expected to be
#   about .5 standard deviations lower in existential well being. 

# 3.4 Direct Comparison of Standardized Coefficients

# The benefit of being of using scale-free units, is that they are directly
# comparable across different contexts. So we can directly compare the coefficient
# of NegativeEMotionality to the coefficient of Conscientiousness by seeing which
# one is expected to have a larger difference in standard deviations of 
# ExistentialWellBeing. 

# Std.NegativeEmotionality = -0.5067
# Std.Conscientiousness    = 0.1118

# So we can say that NegativeEmotionality is more important than 
# Conscientiousness. Since a one unit increase in NegativeEmotionality is associated
# with a higher increase in the outcome than Conscientiousness.

# The downside of this method is that the units are still somewhat arbitrary
# (what does a 1 standard deviation change in a variable really mean), and if 
# the variables in the model are correlated, you can't really easily compare
# the two variables while holding all else constant.

# ---------------------- Unstandardized Coefficients ----------------------

# 3.5 Unstandardized Coefficient Comparison

# This method seeks to compare two variables of different unit scales directly
# *without* changing the scale. To do this, it needs to use "contrast" coding.
# Contrasts essentially categorize how you will compare variables of the model
# directly to one another. You will learn more about this in the multicategorical
# predictor lab if you haven't already learned about them in 250b.

# 3.6 Direct Coefficient Comparison Manually

# There's a few ways to do this in R. You can manually calculate the compared
# variables by hand to put them in the model. 

# Step 1: Decide what Variables to compare
#   Let's say NegativeEmotionality and Conscientiousness.

# Step 2: Create two averaged variables, one where  NegativeEmotionality 
#   and Conscientiousness are added together, and one where they are subtracted 
#   (order of subtraction is arbitrary, it will just affect directionality).
X.add <- (big5$NegativeEmotionality + big5$Conscientiousness)/2
X.sub <- (big5$NegativeEmotionality - big5$Conscientiousness)/2

# Step 3: Fit the model with these two new variables instead of the old ones.
model2 <- lm(ExistentialWellBeing ~ X.add + X.sub + OpenMindedness  + 
               Extraversion + Agreeableness , data = big5)
summary(model2)
#   Note, the coefficients of X.add and X.sub are just the original coefficients 
#   of NegativeEmotionality and Conscientiousness added and subtracted together.

# Step 4: Look at the slope of the subtracted variable (X.sub). Ignore X.add.
#   This value is about -.56 which gives us a direct comparison between 
#   NegativeEmotionality and Conscientiousness. Since this is significant, this 
#   means that the two variables' effects are significantly different than each
#   other. 

# This is essentially making a linear contrast by hand. 

# 3.7 Direct Coefficient Comparison Using linearHypothesis()

# The linearHypothesis() function from the 'car' package lets you specify custom
# hypotheses for your regression models. Typically, this isn't usually done for 
# regression except a few specific instances such as this. But you make these
# custom hypotheses in special contrasts. There are many methods of doing contrast
# testing in R, but I like this function since you don't have to do mental book
# keeping of contrast matrices. 

# In the case you want directly compare NegativeEmotionality with Conscientiousness,
# you just write that hypothesis into the function after plugging in the model object. 

linearHypothesis(model1, "NegativeEmotionality - Conscientiousness = 0")

# Note, the above is case sensitive, but it does not need the spaces. 
# The output shows the significance test, showing that the variables' effects are
# significantly different than one another. However, this method doesn't give you 
# the actual difference of the coefficients. To get that, you just subtract the 
# coefficients by any means.

coef(model1)["NegativeEmotionality"] - coef(model1)["Conscientiousness"]

# Ignore the "NegativeEmotionality" label on that value in general. That's an error.
# This value represents slope of NegativeEmotionality - slope of Conscientiousness.

# More on interpretations later. 

# 3.7 Direct Coefficient Comparison Using PROCESS

# To do direct coefficient testing in PROCESS, you need to use the "linsum = " 
# option to specify your contrast. 

# If you are new to contrasts, basically, put 0's for coefficients you are not 
# comparing and put -1 or 1 for the two variables you are comparing (which 
# variable is -1 or 1 doesn't matter). The order of the values in "linsum =" 
# must follow what you put in "x = ", and there MUST be a 0 in the beginning 
# to represent the intercept (so if you have 5 predictors, you must have 6 
# numbers in linsum).
# The resulting code is the following. 

process(data = big5, x = c("OpenMindedness", "Conscientiousness", "Extraversion",
                           "Agreeableness", "NegativeEmotionality"),
        y = c("ExistentialWellBeing"), 
        linsum = c(0, 0, -1, 0, 0, 1)) # comparing NegativeEmotionality to Conscientiousness

# Look at the table labeled "Linear Combination Estimate and Hypothesis Test".
# The "Weight vector" section just gives you the contrast you put in "linsum = ".
# The bottom row gives you the estimated difference of the variable coefficients 
# (slope of NegativeEmotionality - slope of Conscientiousness), and whether or not
# this difference is significant. Since it is significant, this means the two 
# predictors' effects are significantly different than one another. 

# 3.8 Interpreting the Coefficient Difference

# The interpretation is straight forward, depending on what variable you 
# subtracted from the other. Since we did NegativeEmotionality subtracted
# by Conscientiousness, the coefficient of -0.5599 can be interpreted as:

# Individuals who are one unit higher in negative emotionality are expected to 
# decrease existential well being by about .56 more than individuals who are 
# one unit higher in Conscientiousness.

# Aka, a one unit increase in NegativeEmotionality corresponds with an even 
# greater decrease in ExistentialWellBeing than a one unit increase in 
# Conscientiousness. Just keep in mind that Amanda doesn't like these types of
# causal "increase" interpretations.

