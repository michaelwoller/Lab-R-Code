# ====================== Multiple Linear Regression ======================

# Author: Michael Woller

# ====================== Load Packages ======================

#install.packages("ggformula")
#install.packages("dplyr")
#install.packages("car") 
library(ggformula)  # used for gf_point() and gf_lm()
library(dplyr)      # used for the pipe operator %>%
library(car)        # used to get VIF and Tolerance

# Make sure you load in PROCESS

# ---------------------- Specify File Path ----------------------

filelocation <- "/Users/woller/Documents/250c 2026/Lab 2/SpeedDating.csv" # wherever on your computer
SpeedDating <- read.csv(filelocation, header = TRUE)

# ---------------------- Data Cleaning ----------------------

# Only grabbing complete cases. There will be issues if you use base 
# R methods if you don't. Though the PROCESS method will be fine as it removes
# missing data automatically.

complete.SpeedDating <- SpeedDating %>% select(X, PartnerYesM, LikeF, AttractiveF)
complete.SpeedDating <- complete.SpeedDating[complete.cases(complete.SpeedDating),]

# To just explain what I'm doing if you are curious, first I'm making a subset 
# of the dataset with  with the variables I want by "piping" 
# the SpeedDating data into the select() function. Then I am using the
# complete.cases() function to only keep rows of my data that have complete values.
# Also, the subject ID is named "X" in my dataset for some reason. If you get an
# error above, you might have to change the names of the variables in the 
# select() function, since there might be differences.

# ====================== Fitting Multiple Linear Regression ======================

# 1.1 Multiple Linear Regression 

# Multiple Linear Regression (MLR) is often just called regression as it is the 
# most basic and common form of linear regression people use. 

# It follows the same ideas simple linear regression from last week, except now
# we are including more predictors. These multiple predictors, however, make the
# R^2 effect size more of interest.

# Recall that the R^2 of a model with only one predictor is equivalent to the t
# test for that one predictor. However, now since we have more than one predictor,
# the R^2 conveys more information. 

# Interpretations will be discussed below.

# For the purposes of this demonstration, we will be predicting how much a female
# in a speed dating event likes their partner (LikeF). We will be predicting this
# from two variables: how much the male partner perceives the female likes him 
# (PartnerYesM) and how much the female perceives her partner as being attractive
# (AttractiveF).

# ---------------------- MLR With Base R ----------------------

# 1.2 Using lm() for MLR

# To do MLR with the lm() function, just add more than variable from the dataset.

model1 <- lm(LikeF~PartnerYesM+AttractiveF, data = complete.SpeedDating)
summary(model1)

# The output is the same as a simple regression, but now there are two slopes 
# for PartnerYesM and AttractiveF.

# However, lm() does not include the model correlation coefficient (R), so you
# have to manually find this by taking the square root of the R^2.

sqrt(summary(model1)$r.squared)

# Alternatively, since the model correlation coefficient is just the correlation
# of the observed outcome with the predicted scores, you can find it with cor(). 

pred <- predict(model1)
cor(complete.SpeedDating$LikeF, pred)

# Base R does not have a method of getting the partial and semi-partial correlations.
# There are some packages out there, but from my experience they haven't been updated
# in years and aren't compatible with R at the moment. If you can find something 
# on your own, go off. 

# You can calculate these manually, which I will outline later on.

# ---------------------- MLR With PROCESS ----------------------

# 1.3 Using PROCESS for MLR

# Using PROCESS gives all relevant output easily
# You just need to include the "stand = 1" argument, and save the diagnostics
# if you want to plot.

# Including "stand = 1" prints coefficient specific correlations, partial correlations,
# and semi-partial correlations. It also gives standardized coefficients
# and different effect sizes, which will be relevant in future lectures.

dg <-  process( data = complete.SpeedDating, 
                 y = "LikeF", 
                 x = c("PartnerYesM", "AttractiveF"),
                save = 4,  # This prints the partial and semi-partial correlations
                stand=1)

# Model Summary:
#   R is the model correlation
#   R-sq is the R^2 effect size
#   Adj R-sq is the adjusted-R^2 effect size
#   F is the F statistic for F test of R^2
#   p is the p value for the F test
#   SEest is the RMSE (which we won't cover in this class)

# In the ANOVA table (not labeled but is the table under Model Summary)
#   SS is the sum of squares error
#   df is the degrees of freedom
#   MS is the mean square error

# Model: 
#   There are the two coefficients, standard errors, t statistic, p value, and 95% CI
#   "constant" is the intercept


# Under Scale-free and standardized measures of association
#   r is a predictor specific correlation, equivalent to just using cor()
#   sr is the semi-partial correlation#
#   pr is the partial correlation 
#   standYX is the standardized coefficients (both X and Y are standardized),
#     this is usually the one that people care about
#   standY are the coefficients when only Y is standardized
#   standX are the coefficients when only X is standardized
#   eta-sq is the eta-quare effect size
#   p_eta-sq is the partial eta-square effect size
#   f-sq is the Cohen's f^2 effect size

# ---------------------- Interpretations of MLR ----------------------

# 1.4 Interpretations of Coefficients

# Recall that interpretations need to discuss about difference between two points,
# they need to talk about expected/predicted outcome values, and they need to 
# mention directionality. As long as you include these aspects,
# how you phrase it is up to you.

# From our output, the interpretations of the coefficients are as follows.

# Intercept:
#   For a couple where the male partner rating that the female wants another 
#   date was 0 scale points and the female partner rating of the male’s #
#   attractiveness was 0 scale points, the female is expected to rate her 
#   liking of the male as 2.64 scale points. 
#        Not needed to be mentioned but note that 0 values for either predictor
#        are not possible scores because the scale is from 1-10

# Slope of PartnerYesM:
#   Among two couples who differ by one point on the male’s rating that 
#   the female wants another date and where the female partners rated the male
#   partners attractiveness as the same, the female is expected to like the male
#   .04 scale points more in the couple with the higher male rating of the 
#   female wanting another date. 

# Slope of AttractiveF
#   Among two couples who differ by one point on the female’s rating of male 
#   partner attractiveness and where the male partner’s rating that the female
#   wants another date is the same, the female is expected to like the male .55
#   scale points more in the couple with a higher female perceived attractiveness.

# 1.5 Interpretation of Model Correlation Coefficient

# Recall this was .64. 
# The predicted female ratings of how much they liked their male partner and the
# observed female ratings of how much they liked their partner are correlated 
# at about .62.
 
# 1.6 Interpretation of R^2 effect sizes

# The R^2 coefficient is often interpreted as a percentage of the variation of
# the outcome explained by the predictors.

# In our example, R^2 = .4132. So PartnerYesM and AttractiveF explai about 
# 41% of the variation of LikeF. If you want to, you can look at Cohen's R^2
# effect size benchmarks to determine that this is a strong effect.

# Adjusted-R^2's are interpreted a bit different since it is an adjusted statistic
# that is less biased at the population level. So it is a better estimate of the
# "true" R^2 of the "true" population model.
# So, djusted-R^2 is interpreted as how much the population version of the model 
# is expected to explain the population data. 

# In our example, Adjusted-R^2 = .4. So the population version of our model is 
# expected to explain about 40% of the variation in the population data.

# Bottom line is that R^2 is a measure of how well our model explains what we observe.
# Adjusted-R^2 is a measure of the percentage of all of the population data that 
# the true population model would explain. 

# 1.7 Partial and Semi-Partial Correlations

# Partial correlations remove variability from both the outcome variable
# and the predictor that can be explained by other predictors.
#   It’s the pure correlation between X1 and Y, filtering out influence of X2. 
#   So it is the association of pure X1 on pure Y.

# Semi-partial correlations (sr) only remove variability from the predictor 
# which can be explained by other predictors, but does not remove variability 
# from the outcome that is explained by the other predictors.
#   It’s the correlation between X1 and Y, but we’ve only filtered out 
#   influence of X2 from X1. So it is the association of pure X1 on impure Y. 

# I think a helpful thing to understanding these is to keep in mind that the
# term “partial” comes from “partialling out” variation of one variable from the others.
# As opposed to “partial” being only a part of a correlation.
# So a correlation is "semi-partial" since its only partialling 
# one variable of the two. 

# Interpreting the partial correlation of LikeF and PartnerYesM
#   After having removed the variability from PartnerYesM and LikeF which can 
#   be explained by AttractiveF, the correlation between PartnerYesM and LikeF is .066. 

# Interpreting the sem-partial correlation of LikeF and PartnerYesM
#   The correlation between LikeF and PartnerYesM is .05, after having removed
#   variability from PartnerYesM which can be explained by AttractiveF. 

# Note, the semi-partial correlation will ALWAYS be smaller than the partial correlation.
# This is since you are partialling out from both variables, there will be less
# variability to correlate as a result. Though in this case, they don't differ 
# by that much.

# ====================== Alternative Methods to get Partial/Semi-Partial Correlations ======================

# 2.1 Finding Correlations by Saving Residuals

# It's possible to manually partial out predictors from one-another and find 
# the correlations manually. If you have an automatic method, this is not 
# preferable, but if you are using base R, this might be necessary.

# This is done by putting the variable you want to partial in the outcome, 
# and whatever you are partilling out as the predictor. Save the residuals,
# then correlate the residuals appropriately.

# This method will be necessary if you want to plot the correlations (see next section)

# Example:Find the Partial/Semi Partial correlations of 
#         AttractiveF and LikeF, partialing out PartnerYesM

# Partial out PartnerYesM from AttractiveF and save residuals
AttrF_PartM <- resid(lm(AttractiveF ~ PartnerYesM, data = complete.SpeedDating))
# Partial out PartnerYesM from LikeF and save residuals
LikeF_PartM <- resid(lm(LikeF ~ PartnerYesM, data = complete.SpeedDating))

# Partial Correlation
cor(AttrF_PartM, LikeF_PartM)
# Semi-Partial Correlation 
cor(complete.SpeedDating$AttractiveF, LikeF_PartM)

# You can also do this with PROCESS, you just have to save two seperate model diagnostics. 
dg2 <- process(y = "LikeF", x = c("PartnerYesM"),
                model = 0, data = complete.SpeedDating, save = 4, stand=1)
dg3 <- process(y = "AttractiveF", x = c("PartnerYesM"),
                model = 0, data = complete.SpeedDating, save = 4, stand=1)

cor(dg2$resid, dg3$resid)

# 2.2 Finding Correlations Analytically

# These are just if you are interested and are not required to know.

# Partial correlation of AttractiveF and LikeF, partialing out PartnerYesM
y <- complete.SpeedDating$LikeF
x2 <- complete.SpeedDating$PartnerYesM
x1 <- complete.SpeedDating$AttractiveF

r_yx1 <- cor(y,x1)
r_yx2 <- cor(y,x2)
r_x1x2 <- cor(x1,x2)
r_overlap <- r_yx2*r_x1x2

resid_ryx2 <- sqrt(1-(r_yx2^2))
resid_rx1x2 <- sqrt(1-(r_x1x2^2))

# Get the partial correlation 
partial <- (r_yx1 - r_overlap) / (resid_ryx2*resid_rx1x2)
partial

# Get the semi-partial correlation
semi <- (r_yx1 - r_overlap) / (resid_rx1x2)
semi

# ====================== Visualizations ======================

# 3.1 Visualizing Model Correlation Coefficient

# To visualize correlations, you need to plot the relevant values associated with
# the correlations.

# For example, the model R correlation is the correlation between observed and
# predicted scores (you can grab the predicted scores from whatever function):

gf_point(complete.SpeedDating$LikeF~dgs$pred) %>% 
  gf_lm()

# You can see how the observed scores are correlated with relatively strongly with
# the predicted scores. The steeper the slope, the stronger the correlation.

# 3.2 Visualizing Partial and Semi-Partial Correlations

#  Use the partied out residuals from the previous section from whatever method.

# Plot partialed out LikeF against partialed out AttractiveF
gf_point(dgs2$resid~dgs3$resid) %>% 
  gf_lm()

# The slope here can shope you a magnitude of the strength. It is kind of hard to 
# tell though, since the correlation is strong (.63), but the scale of the plot
# might conceal it. So just keep that in mind. But you can see a linear trend 
# upwards regardless.

# Plot regular LikeF against partialed out AttractiveF
gf_point(complete.SpeedDating$LikeF~dgs3$resid) %>% 
  gf_lm()

# Same rationale with the regression line. 

# Something to keep in mind is that the plot of the semi-partial correlation will
# have more variation than the partial correlation. This is because it only controls
# (partials out) variation from one of the variables as opposed to both.
# Don't be confused by the scale of the Y axis though.
# The Y axis will be negative to positive for the partial correlation (since you
# partiled out LikeF), but will be only positive for the semi-partial correlation
# (since you are using regular LikeF, which is only positive in its scale).

# Generally, you will see more variability for the semi-partial correlation, but
# this is just a poor example since PartnerYesM wasn't a very good predictor.

# ====================== Dichotomous Predictors ======================

# 4.1 Dichotomous Predictor

# Dichotomous (binary) predictors follow a different interpretation strategy than continuous
# predictors, but implementing them into your model is relatively straight forward.

# Binary variables get introduced into the model as a "dummy coded" variable. 
# This has a specific meaning that we will talk about in the future, but in 
# in general, it means the variable is only represented by a 1 or a 0 as potential
# values. But we will discuss this more in the multicategorical predictor lesson.

# For the two groups in your binary variable, the group coded as 0 is considered
# the "baseline" or "reference" group. But more on this in the interpretations.

# The binary variable we will lookd at is DecisionM, which is whether or not the
# male decided to have another date (0 = no, 1 = yes).

# 4.2 Dichotomous Predictors in lm()

# Adding a binary predictor to your lm() model requires one of two things. 

# 1) The variable consists of only 1's and 0's.
# 2) The variable only has two values and is classified as a "factor" object.

# One of these two needs to be true to include a binary predictor. 
# Number 2 above just means that, if the variable consists of two different 
# numbers that aren't 0's and 1's (like -1 and 1 or 3 and 8), then R will think
# they are continuous and not dichotomous. So you can reclassify them using the
# factor() function. 

# If you are not sure on what class your variable is coded as, you can use the
# class() function.

class(SpeedDating$DecisionM)

# Even though the above variable is just 0's and 1's, it is considered an 
# "integer," but this is ok for our lm().

# You can also make this a factor by using factor(). 

SpeedDating$DecisionMFactor <- factor(SpeedDating$DecisionM)

# This factor() method will become more important in future lectures. Just make
# sure you have at least one of the two criteria given above fufilled.

# In terms of actually fitting the model, all you need to do is put your 
# dichotomous predictor into the lm() function.

binaryModel <- lm(LikeF ~ DecisionM, data = SpeedDating)
summary(binaryModel)

# 4.3 Dichotomous Predictors in PROCESS

# PROCESS works similarly in that all you need to is add the variable to the model.
# However, in the case of PROCESS, the variable must be numeric (or integer) as
# its class. Aka, it has to be 0's and 1's, not a character or factor.

# Luckily, our variable is already an integer, so all we need to do is enter it
# in the PROCESS model.

process(data = SpeedDating, y = "LikeF", x = "DecisionM")

# 4.4 Dichotomous Predictor Slope Interpretations

# All interpretations on dichotomous variables are determined on what the 
# "baseline" is. The baseline (or reference group) is whatever group is coded
# with 0, which is the male deciding "no" in our case. 

# When there is a dichotomous variable, the intercept becomes the group mean for
# the baseline group, and the slope becomes the difference between the baseline
# and alternative group. Keep in mind that the difference is Group 1 - Group 0

# Intercept:
#   The mean rating of LikeF for a male who said no is 6.276.

# Slope: 
#   The difference between the mean rating of LikeF for a male who said no and
#   a male who said yes is .169, where the male who said yes has a higher average.

# If you want the mean of Group 1, you just add these the intercept and slope.

6.276+.169

# So the mean LikeF of males who said yes is 6.445. You can fact check this by
# finding the individual group means too.

tapply(SpeedDating$LikeF, SpeedDating$DecisionM, mean, na.rm = TRUE)

# As you can see, the means are the same as the intecept and intercept+slope
# (barring some rounding).

# 4.5 Changing the Reference Group

# A limitation is that you cannot get a significance test of the Group 1 intercept
# from the current model. So in order to do so, you need to recode the baseline
# so that a male saying yes = 0 and them saying no = 1. 

# There's multiple ways to do this. For a numeric variable you can just use 
# the following.

SpeedDating$DecisionM2 <- 1 - SpeedDating$DecisionM

# If you want to recode a factor variable, use the relevel() function.
# Just put in whatever group you want the new reference group to be

SpeedDating$DecisionMFactor2 <- relevel(SpeedDating$DecisionMFactor, ref = "1")

# If you look at the two factors, you can see that the levels switch at the bottom.
# The first level is the baseline.

SpeedDating$DecisionMFactor
SpeedDating$DecisionMFactor2

# Rerunning the model with the new coding scheme will give you the proper significace
# for the "yes" group.

# R
binaryModel2 <- lm(LikeF ~ DecisionM2, data = SpeedDating)
summary(binaryModel2)

# PROCESS
process(data = SpeedDating, y = "LikeF", x = "DecisionM2")

# As you can see, the new intercept is the group mean of the Group 1 yes group.
# The slope of the DecisionM variable is the same in magnitude, but now the 
# sign has changed since we are doing Group 0 - Group 1

# 4.6 Dichotomous Predictors with Covariates

# All adding a covariate does is change the interpretation to controlling for 
# the other predictors.

# Let's add back in PartnerYesM

# With lm()
binaryModel3 <- lm(LikeF ~ DecisionMale + PartnerYesM, data = SpeedDating)
summary(binaryModel2)

SpeedDating$bb <-

binaryModel2 <- lm(LikeF ~ DecisionMale + PartnerYesM, data = SpeedDating)
summary(binaryModel2)

# With PROCESS
process(data = SpeedDating, y = "LikeF", x = c("DecisionM", "PartnerYesM"))

# Interpretations for the intercept and dichotomous variable slope follow 
# similarily to before, but now adding the fact that we are controlling for
# PartnerYesM. The interpretation for PartnerYesM, however, is now for two
# individuals of the baseline variable.

