# ====================== Moderation/Interaction Effects ======================

# Author: Michael Woller

# ====================== Load Packages ======================

#install.packages("ggformula")       # needed for visualization
#install.packages("dplyr")           # needed for data management
#install.packages("interactions")    # needed for plots and simple effects
#install.packages("emmeans")         # needed for simple effects

library(ggformula)
library(dplyr)
library(interactions)
library(emmeans)     # ignore the warning

# Remember to load in PROCESS. 

# ---------------------- Specify File Path ----------------------

# Specifying file path
filelocation <- "/Users/woller/Documents/250c 2026/Lab 9/Big5data.csv" # wherever on your computer
big5 <- read.csv(filelocation, header = TRUE)

# ====================== Moderation/Interaction Effects ======================

# 1.1 Moderation/Interaction Effects

# Moderation/interaction occurs when the effect one predictor has on the outcome
# depends on the value of another predictor in the model (i.e., one predictor
# "moderates" the effect of the other, hence the term's name). For example, 
# the effect of study time on exam scores might depend on whether a student has
# a tutor. With a tutor, more study time may strongly boost scores, but without 
# one, the same extra study time might have a weaker effect. This "it depends" 
# relationship between predictors is what we call an interaction.

# To test for moderation, we create an interaction term by multiplying two
# predictors together and adding it to the regression model. In the above example,
# we'd have a continuous predictor for study time, a dichotomous predictor for
# if the student has a tutor or not, and the interaction term that is the product
# of the study time variable and tutor variable.

# Typically, in psychological research, experiments categorize one variable of 
# th interaction as the "focal predictor" (X), which is focus of the experiment
# and focus on the interpretation. The other predictor in the interaction is
# considered the moderator (M), and the interpretation of this variable is of less
# concern. Therefore, the importance is how M moderates the effect of X rather 
# than vice-versa. The choice of which variable is the focal predictor or the 
# moderator makes no difference on the outcome but only the interpretation.

# A significant interaction term tells us the relationship between the focal
# predictor and the outcome genuinely changes across levels of the moderator. 
# The moderator could be categorical or continuous, but the interpretation
# of its moderating effect depends on the type of variable the moderator is. 

# For the purpose of this lab, we want to see if the effect of an individual's
# political conservatism (Conservatism) on their favorability to right wing 
# authoritarianism (RightWingAuthoritarianism) is moderated by their perceived
# social status amongst their peers (PeerStatus).

# ---------------------- Creating Interaction Models in Base R ----------------------

# 1.2 Interactions in lm()

# There are two main methods of creating interactions with an lm() model. One it 
# to create a new variable that is a product of the predictors you are interested
# in. 

big5$ConPeer <- big5$Conservatism*big5$PeerStatus

model1 <- lm(RightWingAuthoritarianism ~ Conservatism + PeerStatus + ConPeer, 
             data = big5)
summary(model1)

# A warning with this method is you MUST include both the original variables.
# You cannot do an interaction with Con*Peer if you don't include both
# individually. This is because the interaction term alone cannot separate
# the unique effect of each predictor from their combined effect. Leaving
# out a conditional effect forces its influence into the interaction term, which
# will bias your results and make the coefficients very difficult to interpret.
# Think of it like baking: you can't describe the role of sugar in a recipe
# if you never list it as an ingredient on its own. Always include the main
# effects of Conservatism and PeerStatus as separate predictors alongside the 
# Con*Peer term.

# The other method is to just do the product within the lm() function.

model2 <- lm(RightWingAuthoritarianism ~ Conservatism + PeerStatus + 
               Conservatism*PeerStatus, data = big5)
summary(model2)

# Note here, when you include the interaction product in the model above,
# the individual predictors are included automatically. 

model2.5 <- lm(RightWingAuthoritarianism ~ Conservatism*PeerStatus, data = big5)
summary(model2.5)

# However, its generally good practice to include the condtional effects independently. 

# To add a covariate, you simply just have to include it into the model.

model3 <- lm(RightWingAuthoritarianism ~ Conservatism + PeerStatus +
               Conservatism*PeerStatus + PeersAcceptanceFriendship, data = big5)
summary(model3)

# An example of a non-significant interaction would be individual's political 
# conservatism with whether or not their family is unemployed. 
model4 <- lm(RightWingAuthoritarianism ~ Conservatism + FamilyNotUnemployed +
               Conservatism*FamilyNotUnemployed , data = big5)
summary(model4)

# In R, you can add any type of variable as the moderator. Just make sure
# its the appropriate class. For example, keep in mind that for a multicategorical
# moderator, you will need multiple dummy variables, each interacting with your
# focal. For an example of a multicategorical moderator, we will use the 
# multicategorical variable "Humor" which is the humor type an individual 
# identifies most as. First make sure it is a factor class variable (note,
# the levels don't really matter in this example, so I am just doing a basic
# reclassification).

big5$Humor_Factor <- as.factor(big5$Humor)

# Add the factor class into the model.

model5 <- lm(RightWingAuthoritarianism ~ Conservatism  + Humor_Factor +
               Conservatism*Humor_Factor, data = big5)
summary(model5)

# As you can see, if the multicategorical moderator is classified as a factor, 
# the dummy interactions will be included automatically. Note, that there will
# be a left out baseline category in this context as well. If you want to do
# other multicategorical coding schemes, follow the methods layed out in the 
# Multicategorical Predictor lab.

# ---------------------- Creating Interaction Models in PROCESS ----------------------

# 1.3 Interactions in PROCESS

# In PROCESS, you have to specify "w = " with the moderator and include
# "model = 1".  Technically, you can also just make a new product variable 
# (what we did in Line 71 above), but using  "w = " gives you a lot of nice output
# like the simple effects. Keep in mind, what variable goes in "w=" is
# considered the non-focal variable. So the focal variable for the analysis 
# should go in "x =".  If you want to look at the moderation effects for the
# other variable, you need to rerun the model with the "x =" variable in "w =".
# Lastly, you need to make sure that "model = 1". Recall from the first lab
# that, by default, PROCESS assumes "model = 0", which is a linear regression
# model with no interaction. Specifying "model = 1" tells PROCESS we are doing
# a linear regression model *with* an interaction.

# These quirks of the PROCESS macro stem from the fact that it was initially
# developed with only a select amount of built in models for the purpose of
# mediation analysis. While PROCESS can do non-mediation analysis, which is 
# what we've been using it for, its more limited than other regression functions.
# However, it still has a lot of convenient aspects to it that might make it worth
# using for some.

# The PROCESS model will look like the following.

process(data = big5, x = "Conservatism", w = "PeerStatus", 
        y = "RightWingAuthoritarianism", 
        model = 1) 

# As you can see, we have the same coefficients, except the interaction term is
# labeled int_1 (1st interaction), which is also defined in the "Product terms
# key" table. PROCESS will automatically give you a delta-R^2 for the highest
# order interaction term  in the "Test(s) of highest order unconditional
# interaction(s)" table (which just means the interaction term with the most 
# variables, since we only have two, its the highest, but if we had a 
# three-way interaction, that would be the highest). This delta-R^2 is most 
# useful when your moderator is multicategorical because then it represents
# the increase in model fit fit for all the interaction variables that would be
# included.

# What variable is considered the focal predictor and what is considered the
# moderator is found in the next table.

# At the very bottom of the output is the table for "Conditional effects of the
# focal predictor at values of the moderator(s)", which gives some conditional
# effects (the 16th 50th and 84th percentiles by default).
# We will come back to these in a later section. 

# In PROCESS, any other covariates in the model that will NOT have an interaction
# must go into "cov =" option. Essentially, anything in "x =" will be associated 
# with the interaction, and anything in "cov =" will be left alone.

#Example:
process(data = big5, x = "Conservatism", w = "PeerStatus", 
        cov = "PeersAcceptanceFriendship",
        y = "RightWingAuthoritarianism", 
        model = 1)

# If your moderator is a multicategorical variable, this will naturally mean
# you need many interaction terms (one interaction per coded variable). In this
# case, you can specify "mcw = 1" this says that your moderator "w" is 
# multicategorical, and it will give you all the interaction variables
# naturally as well as every conditional effect. Specifying it as "mcw" as equal
# to 1 means you want the multicaterogiacl moderator to be dummy coded. You can
# select different coding schemes if you want by following the steps described
# in the Multicategorical Predictor lab.

# In this case we will use the multicategorical variable "Humor" which is the
# humor type an individual identifies with most. We will dummy code this variable.

process(data = big5, x = "Conservatism", w = "Humor",
        y = "RightWingAuthoritarianism", 
        mcw = 1,
        model = 1)    

# As you will see in this context, the test of delta-R^2 test is for ALL of
# the interaction effects, not just a single one. 

# ---------------------- Interpreting Modeartion Models ----------------------

# 1.4 Interpreting Moderation Models

# Coefficients of moderation models follow similar interpretations to what you
# have learned before, but with some important distinctions. Since we are 
# incorporating the interaction, the coefficients of the conditional effects are 
# *conditional* on a specific level of the other predictor in the interaction,
# typically when the moderator equals 0 (unless they are recentered, which I
# will discuss further below).

# Example:
#   b1 is the *conditional* effect of X when M = 0.
#   It is NOT when M is "held constant".

# The interaction effect will be interpreted as the change in the effect of X
# between two individuals who differ in one unit of M (or by the groups of M, if
# M is categorical).

# To move on to the focal model's interpretations.

# Intercept:
#   The average level of favorability of right wing authoritarianism when an 
#   in individual has no political conservatism and no status among their peers
#   is expected to be -1.66. Note that this doesn't make sense on the variable's
#   scale.

# Slope of Conservatism:
#   For individuals that have no status among peers, two indivdiduals that differ
#   in political conservatism  by one unit are expected to differ by favorability
#   of right wing authoritarianism by about 3.09, with the more conservative
#   individual being more favorable.

# Slope of PeerStatus:
#   For individuals that are not political conservatives, two indivdiduals that 
#   differ in their self perceived peer status are expected to differ by
#   favorability of right wing authoritarianism by about .25, with the individual
#   of higher peer status being more favorable of authoritarianism.

# Slope of Interaction:
#   For two people who differ on self pereceive peer status differ by 1 unit, 
#   the conditional effect of political conservatism on favorability of right
#   wing authoritarianism  is expected to change by -.118 for the person higher
#   in peer status.

# As one can see, the interpretations of the conditional effects are dependent on 
# the level of the other predictor. We are not controlling for the other predictor
# in entirety because by including the interaction effect, we assume that the
# main effects are not constant, so they *can't* be "controlled." Instead, we
# discuss them at a certain level of the other predictor. 

# By default, this conditional level will either be when the other predictor 
# is 0 for continuous moderators or the baseline group level for categorical
# moderators. Obviously, that's not always what we are interested in, so now
# I turn to discuss simple effects.

# ---------------------- Mean Centering ----------------------

# 1.5 Mean Centering

# As a final note on interaction effects, it is often said that centering your
# interaction terms (X and M) at their means can be beneficial. It is a MYTH
# that this will reduce multicolinearity. Centering the variables at their means
# reduces collinearity that are statistical artifacts, i.e., it does not
# remove colinearity due to true relationships within the data. 

# Centering at the mean can be very helpful for interpretations, however. Recall
# how the default conditional effect is when the moderator = 0. This is often
# nonsensical. So recentering at the mean gives the conditional effect of X
# with an average M. This is often much more intuitive. 

# Note, to "center" just means to position your variable with a specific value
# in the middle. Naturally, this value will be 0, but if you subtract your variable
# by a different value (i.e., recenter it), then this new subtracted value will 
# become the center. 

# Let's say we want to recenter PeerStatus from 0 since 0 is not on the scale. 
# We want PeerStatus = 2 to be the center. So we subtract 2 from this variable.

big5$PeerStatus - 2

# Note now how every place where PeerStatus equalled 2 is now 0. Now 2 is the
# "center" where everything less than 2 is negative, and everything greater than
# 2 is positive.

# So, all recentering at the mean refers to is just subtracting the predictors
# by their means.

# We need to recenter both predictors in the interaction term at their means.

big5$PeerStatus_mc <- big5$PeerStatus - mean(big5$PeerStatus)
big5$Conservatism_mc <- big5$Conservatism - mean(big5$Conservatism)

# Then put them into a model (methods of making the interaction still apply).

model6 <- lm(RightWingAuthoritarianism ~ Conservatism_mc + PeerStatus_mc +
               Conservatism_mc*PeerStatus_mc, 
             data = big5)
summary(model6)

# Now the slope of Conservatism_mc can be understood as the conditional effect
# of political conservatism when an individual has average peer status.
# This is called a "simple effect", which we will look over in much more detail
# in the next section.

# For PROCESS, you can plug in manually mean-centered variables if you want to,
# but it has a built in method for centering variables at their mean. You just
# need to include "center = 2". This "center=" option can specify many different
# variables in the model. For example, "center = 1" means every single variable is
# centered at their mean. But "center = 2" just centers the focal predictor and
# the moderator at their means, which is what we want to do.

process(data = big5, x = "Conservatism", w = "PeerStatus",
        y = "RightWingAuthoritarianism",
        center = 2,
        model = 1)

# ====================== Simple/Conditional Effects ======================

# 2.1 Simple Slopes

# When a significant interaction effect is found, the next step is to
# unpack it using simple effects analysis. The significant interaction means
# that the effect is not constant at each level of the moderator, but the
# slope of the main effect only describes the effect of X on Y at a specific 
# levle of M. We know that the level of M matters, so in order to see the effect
# of X on Y at other levels of M, we need to find what are called simple or 
# conditional effects.

# For example, if Conservatism*PeerStatus is significant, we might ask: what
# is the effect of Conservatism on the outcome for people low in PeerStatus, and 
# how does that compare to people high in PeerStatus? Simple effects let
# us describe that relationship separately at meaningful levels of the
# moderator. 

# For continuous moderators typically we examine three levels: one standard 
# deviation below the mean, at the mean, and one standard deviation above the mean.
# This breakdown is what allows us to tell a clear and complete story
# about how and when a predictor matters most, though how far out you would want
# to explore will be data dependent.

# For categorical moderators, you typically want to assess the simple effects at
# all levels of the moderator. 

# Simple slopes are just the slope of the focal predictor + the slope of the 
# interaction effect * a certain level of the moderator. 

# Example, say the conditional effect of X is .5, and the interaction effect
# of X*M is 1.5. The basic conditional effect of X when M = 0 is
#     .5*X + 1.5*X*0 = .5*X
# The conditional effect of X when M = 1 is
#     .5*X + 1.5*X*1 = .5*X + 1.5*X = 2*X

# So to get the simple slopes, you just add whatever interaction coefficient is 
# leftover after plugging in a value of M. 

# Of course, this just gives you the slope value and doesn't tell us if the 
# slope is significant. To get the significance of simple slopes, you can use
# a variety of packages that might find them automatically, or you can manually
# calculate them by recentering/changing the reference group. The latter might be
# necessary or easier depending the context. Probing at specific conditional
# effects can be a very important step at the exploratory phase of data analysis.

# ---------------------- Simple Slopes in R  ----------------------

# 2.2 Simple Slopes of Categorical Moderators in lm()

# Categorical moderators are the most straight forward moderator to handle if 
# dummy coded. All that needs to be done is to change the reference group
# to change the simple slope. Since the conditional effects of X will naturally
# be when M is at the reference group, switching the reference group will switch
# the conditional effect. 

# You can change the reference group in any method you want (see the
# Multicategorical predictors lab). Let's say I wanted to switch our example
# Humor variable to have the baseline of Humor = 5 (the highest value).

# I already made the factor variable, so I will just use relevel().

big5$Humor_Factor_2Ref <- relevel(big5$Humor_Factor, ref = 5)

model7 <- lm(RightWingAuthoritarianism ~ Conservatism + Humor_Factor_2Ref + 
                     Conservatism*Humor_Factor_2Ref, data = big5)
summary(model7)

# Now the conditional effect of Conservatism is when PeerStatus = 5. If you recall
# the conditional effect of Conservatism when Humor = 0 was not significant, but
# the conditional effect of Conservatism when Humor = 5 was significant. Even 
# though Humor = 0 doesn't exist on this scale, I quickly check and the conditional
# effect is still not significant when Humor = 1 as well.

# What we did above applies to dichotomous moderators too, its just binary variables,
# there's only two conditional effects (group 0 and group 1). I'll briefly 
# demonstrate with the Immigrant variable (whether or not the individual is a
# US immigrant). 

model8 <- lm(RightWingAuthoritarianism ~ Conservatism + Immigrant + 
               Conservatism*Immigrant, data = big5)
summary(model8)

# To find the significance of the other slope, just switch the baseline.
# Switching a 0/1 binary varibale indicator is easy. Just do 1 - variable:
big5$Immigrant2 <- 1- big5$Immigrant  

model9 <- lm(RightWingAuthoritarianism ~ Conservatism + 
                      Immigrant2 + Conservatism*Immigrant2, data = big5)
summary(model9)

# Note how only the slope of conservatism changes in magnitude, 
# since now this is the simple effect of conservatism when immigrant status = 1.
# The interaction term slope changes sign because we changed the reference group.

# 2.2 Simple Slopes of Interval Moderators in with sim_slopes() and emtrends()

# Getting simple slopes fo interval moderators is easier than fully continuous
# variables since there are convenient values for each moderator to be tested at.
# For example, with PeerStatus, we have 7 levels, so we can get, at max, only 
# 7 simple slopes. We could manually recenter the variable 7 times to get each
# simpel slope (recall, the naturall centering is PeerStatus = 0, which is not
# in the data scale), but there's a lot of packages that can do this.

# Lets go back to model1 with PeerStatus, reproduced here.
model1 <- lm(RightWingAuthoritarianism ~ Conservatism + PeerStatus + 
               Conservatism*PeerStatus, data = big5)
summary(model1)

# From the 'interactions' package, you can use sim_slopes() to get the simple 
# slopes and p values for each level of PeerStatus. Just specify the interval
# values in the modx.values argument. 

sim_slopes(model1,
           pred = Conservatism,
           modx = PeerStatus,
           modx.values = 1:7) 

# Note new R users, a ":" operator just means "through" for a list of numbers.
# So 1:7 means "1 through 7".

# We can see that the conditional effect of Conservatism is significant at 
# every level of PeerStatus. As a side note, ignor the "Johnson-Neyman" title
# at the top of this output. Technically, what we are doing is "pick a point" 
# centering since we manually specified the modx.values. I will cover what this
# means further down.

# If you want confidence intervals, I found emtrends() from the 'emmeans' 
# package gives them.

emtrends(model1,
         var = "Conservatism",
         specs = "PeerStatus",
         at = list(PeerStatus = 1:7)) # specify the levels of PeerStatus

# This will give you the slope and confidence interval for each of them, but
# does not give you p values. You can still check significance based on the 
# intervals though. There's probably a package out there that will give you both,
# but that is up to you to find.

# 2.3 Simple Slopes of Continuous Predictors with sim_slopes() and lm()

# For continuous predictors that can have irrational numbers, there's not an
# easy decision for what simple slopes you might want to examine. It's
# typical in these cases that you look at the conditional effect at the mean and
# ±1 standard deviation above and below the mean.

# First let's make a model with a continuous moderator using Religiousness, 
# a sum score scale of multiple items that measure individual relgiiousness.

model10 <- lm(RightWingAuthoritarianism ~ Conservatism + Religiousness + 
                Conservatism*Religiousness, data = big5)
summary(model10) 

# The interaction term is significant in this case, so the effect of Conservatism
# on RightWingAuthoritarianism depends on the level of the individuals Religiousness.

# However, the conditional effect is when Religiousness = 0, so lets check what
# the simple slopes are at the mean and ±1 sd above and below it. To do this, we
# can save these as independent variables just to make things simpler.

m <- mean(big5$Religiousness) # mean of religiousness
s <- sd(big5$Religiousness)   # sd of religiousness

# Then you can either recenter your moderator at these values.
# Example at +1 sd above the mean:
model11 <- lm(RightWingAuthoritarianism ~ Conservatism + I(Religiousness -(m+s))+ 
                Conservatism* I(Religiousness -(m+s)), data = big5)
summary(model11) 

# Or you can put these in as the moderator levels in the previous functions we
# covered like sim_slopes().

sim_slopes(model10,
           pred = Conservatism,
           modx = Religiousness,
           modx.values = c(m-s, m, m+s)) 

# ---------------------- Simple Slopes in PROCESS  ----------------------

# 2.4 Simple Slopes in PROCESS for Interval and Continuous Moderators

# Different simple effects are automatically given when you specify a moderator
# in PROCESS. These can be found at the bottom of the output in the "Conditional
# effects of the focal predictor at values of the moderator(s)" table.

# I will reproduce the initial model here for convenience. 

process(data = big5, x = "Conservatism", w = "PeerStatus",
        y = "RightWingAuthoritarianism", 
        model = 1)

# PROCESS will give you the simple effects for the 16th, 50th (median)
# and 84th percentiles. This is the default for all continuous vairbales. Recall,
# that, for PROCESS, since all variables must be inputted numerically, the macro
# will automatically see any variable with more than two values as continuous.
# This means that, by default, its output will give conditional effects at the 
# mean and ±1 SD. However, there are some caveats to this. It assumes that the
# data is normally distributed. In normally distributed data, the 6th, 50th, and
# 84th percentiles are the mean and ±1 sd above and below. If the data is not
# normal, this will be innacurate. So just keep this in mind. 

# In the context of PeerStatus, the 50th percentile is 5 and the 6th and 84th 
# percentiles are 3 and 7. Hence why this was the output given to us. 

# If you want to get the true mean with ±1 sd above and below, you can use the
# "moments = " option, which you can set to equal 1. This will give the "moment
# generated" mean and standard deviation conditioanl effects (what thi smeans
# is beyond this course). 

process(data = big5, x = "Conservatism", w = "PeerStatus",
        y = "RightWingAuthoritarianism", 
        moments = 1,
        model = 1)

# You can see that the values were different than the percenitles, which is what
# we would expect if the data is not normally distributed. Though it is only a bit
# off, so the data isn't that nonnormal.

# If you want to check the mean and ±1 sd above and below of a Likert item variable
# (like PeerStatus), you would want to use the percentiles not the actual moment
# values. This is because it is best to talk about the Likert scale in its whole
# units rather than go into small decimals of units if you can help it. 

# If you are using a non-interval continuous moderator, like the Religiousness
# predictor, then it would be more appropriate to use "moments = 1" like what
# follows.

process(data = big5, x = "Conservatism", w = "Religiousness",
        y = "RightWingAuthoritarianism", 
        moments = 1,
        model = 1)

# If you want to center the moderator to get a conditional effect at a specific
# value, you need to use the "wmodval = " option and put in the value(s) you want
# to get the simple effects for. 

# Let's get the conditional effect of Conservatism when PeerStatus = 7.

process(data = big5, x = "Conservatism", w = "PeerStatus",
        y = "RightWingAuthoritarianism", 
        wmodval = 7, 
        model = 1)

# You can see how the conditional effects table at the bottom now only contains
# the values we put in the "wmodval = " line. 

# You can put in multiple centers at once. Let's get the conditional effects for
# PeerStatus = 1, 2, and 7. 

process(data = big5, x = "Conservatism", w = "PeerStatus",
        y = "RightWingAuthoritarianism", 
        wmodval = c(1,2,7),
        model = 1)

### The default output of PROCESS will give you 16th, 50th, and 84th percentiles, which are ±1 SD above and below the mean if the data is normally distrubuted
### To probe at continuous moderator values, you need to use "moments"
process(data = big5, x = "Conservatism", w = "Religiousness",
        y = "RightWingAuthoritarianism", 
        moments = 1,
        model = 1) # gives ±1 sd from the mean based on the data's curviture (more accurate)
# If you look at the bottom under ANALYSIS NOTES AND ERRORS, you can see where it says "W values in condition tables are the mean and +/1 SD from the mean"
# Note that this is different than the default probing, which gives ±1 sd ASSUMING that the data is normal, but since this doesn't match the true ±1 SD from moment = 1, this means our data is not normal
# This also carries the issue is that it is not constrained to your data, and assumes everything is limitless. These variables have an upper limit, so its not very appropriate to treat them as boundless above
# Generally, if your variable is a Likert item, percentiles are better 

# 2.5 Simple Slopes in PROCESS for Multicategorical Moderators

# If your moderator is multicategorical, PROCESS will automatically give the simple
# effects of each level of the mdoerator.

process(data = big5, x = "Conservatism", w = "Humor",
        y = "RightWingAuthoritarianism", 
        mcw = 1,
        model = 1)

# This is convenient, so no extra probing will be needed. I am not sure if ther is
# a limit to the number of groups in the moderator before PROCESS stops giving
# you the simple effects of each, so just keep that in mind.

# 2.6 Simple Slopes in PROCESS for Dichotomous Moderators

# If the moderator is binary, then PROCESS does not give you the simpe slope
# of the other baseline, unfotrunately. The only way to get it is to manually
# recode the baseline similarily to what has been done in previous labs.

# Initial dichotomous model
process(data = big5, x = "Conservatism", w = "Immigrant",
        y = "RightWingAuthoritarianism", 
        model = 1)

# Recode baseline
big5$Immigrant2 <- 1- big5$Immigrant  

# Dichotomous model with recoded baseline
process(data = big5, x = "Conservatism", w = "Immigrant",
        y = "RightWingAuthoritarianism", 
        model = 1)

# ====================== Simple Effect Probing ======================

# 3.1 Simple Effect Probing

# "Probing" is the prospect of picking specific values of the moderator to 
# test where the significance of an interaction comes from. Recall, if the 
# interaction effect is significant, then we know that the conditional effect of 
# X is  not consistent across M. However, we don't know *where* in M it is not 
# consistent. It is possible that the conditional effect of X only differs when
# M = 1, but not any other value. This is usually important for researchers to 
# figure out, however, it is not always clear what values of the moderator to
# check nor is it always the case the moderator will have natural intervals that
# are few in number (e.g., if you have an interval moderator with 100 levels,
# you're going to get a lot of simple effects).

# There's two methods we will talk about.

# 3.2 Pick a Point Probing

# Pick a point probing is straightforward. The method involves figuring out
# specific values of the moderator that would be of interest to check. This 
# can be due to previously established theory (e.g., we expect the average 
# peer status person to behave differently than those with high or low status)
# or natural reasons (e.g., it's likely that the extremes of peer status probably
# have differing effects than each other, so we should check those).

# Whatever the reason, all you need to do with pick a point probing is to just
# center your moderator at the desired value like we discussed in the previous
# Section 2. There is nothing new to the method itself. 

# 3.3 Johnson-Neyman Approach

# The Johnson-Neyman method for probing involves using anlytical math that scans
# across the moderator data to find points where the conditional effect of X
# switches from non-significant to significant.

# Instead of using a private eye  detective like pick a point probing, this 
# method is more of a helicopter spotlight sweep.

# The method will produce no values, one value, or two values of the moderator
# that show the focal predictor switching from non-significant to significant.

# AKA
# If no values turn up:
#   The conditional effect of X is consistently significant/non-significant across
#   all values of M.
# If one value turns up:
#   The conditional effect of X is non-significant above/below M and significant
#   below/above M (one or the other).
# If two values turn up:
#   The conditional effect is either significant between the two values of M or
#   it is non-significant between those two values.

# In general, its better to use theory for why you are looking at specific values
# of the moderator. It's possible the conditional effect of X is significant at
# multiple levels of M. The Johnson-Neyman approach might miss the more interesting
# or theoretically noteworthy levels by putting emphasis on other values of M. It
# also doesn't show you the different strengths of effects, rather just if they
# are significant or not. That is, even if multiple points of M have a significant
# X, that does not mean that they meaningfully differ from one another (keep in 
# mind, you can have a significant interaction with ALL levels of the moderator
# being significant).

# This is why we will focus on the pick a point approach for the class.

# However, if you do want to do Johnson-Neyman probing in R, you can use the 
# the Use johnson-neyman() function from 'interactions' package. This function
# does the Johnson-Neyman method multiplt times to scan the data of the moderator
# thoroughly.

johnson_neyman(model1, pred = "Conservatism", modx = "PeerStatus")

# What this means is that across the range of observed PeerStatus (1-7),
# every slope of Conservatism is significant. If some values of PeerStatus were
# not significant, they'd appear as a red line and red area. This means that
# we got the "no values turn up" scenario above. THe focal predictor was significant
# across all values of the moderator.

# If you want to do Johnson-Neyman probing in PROCESS< you can use the "jn = 1"
# option.

process(data = big5, x = "Conservatism", w = "PeerStatus",
        y = "RightWingAuthoritarianism", 
        jn = 1,
        model = 1)

# There will be a new table at the bottom. You can see in our model's case, the
# output states the conclusion "There are no statistical significance transition
# points within the observed range of the moderator found using the Johnson-Neyman 
# method". If the conclusion found transition points, it would point those out. 

# Then it lists a table of all the levels of the moderator that the method scanned
# ove under "Conditional effect of focal predictor at values of the moderator".
# You can verify yourself that every single one of these points has a p value of 
# less than .0001.


# ====================== Interaction Plots ======================

# 4.1 Interaction Plots

# Plotting interactions can be an easy way to visualize what the moderation looks
# like. It can also give some idea on whether or not an interaction exists and 
# what levels of the moderator it exists at. However, note that this is not 
# a full proof way of demonstrating interactions. If the model you are trying to 
# fit is poor or there are some aspects you aren't incorporating into the model
# (like hierarchical data structures), then you may see interactions that don't
# truly exist. 

# Interaction plots are still often desirable and required for moderation
# analysis. So you should still investigate these plots and report them, just
# don't treat them as the end all be all.

# ---------------------- Creating Interaction Plots in R ----------------------

# 3.2 Creating Interaction Plots with gf_point()

# What packages you use to create interaction plots is arbitrary, but I will go
# over some convenient selections starting with gf_point() from the 'ggformula'
# package. Here, you can plug in your focal predictor and outcome, then specify
# your moderator in a "color = " line. Note, you have to put a factor class
# variable in here. So if your moderator has a different class, you need to 
# change it. 

gf_point(RightWingAuthoritarianism~Conservatism, 
         data = big5, 
         color=~factor(PeerStatus))%>% # Make sure this is a factor class variable
  gf_lm()

# Each group is separated by color and each simple effect is represented by a
# different color slope. You can customize color and legend. I would just ask AI 
# to figure this out for you since graphing functions can be annoying to figure out

# This plot shows you why the conditional effect of Conservatism was significant
# at every level of PeerStatus even though the interaction was significant. The 
# effect is increasing for every level of PeerStatus, but they are all increasing
# at different rates. So even though the conditional effect of Conservatism is 
# present across all levels of PeerStatus, the effect still differs from level
# to level.

# Here is an example for an interaction plot for a non-significant interaction.

gf_point(RightWingAuthoritarianism~Conservatism, 
         data = big5, 
         color=~factor(FamilyNotUnemployed))%>%
  gf_lm()

# You can see here that the line slopes are very similar to each other. Meaning
# that the conditional effect of Conservatism didn't depend on whether or not
# the individual's family had employment.

# The downside of gf_point is that it doesn't handle continuous moderators easily.
# I'm sure you can get it to work, and that AI would make it easy to do so, but
# I am going to cover some other options instead.

# 3.3 Creating Interaction Plots with interact_plot() 

# Another function you can try is the interact_plot()   function from 
# 'interactions' package. This package can do continuous moderators a bit
# more easily, which is why I am demoing it here.

# To use this function, secify the values of the moderator with the modx.values
# argument. The default moderator level is ± one standard deviation, so keep this
# in mind. Let's fit it with the basic model, specifying the individual levels
# of PeerStatus.

interact_plot(model = model1, pred = Conservatism, modx = PeerStatus, 
              modx.values = c(1,2,3,4,5,6,7),
              data = big5)

# There are options for customization that you can look ask AI about.

# As I said, this function can run continuous variables easily, and does the default
# mean and ±1 sd above and below the mean conditional effects. Using our continuous
# Religiousness predictor, we can fit that plot.

interact_plot(model = model10, pred = Conservatism, 
              modx = Religiousness, 
              data =big5)

# If you recall from the output of model10, this interaction was actually 
# significant. So this interaction plot is a bit misleading, as it looks like
# there isn't much of a difference between the slopes.

# If you want to check more SD's or other specific continuous values you have to 
# manually calculate them. I will resave the mean and sd variables here.

m <- mean(big5$Religiousness) # mean of religiousness
s <- sd(big5$Religiousness)   # sd of religiousness

# Lets say we want to look at ±3sd and ±2sd above/below the mean in addition to 1.
# In the modx.values line, we need to manually calculate these within a c() list.
# For clarity, I am making that list prior, though you can do this within the
# interact_plot() function if you wished.

mod_levels <- c((m - 3*s),  # -3sd below mean
                (m - 2*s),  # -2sd below mean
                (m - s),    # -1sd below mean
                m,          # the mean
                (m + s),    # +1sd below mean
                (m + 2*s),  # +2sd below mean
                (m + 3*s))  # +3sd below mean

# Insert this variable into the modx.values argument, and give a label to each
# level in the modx.labels argument (this will appear in your legend).

interact_plot(model = model10, pred = Conservatism, modx = Religiousness,
              modx.values = mod_levels,
              modx.labels = c("-3SD", "-2SD", "-1SD", "mean", "1SD", "2SD", "3SD") ,
              data =big5)

# Looking at this plot, it seems that there is a difference in slope for -3sd 
# and +3sd, albeit not by that much. I guess this makes sense since the 
# interaction was barely significant.

# We can check if there's a reverse dependency of the effect of Religiousness at
# different Conservatism because maybe the interaction is more clear there.

interact_plot(model = model10, pred = Religiousness, modx = Conservatism,
              data =big5)

# It doesn't appear much clearer here. I guess the strength of the interaction is
# minimal (only changing by .17 for each unit increase of Relgiiousness), so
# the plot makes sense with that context. But this is a good example of how
# looking at interaction plots can be misleading if you don't know the full story.

# 3.4 Creating Interaction Plots in PROCESS

# PROCESS can give you interaction plots, but I don't recommend using them since
# you can't change their appearance at all, and they aren't in very publishabel 
# designs (options in data visualization is KEY). It migh tbe useful for some
# quick visualizations before you finish the analysis however. 

# All you need to do to get the plot is to input the option "plot = 1" and PROCESS
# will make an interaction plot with all the conditional effects it generated. 
# Recall that the default conditional effects are the percentiles, so this is what
# you will see if you do not manually input custom simple slopes.

process(data = big5, x = "Conservatism", w = "PeerStatus", 
        y = "RightWingAuthoritarianism", 
        plot = 1, 
        model = 1)

# If you want to get an interaction plot for your specific conditional effects,
# you need to use the "wmodval = " line as discussed in Section 2.4.

process(data = big5, x = "Conservatism", w = "PeerStatus", 
        y = "RightWingAuthoritarianism", 
        plot = 1,
        wmodval = c(1,3,5,7),
        model = 1)

# PROCESS can give easy plots for continuous moderators too.

process(data = big5, x = "Conservatism", w = "Religiousness", 
        y = "RightWingAuthoritarianism", 
        plot = 1,
        moment = 1,
        model = 1)

