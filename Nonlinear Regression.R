# ====================== Nonlinear Regression ======================

# Author: Michael Woller

# ====================== Load Packages ======================

#install.packages("ggformula")    # needed for visualization
#install.packages("mosaic")       # used for BoxCox function

library(ggformula) 
library(DescTools) 

# Remember to load in PROCESS. 

# ---------------------- Specify File Path ----------------------

# Specifying file path
filelocation <- "/Users/woller/Documents/250c 2026/Lab 8/FearData.csv" # wherever on your computer
feardata <- read.csv(filelocation, header = TRUE)

# ====================== Nonlinear Models ======================

# 1.1 Nonlinear Models

# Though it might seem like an oxymoron, linear regression can be used to model
# nonlinear relationships. The "linear" in linear regression actually refers to
# the fact that the model is linear in its coefficients (the betas), not that the
# relationship between the predictors and the outcome must be a straight line.
# This means we can include transformed predictors—such as squared terms,
# cubic terms, or other functions of variables—to capture curved relationships,
# while still estimating the model using linear regression techniques.

# For example, including a term like X^2 allows the model to fit a curve rather
# than a straight line. As long as the model remains linear in the coefficients
# (e.g., β1X + β2X^2), it is still considered a linear model, even though the
# relationship between X and the outcome is nonlinear.

# If you are curious what a nonlinear coefficient would be, an example might be
# Y = b0 + exp(b1)*X. But this is not what we will be talking about when referring
# to "nonlinear" models.

# There's multiple types of nonlinear models that you can run depending on what
# type of nonlinearity is present in the data. 

# We will be exploring the nonlinearity of how much an individual was afraid
# of a "piggy jump scare" at a haunted house in predicting how much they
# enjoyed the piggy jump scare. 

# It's possible that individuals only enjoyed the piggy when it wasn't scary or 
# was really scare, but they did not care for it when it was moderately scary. 
# This would be a nonlinear relationship.

# ---------------------- Visualizing Nonlinearity ----------------------

# 1.2 Visualizing Nonlinearity

# Typically, its important to visualize your data before finalizing your model, 
# but this is especially the case whe your model might be nonlinear. We need to
# see the type of nonlinearity that might be present to determine what kind of
# nonlinear model we should fit. If the relationship is a U shape, we might
# want to fit a quadratic model, where if the relationship is more like a 
# W, maybe a quartic model would be better, etc.

# Start by plotting the outcome afainst the predictor to get a general
# idea of the relationship's curve.

gf_point(EnjoymentPiggy~FearPiggy, data = feardata) +
  geom_smooth(method = "lm")

# The scatterplot doesn't seem super nonlinear, but you can see how the clustering
# of points starts to become lower on the the ends of the plot. This might be
# due to some trace amount of nonlinearity in a ∩ shape. So we can test for that.
# The fitted linear line in general is terribly flat, so it doesn't seem
# like there might be much of a linear relationship (spoiler, its non-significant). 
# However, keep in mind that flat linear line can still manifest as a curved 
# polynomial line (think how  the average of the U shape plot would still amount 
# to a flat line).

# But to further investigate this, we can make a regular linear model and plot
# the residuals against FearPiggy. 

linear_model <- lm(EnjoymentPiggy~FearPiggy, data = feardata)
gf_point(resid(linear_model)~FearPiggy, data = feardata) +
  geom_smooth(method = "lm")

# Recall that the better the straight line model fits, the closer the average
# of the residual points will be to 0. But we can also tell at what ends of the 
# model does the straight linear line fit the worst. By looking at the left and
# right ends (FearPiggy = 0, and FearPiggy = 8), we can see the largest deviation
# from the predicted line (which would be at 0 for zero predicto error). So our
# straight line fits the worst at the ends, which is roughly what we took away
# from the previous plot.

# These residual plots are helpful at diagnosing at what points in the plot is the
# linear line fitting the worst. So lets try to see if we can fit a nonlinear
# line to detect significant nonlinearities given that the linear relationship
# clearly is nonexistent. 

# Spoiler, the nonlinear models also don't fit very well, and this is a result
# of not having good data foreplanned when making these labs. For future TA's 
# that might read this, I suggest looking around for an actual nonlinear variable
# and dataset.

# ====================== Polynomial Models ======================

# 2.1 Polynomial Models

# A polynomial line is a line that has one or more inflection point (i.e., a 
# local minimum or maximum). Polynomial lines are determined by an exponential
# where the type of polynomial line is the degree of the exponential.

# X = straight line (not polynomial)
# X^2 = quadratic line (one minimum or maximum, basically a U shape)
# X^3 = cubic line (one minimum a one maximum)

# Typically, a cubic line is the highest you will get as it will be rare to find
# a model that can has a higher trend than a cubic, and even then true cubic
# models are very rare (or hard to estimate with good power even if they do exist). 
# You also want to avoid overfitting a model by going to high on the exponential.

# For those unfamiliar, "overfitting" a model means to build a model that fits
# the current dataset very closely, including random noise or idiosyncrasies,
# rather than capturing the true underlying pattern that stems from the population.
# Aka, it mixes up noise from signal. Or it over emphasizes weird sample-quirks,
# making generalization to the population less sound.

# You can't really know at what point you are overfitting, so you generally need
# to stay away from the possibility. 

# I'm going to assume most people reasing this won't have a background in calculus
# but essentially, whenever you make a polynomial line, you must include all
# degrees of the slope.

# Example Quadratic:
#   Y = b0 + b1*X + b2*X^2

# Example Cubic:
#   Y = b0 + b1*X + b2*X^2 + b3*X^3

# Each coefficient of X describes a different specific aspect of the curve, but that's
# beyond the mathematical requirements of the class. What you DO need to know, is
# that whenever you are fitting the function, you need to include each X variable.

# 2.2 Quadratic Models

# I will be focusing on fitting a quadratic model since this seems like the most
# plausible polynomial model for our example.

# To do a quadratic model you will need to get a squared term of the predictor.
# To do this, you have TWO options. One is to create a new variable that is 
# simply the old variable squared. 

feardata$FearPiggy2 <- feardata$FearPiggy^2

# Fit this into the data to get the output.

polynomial_model <- lm(EnjoymentPiggy~FearPiggy + FearPiggy2, data = feardata)
summary(polynomial_model)

# Keep in mind that 

# The other option is to use the I() function in a lm() model, which allows
# you to transform a variable mid lm() function.

polynomial_model2 <- lm(EnjoymentPiggy~FearPiggy+I(FearPiggy^2), data = feardata)
summary(polynomial_model2)

# For any kind of polynomial regression in PROCESS, you need to make a new
# variable that is the transformed predictor (which I already did above). 

process(data = feardata, y = "EnjoymentPiggy", cov = "FearPiggy",
        x = "FearPiggy2")

# You don't need to separate the two FearPiggy terms between the "cov = " and 
# "x = parts", I just did this arbitrarily.

# As we can see from these outputs, the quadratic line isn't a good fit either.
# You can tell this from the significance of the quadratic term, which is not
# significant t(99) = -1.77, p = .242. So we fail to reject that there's a 
# quadratic relationship between how much one is afraid of the piggy jump scare
# and how much one enjoys the piggy jump scare. You can also tell that by the
# non-significant R^2 in this case. Keep in mind though, you can still have a 
# significant R^2 if you have significant coefficients along with the quadratic
# term, so R^2 isn't a full proof way of testing the nonlinear trend, unless
# you do a delta-R^2 test with and without the nonliner term.

# Generally, you don't interpret the polynomial term because its interpretation
# doesn't make that much sense. You can interpret the linear FearPiggy as the 
# linear relationship (similar to what is done normally).

# If you want to understand this coefficient intuitively, you can think of the 
# FearPiggy^2 term as an interaction (if you haven't done the Interaction lab yet, 
# you might want to come back to this). Where the effect of FearPiggy depends
# on the level of FearPiggy. That is, knowing the polynomial coefficient is
# about -.05, we know that as FearPiggy increases, the effect of Fearpiggy
# on EnjoyPIggy decreases. Essentially, the effect of FearPiggy is stronger
# the less scared the individual was. As the individual got more scared, 
# FearPiggy becomes a worse predictor. 

# 2.3 Plotting Polynomial Models

# Similar to how we plotted the linear line, now we want to plot the fit of the 
# quadratic line to see how well it works. You can get the residuals wherever. 

gf_point(resid(polynomial_model)~FearPiggy, data = feardata) 

# The residuals of the quadratic model seem the same as the linear model, so it
# does not seem that the quadratic model fit better, which makes sense since the
# quadratic term was not significant.

# You can see what the predicted quadratic line should look like by fitting the
# predicted scores against the predictor (don't fit a geom_smooth() line).

gf_point(predict(polynomial_model)~FearPiggy, data = feardata) 

# As you can see, we are predicting the points should follow this polynomial line.
# NOTE, don't be fooled by the shape here. Note the range of the Y-axis. It's 
# very small. So, in reality, the quadratic line will be very flat, which is
# why it is non-significant. 

# We can plot both at once to see the difference between predicted scores and
# residuals.

gf_point(resid(polynomial_model) ~ FearPiggy, 
         data = feardata, color = "red") %>%
  gf_point(predict(polynomial_model) ~ FearPiggy, 
           data = feardata, color = "blue")

# So the residuals were generally much lower than the quadratic line predicted.
# Like I said before, you can see how the quadratic line is much flatter than the
# unadjusted plot shows.

# So in conclusion, a polynomial nonlinear model was a poor fit for our data.

# ====================== Piecewise Regression ======================

# 3.1 Piecewise Regression 

# Piecewise regression fits separate linear relationships across different
# ranges of a predictor variable by introducing one or more breakpoints (also
# called knots). Instead of assuming a single straight line describes the entire
# relationship, piecewise regression allows the slope to change at specified
# values of the predictor. This results in multiple line segments that are joined
# together, often continuously, at the breakpoints. 

# Each segment has its own slope, which lets the model capture nonlinear changes 
# in the relationship across different ranges of the predictor. This is especially
# useful when the effect of a variable is not constant—for example, when a
# predictor has little effect up to a certain point and then becomes stronger,
# weaker, or changes direction after that point. A common example of this might
# be A/B clinical designs. Piecewise regression provides a flexible way to
# model such patterns while still using linear regression techniques.

# Basic piecewise regression only has one breakpoint. With more technical knowledge
# you can fit multiple breakpoints, which can help model certain experiments 
# like A/B/A experimental designs. 

# As a side note, piecewise regression is a type of "spline" regresison, which 
# seeks to fit models from line segments. Piecewise regression fits straight lines,
# but is possible to fit complex curvatures using smoothing functions. Though,
# unless you have very complex data, you would most likely overfit while doing so.
# (I've only ever seen the need for more complex splines in econometrics since
# they have large datasets with complex nonlinearities and interaction effects.
# Most nonlinearities in Psychology could probably be fit with a couple of 
# piecewise joints at best or a growth curve model at worst). 

# 3.2 Segmented Piecewise Regression 

# An uncommon piecewise model that you can fit is a segmented model, where the 
# two lines are disconnected. So basically, there will be two lines with 
# different slopes and intercepts, its just the second half of the first line
# disappears, and the first half of the second line disappears.

# To make this kind of piecewise line, we need to make a breakpoint somewhere.
# I'm going to pick FearPiggy = 5 to be the breakpoint since that's in the middle
# of the scale. 

# To do this, first we need to make a unique indicator variable that ahs the value
# of 0 for each FearPiggy value that is < 5, and 1 for each FearPiggy value that
# is ≥ 5. 

feardata$XJ <- (feardata$FearPiggy >= 5) # >= is greater than or equal to

# You always include the break point into the second line segment. This is
# because the second lines' correct start point will depend on the value of the
# break point. I.e., if it does not include 5, the breakpoint will NOT be at 5 but
# at the next value (FearPiggy = 6). 

# Now make an interaction that will represent the slope of FearPiggy after 
# the value of 5. 

feardata$FearPiggyXJ <- (feardata$FearPiggy*feardata$XJ)

# This step creates the second line segment basically, so it only includes values
# 5 or above. 

# Then fit a model with the regular FearPiggy variable, the break point indicator
# XJ, and the interaction FearPiggyXJ. 

piecewise1 <- lm(EnjoymentPiggy~FearPiggy+XJ+FearPiggyXJ, data = feardata)
summary(piecewise1)

# Looking at the output, we can see that the slope of the first line segment
# (FearPiggy = .20) is not significant. The second coefficient 
# (FearPiggyXJ = -.42) is the difference between the slope of the first line 
# segment and the second line segment. Since this is also not significant, 
# we determine that there is no significant difference between the slope of
# line segment one and line segment two. If you want the value of the slope of
# the second line segment, you can add the first slope and the difference 
# together in the arithmetic: interaction - slope.

-0.3829 - 0.1132

# So the slope of the second line segment is about -.27. However, if you want
# the significance of this, you need to switch the coding scheme of XJ and rerun
# the model. Note, the line segments need the same length, so the new indicator
# needs to be for everything < 5.

feardata$XJ2 <- (feardata$FearPiggy < 5)
feardata$FearPiggyXJ2 <- (feardata$FearPiggy*feardata$XJ2)

piecewise2 <- lm(EnjoymentPiggy~FearPiggy+XJ2+FearPiggyXJ2, data = feardata)
summary(piecewise2)

# The slope of the second line segment was not significant. This os nsurprisng
# given that the slope of FearPiggyXJ was not significant. So we know there that
# should be no difference between the two slopes. Note that the slope of the 
# interaction has the same magnitude (.38) but the sign will change to keep
# the arithmetic consistent.

# The general interpretations of the slopes is the same as we've done before,
# it's just you need to specify that you are only interpreting for a specific
# range of values.

# Example slope of first segment:
#   For two individuals who are below a score of 5 in their fear of the piggy
#   jump scare and are differ in one unit of their fear, the individual who
#   was more afraid is expected to be about .11 point higher in their enjoyment
#   of the piggy jump scare.

# The other slope has a similar interpretation, but now you should specify its for
# individuals who had a score of 5 or above. 

# Example of slope of line break interaction:
#   The difference in slopes for individuals who have a fear piggy score of less
#   than 5 and those who have a score of 5 or greater is about .38

# If you think about it, the piecewise line is basically an interaction term 
# both in interpretation and in function! (Come back to this idea if you haven't 
# done the Moderation lab section yet).

# Plotting the predicted scores we can see the general shape of the segmented
# piecewise regression we fit.

gf_point(predict(piecewise1)~FearPiggy, data = feardata)

# As one can see, there's is a two different lines with different projections of
# start and end, which is what I was getting at with the "different slopes and
# intercepts" at the beginning. However, don't be confused by the scale of the 
# Y-axis, these are actually very small slopes. To show, I'll plot these with
# the residuals.

gf_point(resid(piecewise1) ~ FearPiggy, 
         data = feardata, color = "red") %>%
  gf_point(predict(piecewise1) ~ FearPiggy, 
           data = feardata, color = "blue")

# As you can see, the piecewise segments at the top are incredibly flat to the
# point where you can hardly tell its a piecewise regression. The residuals
# are also very far off, which explains why the model doesn't fit super well.

# 3.2 Joint Piecewise Regression 

# The joint piecewise regression is a much more popular technique of nonlinear
# regression models as it allows a trend to continue directly off of one line
# to another line. This is helpful for within-subjects studies, which are probably
# the most common scenario you would want to run this type of model for. 

# In concept, a joint piecewise regression uses the "breakpoint" or knot to act
# as a joint that connects the lines together, like a door-hinge. Mathematically, 
# the joint piecewise model is two lines with different slopes but the same 
# intercept, its just one line goes to the right from the intercept and one line
# goes left from the intercept.

# To make this model, you need to start by making the indicator term (see the
# segmented piecewise model for explanation).

feardata$XJ <- (feardata$FearPiggy >= 5) # >= is greater than or equal to

# Next you need to center your predictor at the joint. To "center" just means
# to subtract your predictor by the value you wish to "center" at. I'm going
# to subtract FearPiggy by the knott and save it into a new variable.

feardata$FearPiggyc <- feardata$FearPiggy - 5 

# This makes all values of FearPiggy = 5 to become 0, all values less than
# 5 are negative, and all values greater than 5 are positive. Hence the term
# "centering." 

# Then you need to make the interaction term with the indicator and your
# new centered variable.

feardata$FearPiggycXJ <- (feardata$FearPiggyc*feardata$XJ)

# Then just run the model with the centered FearPiggyc and the interaction
# FearPiggyc. DO NOT add the indicator XJ term. Doing so will result in a 
# segmented piecewise line.

piecewise3 <- lm(EnjoymentPiggy~FearPiggyc+FearPiggycXJ, data = feardata)
summary(piecewise3)

# The slope of FearPiggyc (.21) is the slope before the joint, and the slope
# of the interaction FearPiggycXJ (-.42) is the difference in slopes before and
# after the joint. If you want to find the slope of the line after the joint, 
# just add these two together in the arithmetic: interaction + slope

-0.4191 + 0.2067

# So the slope after the joint at 5 is about -.21. The slope before the
# joint is not significant, and the interaction at the joint is not significant
# either. So we don't expect a non-zero difference between the before joint
# slope and after joint slope. So its likely that .21 and -.21 are not
# statistically significantly different from eachother. However, to figure out
# the signficiance of the after joint slope, we need to change the indicator.
# This is akin to what you did with dichotomos predictors. 

# The line segments need the same length, so the new indicator needs to be for 
# everything < 5.

feardata$XJ2 <- (feardata$FearPiggy < 5)

# Make an interaction with the centered FearPiggyc.

feardata$FearPiggycXJ2 <- (feardata$FearPiggyc*feardata$XJ2)

# Then plug this into the model with the centered FearPiggyc. 

piecewise4 <- lm(EnjoymentPiggy~FearPiggyc+FearPiggycXJ2, data = feardata)
summary(piecewise4)

# Here we can see that the slope of the line after the joint at 5 is not 
# significant, like we expected. Note that the magnitude of the interaction (.42)
# is the same, but the sign changed to keep the arithmetic consistent. 

# The general interpretations of the slopes is the same as we've done before,
# it's just you need to specify that you are only interpreting for a specific
# range of values.

# Example slope of first line:
#   For two individuals who are below a score of 5 in their fear of the piggy
#   jump scare and are differ in one unit of their fear, the individual who
#   was more afraid is expected to be about .21 point higher in their enjoyment
#   of the piggy jump scare.

# The other slope has a similar interpretation, but now you should specify its for
# individuals who had a score of 5 or above. 

# Example of slope of line break interaction:
#   The difference in slopes for individuals who have a fear piggy score of less
#   than 5 and those who have a score of 5 or greater is about .41.

# If you think about it, the piecewise line is basically an interaction term 
# both in interpretation and in function! (Come back to this idea if you haven't 
# done the Moderation lab section yet).

# Plotting the predicted scores we can see the general shape of the joint
# piecewise regression we fit.

gf_point(predict(piecewise3)~FearPiggy, data = feardata)

# We can see that unlike the segmented piecewise model, this piecewise model
# connects when FearPiggy equals 5! Like I mentioned above with there being two
# lines of equal intercepts but just two different slopes, you can imagine
# the Y-axis splitting down the middle at FearPiggy = 5, where there is one
# line to the left that ends in the middle, and one line that lies to the 
# right and ends in the middle. If you were to plot both of these lines in full
# their intercept between the two would be at FearPiggy = 5, but they just
# have different slopes. This is how the joint piecewise line works conceptually
# differen than the segmented piecewise line. 

# However, don't be fooled by the scale of the Y-axis. If you notice, it's actually
# a very small range, meaning our joint piecewise line is very flat, which is
# what we'd expect with two non-significant slopes. But to see the full extent,
# I can plot the predicted scores with the residuals here.

gf_point(resid(piecewise3) ~ FearPiggy, 
         data = feardata, color = "red") %>%
  gf_point(predict(piecewise3) ~ FearPiggy, 
           data = feardata, color = "blue")

# You can see at the top in blue our predicted scores. It hardly looks like
# a piecewise line because of how non-significant it is. The residuals in red 
# are far off from the predicted score line, which goes to show why our model
# is such a poor fit.

# If you ever wanted to do more than one joint for a piecewise regression, 
# you can add more knots, but instead of being dummy coded like XJ was, you
# need to make these knots sequentially coded (see the Multicategorical Predictors
# lab). The interpretation just gets more additive and complex, but you can probably
# get AI to easily help with that.

# ====================== Log Transformations ======================

# 4.1 Log Transformations

# Log transformations are probably the most universally applied method for
# dealing with nonlinearity. Essentially, when you have a variable that is
# notably skewed, you can rescale it using logarithms, which reshapes the
# distribution by pulling in large values and spreading out smaller ones. This
# often makes relationships more linear, stabilizes variance, and improves how
# well regression models fit the data. 

# NOTE NOTE NOTE! Whenever you see a "log" in statistics, it always refers
# to a natural log (which is otherwise denoted as "ln" in other domains or
# manual calcualtors). This is because logarithms of other bases are virtually
# never used in statistics meanwhile the natural logarithm is incredibly helpful
# for a variety of reasons. 

# When you rescale a variable with a natural logarithm, you are compressing
# larger values more than smaller values, which helps reduce skew and makes
# extreme values less influential. Natural logarithms also change the
# relationship between variables so that multiplicative or exponential patterns
# become more linear and easier to model using regression. 

# So the log() function in R gives you the natural log by default. Inversely,
# if you ever want to use Euler's number "exponentiation", use the exp() function.

# For quick maths, exp(log(X)) = X and log(exp(X)) = X, though interpretations
# will change when you do something like this with your regression variables,
# which I will cover later below.

# 4.2 Log Transformations in R

# A caveat of log transformations is that the variable must be positive. So if 
# you have negative values or zeros, you need to shift your data.

# Since our data has 0's in it, we need to shift the scale up by 1. If we didn't
# we'd get negative infinities by taking log(0).

feardata$FearPiggyc2 <- feardata$FearPiggy + 1 
feardata$EnjoymentPiggyc <- feardata$EnjoymentPiggy + 1 

# Log transformations can be applied to predictors or outcomes. Using our
# recentered variables, I will show some plots of each.

# Non-transformed model
gf_point(EnjoymentPiggyc~FearPiggyc2, data = feardata)%>%
  gf_lm()

# Log-transformed Y (note the points cut off )
gf_point(log(EnjoymentPiggyc)~FearPiggyc2, data = feardata)%>%
  gf_lm()

# Log-transformed X
gf_point(EnjoymentPiggyc~log(FearPiggyc2), data = feardata)%>%
  gf_lm()

# Both Y and X log-transformed
gf_point(log(EnjoymentPiggyc)~log(FearPiggyc2), data = feardata)%>%
  gf_lm()

# What you can note in the above plots is the scale of the Y and X-axes. They
# become more condensed. In general, the data condenses more on the right side,
# which if you know how logarithms scale, this make sense. Briefly, logs scale
# in magnitudes to how far away they are from 0. So the distance of 1 to 2 in 
# log units is of magnitudes more than the distance of 0 to 1. So essentially,
# all the points further out become squished inwards whereas the points close to
# 0 don't get squished as much.

# You can see this in the histogram, as the histogram of the log-variable 
# condenses the larger values together (that's part of how log-transformations
# it help with nonnormality and nonlinearity.

hist(feardata$EnjoymentPiggyc)
hist(log(feardata$EnjoymentPiggyc))

# You can add slight modifications to the logarithms to adjust the severity of 
# transformations. You should not really play around with this unless you know
# what you are doing because its bad practice to just guess numbers and transform.
# AKA you WILL get critique in reviewer feedback about this.
# It not only can over fit but doing this method will ruin interpretability. 
# Interpreting log units is hard enough, but doing partial deviations of 
# log units are not interpretable. 

# To visualize how much these deviations make an impact on the data (which isn't
# that much becuase this data isn't the best): 

gf_point(log(EnjoymentPiggyc+.01)~log(FearPiggyc2+.01), data = feardata)%>%
  gf_lm()
gf_point(log(EnjoymentPiggyc+.1)~log(FearPiggyc2+.1), data = feardata)%>%
  gf_lm()
gf_point(log(EnjoymentPiggyc+.5)~log(FearPiggyc2+.5), data = feardata)%>%
  gf_lm()
gf_point(log(EnjoymentPiggyc+1)~log(FearPiggyc2+1), data = feardata)%>%
  gf_lm()

# 4.2 Log Transformation Interpretations

# Interpreting log variables is often difficult bordering on useless. When
# you transform a variable, everything about the variable is in "log" units.

# Let's look at the output for the log-transformed outcome model.

logmodel <- lm(log(EnjoymentPiggyc)~FearPiggyc2, data = feardata)
summary(logmodel)

# To interpret the coefficient of a log transformed outcome, you look at differences
# in units on X in terms of log-units of Y.

# Interpretation of b1:
#    Between two individuals who differ in FearPiggy by 1 point, the more
#    afraid person is expected to be .01 lower in log-units of EnjoymentPiggy.

# These coefficients are essentially percentage changes of the outcome.

# If you want to transform the slope to ge the exact percent change of Y, 
# you can use the equation to get a percent change of the outcome
# %ΔY = (exp(b1)−1)×100

(exp(-.017)-1)*100

# So for two individuals who differ in FearPiggy by 1 point, the more  
# afraid is expected to be 1.68% lower in EnjoymentPiggy.

# If you log-transform a predictor, you cannot simply take the exponential of the 
# slope to back transform. That would be a bad thing to do as the slope now
# represents the change in Y for a one-unit change in log(X), not X itself.
# Because the relationship is now nonlinear, the effect of X depends on its
# current value, so there is no single “back-transformed” slope on the original
# scale. Instead, the coefficient should be interpreted in terms of proportional
# or percentage changes in X (e.g., a one-unit increase in log(X) corresponds to
# multiplying X by a constant factor).

# Let's take an example of the log-transformed predictor model.

logmodel2 <- lm(EnjoymentPiggyc~log(FearPiggyc2), data = feardata)
summary(logmodel2)

# Interpretation of b1:
# Between two individuals who differ in log(FearPiggy) by 1 unit, the individual 
# with the higher FearPiggy is expected to be 0.1186 units higher in EnjoymentPiggy.

# If you want to express this in percentage terms, you need to think about
# proportional differences in X rather than 1-unit differences. A 1-unit difference
# in log(X) corresponds to multiplying X by e (≈ 2.72).

# %ΔY = (exp(b1) − 1) × 100

(exp(0.1186) - 1) * 100

# So, for two individuals where one has about 2.72 times (≈ e times) higher
# FearPiggy than the other, the person with higher FearPiggy is expected to have
# about 12.6% higher EnjoymentPiggy.

# This is why you can't just take exp(b1) and call it a day, since this coefficient
# is not interpreting Y on the original units of X. So interpreting it base on 
# percent change like above needs to be done instead.

# 4.3 Box-Cox Transformations using boxcox()

# Box-Cox transformations can give you an optimal way to transform the outcome
# to better meet the assumptions of linear regression, particularly normality
# and constant variance. Instead of choosing a preset transformation (like log) 
# by guesswork, the Box-Cox method searches across a range of possible
# exponential transformations for the outcome and identifies the one that is 
# the most optimal transformation for improving the model fit 
# (based on log-likelihoods).

# The transformation is controlled by a parameter (lambda, λ), where different
# values correspond to different transformations (e.g., λ = 1 is no change,
# λ = 0 approximates a log transformation, λ = 0.5 is a square root). This makes
# Box-Cox a flexible, data-driven approach to handling skewness and nonlinearity,
# helping produce a model that better satisfies regression assumptions.

# The downside of the Box-Cox method is that it destroys interpretability. That is
# once you Box-Cox transform your variable, their coefficients can't be understood
# conceptually. All you can do is focus on model fit. Due to this reason, this 
# method isn't very popular since most researchers care about interpretability, 
# in which case they need a different method, or they care about model fit, in 
# which case there's better methods to use. 

# Regardless you can do these with the boxcox() function from the "MASS" package.
# First fit the linear model (with positive only variables, so recenter the
# variables if you need to, see Section 4.2).

piggy_linear <- lm(EnjoymentPiggyc~FearPiggyc, data = feardata)

# Then put the model in the boxcox() function and select a sequence of λ 
# transformations to check (-3 to 3 is usually more than enough).

bc <- boxcox(piggy_linear, lambda = seq(-3,3))

# The plot output will give you the log-likelihoods of the mode for every
# λ that was checked. The range within the vertical dotted lines are the best
# fits, with the center dotted line being the most optimal fit. 

# To get the the specific λ value that was best, extract is using this:

best_lambda <- bc$x[which(bc$y==max(bc$y))]
best_lambda

# So the most optimal transformation of the outcome was about 1.42. 

# You can transform the outcome with this exponential manually and rerunning
# the model.

feardata$EnjoymentPiggyBEST <- feardata$EnjoymentPiggyc^ best_lambda
boxcox_model <- lm(EnjoymentPiggyBEST~FearPiggyc, data = feardata)
summary(boxcox_model)

# So this is the most optimal transformation, which really didn't do a whole lot
# since the model wasn't nonlinear and was a poor fit in general. But now we 
# can't even interpret the coefficient! 

