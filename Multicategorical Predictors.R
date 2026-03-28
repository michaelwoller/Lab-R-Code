# ====================== Multicategorical Predictors ======================

# Author: Michael Woller

# ====================== Load Packages ======================

#install.packages("dplyr")    # needed for data management
#install.packages("mosaic")   # gives group descriptive statistics

library(dplyr)
library(mosaic)

# Remember to load in PROCESS. 

# ---------------------- Specify File Path ----------------------

# Specifying file path
filelocation <- "/Users/woller/Documents/250c 2026/Lab 7/covidstress.csv" # wherever on your computer
covid <- read.csv(filelocation, header = TRUE)

# ====================== Multicategorical Predictors ======================

# 1.1 Multicategorical Predictors

# Previously, you learned about single dichotomous predictors, where you have 
# a binary variable with one reference group and a slope that representes the
# difference between the two groups. It is possible to expand this idea into 
# multicategorical variables that have 3+ groups. This can be done through
# various coding techniques that we will go over below. These can be used for
# nominal or ordinal variables, though some of the methods are  more preferable 
# or certain variable types over others.

# A through line through each method is that we will need to have multiple 
# predictors and multiple slopes in the model, approximately one for each group, 
# except that, like a dichotomous predictor, there is a group that is left out
# of the model (the details of which are method dependent). We'll go over the
# specifics when we get to each method. 

# Another through line is that each method can be represented by a coding scheme,
# a "contrast" matrix If you have taken the ANOVA course before regression, this 
# concept will hopefully make sense. If not, then you might need to go over my
# section on the contrast matrix below.

# At the end of the day, all of these methods are comparing group means of the
# outcome. What differes is what *combination* of group means (e.g., mean 1 vs
# mean 2, means 1 & 2 vs means 3, etc.). 

# Our model of interest is if we can predict and invidual's concern over COVID-19
# from the level of education of their mother. The mother's education variable
# has 7 levels:
# 1) "None" (no college)
# 2) "Up to 6 years of school" 
# 3) "Up to 9 years of school"
# 4) "Up to 12 years of school" (Highschool degree)
# 5) "Some College or equivalent"
# 6) College degree
# 7) PhD/Doctorate

# 1.2 Group Descriptives

# Since these methods will all be comparing group means, it will be very helpful
# to check the group descriptive statistics before hand. You can do this numerous
# ways, but I am just using the favstats() function from the 'mosaic' package.

# Get descriptive statistics for COVID_concerns
mosaic::favstats(COVID_Concerns ~ Educ_Mom, data = covid)

# In terms of average COVID-19 concern levels, individuals whose mom has a PhD
# or Doctorate have the highest average level (23.9), whereas the individual whose
# mom has only a highschool degree ("Up to 12 years of school") have the lowest
# average COVID-19 concern (21.59). This can give us a glimpse about what 
# relationships we might see. But lets move onto the coding schemes now.

# A lot of the explanation will be done in the Dummy/Indicator Coding section,
# so if you want to learn more about another coding scheme, you might need to 
# read some of the following section. But as dummy coding is by far the most 
# popular method and that its rules and options apply to the other coding schemes,
# I am going to put the bulk of the material into Section 2. If you are interested
# in the other coding schemes, refer to the dummy coding section for more detail.

# ====================== Dummy/Indicator Coding ======================

# 2.1 Dummy/Indicator Coding

# The most popular method for dealing with multicategorical predictors is to use
# dummy (or indicator) coding. This builds directly on what you have already
# learned about dichotomous (0/1) predictors. Previously, a dichotomous variable
# used 0 and 1 to represent two groups, where the coefficient told us the
# difference between those groups (with 0 as the reference group). Dummy coding
# extends this idea to variables with more than two categories by creating
# multiple 0/1 variables, each comparing one category to a chosen reference group.
# For example, if a variable has three categories, we would create two dummy
# variables, each coded 0 or 1, and each coefficient would represent the
# difference between that category and the reference category. This allows us to
# include multicategorical predictors in regression models while keeping the same
# interpretation framework students already understand from dichotomous variables.

# For the sake of example, we will make the "none" mother's education group as
# the baseline. Therefore, each of the dummy coded variables will represent the
# difference between group means for that respective group and the "none" group.

# ---------------------- Dummy Codes in Base R ----------------------

# 2.2 Dummy Coding with factor()

# There's many ways to dummy code in R, and a lot of programs will automatically
# dummy code variables. What you do might depend on whether or not your
# multicategorical variable is numeric or a character variable. If you do not know
# the class of your variable, you can use the class() function.

class(covid$Educ_Mom)

# So we can see this is a 'character' class object, which just means that
# the values are not numeric. If you are ever curious about the structure of a 
# variable or dataset, I recommend using the str() function.

str(covid$Educ_Mom)

# Here you can see "chr" which stands for a 'character' class, [1:1263], which
# is the sample size of the variable, and the different characters in the variable
# (it might be cut off for being too long).

# What we will do is turn this variable into a 'factor' class object.
# If your variable is a 'character' class, it is actually not necessary you 
# transform it in order to dummy code, but doing so will make steps in the future
# much easier.

# To turn a variable into a factor, we input the variable, we define the levels
# (with names), and we specify the order.

# We'll code this into a new factor variable called "Educ_Mom_Factor" using factor()
covid$Educ_Mom_Factor <- factor( 
  covid$Educ_Mom,
  levels = c("None", "Up to 6 years of school", "Up to 9 years of school",
             "Up to 12 years of school",
             "Some College or equivalent", "College degree",
             "PhD/Doctorate"), # make sure everything is spelled correctly
  ordered = FALSE) 

# The last argument "ordered = FALSE" dictates whether or not the inputted variable
# has to be in order If you want the ability to recode your reference variable,
# this needs to be set at FALSE. Even though our variable is technically ordinal,
# we want this argument to be set to FALSE so we can change the baseline and
# use the variable in other coding schemes as well.

# If your variable is numeric (no character strings, just numbers), converting
# it to a factor variable will be necessary, or else lm() will think your
# variable is continuous. 

# In this case, just list the values in the levels = line (e.g. levels = c(1,2,3,4)).

# Now check how the new variable is recoded wiwth str().

str(covid$Educ_Mom_Factor)

# Now it is classified as a "Factor" object with 7 levels. The group that each 
# individual is categorized as is no longer filled with character strings, as you
# can tell due to the names not being within quotations. Now, each of the group
# names is associated with a factor level in the order that you listed above. 
# E.g. "None" = 1, "Up to 6 years of school" = 2, etc.

# If you want to check what the baseline group in a factor variable is, 
# you can use levels().
levels(covid$Educ_Mom_Factor) 

# Note, the first group in this list will always be the reference baseline group.
# (I will cover how to recode the baseline later).

# If you want a list of the contrasts or the coding table for each group you can
# use contrasts(). This is only possible since, by default, a "Factor" class
# variable will be treated as a dummy coded variable for the purpose of regression.
# Therefore, the baseline above is the top row with only 0's. 

contrasts(covid$Educ_Mom_Factor)

# I will come back to contrasts later on. 

# Next, run this in a model and you can see the output is per dummy coded variable.

Model_Factor <- lm(COVID_Concerns ~ Educ_Mom_Factor, data = covid)
summary(Model_Factor)

# You can see the output is given per dummy coded variable described as the 
# the variable name + the group name. Since "none" is the baseline reference, it 
# is not included as a predictor, and its group mean is the value of the intercept.
# Each coefficient is thus the difference between the baseline and the predictor 
# group following the form: Group - Baseline

# 2.3 Coding Unique Variables

# Another method you can do is to manually create new dummy coded variables
# for each group. This involves making a new column variable that has "1's" for
# rows that are part of that group and "0's" on everything else.

# This isn't necessary for dummy coded variables, However, this can be helpful
# for other coding schemes we will cover next week It's also possible some data
# sets you will work with already have dummy coded variables.

# The following code forms your dummy codes. You can name the variables whenever.
# The order for the levels doesn't matter, but it helps if you keep it in line
# if there is a natural order just for organization.

covid <- transform(covid,
                   d1 = (Educ_Mom == "None"),
                   d2 = (Educ_Mom == "Up to 6 years of school"),
                   d3 = (Educ_Mom == "Up to 9 years of school"),
                   d4 = (Educ_Mom == "Up to 12 years of school"),
                   d5 = (Educ_Mom == "Some College or equivalent"),
                   d6 = (Educ_Mom == "College degree"),
                   d7 = (Educ_Mom == "PhD/Doctorate"))

# The next lines of code inserts 0's into the dummy coded cells that have
# FALSE and 1's into the cells that have 1's. This isn't necessary either, but
# make the output easier on the eyes on my opinion.

cols <- c("d1","d2","d3","d4","d5","d6","d7")

covid[cols] <- lapply(covid[cols], as.integer)

# Note that if your categorical variables is numerically code 
# (i.e., "None" = 1, "Up to 6 years o school" = 2, etc.),
# you should just do Educ_Mom == 1, Educ_Mom == 2, etc. in the transform 
# code above.  Essentially, just put what ever value to the right of the == 
# that the group corresponds to (remember, use "" around character variables)

# Now run your model predicting COVID_concerns with all variables
# PLEASE NOTE that lm() will automatically use the LAST variable as the baseline, 
# so you need to include "d1" very last since we ar eusing "None" education as
# the baseline.

Model_Dummy <- lm(COVID_Concerns ~ d2 + d3 + d4 + d5 + d6 + d7 + d1, data = covid)
summary(Model_Dummy)

# As you can see, the results for this method are identical to the factor() method above

# ---------------------- Dummy Codes in PROCESS ----------------------

# 2.3 Dummy Codes in PROCESS

# Multicategorical predictors are easy to handle in PROCESS. 
# The only caveat is that your variable must be numerically coded.

# If your variable is not numeric, this is simple to do using the factor()
# function above. If your variable is a factor, recreate Line 129. Then run this
# variable through the as.integer()  function to give numeric coding to each 
# group automatically (going from the first factor group, starting at 1, to the
# last group going up one number). The baseline factor will be the lowest in this
# coding, which is important to keep in mind since PROCESS will treat the lowest
# numeric group as the baseline. 

covid$Educ_Mom_Numeric <- as.integer(covid$Educ_Mom_Factor)

# If your categorical variable is already numerically coded, this step is not 
# needed, just make sure the baseline has the lowest value in numeric.

# In PROCESS, use the "mcx = 1" option to dummy code your "x" variable. 
# "mcx" means your X variable is "multicategorical." Note, your multicategorical
# variable *must* go in "x = ". I'll cover adding covariates down below in 
# section 2.9.

process(data = covid, x = "Educ_Mom_Numeric",
        y = "COVID_Concerns",
        mcx = 1) 

# As you can see, PROCESS automatically makes dummy codes (labeled X1 for the 
# first dummy code and so forth). As mentoined before, the lowest numeric coding 
# is the baseline reference group (i.e. "none"). Make sure your desired reference 
# code is coded as the lowest value .

# The order of X variables will be in the order of your numeric coded variable.
# E.g., Group 2's dummy variable is X1, Group 3's dummy variable is X2, etc.

# If you are wondering what the order is specifically, look at the top to see the
# dummy code contrast at the top under the large "Coding of categorical X variable
# for analysis" table. I'll go over this table in much more detail in section 2.8,
# but to simply check what variable corresponds to what group, look at the first
# column for the multicategorical variable (Educ_Mom_Numeric in this case).
# For each group, you can look over at the X columns to see what X each
# group corresponds to by looking which column the group has a "1" in. 

# E.g., Education group 1 has no 1's and only 0's, so it's the baseline. Education
# group 2 has a 1 in the X1 column, so its dummy variable is X1. Education group
# 7 has a 1 in the X6 column, so its dummy variable is X6. Etc. 

# ---------------------- Dummy Variable Interpretations ----------------------

# 2.5 Dummy Code Interpretations

# As dummy coded slopes are just the difference between the reference group and
# the dummy group, the slopes are simply interpreted as the mean difference.

# Intercept:
#   The mean COVID-19 concern for individuals whose mothers have no education is
#   about 22.08 points.

# B1: 
#   Individuals whose mothers have up to 6 years of schooling have about .39 more
#   average COVID-19 concern when compared to individuals whose mothers have
#   no education.

# B2: 
#   Individuals whose mothers haveup to 9 years of schooling have about .21 less
#   average COVID-19 concern when compared to individuals whose mothers have
#   no education.

# B3: 
#   Individuals whose mothers have up to 12 years of schooling have about .49 less
#   average COVID-19 concern when compared to individuals whose mothers have
#   no education.

# B4: 
#   Individuals whose mothers have some college or equivalent education 
#   have about .23 less average COVID-19 concern when compared to individuals
#   whose mothers have no education.

# B5: 
#   Individuals whose mothers have a college degree have about .19 more
#   average COVID-19 concern when compared to individuals whose mothers have
#   no education.

# B6: 
#   Individuals whose mothers have doctorate degree have about 1.82 more
#   average COVID-19 concern when compared to individuals whose mothers have
#   no education.

# However, none of the slopes are significantly different than zero. Therefore,
# we would fail to reject the null that any of these groups have mean COVID-19
# concern differences in the population. 

# ---------------------- Recoding Baseline Group ----------------------

# 2.6 Recoding Baseline Group

# Since dummy variable slopes only give the difference between the baseline and
# the dummy group, it doesn't inherently extend flexibility to any group 
# comparison. E.g., if you wanted to compare "Some College or equivalent" with 
# "PhD/Doctorate" you would need to change the baseline group to either
# "Some College or equivalent" or "PhD/Doctorate". Depending on how you created
# your dummy variables, there's a few ways to do this that I will cover.

# I will repeating the interpretations for each subsection, presuming that the 
# reader might not reread each section. 

# 2.7 Recoding Factor Variables

# If you want to change the baseline group for a factor class predictor, you 
# just need to make a new factor variable that switches the order. 
# You can do this with the relevel() function easily.

# The following code recodes the groups to have "College degree" as the baseline 
# into a new variable called Educ_Mom_Factor2. Add the new baseline into the
# "ref = " option.

covid$Educ_Mom_Factor2 <- relevel(covid$Educ_Mom_Factor, ref = "College degree")

# You can confirm the new reference group using the levels() function.

levels(covid$Educ_Mom_Factor2)

# "College degree" is first in this list, so we know its the reference group now.

# Another method you can do is to simply change the order in the initial factor() 
# function by putting "College degree" first. The benefit of this method is you 
# can change the order of each group, if that is something that matters to you.

covid$Educ_Mom_Factor2 <- factor( 
  covid$Educ_Mom,
  levels = c("College degree", "None", "Up to 6 years of school", 
             "Up to 9 years of school", "Up to 12 years of school",
             "Some College or equivalent", "PhD/Doctorate"), 
  ordered = FALSE)

# Then all you need to do is fit the new model.

Model_Factor2 <- lm(COVID_Concerns ~ Educ_Mom_Factor2, data = covid)
summary(Model_Factor2)

# Now the intercept equals the mean COVID-19 concern for individuals whose
# mothers have some college or equivalent in education (you can fact check this
# if you want). The slope for the PhD dummy slope is about 1.64, which means 
# that the difference in mean COVID-19 concern between individuals whose mother
# has some college education is 1.64 lower than the mean for individuals whose 
# mothers have a doctorate degree. This is not significant, so this is not 
# a statistically significant distinction.

# If you ever want to check the contrast matrix for your new factor variable,
# you can use the contrast() function with your factor class variable.

contrasts(covid$Educ_Mom_Factor2)

# I think this is easier to understand if you ignore the column names, and think of 
# each column as representing dummy variable 1 through dummy variable 6 (like
# what's done in the ROCESS output).

# 2.7 Reordering Unique Variables for PROCESS

# If you did individual variables for each dummy code, you just need to
# place the one you want as the baseline last. Since "d6" was the dummy variable
# for "Some College or equivalent", just put that last in the lm() model.

Model_Dummy2 <- lm(COVID_Concerns ~ d1 + d2 + d3 + d4 + d5 + d7 + d6, data = covid) 
summary(Model_Dummy2)

# Now the intercept equals the mean COVID-19 concern for individuals whose
# mothers have some college or equivalent in education (you can fact check this
# if you want). The slope for the PhD dummy slope is about 1.64, which means 
# that the difference in mean COVID-19 concern between individuals whose mother
# has some college education is 1.64 lower than the mean for individuals whose 
# mothers have a doctorate degree. This is not significant, so this is not 
# a statistically significant distinction.

# 2.8 Numeric Coded Variables

# If your variable is numeric, there are a few options
# The first option is to recode the factor like above and then just save it 
# as numeric again. 

covid$Educ_Mom_Numeric2 <- as.integer(covid$Educ_Mom_Factor2)

# Now the new baseline variable will be the lowest value, which can be used with 
# PROCESS (keep in mind, if you are using lm(), you can't use numeric variables).

# Another option for PROCESS is to somehow make your desired reference code 
# into the smallest value regardless. Since PROCESS automatically makes the 
# lowest non-negative value the baseline reference, all you need to do is make 
# the desired baseline lower in value than all the others. I will make the numeric
# code for "Some College or equivalent" equal 0 and save it into a new variable.

covid$Educ_Mom_Numeric3 <- covid$Educ_Mom_Numeric 
covid$Educ_Mom_Numeric3[covid$Educ_Mom_Numeric == 6] <- 0 

# For new R users, the last line of code finds every row that corresponds to the
# old variable equaling 6, and inserts a 0 into the corresponding row for the
# new variable. Since no group in this dataset was coded as 0, no need to 
# recode that group. If there was a group that was coded as 0, you would need
# to change that value first.

# Both numeric coding stules will give you the same PROCESS output using.

process(data = covid, x = "Educ_Mom_Numeric2", y = "COVID_Concerns", mcx = 1)
process(data = covid, x = "Educ_Mom_Numeric3", y = "COVID_Concerns", mcx = 1)

# Yay! They're the same! Just keep in mind in the dummy contrast matrix at the top
# that the new group "1" and "6"  in the latter model is NOT the old group 1 and 6.

# 2.9 Custom Contrast Method

# This section goes into a lot of detail, so bare with me, but this will be helpful
# for the other, non-dummy code methods or if you are unfamiliar with what a 
# contrast is.

# Another method to change the coding scheme is to manually create a contrat 
# matrix that matches the coding scheme you want to do. You can check the current
# coding scheme by using contrast() on your factor variable:

contrasts(covid$Educ_Mom_Factor)

# I think its easier to understand if you ignore the column names, and think of 
# each column as representing dummy variable 1 through dummy variable 6 (like
# what's done in the ROCESS output).

# You can also check the the matrix by looking at the at the top table in the 
# PROCESS output:

process(data = covid, x = "Educ_Mom_Numeric", y = "COVID_Concerns", mcx = 1)

# This contrast comparison dictates what specific groups will be compared to others.

# These contrast matrices are specifically for dummy variables, but you can craft
# them in any specific way, even to compare more than two groups at a time, if you
# know how to properly specify the matrix. 

# This method will become very important when learning about other coding 
# schemes than dummy codes as many of them compare multiple group averages against
# others. 

# To understand how this works, look at the contrast matrix. What ever group has 
# all 0's across each column is the baseline as it is not included in the model. 
# And for each group row, whatever column has a "1" in it represents what 
# that dummy code is going to be

# The previous dummy code methods I discussed automatically change this contrast
# matrix whenever you recode the baseline. However, you can do this manually by 
# changing the matrix directly. 

# All you need to do is change the baseline group is to find the group row you 
# want as the baseline, change all of its values to 0, then go back and 
# change the old baseline to be included in some variable (make one its
# columns include a 1 for a variable that doesn't have a group). 

# This takes a bit of mental organization

# What you will need to do is to write out in one c() list the order of 1's 
# and 0's from the first row progressing down each row.

# If we look at the regular output from PROCESS before recoding, we get 

# Coding of categorical X variable for analysis: 
# X1        X2        X3        X4        X5        X6
# 0.0000    0.0000    0.0000    0.0000    0.0000    0.0000
# 1.0000    0.0000    0.0000    0.0000    0.0000    0.0000
# 0.0000    1.0000    0.0000    0.0000    0.0000    0.0000
# 0.0000    0.0000    1.0000    0.0000    0.0000    0.0000
# 0.0000    0.0000    0.0000    1.0000    0.0000    0.0000
# 0.0000    0.0000    0.0000    0.0000    1.0000    0.0000
# 0.0000    0.0000    0.0000    0.0000    0.0000    1.0000

# This 7 x 6 matrix can be written as the list going down each row one by one as:
# c(0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1)

# Or, if you want to keep helpful mental organization and include line breaks, 
# the list is:
# c(
#   0,0,0,0,0,0,
#   1,0,0,0,0,0,
#   0,1,0,0,0,0,
#   0,0,1,0,0,0,
#   0,0,0,1,0,0,
#   0,0,0,0,1,0,
#   0,0,0,0,0,1
# )

# What you want to do is then move the row solely consisting of 0's 
# (representing the reference group) to a new row you want as the reference 
# group, and then go back and change the original reference group row. 
# Since "College Degree" is our desired baseline reference (which is the 6th 
# group and thus the 6th row in the matrix), we move the row of 0's to the 6th line 
# and go back and change X5 (the old variable for "College Degree") to
# represent "none". What you can do is save this matrix as a new variable in R 
# to input into PROCESS later.

New_Contrast <- c(
  0,0,0,0,1,0, # "none" is now X5
  1,0,0,0,0,0,
  0,1,0,0,0,0,
  0,0,1,0,0,0,
  0,0,0,1,0,0,
  0,0,0,0,0,0, # "College Degree" is now excluded as the baseline group
  0,0,0,0,0,1
)

# Note, if you wanted, you can completely reorganize the variables so that 
# X1 = "none", X2 = "Up to 6 years of school", and so forth, but 
# this isn't necessary.

# You can now input this custom contrast into PROCESS by using the "mcx = 5" 
# option. When this option is set to 5, PROCESS knows to read in a custom
# contrast for the multicategorical predictors. You then have to input this
# cusstom contrast into the "xcatcode = " option.

process(data = covid, x = "Educ_Mom_Numeric", y = "COVID_Concerns",
        mcx = 5, 
        xcatcode = New_Contrast) 

# If you look at the top matrix, you can see now it uses are new reference coding!
# The output should be identical to the factor() function method used above.

# If you want to fit a custom contrast in base R, you can input the above contrast
# into the "contrasts" function of a new variable in your dataset. This contrast
# must be a matrix and not a c() string, however.

# Use the matrix() function, plug in your contrast c() string, then state that the
# matrix has 7 rows (nrow) and state you want the matrix to be split up by row.
# This will mean that after each 7 values in the string, the matrix will be 
# split into the next line. Since this is a 7 x 7 matrix, we just need to split
# 7 columns by 7 rows. 

New_Contrast2 <- matrix(c(
  0,0,0,0,1,0,  # row 1
  1,0,0,0,0,0,  # row 2
  0,1,0,0,0,0,  # row 3
  0,0,1,0,0,0,  # row 4
  0,0,0,1,0,0,  # row 5
  0,0,0,0,0,0,  # row 6 = baseline ("College Degree")
  0,0,0,0,0,1   # row 7
), nrow = 7, byrow = TRUE)

# Then plug this into the contrast() function with the variable as a factor
# class. I'm just going to make a third factor variable now.

covid$Educ_Mom_Factor3 <- covid$Educ_Mom_Factor

# Then, plug this factor variable into the contrast() function, and save the 
# above contrast matrix into it.

contrasts(covid$Educ_Mom_Factor3) <- New_Contrast2

# If you check the contrast again, you can see how it changed (and the columns
# make more sense this time). 

contrasts(covid$Educ_Mom_Factor3)

# All you have to do now is add the factor variable back in, and the new
# contrast will be applied, thus getting you the new dummy codes.

Model_Dummy3 <- lm(COVID_Concerns ~ Educ_Mom_Factor3, data = covid) 
summary(Model_Dummy3)

# NOTE, if you do this method of creating custom contrasts, you need to make
# sure that the contrast is correct. 


# ---------------------- Multicategorical with Covariates ----------------------

# 2.10 Multicategorical with Continuous Covariates

# Adding continuous covariates to the model is straightforward. The same rules 
# for the baseline group follows as what was outlined in the previous sections.
# The only thing to do is to add the variables. This is straight forward when 
# using lm() in base R. 

# For the factor variable method, simply add the covariate, which in this case 
# will be COVID_Compliance (a self measurement on how much an individual complies
# with COVID-19 policies). 

Model_Factor3 <- lm(COVID_Concerns ~ Educ_Mom_Factor + COVID_Compliance,
                    data = covid)
summary(Model_Factor3)


# For the unique dummy variable method, just add the covariate in. R can still
# distinguish which variables are the dummy variables, and will still use the
# last one is the reference code (which is the "none" group).

Model_Dummy3 <- lm(COVID_Concerns ~ d2 + d3 + d4 + d5 + d6 + d7 + d1 + COVID_Compliance,
                   data = covid)
summary(Model_Dummy3) 

# In PROCESS, you can include the continuous covariate into either the "x =" option
# or the "cov = " option. If you put it in "x = ", the multicategorical variable
# must come first in the list. 

process(data = covid, x = c("Educ_Mom_Numeric", "COVID_Compliance"),
        y = "COVID_Concerns", mcx = 1)


# 2.11 Categorical covariates

# The process of adding multiple multicategorical variables gets a bit more
# convoluted. Let's add a new variable for an individuals Education (Educ).

# You can't really do the unique coded variable method easily with lm(), since 
# the function won't know what dummy code is for what variable From my experience,
# sometimes it gets the groups/variables correct, but other times it does not. 

# Instead, you can just use the factor method and add the new factor variable 
# into the model.

covid$Educ_Factor <- factor( 
  covid$Educ,
  levels = c("None", "Up to 6 years of school", "Up to 9 years of school",
             "Up to 12 years of school", "Some College or equivalent", 
             "College degree", "PhD/Doctorate"),
  ordered = FALSE) 

# Keep in mind that the group names for the Educ variable differ than the Educ_Mom
# variable.

# Next just add this to the model.

Model_Dummy4 <- lm(COVID_Concerns ~ Educ_Mom_Factor + Educ_Factor, data = covid)
summary(Model_Dummy4)

# As far as I can tell, PROCESS can't handle more than one multiplecategorical
# variable outside of interactions. PROCESS is built with a limited number
# of possible models it can do (see the PROCESS book for a list). It's not the 
# most flexible program for specific regression models.

# 2.12 Interpretation and Inference of Covariates

# If you want to interpret a dummy coded variable with a covariate, you just 
# need to control the covariate by holding the continuous one constant or keep
# group membership the same for the categorical variable.

# Example of b1 with continuous covariate:
#   Holding COVID-19 compliance constant between individuals, the mean COVID-19
#   concern score for individuals whose mother has 6 years of school is expected
#   to be .35 higher than the average COVID-19 concern for individuals whose
#   mothers have no education.

# All multiple categorical dummy coded variables have a bseline. In the example
# that we use (mother's education & individual's education), the baseline I 
# used was no education for either. This will factor into the interpretations for
# this model.

# Example of b1 with categorical covariate:
#   For individuals who have the same education level, the mean COVID-19
#   concern score for individuals whose mother has 6 years of school is expected
#   to be .35 higher than the average COVID-19 concern for individuals whose
#   mothers have no education.

# Note, with multiple categorical variables, the intercept is thus the mean
# outcome at *both* baselines.

# To interpret the covariate(s), there are some more caveats.

# First, if your covariate is simply there to control variation and you are focusing 
# on the categorical variables, you can ignore and not interpret the 
# covariate's slope(s).

# Continuous covariates can be interpreted the same way as usual, except now you
# state that you are keeping the group membership constant.

# Example: 
#   For two individuals whose mother has the same level of education, the 
#   individual with higher COVID-19 compliance is expected to be .44 higher
#   in COVID-19 concern. 

# If you are interpreting a categorical covariate, you are still holding
# group membership constant. 

# Example for slope of individual having PhD.
#   For individuals whose mothers have the same education level, the mean COVID-19
#   concern score for individuals who have a PhD is expected to be 1.6 higher
#   than an individual who has no education.

# In general, the interpretations for models with multiple categorical variables
# gets messy, and usually they are done with interactions. Since adding no 
# interactions assumes that specific dual group membership does not matter (which
# is most likely not the case in many circumstances). Thus, I'd rather tell someone
# to either run interactions, or possible just run an ANOVA/ANCOVA for easier
# model results to interpret. 

# For the purposes of doing inference with covariates, it might be of interest
# to treat all your dummy codes as a group. AKA, to test how much contribution to 
# the R^2 effect size your mutlticategorical variable added, after
# controlling for the covariate(s). So you will need to treat each dummy code
# as a group and do a delta-R^2 test on them (see the Inference lab material).

# ====================== Sequential Coding ======================

# 3.1 Sequential Coding

# Moving on from dummy codes, sequential coding is an alternative way to structure
# your categorical predictors. Instead of comparing every group to a single
# baseline group (like dummy coding does), sequential coding compares groups in
# a step-by-step fashion. Each category is compared to the one that comes before
# it in some meaningful order (e.g., education level, time, dosage). 

# In this context thee interpretation of coefficients changes: rather than asking
# "How is this group different from the baseline?", we ask "How is this group
# different from the previous group?" This can be especially useful when your
# categories have a natural ordering (i.e. ordinal variables) and you are
# interested in incremental changes across levels rather than comparisons to a 
# single reference group. Though, you can still use sequential coding with 
# non-ordinal variables as well.

# In short, dummy coding compares everything to one group, while sequential
# coding compares each group to the next in line.

# As a side note, if you ever want to do piecewise regression with multiple 
# joints (see the future Nonlinear Regression lab), you should use
# sequential coding for the joints. 

# 3.2 Sequential Coding in Base R

# Unfortunately, lm() defaults to dummy coded variables for categorical variables,
# so you must code these manually. We want to use the trasnform() function to 
# specify unique variables at certain values of Educ_Mom. This method is much
# easier to do with a numeric coded variable, so I recommend recoding your variable
# as numeric if it isn't already (you don't have to do this method, but you need
# to use a lot of | "or" operators that I am not going to cover for sequential
# coding).

# The coding scheme follows sequentially (e.g., S1 is everything above 1, 
# S2 is everything above 2, etc.). The order is arbitrary, but if there's a 
# natural ordering, it would make most sense to follow that. If you are doing 
# a specific order, you can work out the logic with =, >, or < below, or you can
# just change your numeric coding order (see Section 2.8).

covid <- transform(covid,
                   s1 = (Educ_Mom_Numeric > 1),
                   s2 = (Educ_Mom_Numeric > 2),
                   s3 = (Educ_Mom_Numeric > 3),
                   s4 = (Educ_Mom_Numeric > 4),
                   s5 = (Educ_Mom_Numeric > 5),
                   s6 = (Educ_Mom_Numeric > 6))

# Wecan then turn TRUE/FALSE's into 1/0's. This isn't necessary, but I think is 
# helpful for clarity.

ss <- lapply(covid[, c("s1","s2","s3","s4","s5","s6")], as.integer)
covid[, c("s1","s2","s3","s4","s5","s6")] <- ss

# Then we need to pug the variables into the lm() model. Note, that, unlike dummy
# codes, there is no "reference" group with sequential variables, so we just 
# include all variables into the model (order doesn't matter).

model_squential <- lm(COVID_Concerns ~ s1 + s2 + s3 + s4 + s5 + s6, data = covid)
summary(model_squential)

# 3.3 Using PROCESS

# Doing alternative coding schemes is very easy with PROCESS. For seuqneital coding
# you just need to change the "mcx =" option to "mcx =2" (make sure your categorical
# variable is the first variable in the "x=" list if you have multiple).

process(data = covid, x = "Educ_Mom_Numeric", y = "COVID_Concerns", mcx = 2) 

# If you look at the coding matrix at the top, you can see which education group 
# relation corresponds to which variable. Recall that the variables no longer 
# represent comparisons between a group with the reference group but with the
# current group and the previous group. This contrast matrix is what represents
# these comparisons.

# 3.4 Using Custom Contrasts

# Knowing how the sequential coding contrast matrix should look like, you can 
# also make the contrast matrix manually and put into PROCESS or into your factor
# categorical variable (see Section 2.9 for a full breakdown on how this works).

Sequential_Matrix <- c(
  0,0,0,0,0,0,  # row 1
  1,0,0,0,0,0,  # row 2
  1,1,0,0,0,0,  # row 3
  1,1,1,0,0,0,  # row 4
  1,1,1,1,0,0,  # row 5
  1,1,1,1,1,0,  # row 6 
  1,1,1,1,1,1   # row 7
)
Sequntial_Contrast <- matrix(Sequential_Matrix, nrow = 7, byrow = TRUE)

# Then you just need to plug this sequential contrast matrix into the contrast()
# function with a factor class variable (if your categorical variable is not
# a factor object, it needs to become one, see Section 2.2)

covid$Educ_Mom_Factor_Seq <- covid$Educ_Mom_Factor
contrasts(covid$Educ_Mom_Factor_Seq) <- Sequntial_Contrast

# Then just plug it the sequential coded factor variable into the lm() model.

model_squential2 <- lm(COVID_Concerns ~ Educ_Mom_Factor_Seq, data = covid)
summary(model_squential2)

# This method will just require you specify the matrix correctly. If you do NOT
# do this, the results you get will be wrong. So just double check the matrix
# is correct.

# You can also do this for PROCESS too, but there's no need since PROCESS can just
# do it automatically.

# 3.5 Sequential Coefficient Interpretations

# The intercept of the sequentially coded groups is just the group mean of the 
# "first" group in sequential order, which is the "none" mother's education
# group. Each coefficient is then comparing the subsequent group mean to the
# previous group means.

# Example b1:
#   Individuals whose mothers have up to 6 years of schooling have about .39 more
#   average COVID-19 concern when compared to individuals whose mothers have
#   no education.

# Example b2:
#   Individuals whose mothers have up to 9 years of schooling have about .61 less
#   average COVID-19 concern when compared to individuals whose mothers have
#   6 years of education.

# Example b6:
#   Individuals whose mothers have a PhD or doctorate have about 1.64 more
#   average COVID-19 concern when compared to individuals whose mothers have
#   a college degree.

# ====================== Helmert Coding ======================

# 3.1 Helmert Coding

# Helmert coding is a method to code multicategorical variables where each
# group is compared to the average of the group that come after it.
# Unlike dummy coding, which compares each group to a single reference group,
# Helmert coding makes a series of sequential comparisons across levels of the
# variable. For example, the first coefficient compares the first group to the
# average of all remaining group means, the second coefficient compares the 
# second group average to the average of the groups means that follow, and so on. 
# This means that each coefficient represents a different type of comparison, 
# rather than all comparisons being tied to one baseline group. Helmert coding 
# is useful when there is a meaningful ordering to the categories and you are 
# interested in how earlier groups differ from later ones overall, rather than
# just from a single reference category or for two adjacent groups like sequential
# coding. Note that even though it is help for ordinal variables, Helmert coding's
# order of coefficients is arbitrary and any comparison can be made.

# Let’s say for the example that we want to do the following comparisons:
# No education vs the rest
# PhD vs everything except No Education
# 6 years vs 9 years, 12 years, Some College, and College
# 9 years vs 12 years, Some College, and College
# 12 years vs Some College and College
# Some College vs College

# Notice how the numeric groups are not necessarily in order of increasing 
# education, though you could certainly make it out to be that way if you wished.

# 3.2 Helmert Coding in Base R

# There is no easy way to do Helmert coding in R, so we will have to manually
# code unique variables to plug into our model. These variables have to follow
# the specific coding strategy for Helmert coding.

# The coding scheme follows fractions based on a specific coding pattern
# (see Amanda's slides for more detail). An easy way to think of it is on the
# left side, we have a negative that starts at -(1 - #groups)/#groups, where 
# both the numerator and denominator magnitudes decrease by 1 for each new 
# variable. The right side of each variable is just (1/#groups) where the 
# denominator decreases in magnitude by 1 for each new variable the last
# variable number should be -1/2 and 1/2.

# If you are familiar with making orthogonal contrasts for ANOVAs, the Helmert
# contrast matrix follows those same rules and principals. For the same reason,
# you can represent this with whole numbers instead of fractions if you wish.
# However, recall, that you will need to rescale your coefficients, standard errors
# and test statistics back down (aka, if you multiplied by 7, you will later 
# have to divide by 7) to get the appropriate statistics. This rescaling will
# be specific per coefficient, so I recommend just sticking to the fractions.

# Importantly, the groups represented by each variable can be in ANY order. 
# Meaning you don't have to go sequentially in a natural order. 

# To make the groups, you just need to create variables that follow this contrast
# matrix in arithmetic, where the groups that come after the addition just
# have to include every other group that hasn't appeared on the left side yet.
# This is easiest when your variable is numerically coded, so I would do that first
# if you haven't (see Section 2.8). 

covid <- transform(
  covid,
  h1 = -6/7 * (Educ_Mom_Numeric == 1) +
        1/7 * (Educ_Mom_Numeric > 1),
  
  h2 = -5/6 * (Educ_Mom_Numeric == 7) +
       1/6 * ((Educ_Mom_Numeric == 2) |
              (Educ_Mom_Numeric == 3) |
              (Educ_Mom_Numeric == 4) |
              (Educ_Mom_Numeric == 5) |
              (Educ_Mom_Numeric == 6)),
  
  h3 = -4/5 * (Educ_Mom_Numeric == 2) +
       1/5 * ((Educ_Mom_Numeric == 3) |
              (Educ_Mom_Numeric == 4) |
              (Educ_Mom_Numeric == 5) |
              (Educ_Mom_Numeric == 6)),
  
  h4 = -3/4 * (Educ_Mom_Numeric == 3) +
       1/4 * ((Educ_Mom_Numeric == 4) |
              (Educ_Mom_Numeric == 5) |
              (Educ_Mom_Numeric == 6)),
  
  h5 = -2/3 * (Educ_Mom_Numeric == 4) +
       1/3 * ((Educ_Mom_Numeric == 5) |
              (Educ_Mom_Numeric == 6)),
  
  h6 = -1/2 * (Educ_Mom_Numeric == 5) +
       1/2 * (Educ_Mom_Numeric == 6)
)

# For some specific R knowledge, the "|" operator you can see on the right hand 
# sides of each Helmert coded variable represents "OR". This means we are saying 
# that education is group 2 OR group 3 OR etc.
# See Amanda or my slides for more info if you are confused.

# Next you just need to plug in all of the Helmert variables into the model. Since
# there is no "baseline" group, the order doesn't matter.

model_Helmert <- lm(COVID_Concerns ~ h1 + h2 + h3 + h4 + h5 + h6, data = covid)
summary(model_Helmert)

# 3.3 Helmert Coding in PROCESS

# It's easy to do other multicategorical coding strategies in PROCESS. For Helmert
# coding, just change the "mcx= " option to be "mcx = 3". Make sure the first
# variable in the "x = " line is multicategorical. 

process(data = covid, x = "Educ_Mom_Numeric", y = "COVID_Concerns", mcx = 3)

# You can see the specific fractions in the matrix a the top. This represents
# the specific Helmert contrast matrix (see Section 3.2 or lecture slides for
# more info). 

# Note that PROCESS will automatically read in the numeric order as the order 
# you want to do Helmert coding in, so this output is assuming we are looking 
# at the coding in the natural order (e.g., no education compared to any education,
# 6 years of education compared more than 6 years of education, etc.).
# If we want to change the order to what we had above, we have to reorder the 
# values. This can be done anyway (see the strategies in Section 2), but I am 
# just going to do it by converting it into a factor variable and back to a numeric
# since the mental head work is less for me this way.

# To follow the order that we laid out in Section 3.1, I just need to put
# "PhD/Doctorate" as second and shift everything else up.

covid$Educ_Mom_Helmert <- factor( 
  covid$Educ_Mom,
  levels = c("None", "PhD/Doctorate", "Up to 6 years of school",
             "Up to 9 years of school", "Up to 12 years of school",
             "Some College or equivalent", "College degree"),
  ordered = FALSE) # Keep this FALSE
covid$Educ_Mom_Numeric_Helmert <- as.integer(covid$Educ_Mom_Helmert)

# Put this back into the model.

process(data = covid, x = "Educ_Mom_Numeric_Helmert", y = "COVID_Concerns", mcx = 3) 

# Keep in mind, that if you recoded the groups the way I did, the contrast matrix
# in the PROCESS output won't key you into which groups are represented by which 
# X's (since the left column of the group labels will stil be 1 - 7, but these
# aren't the same 1 - 7 as before). So, you will have to remember the order
# you put the groups in.

# 3.4 Helmert Coding with Custom Contrasts

# Knowing how the Helmert coding contrast matrix should look like, you can 
# also make the contrast matrix manually and put into PROCESS or into your factor
# categorical variable (see Section 2.9 for a full breakdown on how this works).

# Keep in mind, if you wand to do custom ordering, you will need to change the
# positioning of the negative fraction to the corresponding row (the row
# represents the group, so since we want to comapre PhD against other education,
# then the last row gets -5/6 and the others get 1/6)

Helmert_Matrix <- c(
  -6/7,   0,     0,     0,     0,     0,   # row 1
  1/7,  1/6,  -4/5,    0,     0,     0,    # row 2
  1/7,  1/6,   1/5,  -3/4,    0,     0,    # row 3
  1/7,  1/6,   1/5,   1/4,  -2/3,    0,    # row 4
  1/7,  1/6,   1/5,   1/4,   1/3,  -1/2,   # row 5
  1/7,  1/6,   1/5,   1/4,   1/3,   1/2,   # row 6
  1/7, -5/6,   0,     0,     0,     0      # row 7
)
Helmert_Contrast <- matrix(Helmert_Matrix, nrow = 7, byrow = TRUE)

# Note, you can do this with whole numbers, but your coefficients, standard errors,
# test statistics, and confidence intervals will be scaled different for how you
# scaled each column. So I recommend just using fractions.

# Then you just need to plug this Helmert contrast matrix into the contrast()
# function with a factor class variable (if your categorical variable is not
# a factor object, it needs to become one, see Section 2.2).

covid$Educ_Mom_Factor_Helmert2 <- covid$Educ_Mom_Factor
contrasts(covid$Educ_Mom_Factor_Helmert2) <- Helmert_Contrast

# Then just plug it the sequential coded factor variable into the lm() model.

model_Helmert2 <- lm(COVID_Concerns ~ Educ_Mom_Factor_Helmert2, data = covid)
summary(model_Helmert2)

# This method will just require you specify the matrix correctly. If you do NOT
# do this, the results you get will be wrong. So just double check the matrix
# is correct.

# You can also do this for PROCESS too, but there's no need since PROCESS can just
# do it automatically.

# 3.5 Interpreting Helmert Coefficients

# Interpreting Helmert coded groups is comparing one group mean to the average
# of the other group means.
#   E.g., Mean 1 vs (Mean 2 + Mean 3 + Mean 4) / 3
# Note that its often not represented this way due to short hand, but when you
# are comparing "groups", you are comparing "group averages."
# So by nature, you are comparing the average of multiple group averages.

# This is important because it is a *very* common mistake for people (and AI too) 
# to say you are comparing the "average of group 2, 3, and 4" or the "grand mean of
# group 2, 3, and 4", but this is incorrect. The average of each group (or the
# grand mean) is NOT the mean of the group means. They will only be equal to
# each other when the group sample sizes are equal. When the group sample
# sizes are not equal (as they often aren't), then the mean of the group means
# will be different than the grand mean. Just keep this distinction in mind.

# The intercept of the Helmert coded groups is the average of the group means.
# So it is the mean of the average mother's education for level 1 through 7. 

# Keep in mind that Helmert coding arithmetic is: later groups - current group.
# So a negative sign means the current group mean is larger than the average of 
# the other group means.

# Here are example coefficient interpretations. 

# Example b1:
#   Individuals whose mothers have no education have an average COVID-19 concern
#   that is about .25 lower than the average of the other education level averages.

# Example b2:
#   Individuals whose mothers have a PhD or doctorate have an average COVID-19 
#   concern that is 1.89 greater than the average of the rest of the education
#   group means.

# Example b6:
#   Individuals whose mothers have a some college or equivalent have an average
#   COVID-19 concern that is .41 lower than the average for individuals whose
#   mother has a college degree.

# ====================== Effects Coding ======================

# 4.1 Effects Coding

# Effects coding is a method for coding multicategorical variables where each
# category is compared to the mean of the means rather than to a single
# reference group. Like dummy coding, it uses a series of 0/1-type variables,
# but with one key difference: the reference category is coded as -1 instead of 0.
# This ensures that the coefficients represent how much each group differs from
# the average of all groups, rather than from a specific baseline group.

# As a result, the intercept in an effects-coded model represents the grand mean
# of the outcome, and each coefficient tells us how far a given category is above
# or below that overall mean. This can be especially useful when no single group
# serves as a natural reference, or when you are interested in understanding how
# each group compares to the overall pattern rather than to one specific category.

# The downside is that you have to leave out one group for the sake of estimating
# the model. The choice of group you leave out has no practical consequence. It 
# just means that if you want the effects coded coefficient for that group, you
# have to change the leave out group and rerun the model.

# As a side note, effects coding is helpful for LASSO, Ridge, or Elastic Net
# regression (see the Prediction lab) because it treats all categories more
# evenly when coefficients are being shrunk. Unlike dummy coding, which depends
# on a chosen reference group, effects coding reduces the influence of that
# arbitrary choice and can lead to more stable results when selecting variables.
# I'm not personally sure of how much of a benefit this is for these methods, and
# I'm willing to bet the majority of people still use dummy codes (whether they
# should or should not). But this is worth keeping in mind.

# 4.2 Effects Coding in Base R

# Unfortunately, lm() defaults to dummy coded variables for categorical variables,
# so you must code these manually. We want to use the trasnform() function to 
# specify unique variables at certain values of Educ_Mom. This method can
# be easily done with a categorical/factor class variable or a numeric variable,
# but I am just going to use numeric to keep consistent with the other methods.

# All you need to do is select the group you are leaving out in the analysis and
# subtract each other other group by this leave-out group.

# Let's have the PhD/Doctorate group as the leave-out effect.

covid <- transform(covid,
                   e1 = (Educ_Mom_Numeric == 1)-(Educ_Mom_Numeric == 7),
                   e2 = (Educ_Mom_Numeric == 2)-(Educ_Mom_Numeric == 7),
                   e3 = (Educ_Mom_Numeric == 3)-(Educ_Mom_Numeric == 7),
                   e4 = (Educ_Mom_Numeric == 4)-(Educ_Mom_Numeric == 7),
                   e5 = (Educ_Mom_Numeric == 5)-(Educ_Mom_Numeric == 7),
                   e6 = (Educ_Mom_Numeric == 6)-(Educ_Mom_Numeric == 7))

# Then insert them into your lm() model. The order doesn't matter.

model_effects <- lm(COVID_Concerns ~ e1 + e2 + e3 + e4 + e5 + e6, data = covid)
summary(model_effects)

# If you want to change the leave-out group, say, to the "none" group then you
# must change which numeric gets subtracted.

covid <- transform(covid,
                   e1 = (Educ_Mom_Numeric == 2)-(Educ_Mom_Numeric == 1),
                   e2 = (Educ_Mom_Numeric == 3)-(Educ_Mom_Numeric == 1),
                   e3 = (Educ_Mom_Numeric == 4)-(Educ_Mom_Numeric == 1),
                   e4 = (Educ_Mom_Numeric == 5)-(Educ_Mom_Numeric == 1),
                   e5 = (Educ_Mom_Numeric == 6)-(Educ_Mom_Numeric == 1),
                   e6 = (Educ_Mom_Numeric == 7)-(Educ_Mom_Numeric == 1))

model_effects2 <- lm(COVID_Concerns ~ e1 + e2 + e3 + e4 + e5 + e6, data = covid)
summary(model_effects2)

# As you can see, every coefficient that isn't associated with the added PhD 
# group does not change. However, to get the single final effect, we had to recode
# and rerun everything again. This is annoying but not that hard to do.

# 4.3 Effects Coding in PROCESS

# It's easy to do other multicategorical coding strategies in PROCESS. For Helmert
# coding, just change the "mcx= " option to be "mcx = 4". Make sure the first
# variable in the "x = " line is multicategorical. 

process(data = covid, x = "Educ_Mom_Numeric", y = "COVID_Concerns", mcx = 4)

# Note, PROCESS automatically puts the lowest numeric as the leave-out for
# effects coding, which in this case, "none" mother educaiton group is left out.
# If you want another group left out you need to recode the order. You can do this
# any method, but I am jsut going to convert it to a factor variable, change the 
# order, and change it back to a numeric variable after. This is easier in my 
# headspace to do personally. 

# Let's make PhD/Doctorate the lowest value so it will be left out.

covid$Educ_Mom_Factor_Effects <- factor( 
  covid$Educ_Mom,
  levels = c("PhD/Doctorate", "None", "Up to 6 years of school", 
             "Up to 9 years of school", "Up to 12 years of school",
             "Some College or equivalent", "College degree"),
  ordered = FALSE) # Keep this FALSE
covid$Educ_Mom_Numeric_Effects <- as.integer(covid$Educ_Mom_Factor_Effects)

process(data = covid, x = "Educ_Mom_Numeric_Effects", y = "COVID_Concerns", mcx = 4) 

# You can double check the contrast matrix in the output to see which variable 
# is left out (the one with just -1's in the rows).

# 4.4 Effects Coding with Custom Contrasts

# Knowing how the effects coding contrast matrix should look like, you can 
# also make the contrast matrix manually and put into PROCESS or into your factor
# categorical variable (see Section 2.9 for a full breakdown on how this works).

 Effects_Matrix <-c(
   -1, -1, -1, -1, -1, -1,  # row 1 (leave-out)
    1,  0,  0,  0,  0,  0,   # row 2
    0,  1,  0,  0,  0,  0,   # row 3
    0,  0,  1,  0,  0,  0,   # row 4
    0,  0,  0,  1,  0,  0,   # row 5
    0,  0,  0,  0,  1,  0,   # row 6
    0,  0,  0,  0,  0,  1    # row 7
 )
Effects_Contrast <- matrix(Effects_Matrix, nrow = 7, byrow = TRUE)

# Note, you can do this with whole numbers, but your coefficients, standard errors,
# test statistics, and confidence intervals will be scaled different for how you
# scaled each column. So I recommend just using fractions.

# Then you just need to plug this Helmert contrast matrix into the contrast()
# function with a factor class variable (if your categorical variable is not
# a factor object, it needs to become one, see Section 2.2).

covid$Educ_Mom_Factor_Effects2 <- covid$Educ_Mom_Factor
contrasts(covid$Educ_Mom_Factor_Effects2) <- Effects_Contrast

# Then just plug it the sequential coded factor variable into the lm() model.

model_effects3 <- lm(COVID_Concerns ~ Educ_Mom_Factor_Effects2, data = covid)
summary(model_effects3)

# If you want to switch the code, you need to rearange the row with -1's and 
# rerun the model.

# This method will just require you specify the matrix correctly. If you do NOT
# do this, the results you get will be wrong. So just double check the matrix
# is correct.

# You can also do this for PROCESS too, but there's no need since PROCESS can just
# do it automatically.

# 4.5 Effects Coefficient Interpretations

# The intercept of the effects coded groups is the mean of the group means. Each
# coefficient is compared to this value and is just the difference between each
# individual group mean and the intercept (mean of the group means).

# Interpreting effects coded groups is comparing one group mean to the average
# of the other group means (or sometimes just the "average effect", but I don't
# think Amanda likes that verbage).
#     E.g. for four groups, Mean 1 vs (Mean 1 + Mean 2 + Mean 3 + Mean 4) / 4
# Note that its often not represented this way due to short hand, but when you
# are comparing "groups", you are comparing "group averages."
# So by nature, you are comparing the average of multiple group averages.

# This is important because it is a *very* common mistake for people (and AI too) 
# to say you are comparing the "average of the groups", the "grand mean of
# the groups" or the "overall average", but this is incorrect. The average of 
# each group (the grand mean) is NOT the mean of the group means. They will only
# be equal to each other when the group sample sizes are eaual. When the group 
# sample sizes are not equal (as they often aren't), then the mean of the group 
# means will be different than the grand mean. Just keep this distinction in mind.

# Here are example coefficient interpretations with PhD as the leave-out group. 
# Note the arithmetic is Group - Intercept. So a negative sign means the 
# average of the group means is larger. 

# Example b1:
#   Individuals whose mothers have no education are expected to have an average
#   COVID-19 concern that is .18 larger than the average COVID-19 concern of the 
#   different mother education level means.

# Example b3:
#   Individuals whose mothers have at least 12 years of education are expected 
#   to have an average COVID-19 concern that is .7 lower than the average
#   COVID-19 concern of the  different mother education level means.
