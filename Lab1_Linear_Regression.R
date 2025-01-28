# Introduction to Linear Regression 
# Session 1
# Advanced Analytic Methods for Environmental Epidemiology

####***********************
#### Table of Contents ####
####***********************

## Pre-Class Material ##
# 0: Preparation 
# 1: Examine Data
# 2: Plot Data

## Live Session Material ## 
# 3: Define Research Question 
# 4: Review Data Structure
# 5: Fit Regression Model 
# 6: Review Model Results
# 7: Assess Model Assumptions 
# 8: Examine Effect Modification
# 9: Breakout Exercises

## Bonus Material ##
# 10: Data Manipulation 
# 11: Summarizing Data 
# 12: Bonus Exercises

####************************
#### Pre-Class Material ####
####************************

####********************
#### 0: Preparation ####
####********************

# 0a Load packages

# install.package("readr")

library(readr) # readr is a package for reading in various types of data. 

# install.package("dplyr")

library(dplyr) # dplyr is a package for data manipulation

# 0b Declare folder paths 
# check the intro_to_R.docx document if you have difficulty finding the 
# file path for your computer

county_data = "/Users/tamarapichardo/Desktop/p8332_lab1/data/Lab1_2010_County_Data.csv"

####*************************
#### 1: Examine Data    #####
####*************************

# Read in data 

dta = read_csv(file = "./data/Lab1_2010_County_Data.csv")

# read_csv() is a bit faster than read.csv, and has more predictable defaults. 
# even RStudio uses read_csv as the default
# read_csv() comes from the readr package

##** PreClass Task: confirm dimensions **##
# A) Check the dimensions of the dataset.
# You should have 15 columns and 500 rows.
# We can check the dimensions of an object with the dim() command. 
##*****************************************************************## 

# unlike SAS, R is case sensitive 

dim(dta)
dim(dta)

# Examine environment
# ls(), aka list, shows you what objects you have in the environment

ls()

# Examine the data frame.  
# The dta data.frame is an object whose dimensions are 
# 500 observations by 15 variables.  
# Typically each row is an observation and each column is a variable. 

dim(dta) # size of the object 
names(dta) # names of columns
head(dta) # show first 6 rows 
View(dta) 


####*******************************************
# Variable Codebook 
# FIPS: unique identifier for each county 
## from the Behavioral RIsk Factor Surveillance System Survey 
##      of the Centers for Disease Control and Prevention
# AveBMI: average Body Mass Index (BMI) for each county in 2010
#         BMI is calculated from body weight / square of height (units are kg/m2)
## from the Air Quality System 
##      of the US Environmental Protection Agency 
# AvePM: annual ambient PM2.5, based on inverse-distance weighted average of EPA monitors
## from 2010 American Community Survey 
##      of the US Census Bureau
# NumTot: total population in county
# PerBlack, PerLatinx, PerAsam, PerWhite: racial composition 
# MedHInc: median household income 
# MedHVal: median household value among homeowners 
# FemaleUnemp, MaleUnemp: unemployment rate by sex
# unemployment has a specific definition, not just 'not working', must be seeking work. 
# LTHS: percentage of population over 18 with less than high school education 
## from the National Oceanic and Atmostpheric Agency
# ClimateRegion: from NOAA, divides county in area with similar climates
####*******************************************

# to learn more about a function use the ? 
# ? shows the documentation for the command 
# documentation: a page written by the author of the command 
# include definitions of each parameter, output, and example code

?head

# Lets examine a single column in the dataframe. 
# Columns in a data frame may be directly accessed using 
# the data.frame$column.name  syntax.
# To review the definitions of mode, class and length
# check the base R cheatsheet and chpater 3 of Paradis tutorial. 

dta$AvePM[1:10]
mode(dta$AvePM) # can be numeric, integer, character, logical
length(dta$AvePM)

# mode() and length() are useful because 
# incompatible modes and lengths are common issues.

# in R, we can include . and _ in our variable names 
# this is not always the case in other programs: 
# for example, in SAS and Python the . has other uses.

# if you do not specify the dataframe, 
# R will assume that AvePM is its own object in the environment 
# and will not be able to find it.

mode(dta$AvePM)

# We can extract dataframe values based on their row+column position. 
# value = dataframe[row, column]

dta[1,2]

# We can also extract rows or columns this way.

row1    = dta[1, ]
column1 = dta[ ,1]

# We can create new columns by assigning them. 

dta$AveBMI.sq = dta$AveBMI * dta$AveBMI 

# Check for the new column 

head(dta) 

# Summarizing data

# We ALWAYS need to look at our data before starting our analyses. 
# Summarizing data is an easy way to catch errors 

mean(dta$AvePM)

# You can get the summary of every variable in a dataframe. 

summary(dta) 

##** PreClass Task: Confirm AvePM statistics **##
# A) Check the minimum and maximum AvePM in the dataset and 
# and confirm that they match the values below.
# The minimum AvePM should be 3.866667 ug/m3
# The maximum AvePM should be 16.07774 ug/m3
# hint: Look at the summarizing data section: 
# what command would you use to calculate a minimum rather than mean?
##*****************************************************************## 

####**************************
#### 2: Plotting Data    #####
####**************************

# Basic plotting functions.  

# 2a Histograms
# Histograms for county average BMI.  

?hist
hist(dta$AveBMI)

# We can set the number of bins with breaks option 

hist(dta$AveBMI, 
     breaks=1)

hist(dta$AveBMI, 
     breaks=20)

# We can set titles with xlab, ylab, and main 
# las sets the axes labels orientation 

hist(dta$AveBMI, 
     breaks = 20, 
     xlab = "Average BMI", 
     ylab = "Count of Counties", 
     main = "Histogram of Average BMI", 
     las = 1) 

# Instead of the histogram you can also plot density. 
# density is relative to the total data
# the y axis is percentage not counts
# the density plots are smoothed over the range of data

plot(density(dta$AveBMI, na.rm = TRUE), 
     col="red", 
     las = 1)

# we can also overlay multiple plots on the same graph

hist(dta$AveBMI, breaks=20, 
     xlab = "Average BMI", ylab = "Density", 
     main = "Histogram of Average BMI", 
     freq = FALSE, las = 1) 
lines(density(dta$AveBMI, na.rm = TRUE), col = "red")

# 2b Boxplots
# Plot the distribution of annual PM among the sampled counties

boxplot(dta$AvePM, xlab = "Nationwide", ylab = expression("Average PM"[2.5]))

# Plot the distribution of annual PM for each climatic region,   
# we can use split to divide data into categories

boxplot(split(dta$AvePM, dta$ClimateRegion), 
        xlab = "NOAA Climate Region", ylab = expression("Annual PM"[2.5]))

boxplot(split(dta$AveBMI, dta$ClimateRegion), 
        xlab = "NOAA Climate Region", ylab = expression("Annual PM"[2.5]))

# 2c Scatterplots 

plot(x = dta$AvePM,
     y = dta$AveBMI, 
     xlab = expression("Average PM"[2.5]), ylab = "Average BMI", 
     main = "Scatterplot: \nCounty-average PM vs. BMI")

# we can add the least square fitted line (ie linear regression) to the above plot
# we will cover the lm() command in the next section

abline(lm(AveBMI ~ AvePM, data = dta), col = "blue", lwd = 2)

# 2d Multiple plots
# We can enhance the graph by specifying some options.  
# If you type help(plot) or help(par) for graphical parameters, 
# you will find that there are many more options
# that you can adjust to make the plot look exactly as you'd like. 

# At times, you will find it convenient to have 
# more than one graph on a single page.  
# R has a function that allows us to do that.  
# Here we divide the graphical page into one row and two 
# columns leaving two spaces for the following two plots.  
# It will remain set-up this way until you reset it.

par(mfrow = c(1,2), las = 1)

boxplot(split(dta$AvePM, dta$ClimateRegion), 
        xlab = "NOAA Climate Region", 
        ylab = expression("Average PM"[2.5]),
        main = expression("Average PM"[2.5]*" Across Climate Regions"))

plot(dta$AvePM, dta$AveBMI, 
     xlab = expression("Average PM"[2.5]), 
     ylab = "Average BMI", 
     main = "Scatterplot: County-average PM vs. BMI")

# 2e Save plots
# we can use the pdta() command to save outputs of R into a pdta file

pdf(paste0(OutputPath, "Histograms.pdf"), width = 10) 

hist(dta$AveBMI, freq = FALSE, xlab = "Average BMI", ylab = "Density", las = 1)
lines(density(dta$AveBMI, na.rm = TRUE), col = "red")

# we use dev.off() to tell R to stop sending output to the pdta file. 

dev.off()  

# R also has commands like png() and jpg() for other types of files

##** PreClass Task: Plot data **##
# A) Plot a histogram of AvePM 
# B) Plot a scatter plot of AvePM and AveBMI, where AvePM in on the x-axis
# C) Save both plots as two .png's 
##*****************************************************************## 

####***************************
#### Live Session Material ####
####***************************

# We will first define our research question, 
# then examine the regression model, 
# then review the results and code to wrangle the results 
# then we will assess the appropriateness of the model
# and then we will do some exercises in breakout groups.

####**************************************
#### 3: Define Research Question     #####
####**************************************

# In 2010, was county-average BMI associated with county-average annual PM2.5? 
# Reminder: 
# Body mass index is defined as the individual's body weight 
#     divided by the square of their height therefore the units are kg/m2.
# The county-average BMI was computed by taking a weighted average of survey 
#     responses in the Behavioral Risk Factor Surveillance System (BRFSS), 
#     run by the CDC, where the weights are based on how representative the 
#     respondent is of their county. 
# The PM2.5 data came from computing the inverse-distance-weighted average of 
#     nearby USEPA Air Quality System (AQS) monitors relative to county centroid. 
#     Units are micrograms per cubic meter (ug/m3).

####***********************************
#### 4: Review Data Structure     #####
####***********************************

####*******************************************
# Variable Codebook 
# FIPS: unique identifier for each county 
## from the Behavioral RIsk Factor Surveillance System Survey 
##      of the Centers for Disease Control and Prevention
# AveBMI: average Body Mass Index (BMI) for each county in 2010
#         BMI is calculated from body weight / square of height (units are kg/m2)
## from the Air Quality System 
##      of the US Environmental Protection Agency 
# AvePM: annual ambient PM2.5, based on inverse-distance weighted average of EPA monitors
## from 2010 American Community Survey 
##      of the US Census Bureau
# NumTot: total population in county
# PerBlack, PerLatinx, PerAsam, PerWhite: racial composition 
# MedHInc: median household income 
# MedHVal: median household value among homeowners 
# FemaleUnemp, MaleUnemp: unemployment rate by sex
# unemployment has a specific definition, not just 'not working', must be seeking work. 
# LTHS: percentage of population over 18 with less than high school education 
## from the National Oceanic and Atmostpheric Agency
# ClimateRegion: categorical variable of areas with similar climates
####*******************************************

# 4a Review data structure
# Each row is one observation for a single county 
# all measured for the same year: 2010
# Prior to creating the regression model, 
# let's look at the distribution of AveBMI. 

summary(dta$AveBMI)

hist(dta$AveBMI, freq = FALSE, xlab = "Average BMI", ylab = "Density", las = 1)
lines(density(dta$AveBMI, na.rm = TRUE), col = "red")

# 4b Examine values further
# We see that there is one county with very high AveBMI. 
# We can look at the data and see which observation has the highest BMI.

LargestBMI = dta[dta$AveBMI == max(dta$AveBMI),]

# 4c Plot correlation
# Let's take a look at the correlation between BMI and PM2.5. 

plot(x = dta$AvePM,
     y = dta$AveBMI, 
     xlab = expression("Average PM"[2.5]), ylab = "Average BMI", 
     main = "Scatterplot: \nCounty-average PM2.5 vs. BMI")

##** Class Question **##
# A) Are PM2.5 and BMI postively correlated? 
# B) Does it look like they are strongly correlated? 
# C) When we fit the adjusted regression model, 
#       will the association between PM2.5 and BMI be 
#       the same as this correlation? 
##*****************************************************************## 

####**********************************
#### 5: Fit Regression Model     #####
####**********************************

# 5a Review arguments of lm command

?lm

# 5b Fit model

##** Class Question **##
# A) What is the dependent variable? What are the independent variables? 
##*****************************************************************## 

mod = lm(AveBMI ~ AvePM + PerBlack + PerLatinx + PerAsianAm + 
          MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp + ClimateRegion, 
          data = dta, 
          na.action = na.omit)

# Notice that we used na.action to tell the function 
# how to deal with missing data. 
# na.omit tells lm() to drop any observation that is 
# missing data for any variable in the formula.
# We will cover how to address missing data in Session 13: Missing Data.

####**********************************
#### 6: Review Model Results     #####
####**********************************

# 6a Output model summary 

summary(mod)

# From the output you get: the model you ran, distribution of residuals, 
# coefficient estimates, standard errors, t-values, and p-values,
# and a description of the model in terms of residuals standard error, 
# R-squared (proportion of total variation in the data which is explained by the model, 
# and R-squared adjusted for the number of the parameter used in the fitted model),
# and F-test.

##** Class Question **##
table(dta$ClimateRegion)
# A) Why are there only 8 coefficients for ClimateRegion if there are 9 ClimateRegions?
# B) What is the interpretation of the ClimateRegionupper_midwest coefficient?
##*****************************************************************## 

# 6b Examine model object

names(mod)
names(summary(mod))

# 6c Extract model components
# summary() and mod are lists, so we can extract specifc values

summary(mod)$coefficients
summary(mod)$coefficients[2,] 
summary(mod)$coefficients[2,1] 

##** Class Question **##
# A) What is the estimated intercept? 
# B) What is the interpretation of the intercept?
##*****************************************************************## 

# 6d Calculate confidence intervals
# We can use the standard errors to calculate 95% confidence intervals

Beta.pm.fit = summary(mod)$coefficients[2,1]
Beta.pm.se  = summary(mod)$coefficients[2,2]
Beta.pm.lci = Beta.pm.fit - 1.96 * Beta.pm.se
Beta.pm.uci = Beta.pm.fit + 1.96 * Beta.pm.se

##** Class Question **##
# A) What is the association between PM2.5 and BMI? 
#       (report both the estimate and the 95% confidence intervals) 
# B) How would you interpret this association? 
#       What about the confidence interval?
# C) Is this association statistically significant at a signficance level of 
#       alpha = 0.05? 
#    What does statistical significance mean?
##*****************************************************************## '

# 6f Extract results
# Sometimes it is useful to extract the whole object as a dataframe 

# we can write code to extract it ourselves
my.coeff = summary(mod)$coefficients
# or
my.coeff = data.frame(summary(mod)$coefficients)
names(my.coeff) = c("est", "sterr", "tval", "pval")

# we can also use the broom package to extract these model objects as a dataframe 

# install.packages("broom")
library(broom)

mod.tidy    = tidy(mod)
modfit.tidy = glance(mod)

# note that broom does not work for every type of statistical model.

# 6g Visualize association
# since the slope is constant, 
# we can simply plot straight lines with slope = coefficient 
# The y-axis is the change in BMI relative to zero PM2.5
par(mfrow=c(1,1))
AvePM.range = c(min(dta$AvePM): max(dta$AvePM))
ChangeBMI = AvePM.range * Beta.pm.fit
plot(AvePM.range, ChangeBMI , 
     xlim = c(0, max(dta$AvePM)),
     ylim = c(0, max(dta$AvePM) * Beta.pm.uci), 
     xlab = expression("Average PM"[2.5]), 
     ylab = "Associated Change in \n Average BMI")
abline(a = 0, b = Beta.pm.fit, col = "red", lwd = 2)
abline(a = 0, b = Beta.pm.lci, col = "grey", lwd = 2)
abline(a = 0, b = Beta.pm.uci, col = "grey", lwd = 2)

####**************************************
#### 7: Assess Model Assumptions     #####
####**************************************

# Now let's see if the model follows the assumptions of a linear model.

##** Class Question **##
# What are the four assumptions of a linear model? 
##*****************************************************************## 

# To check Linearity: plot the data and residuals, compare models. 
#   We will learn how to fit nonlinear terms such as smoothing functions.
#   in Session 3: Nonlinearity
# To check Normality: look at the residuals. 
#   Use plots such as the Q-Q plot (normal probability plots); 
#   the scatterplot should lie on the diagonal straight line.
# To check Independence and Constant Variance plot the residuals vs fitted data. 
#   If the data are independent there should be no pattern in the data; 
#   if the variance is not constant you will see a shrinking or expanding cloud. 
#   We will learn what to do when these assumptions are violated in later sessions
#
# For this exercise, we will also remove highly influential outliers 

# 7a Review residual distribution
# Remember that model is stored as a list, 
# and the residuals are an element of the list. 

par(mfrow=c(3,1))
plot(mod$residuals)
hist(mod$residuals)
plot(density(mod$residuals))

##** Class Question **##
# Do the residuals look normally distributed? Centered around 0?
##*****************************************************************## 

# 7b Create diagnostic plots
# In the case of linear model, 
# using plot(model) yields diagnostic plots

par(mfrow=c(2,2))
plot(mod)

# 7c Hand-code diagnostic plots
# Its valuable to recognize that R commands are not magic- 
# We can often create our own versions of the code.  
# The same plots can be obtained by the following code:

# 7c.i get the residuals and plot against fitted

par(mfrow=c(2,2))
FittedValues = fitted(mod)    # or FittedValues = mod$fitted.values
Residuals    = residuals(mod) # or Residuals    = mod$residuals
plot(FittedValues, Residuals, main = "Residuals Vs Fitted")    

# 7c.ii QQ plot

qqnorm(Residuals)
qqline(Residuals)

# 7c.iii get the square root standardized residuals and plot against fitted

RootStandardResiduals = sqrt(abs(scale(resid(mod))))
plot(FittedValues, RootStandardResiduals, main = "Scale-Location")

# 7c.iv get the leverage and plot against standardized residuals
# leverage is how much data point deviates from the general trend

Leverage          = hat(model.matrix(mod))
StandardResiduals = scale(resid(mod))
plot(Leverage, StandardResiduals, las = 1, xlab = "Leverage", 
     ylab = "Standardized Residuals", main = "Residuals vs Leverage")
abline(h = 0, col = "red")

# 7d Check model assumptions with diagnostic plots

par(mfrow=c(2,2))
plot(mod)

# Linearity:
#    We will cover this issue in Session 3: NonLinearity
#    We can assess whether the residuals vary by the dependent variable 
#    though I have found that comparing models with nonlinear terms 
#    is more informative
#plot(x = dta$AvePM,
#     y = mod$residuals, 
#     xlab = expression("Average PM"[2.5]), ylab = "Residuals", 
#     main = "Scatterplot: \nCounty-average PM2.5 vs. Residuals")

# Normality: 
#    We can look at this guide to help us interpret the QQ plot 
#    http://seankross.com/2016/02/29/A-Q-Q-Plot-Dissection-Kit.html

##** Class Question **##
# Do the residuals look normally distributed? 
# Are one or both tails bigger than in a normal distribution?
##*****************************************************************## 

# Independence: 
##** Class Question **##
# Looking at the residuals vs fitted plot,
# do the residuals generally increase or decrease with BMI? 
##*****************************************************************## 

# Constant Variance: 
##** Class Question **##
# Does the variance of the residuals vary across AveBMI? 
##*****************************************************************## 

# Outliers: 
#   Based on the plot() results, we see that observation #154 has a leverage of 1, 
#   so we will see what happens when we remove it.
#   From the residuals vs fitted, we see that observation #169 has a very large residual;
#   we'll also  check how influential this point is.

# 7f Potential outliers

# 7f.i Look at observation 169

dta[169,]

##** Class Question **##
# Do you see any surprising values? 
##*****************************************************************## 

# 7f.ii Remove the potential outliers

dta.noOutliers = dta[-c(169, 154),]

# 7f.iii Fit regression model

mod.noOutliers = lm(AveBMI ~ AvePM + PerBlack + PerLatinx + PerAsianAm + 
                       MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp + 
                     ClimateRegion, 
                     data = dta.noOutliers, na.action = na.omit)
 
# 7f.iv Compare models

summary(mod)
summary(mod.noOutliers)

##** Class Question **##
# Did removing the outliers change the model? 
##*****************************************************************## 

# 7f.v Assess influence on PM2.5 coefficient

Beta.pm.mod            = summary(mod)$coefficients[2,1]
Beta.pm.mod.noOutliers = summary(mod.noOutliers)$coefficients[2,1]

100 * (Beta.pm.mod - Beta.pm.mod.noOutliers) / Beta.pm.mod.noOutliers

##** Class Question **##
# Did removing the outliers change the coefficient of interest (Beta.AvePM)? 
##*****************************************************************## 

# Removing outliers should not be done automatically, we need to think about it. 
# and we should never decide to remove outliers 
# just because it changes the model results to agree with our hypothesis!
# Rather, the principle is that we do not want our results to depend on a single observation
# In real life I would have go back and checked whether there could be an error
# and I would assess whether one of the outliers was uniquely influential

####***************************************
#### 8: Examine Effect Modification   #####
####***************************************

# Does the association between county-level annual PM2.5 and county average BMI 
# vary by climate region? 
# Let's first add an interaction term. 

# 8a Fit regression model with interaction term

mod.interaction = lm(AveBMI ~ AvePM*ClimateRegion + PerBlack + PerLatinx + PerAsianAm + 
                        MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp, 
                      data = dta.noOutliers, na.action = na.omit)

# The astrix tells lm() to create a term for AvePM, 
# a term for each ClimateRegion (except the reference region), 
# and an interaction term for each ClimateRegion. 

# 8b Review interation model  

summary(mod.interaction)

##** Class Question **##
# A) What is the interpretation of the value of the pm coefficient?
# B) What is the interpretation of the value of the pm*upper_midwest coefficient?
##*****************************************************************## 

# We see that the term for pm*upper_midwest is almost significant.  
# We can use the values from the model to estimate 
# the total PM-AveBMI association within theUpper Midwest
# and to estimate the confidence intervals of the association 

# 8c Compute the within-region association and its uncertainty 

# 8c.i Extract coefficents and covariances
# here we create tables of coefficients and covariance
coef.mat = summary(mod.interaction)$coefficients
var.mat  = vcov(mod.interaction)

# 8c.ii Compute total term for within-region association
# sum of the term in the reference region plus the term for Upper Midwest

beta.PM_upper_midwest = coef.mat["AvePM",1] + 
                         coef.mat["AvePM:ClimateRegionupper_midwest",1]

# 8c.iii Compute variance of within-region association 
# in order to compute standard error
# We must compute the variance for the total term 
# Var(Beta1 + Beta3) = Var(Beta1) + Var(Beta3) + CoVar(Beta1, Beta3) + CoVar(Beta3, Beta1)
# Var(Beta1 + Beta3) = Var(Beta1) + Var(Beta3) + 2*CoVar(Beta1, Beta3) 

var.PM_upper_midwest  = var.mat["AvePM", "AvePM"] + 
  var.mat["AvePM:ClimateRegionupper_midwest", "AvePM:ClimateRegionupper_midwest"] +
  2*var.mat["AvePM", "AvePM:ClimateRegionupper_midwest"]

se.PM_upper_midwest  = sqrt(abs(var.PM_upper_midwest))

# 8c.iv Compute confidence intervals 
# using ste for within-region association

lci.PM_upper_midwest = beta.PM_upper_midwest - 1.96*se.PM_upper_midwest
uci.PM_upper_midwest = beta.PM_upper_midwest + 1.96*se.PM_upper_midwest

UMWval = paste(round(beta.PM_upper_midwest, 3), " (95% CI: ", round(lci.PM_upper_midwest, 3), ", ", 
               round(uci.PM_upper_midwest, 3), ")", sep = "")
UMWval

##** Class Question **##
# What is the interpretation of this effect estimate? 
##*****************************************************************## 

# 8d Assess model fit
# we can ask whether including the interaction terms improved model fit 
# anova() provides a nice command for the likelihood ratio test (LRT)

anova(mod.noOutliers, mod.interaction, test="LRT")

##** Class Question **##
# Does the LRT suggest that there is effect modificaiton by region? 
##*****************************************************************## 

# 8e Fit stratified model
# we can isolate our model to one strata of the data with the subset command 
# we could fit a stratified model for each ClimateRegion
mod.stratified = lm(AveBMI ~ AvePM + PerBlack + PerLatinx + PerAsianAm + 
                       MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp, 
           data = dta.noOutliers, na.action = na.omit,
           subset = (ClimateRegion == "upper_midwest"))

summary(mod.stratified)

##** Class Question **##
# What is the difference between a model with an interaction term
# and a stratified model? 
##*****************************************************************## 

####******************************
#### 9: Breakout Exercises   #####
####******************************

# 9a What is the effect estimate including 95% CI for Median 
# Household Value (MedHVal)?

# 9b Compute the association and confidence intervals for 
#    a 10 ug/m3 increase in AvePM in the main model (mod)

# 9c What is the association between AvePM and AveBMI in an unadjusted model? 

# 9d What is the effect estimate (estimate and 95%CI)
#    for AvePM in the southeast region?

# Challenge question: if you have time
# 9e Do our data suggest that the association between 
#    annual average PM and average BMI is different in  
#    counties with below average male unemployment, 
#    and those with above average male unemployment? 

####*******************************************
# hints: 
# When we multiply a coefficient by a unit, we also multiply the standard errors 
# for example, we would use 
# 5*(beta.ohio_valley + 1.96*ste.ohio_valley) 
# not (5*beta.ohio_valley + 1.96*ste.ohio_valley)

# What does an unadjusted model look like? 

# You will need to make a categorical variable for male unemployment. 
# MedianMaleEmp = median(dta$MaleUnemp, na.rm = TRUE)
# dta$MaleUnempAboveMedian = if_else(dta$MaleUnemp > MedianMaleEmp, "Above Median", "Below Median")
####*******************************************





####*******************************************
#### * Bonus Material * #####
####*******************************************

####*******************************
#### 10: Data Manipulation    #####
####*******************************

dta = read_csv(file = "./data/county_pm_bmi_census.csv")

# often our raw data aren't ready for analysis, 
# and we need to remove observations, create variables, 
# combine datasets, etc.

# 10a Creating Columns 
# we can create new columns by assigning them 

dta$aveBMIsq = dta$aveBMI * dta$aveBMI 

# the dplyr package is a popular package for manipulating data
# dplyr is part of a larger family of packages called the tidyverse 
# the tidyverse packages share a common syntax and philosophy. 
# You can learn more about the tidyverse and its many awesome functions 
# in the R for Data Science book by Hadley Wickham 

library(dplyr) 

# in dplyr, we use mutate() to create new variables 

dta1 = mutate(dta, aveBMIsq = aveBMI*aveBMI)

# within mutate(), 
# the function knows that you are referring to columns within dta, 
# so you do not need the dta$ notation 
# we can also use if else statements within dplyr 
# dplyr automatically does operations row by row 
# so adding two columns a and b would create a third column c with values 
# c1 = a1+ b1, c2=a2+b2, c3 = a3+b3
# dta1 = mutate(dta1, c = a + b)

# We often create new dataframes when manipulating data 
# It is important to use consistent naming 
# so you can keep track of what you are doing. 
# If we use the same name for the dataframe, 
# then we are rewriting that dataframe, 
# and we lose previous versions 
# Often, we want to use sequential of informative names 
# so that we can keep track of our progress 
# and catch errors 
# sequential names: dta1, dta2, dta3
# informative names: dta_onlyobese, dta_bmisq
# naming might seem trivial, but it has helped me stay organized and fix my mistakes
# normally I don't create new dataframe for every transformation, 
# but in this code I did so that is it easy to follow how the code changes the data

# if_else()
# if_else(logical statement, value if true, value if false)
dta2 = mutate(dta1, aveObese = if_else(aveBMI >=30, "average obese", "average not obese"))

# the cut() function 
# the "cut()" function will divide a variable into a set number of levels. 
# We will divide AveBMI into five levels, 
# and then store that information in a new variable, AveBMI5
# The basic cut() function chooses five equally spaced intervals of AveBMI. 
# Since cut() creates intervals that are open on the lower side and closed on the other 
# In the lowest interval, the lowest value will not be included
# since it is the "open end point" 
# the arguement include.lowest = TRUE will include the lowest value in the lowest bin 
# we almost always want to inlcude the lowest value. 

dta3 = mutate(dta2, aveBMI5 = cut(aveBMI, 5), include.lowest=TRUE)

# Another way to cut your data is to specify cut points 
# that are quintiles of AveBMI. 
# The categories have equal numbers of subjects, 
# but are not equally spaced on the AveBMI axis.  

dta2$aveBMI5 = cut(dta2$aveBMI, quantile(dta$aveBMI, c(0,.2,.4,.6,.8,1)), include.lowest=TRUE)
dta3         = mutate(dta2, aveBMI5 =  cut(aveBMI, quantile(aveBMI, c(0,.2,.4,.6,.8,1)), include.lowest=TRUE))

# Note that here in line 152 I create a new dataframe, dta3, 
# and rewrite in in line 158 
# These lines are making the same transformation so its not an issue to rewrite
# In the first draft of code, I usually avoid rewriting objects 
# until I am certain that that line works correctly

# dplyr provides a special syntax to pass along objects

dta4 = dta3 %>% mutate(aveBMIsq = aveBMI*aveBMI)

# same result as
dta4.a = mutate(dta3, aveBMIsq = aveBMI*aveBMI)

# these pipes can be connected, like this 

dta4 = dta3 %>% mutate(aveBMIsq = aveBMI*aveBMI) %>% 
  mutate(aveObese = if_else(aveBMI >=30, "average obese", "average not obese"))

# 10b Subsetting Data 
# we can use the filter() command to keep only rows/observations we are interested in 

dta.obese = dta4 %>% filter(aveObese == "average obese")

# filter() can accept a range of logical statements 

dta.obese2 = dta4 %>% filter(aveBMI >=30)

# similarly, we can use the select() command to keep only 
# the columns/variables we are interested in 

dta.obese3 = dta4 %>% select(fips, aveObese)

# we can also use select()with the minus sign to remove columns
# here we are removing these three spatial variables which 
# helped us connect the pm and county data, but are no longer necesary 

dta5 = dta4 %>% select(-AREA, -PERIMETER, -geometry)

# 10c Combining Data 
# we can combine data with the join commands 
# the dplyr cheatsheet is a good reference for understanding 
# the different ways of combining data. 
# here we will use left_join() to add unemployment data from the 2010 US census 

unemp = read.csv(file = "./data/2010_census_unemployment_data.csv")

# we join by FIPS codes, so we combine rows that have matching FIPS

dta6 = left_join(dta5, unemp, by = "fips")

# look at the new dataframe... does it contain the unemployment variables? 

View(dta6)

# There are other join options 
# for example, inner_join, will only keep observations 
# with a successful match, 
# and full_join will keep all observations from both data frames 
# check the dplyr cheatsheet to see your options
# CRAN hosts several excellent cheatsheets on its site 
# https://www.rstudio.com/resources/cheatsheets/
# I still refer to my cheatsheets

# for example, inner_join() only keeps rows that do have a match for FIPS. 

dta.inner = dta5 %>% inner_join( unemp, by = "FIPS")

# Here is another example of a join that is part of a pipe 
# I will cover this sort of code in the data manipulation workshop 

NOAAClimateRegions = read_csv(paste0(DataPath, "noaa_climate_regions_FIPS.csv"))

# join 
dta7 = dta6 %>% 
  mutate(state_fips_code = stringr::str_sub(FIPS,0,2)) %>% 
  left_join(NOAAClimateRegions, by = "state_fips_code") %>% 
  select(-state_fips_code)

# We must chage the mode of the column to make them compatiable 

NOAAClimateRegions = NOAAClimateRegions %>% 
  mutate(state_fips_code = as.character(state_fips_code))

dta7 = dta6 %>% 
  mutate(state_fips_code = stringr::str_sub(FIPS,0,2)) %>% 
  left_join(NOAAClimateRegions, by = "state_fips_code") %>% 
  select(-state_fips_code)

# 10d Cleaning the environment 
# let's remove dataframe we are no longer using
rm( dta, dta.inner, dta.obese, dta.obese2, dta.obese3, 
   dta1, dta2, dta3, dta4, dta5, dta6, unemp, NOAAClimateRegions)

####******************************
#### 11: Summarizing Data    #####
####******************************

# We ALWAYS need to look at our data before starting our analyses 
# summarizing data is an easy way to catch errors 
# E.g., the first time I created this data set, the maximum average BMI was 400,
# and I realized my scale was wrong 

# just as with data manipulation, there is a base R and 
# a tidyverse method for many tasks 
# Personally, I mostly use the tidyverse method. 
# the tidyverse method outputs dataframes, 
# which I can then easily save or use in other analyses 
# the summarize command also allows me to compute multiple summary stats, 
# for multiple variables, at the same time. 
# the base R commands are simple and require less typing 
# I will present both here. 

# 11a Means
# we can compute the mean of any vector 

MeanPM1 = mean(dta7$AvePM)
MeanPM2 = dta7 %>% summarize(meanPM = mean(AvePM))

# what is the different between MeanPM1 and MeanPM2? 

class(MeanPM1)
class(MeanPM2)

# 11b Medians 
# we can compute the median of any vector 

MedianPM1 = median(dta7$AvePM)
MedianPM2 = dta7 %>% summarize(medianPM = median(AvePM))

# 11c Quantiles 

QuantilePM1 = quantile(dta7$AvePM, c(0.1,0.9))
QuantilePM2 = dta7 %>% summarize(tenthPM = quantile(AvePM, 0.1),
                                 ninetithPM = quantile(AvePM, 0.9))

# do two methods yield the same results? 

identical(QuantilePM1[1], QuantilePM2$tenthPM[1])
QuantilePM1[1] 
QuantilePM2$tenthPM[1]


# the dplyr cheat sheet shows the list of the 
# many functions you can use with summarize 

# 11d Multiple summaries 
# in base R, we can quickly create summaries with the summary and table commands 
# you can get the summary of an individual variable 

summary(dta7$AvePM)
summary(dta7$AveBMI)

# the results for summary are not very interesting for character variables 

summary(dta7$AveObese) 
table(dta7$AveObese) # table is more useful for character variables

# we can rewrite the AveObese variable as a factor
# the results are more useful for factors

dta8 = dta7 %>% mutate(AveObese = as.factor(AveObese))
summary(dta8$AveObese)

# you can get the summary of every variable in a dataframe 

summary(dta8) 

# you can create count cross-tabulations with the table() command 
# aka frequency tables

table(dta8$AveObese, dta8$State)

# we can save the results 

ObesityByState1 = table(dta8$AveObese, dta8$State)

# One advantage of these commands is that they are simple - 
# you don't have to write as much as with the dplyr method 
# the main disadvantage is that the result is not a dataframe, 
# and can be harder to manage. 

# Multiple summary stats in dplyr 
# we can create cross-tabulations using the group_by function and n()
# n() here does not require its own arguement

dta8 %>% group_by(State, AveObese) %>%
  summarize(Count = n())

# we can save the results 

ObesityByState2 = dta8 %>% group_by(State, AveObese) %>%
  summarize(Count = n())

# What is the highest county-average PM in each state? 
# (among the counties measured!) 

PM25ByState = dta8 %>% group_by(State) %>% 
  summarize(MaxPM = max(AvePM))

# 11e  Pipes 
# using the %>% pipes, we can combine multiple manipulations 
# into a single step

# what is the average PM and BMI within Pennsylvania? 

MeanPM_MeanBMI = dta8 %>% filter(State == "PA") %>% 
  summarize(meanPM = mean(AvePM), 
            meanBMI = mean(AveBMI))

# 1f Correlations 
# we can compute the pearson or spearman correlation with cor() 

cor(dta8$AveBMI, dta8$AvePM, method = "pearson")
cor(dta8$AveBMI, dta8$AvePM, method = "spearman")

####*****************************
#### 12: Bonus Exercises    #####
####*****************************

# Data Manipulation 
# 4a create a categorical variable for annual average PM2.5 with 4 quantiles 
# 4b create dummy variable for whether a county is in New York, such that 1= in NY and 0 = not NY 
# 4c create dummy variable for whether a county's annual average PM2.5 is greater or smaller than the mean PM2.5 across all counties
# 4d create a dataset of only counties with annaul average PM2.5 >10 

# Summarizing Data
# 4e compute the correlation between aannual average PM2.5 and female unemployment
# 4f find the mean percent female unemployment among all sampled counties
# 4g compute the summary statistics of female unemployment
# 4h find the average percent female unemployment among the the sampled counties in NY 

#############################################
# hints: 
# in R, NA refers to missing data. 
# you can check whether a value is missing with is.na() 

a = NA
is.na(a) 
a = c(1,NA,2)
is.na(a)

# sometimes you need to tell functions to ignore or exclude missing values
# different functions have different arguments to address missing values 
# na.rm is common 
# often the ?function command can show you what arguments a function uses
# or you can check the full documentation

mean(dta8$FemaleUnemp)
mean(dta8$FemaleUnemp, na.rm= TRUE)
cor(dta8$FemaleUnemp, dta8$MaleUnemp,use="complete")

# also, you can use objects inside of commands 

a   = 100
dta8 = dta8 %>% mutate(AveBMI_rescaled = AveBMI * a)

#############################################
