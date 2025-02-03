# Quantile  Regression 
# Session 2
# Advanced Analytic Methods for Environmental Epidemiology

####***********************
#### Table of Contents ####
####***********************

## Pre-Class Material ##
# 0: Preparation 
# 1: Load Data 
# 2: Define Research Question
# 3: Calculate and Compare Quantiles

## Live Session Material ## 
# 4: Review Research Question
# 5: Fit Median Model 
# 6: Compare Association at Different Quantiles
# 7: Group Exercises 

## Bonus Material ## 
# Footnote 1: Standard Errors
# Footnote 2: ggplot2 Options
# Footnote 3: Interactions in Linear Regression

####************************
#### Pre-Class Material ####
####************************

####********************
#### 0: Preparation ####
####********************

# 0a Install packages 

# quantreg features the quantile regression and associated functions
#install.packages("quantreg")

# ggplot2 has plotting commands 
#install.packages("ggplot2")

# 0b Load packages
library(readr)
library(dplyr) 
library(quantreg)
library(ggplot2)

#0c Declare directories

file_path <- "/Users/yukilow/Desktop/AAMEHS/lab_1/Lab1_2010_County_Data.csv"

####******************
#### 1: Load Data ####
####******************

# 1a Load data 

dta <- read_csv(file_path)

# 1b Remove counties with missing data 
# not best practice to blindly remove observations 
# but we will discuss more thoughtful ways to address missing data 
# in the Missing Data session 

dta <- dta %>% 
  filter(complete.cases(dta))

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

####*********************************
#### 2: Define Research Question ####
####*********************************

# Does the distribution of county-average BMI vary by annual PM2.5? 
#    Does the central tendency (median) vary by annual PM2.5? 
#    How does PM2.5 affect other aspects of the distribution 
#    (e.g., 99th percentile)

####****************************************
#### 3: Calculate and Compare Quantiles ####
####****************************************

# Before running quantile regression models, 
# let's calculate some quantiles ourselves, and compare between groups. 
# This will illustrate the distinction between what quantile regression estimates 
# and what linear regression estimates. 

# To simplify our comparison, we will look at just the two climate regions 
# with the most observations

# 3a Isolate the top 2 regions 

# 3a.i Identify regions with the most observations
table(dta$ClimateRegion)

# 3a.ii Make new dataframe with just these regions 
# the | symbol is 'or' in R, meaning that we are filtering for rows 
# where at least one of these conditions are true.
dta.OhioValley_Southeast <- dta %>% 
  filter(ClimateRegion == "ohio_valley" | ClimateRegion == "southeast")

##** PreClass Task: Create Boxplot **##
# A) Create a boxplot that will allow us to visually compare the mean, 
# 25th percentile (first quartile), 50th percentile (median), and 75th 
# percentile (third quartile).
# B) Which region has a larger 25th percentile?
# Note that in the boxplot, the whiskers are not necesarily quantiles
# IQR is interquartile range
# lower whisker = max(min(x), Q_1 – 1.5 * IQR) 
# upper whisker = min(max(x), Q_3 + 1.5 * IQR) 
# In other words, we only plot individual points if they are 
# lower than 25th percentile minus 1.5*IQR or
# high than 75th percentile plut 1.5*IQR
# hint: look at section 2b of the linear regression lab to see code for 
# making boxplot
##*****************************************************************## 

# 3c Calculate quantiles 
# 3c.i We will first break up the data according to region 
dta.OhioValley <- dta %>% 
  filter(ClimateRegion == "ohio_valley")
dta.Southeast <- dta %>% 
  filter(ClimateRegion == "southeast")

# 3c.ii Calculate quantiles in each region 
# we use the quantile() command 
# the first arguement is the vector of numbers that we are interested in 
# (remember we use $ to identify a column of a dataframe)
# the second argument is the quantile to estimate, represented as a decimal 
Ohio.median <- quantile(dta.OhioValley$AveBMI, 0.5)
Ohio.q1q2q3 <- quantile(dta.OhioValley$AveBMI, c(0.25, 0.5, 0.75))

# 3d Next, let's see how sensitive the mean and median are to outliers 

# 3d.i Let's calculate the mean BMI for Ohio Valley region

OhioValley.Mean <- mean(dta.OhioValley$AveBMI)

# 3d.ii Let's remove that extremely high value, and see whether the mean changed 

dta.OhioValley.noExtreme <- dta.OhioValley %>% 
  filter(AveBMI < 40)

OhioValley.Mean.noExtreme <- mean(dta.OhioValley.noExtreme$AveBMI)

# 3d.iii Compare values 

OhioValley.Mean - OhioValley.Mean.noExtreme

# 3d.iii Does the median change when we remove the extreme value? 

OhioValley.Median <- quantile(dta.OhioValley$AveBMI, 0.5)[[1]]
OhioValley.Median.noExtreme <- quantile(dta.OhioValley.noExtreme$AveBMI, 0.5)[[1]]
OhioValley.Median - OhioValley.Median.noExtreme

##** PreClass Task: Compare quantiles **##
# A) Which region has the smallest minimum BMI? 
# remember, each observation/row represents a county within that region
# B) Which region has the largest 25th percentile? 75th? 
# C) Does removing that extreme observation influence the results 
#       of our comparison?
##*****************************************************************## 

# 3e Remove that county with very high BMI 
# most likely an error, in real life we would investigate further
dta <- dta[-c(169),]

# 3f Clean up the environment 
rm(list = ls(pattern = "Ohio"))
rm(list = ls(pattern = "dta."))

####***************************
#### Live Session Material ####
####***************************

####*********************************
#### 4: Review Research Question ####
####*********************************

# Does the distribution of county-average BMI vary by annual PM2.5? 
#    Does the central tendency (median) vary by annual PM2.5? 
#    How does PM2.5 affect other aspects of the distribution 
#    (e.g., 99th percentile)

####*************************
#### 5: Fit Median Model ####
####*************************

# 5a Create median model 
# how do we tell the rq function which quantile to estimate? 

?rq 

# the parameter tau is the quantile(s) to be estimated 
# median is 50th percentile, so tau = 0.5

Mod50th <- rq(AveBMI ~ AvePM + PerBlack + PerLatinx + PerAsianAm + 
            MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp + ClimateRegion, 
            data = dta, 
            tau = 0.5)

##** Class Question 1 **##
# A) Do we have any warnings? What do they mean? How do we learn more?  
##*****************************************************************## 

FAQ()

# 5b View median model results
# we can use summary just as before 
# alpha sets the Type I error rate for the confidence intervals

summary(Mod50th, alpha = 0.05)

# The output is slightly different from lm()
# We get upper and lower bounds rather than standard errors and T tests. 
# I explain how to get standard errors in the Footnotes Section

##** Class Question 2 **##
# A) What is the value and interpretation of the coefficient for PM2.5? 
##*****************************************************************## 

# 5b.ii Extract coefficients

Coeff.Mod50th <- summary(Mod50th, alpha = 0.05)$coefficients

# 5b.iii Report effect estimates for PM2.5

Coeff.Mod50th.PM2.5 <- Coeff.Mod50th[2,1:3]
print(paste0("Association for PM2.5 and 50th percentile BMI: ", 
             round(Coeff.Mod50th.PM2.5[1], 3), " 95% CI: (",
             round(Coeff.Mod50th.PM2.5[2], 3), ", ", 
             round(Coeff.Mod50th.PM2.5[3], 3), ")"))

# 5c Compare estimate of association for mean aveBMI

# 5c.i Fit mean model
ModMean <- lm(AveBMI ~ AvePM + PerBlack + PerLatinx + PerAsianAm + 
              MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp + ClimateRegion, 
              data = dta)

# 5c.ii Create table of coefficients from both models

coeff.table  <- data.frame(Fit.Mod50th = summary(Mod50th)$coefficients[,1],
                           Fit.ModMean = summary(ModMean)$coefficients[,1])

# 5c.iii Make a table of percent differences

coeff.table <- coeff.table %>% 
  #compute percentage difference
  mutate(PercentDiff = 100*(Fit.Mod50th - Fit.ModMean)/Fit.ModMean) %>% 
  # round numbers 
  mutate(Fit.Mod50th   = round(Fit.Mod50th, 3), 
         Fit.ModMean = round(Fit.ModMean, 3),
         PercentDiff   = round(PercentDiff, 1))

coeff.table

# 5.c.iv Report both effect estimates

# Calculate confidence interval for coefficient from mean model 

Coeff.ModMean.PM2.5 <- summary(ModMean)$coefficients[2,]
ModMean.Beta.pm.fit <- summary(ModMean)$coefficients[2,1]
ModMean.Beta.pm.se  <- summary(ModMean)$coefficients[2,2]
ModMean.Beta.pm.lci <- ModMean.Beta.pm.fit - 1.96 * ModMean.Beta.pm.se
ModMean.Beta.pm.uci <- ModMean.Beta.pm.fit + 1.96 * ModMean.Beta.pm.se

print(paste0("Association for PM2.5 and Mean BMI: ", 
             round(ModMean.Beta.pm.fit, 3), " 95% CI: (",
             round(ModMean.Beta.pm.lci, 3), ", ", 
             round(ModMean.Beta.pm.uci, 3), ")"))
print(paste0("Association for PM2.5 and 50th percentile BMI: ", 
             round(Coeff.Mod50th.PM2.5[1], 3), " 95% CI: (",
             round(Coeff.Mod50th.PM2.5[2], 3), ", ", 
             round(Coeff.Mod50th.PM2.5[3], 3), ")"))

# 5d Visually compare coefficients
# for these plots, we will use ggplot2 

# 5d.i Combine estimates from each model into a single dataframe 
# assemble estimates from each Model
# note that the code here is different for the rq model and the lm Model 
# since the summary.rq() directly outputs confidence intervals 
# and summary.lm outputs standard errors.

# create dataframe 

coeff.table <- data.frame(
  ModelName = c("Mean Model", "Median Model"), 
  coeff = c(ModMean.Beta.pm.fit, Coeff.Mod50th.PM2.5[1]), 
  lci = c(ModMean.Beta.pm.lci, Coeff.Mod50th.PM2.5[2]), 
  uci = c(ModMean.Beta.pm.uci, Coeff.Mod50th.PM2.5[3]) )

# 5d.ii Forest plot
# with ggplot, we can keep our plot in the environment
# as a gpglot object 
# and then display it in the plot panel 
# by entering the plot's name
             
ForestPlotMeanMedian <- ggplot(data = coeff.table, 
            # defines what dataset ggplot will use    
  # aes() defines which variables the geoms will use   
  aes( # defines variable for the x axis
      x = ModelName,  
      # defines the variable for the point along the y axis
      y = coeff,      
      # defines the lower bound of the confidence interval
      ymin = lci,     
      # define the upper bound of the confidence interval 
      ymax = uci)) +  
  # creates a point (y) with line defined by ymin and ymax
  geom_pointrange() +   
  # creates lines with bars, i.e. here the CIs
  geom_errorbar()+      
  # add a dashed line at y=0
  geom_hline(aes(yintercept = 0.0), lty = 2) +
  # labels for axes
  xlab("Model Name") +    
  ylab(expression("Coefficient for PM"[2.5]~" (95% CI)"))

ForestPlotMeanMedian # produces the plot in the plots panel

##** Class Question 3 **##
# A) Are the effect estimates the same?
# B) If not, what some possible explanations for the difference? 
##*****************************************************************## 

####***************************************************
#### 6: Compare Association at Different Quantiles ####
####***************************************************

# often, we are interested in how associations vary for different quantiles 

# 6a Two tau's 
# the quantreg package incldues a way to estimate 
# Models for multiple tau's at once 
# we create a vector of the tau's

Mods25.50 <- rq(AveBMI ~ AvePM + PerBlack + PerLatinx + PerAsianAm + 
                  MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp + ClimateRegion, 
                data = dta, tau= c(0.25, 0.50)) # c() creates a vector

summary(Mods25.50)

# 6b Combine estimates from each model into a single dataframe 
# assemble estimates from each model
# we will save the summary as an object 
# so we do not have to keep re-summarizing the models. 

summary25.50 <- summary(Mods25.50, alpha = 0.05)

# we use the brackets to specify which model we are extracting

Model25th   <- c(summary25.50[[1]]$coefficients[2,1:3])
Model50th   <- c(summary25.50[[2]]$coefficients[2,1:3])
print(paste0("Association for PM2.5 and 25th percentile BMI: ", 
             round(Model25th[1], 3), " 95% CI: (",
             round(Model25th[2], 3), ", ", round(Model25th[3], 3), ")"))
print(paste0("Association for PM2.5 and 50th percentile BMI: ", 
             round(Model50th[1], 3), " 95% CI: (",
             round(Model50th[2], 3), ", ", round(Model50th[3], 3), ")"))

##** Class Question 4 **##
# Is the association between PM2.5 and the 25th percentile of BMI 
# the same as the association for the 50th percentile?
##*****************************************************************## 

# 6b Plot two quantile models 

# create dataframe 

coeff.table <- rbind(Model25th, Model50th)
coeff.table <- as.data.frame(coeff.table, stringsAsFactors = FALSE)

# set names for dataframe

names(coeff.table) <- c("coeff", "lci", "uci")
coeff.table        <- coeff.table %>% 
                      mutate(ModelName = c("Model 25th", "Model 50th")) 

# 6b.ii Plot

ForestPlot.25.50 <- ggplot(data=coeff.table, # defines what dataset we are using
             aes(x=ModelName,  # defines variable for the x axis
                 y=coeff,      # defines the variable for the point along the y axis
                 ymin=lci,     # defines the lower bound of the confidence interval
                 ymax=uci)) +  # define the upper bound of the confidence interval   
  geom_pointrange() +          # creates a point (y) with line defined by ymin and ymax        
  geom_errorbar()+             # creates lines with bars
  geom_hline(aes(yintercept=0.0), lty=2) + # add a dashed line at y=0 
  xlab("Model Name") +         # labels for axes
  ylab(expression("Coefficient for PM"[2.5]~" (95% CI)"))

ForestPlot.25.50

# 6c Plot Multiple Quantiles
# we can just use the same procedure as before, 
# but with additional models for every quantile of interest

# 6c.i Create the models
# seq() creates a sequence with intervals set by the by arguement 

TauList <- seq(0.1, 0.9, by = 0.1)
TauList

qr.Mods  <- rq(AveBMI ~ AvePM + PerBlack + PerLatinx + PerAsianAm + 
                 MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp + ClimateRegion, 
               data = dta, 
               tau = TauList)

# 6c.ii Assemble estimates from each model

summary.qr.Mods <- summary(qr.Mods, alpha = 0.05)

Model10th   <- c(summary.qr.Mods[[1]]$coefficients[2,1:3])
Model20th   <- c(summary.qr.Mods[[2]]$coefficients[2,1:3])
Model30th   <- c(summary.qr.Mods[[3]]$coefficients[2,1:3])
Model40th   <- c(summary.qr.Mods[[4]]$coefficients[2,1:3])
Model50th   <- c(summary.qr.Mods[[5]]$coefficients[2,1:3])
Model60th   <- c(summary.qr.Mods[[6]]$coefficients[2,1:3])
Model70th   <- c(summary.qr.Mods[[7]]$coefficients[2,1:3])
Model80th   <- c(summary.qr.Mods[[8]]$coefficients[2,1:3])
Model90th   <- c(summary.qr.Mods[[9]]$coefficients[2,1:3])

# create dataframe 

coeff.table <- rbind(Model10th, Model20th, Model30th, Model40th, 
                     Model50th, Model60th, Model70th, Model80th, 
                     Model90th)

coeff.table <- as.data.frame(coeff.table, stringsAsFactors = FALSE)

# set names for dataframe

names(coeff.table) <- c("coeff", "lci", "uci")
coeff.table        <- coeff.table %>% 
          mutate(ModelName = c("10th", "20th", "30th", "40th","50th", "60th", 
                               "70th", "80th", "90th"))

# 6b.ii Plot

ForestPlot.Mods <- ggplot(data=coeff.table, # defines what dataset we are using
                  aes(x=ModelName,  # defines variable for the x axis
                      y=coeff,      # defines the variable for the point along the y axis
                      ymin=lci,     # defines the lower bound of the confidence interval
                      ymax=uci)) +  # define the upper bound of the confidence interval   
  geom_pointrange() +               # creates a point (y) with line defined by ymin and ymax        
  geom_errorbar()+                  # creates lines with bars
  geom_hline(aes(yintercept=0.0), lty=2) + # add a dashed line at y=0 
  xlab("BMI Quantile") +              # labels for axes
  ylab(expression("Coefficient for PM"[2.5]~" (95% CI)"))

ForestPlot.Mods

##** Class Question 5 **##
# A) Does the association between PM2.5 and BMI vary when we look at different 
#       quantiles? 
##*****************************************************************## 

####*************************
#### 7: Group Exercises #####
####*************************

# 7a What is the adjusted association between county-average annual PM2.5 
#       and the 35th percentile of county-average BMI? 
#       Make sure to include interpretation and confidence intervals. 

# 7b Plot the coefficient and confidence intervals for the association between 
#       MedHVal and the 35th and 65th percentile of AveBMI.

####********************************
#### Footnote: Standard Errors  ####
####********************************

# The author of the rq package, Roger Koenker, 
# included multiple possible algorithms for estimating 
# the uncertainity of the model. 
# more information can be found with 
# ?summary.rq
# under the heading 'se'
# Briefly, when the data has fewer than 1001 observations, then 
# the model will use the rank method, which does not estimate standard errors, 
# only confidence intervals. 
# For larger datasets, or if you set the se manually, 
# the model will use a different method that does compute standard errors. 
# In that case, you can use similar code as with linear regression 
# to extract confidence intervals. 



####**********************************
#### Footnote 2: ggplot2 Options  ####
####**********************************

# More ggplot2 options
# there are many parts of the plot you can specify 
# if you don't like the defaults 
# check the ggplot cheatsheet or just look online 
# for help 
# here is just some example code. 

ForestPlot.fancy <- ggplot(data=coeff.table, 
             # tell geom_functions which variables to use
             aes(x=ModelName, y=coeff, ymin=lci, ymax=uci)) + 
  # point is a diamond , increase size 
  geom_pointrange(shape = 18, size = 1.5) +       
  # increase the size of the error bars
  geom_errorbar(size = 1.5)+ 
  # changes the color of the line
  geom_hline(aes(yintercept=0.0), lty=2, color ="grey") + 
  # flip coordinates (puts labels on y axis)
  coord_flip() +                                     
  xlab("Model\nName") + ylab(expression("Coefficient for PM"[2.5]~" (95% CI)")) +
  # change the angle of the title of the y-axis
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +  
  # change the size of the axis titles
  theme(axis.title = element_text(size = 28)) +                 
  # change the size of the axis text
  theme(axis.text = element_text(size = 20)) +      
  # use a white background without gridlines
  theme(panel.background = element_rect(fill = 'white', color = "black")) 

ForestPlot.fancy

####****************************************************
#### Footnote 3: Interactions in Linear Regression  ####
####****************************************************

# Does the association between county-level annual PM2.5 and county average BMI 
# vary by climate region? 
# Let's add an interaction term 

# F3a Fit regression model 

mod.interaction <- lm(AveBMI ~ AvePM*ClimateRegion + PerBlack + PerLatinx + PerAsianAm + 
                        MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp, 
                      data = dta, na.action = na.omit)
# F3b Review Model  

summary(mod.interaction)

# We see that the term for pm*upper_midwest is almost significant 
# We can use the values from the model to estimate 
# the PM-AveBMI association within the  Upper Midwest
# and to estimate the confidence intervals of the association 

# F3c Compute the within-region association and its uncertainty 

# F3.i Extract coefficents and covariances
# here we create tables of coefficients and covariance
coef.mat <- summary(mod.interaction)$coefficients
var.mat  <- vcov(mod.interaction)

# F3.ii Compute total term for within-region association
# sum of the term in the reference region plus the term for Upper Midwest

beta.PM_upper_midwest <- coef.mat["AvePM",1] + 
  coef.mat["AvePM:ClimateRegionupper_midwest",1]

# F3.iii Compute variance of within-region association 
# in order to compute standard error
# We must compute the variance for the total term 
# Var(Beta1 + Beta3) = Var(Beta1) + Var(Beta3) + CoVar(Beta1, Beta3) + CoVar(Beta3, Beta1)
# Var(Beta1 + Beta3) = Var(Beta1) + Var(Beta3) + 2*CoVar(Beta1, Beta3) 

var.PM_upper_midwest  <- var.mat["AvePM", "AvePM"] + 
  var.mat["AvePM:ClimateRegionupper_midwest", "AvePM:ClimateRegionupper_midwest"] +
  2*var.mat["AvePM", "AvePM:ClimateRegionupper_midwest"]

se.PM_upper_midwest  <- sqrt(abs(var.PM_upper_midwest))

# F3.iv Compute confidence intervals 
# using ste for within-region association

lci.PM_upper_midwest <- beta.PM_upper_midwest - 1.96*se.PM_upper_midwest
uci.PM_upper_midwest <- beta.PM_upper_midwest + 1.96*se.PM_upper_midwest

UMWval <- paste(round(beta.PM_upper_midwest, 3), " (95% CI: ", round(lci.PM_upper_midwest, 3), ", ", 
                round(uci.PM_upper_midwest, 3), ")", sep = "")
UMWval

# F3.v Assess model fit
# we can ask whether including the interaction terms improved model fit 
# anova() provides a nice test comparing model fit

mod.noInteraction <- lm(AveBMI ~ AvePM + PerBlack + PerLatinx + PerAsianAm + 
            MedHInc + MedHVal + LTHS + FemaleUnemp + MaleUnemp + ClimateRegion, 
          data = dta, 
          na.action = na.omit)

anova(mod.noInteraction, mod.interaction)

# the results of the anova are not statistically significant, 
# indicating that as a whole interaction between pm and region does not improve model fit
# i.e. no evidence of effect modification by region


