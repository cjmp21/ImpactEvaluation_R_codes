
## Module 9 R Tutorial
## Regression Discontinuity Designs

# The data we use is from a study by Owen Ozier, who uses a regression-discontinuity 
# design to evaluate the impact of secondary schooling in Kenya (Ozier, Owen.
# "The Impact of Secondary Schooling in Kenya: A Regression Discontinuity Analysis.
# " Journal of Human Resources 53, no. 1 (2018): 157-188.
# https://www.muse.jhu.edu/article/684446.). 

# As with past tutorials, we caution that we are simplifying the analysis in 
# order to illustrate how to accomplish core concepts. As a result, we may not 
# arrive at precisely the same numerical results as published in the paper. 
# If you would like to see the complete results, we encourage you to read the full study.

# The paper estimates the effect of secondary schooling on future outcomes 
# by exploiting a jump in the probability of admission to secondary school 
# as performance on a national primary school examination crosses a threshold. 
# We will first examine the probability of gaining admission to secondary school 
# on each side of the jump in order to test the viability of the RDD design. 
# We will then estimate the effect of secondary school on non-verbal reasoning 
# and vocabulary using an RDD design.


## Part 1.  Import `module_9_data.dta` and describe the observations.
library(haven)
library(plyr)
library(dplyr)
library(lmtest)
library(sandwich)

module_9_data <- read_dta("C:/Users/charl/Documents/10. Diligencias Colombia/Georgetown Ux - Impact Evaluation Methods with Applications in Low- and Middle-Income Countries/4. Stata R Excercises/Excercise 9 (R)/Module 9/Data/module_9_data.dta")
View(module_9_data)

summary(module_9_data)

# The data should have 5 variables and 2,169 observations. Test scores are 
# normalized to be centered on zero, and range from -2.51 to 1.68 (test variable).

# We will now estimate the relationship between the (normalized) test score and 
# the probability of attending secondary school, allowing for a jump at zero. 
# We restrict the sample to include only scores between -0.8 and 0.8.


## Part 2: Regress `secondary` on `test`, `jump`, and an interaction between 
##         `test` and `jump`. Note that `test` is centered such that `jump = 1` 
##         corresponds with `test = 0`

module_9_data$if_1 <- ifelse(abs(module_9_data$test)<0.8 ,1,0)

module_9_data_2 <- module_9_data[module_9_data$if_1 == 1
                                 , c("test","secondary","jump","rv","female","if_1")]

model_1 <- lm(module_9_data_2$secondary ~ module_9_data_2$test + module_9_data_2$jump + 
                module_9_data_2$test*module_9_data_2$jump)
model_1_robust <- coeftest(model_1, vcov = vcovHC(model_1, "HC1"))

summary(model_1)
print(model_1_robust)

# This is a linear probability model, with a jump at zero, with the restriction 
# that the slope of the relationship between test scores and attendance is the 
# same on both sides of the cut-off.  The results suggest that receiving a score 
# above the cutoff appears to increase the probability of receiving secondary 
# education.  That is, the coefficient on `jump` is 0.1636, and is highly statistically 
# significant.

# However, we might be concerned that this result is driven by our linearity 
# assumption, and that the true relationship between test scores and attendance 
# could exhibit non-linearities.  

# Install the command `rdplot` by entering the command `ssc install rdrobust`. 
# Then use `rdplot` to plot the probability of receiving secondary education 
# by test score, only considering test scores between -1 and 1.  
# This command will create polynomial graphs on each side of the cut-off.

install.packages("rdrobust")
library(rdrobust)

rdplot(module_9_data$secondary, module_9_data$test,subset = abs(module_9_data$test) < 1)
rdrobust(module_9_data$secondary, module_9_data$test, p = 4, h =0.990, subset = abs(module_9_data$test) < 1
         , kernel = "Uniform")

# Each dot represents the secondary school attendance rate of about 25 observations 
# of individuals with scores around that value.  Although the dots bounce around, 
# so there is a lot of variation around the plotted lines, the figure provides 
# further evidence of a jump in the probability of attending secondary school as 
# the cut-off test score of zero is crossed.

# Next, we estimate of the effect of the test score on verbal reasoning and 
# vocabulary scores using the variable `rv`, as before allowing for a jump 
# at the score of zero. The variable `rv` is a combined Z-score constructed 
# from a test designed to measure cognitive ability, so it measures scores in 
# standard deviations relative to the average respondent. 

# Again, only consider values of the variable `test` between -0.8 and 0.8.

## Part. 4: Regress `rv` on `test`, `jump`, and an interaction between `test` and `jump`. 

model_2 <- lm(module_9_data_2$rv ~ module_9_data_2$test + module_9_data_2$jump + 
                module_9_data_2$test*module_9_data_2$jump)
model_2_robust <- coeftest(model_2, vcov = vcovHC(model_2, "HC1"))

summary(model_2)
print(model_2_robust)

## Note: The "Jump" variable is a dichotomous-like variable. It represents the cut-off
## or the discontinuity in the data. The significance of that variable (0.04) suggest
## a correct implementation of this methodology to evaluate this experiment.

# Again, we've used a simple linear regression, assuming a constant slope on 
# either side of the cut-off. The results suggest that scoring above the cutoff 
# leads to a statistically significant increase in the combined reasoning and 
# vocabulary score. Individuals to the right of the cutoff score about 0.115 
# standard deviations higher on average, suggesting that attending secondary 
# school increases reasoning and vocabulary skills.

# Because the probability of attending secondary school does not jump from zero 
# to one, but increases by only about 16 percentage points (see the coefficient 
# on `jump` in Task 2 above), we have a fuzzy RDD design. That coefficient in 
# the regression of learning outcomes on test scores, is thus the intent to treat 
# (ITT) estimate of the effect of attending secondary school on combined reasoning 
# and vocabulary scores.

# To calculate the local average treatment effect (LATE), that is, the effect 
# of secondary school on those who were admitted because they just passed the 
# threshold, but otherwise would not have been, we use a two-stage least 
# squares regression.

## Part. 5: Use the `ivregress` command to estimate the LATE of completing 
## secondary school on `rv`, using `jump` as an instrument for completing 
## secondary school. 

install.packages("ivreg", dependencies = TRUE)
library("ivreg")

module_9_data_2$test_jump <- module_9_data_2$test*module_9_data_2$jump

two_SLS_model_3 <- ivreg(module_9_data_2$rv ~ module_9_data_2$test +  
                           module_9_data_2$test_jump +
                           module_9_data_2$secondary |
                           module_9_data_2$test +  
                           module_9_data_2$jump + 
                           module_9_data_2$test_jump
                         ) ## instrumental variable after | symbol
summary(two_SLS_model_3)
coeftest(two_SLS_model_3, vcov = vcovHC(two_SLS_model_3, "HC1")) 

# The LATE is about 0.695 (which corresponds closely to the ratio of the ITT effect 
# to the compliance rate) and this result is statistically significant. So for the 
# compliers, attending secondary school increases performance on the verbal reasoning 
# and vocabulary tests by almost 0.7 standard deviations.

# Finally, we create a plot demonstrating the change in learning outcomes as captured 
# in the variable `rv` as test scores cross the cutoff.

## Part 6: Use `rdplot` to plot `rv` by test score, considering test scores 
## between -1 and 1.

rdplot(module_9_data$rv, module_9_data$test,subset = abs(module_9_data$test) < 1)
rdrobust(module_9_data$rv, module_9_data$test, p = 4, h =0.990, subset = abs(module_9_data$test) < 1
         , kernel = "Uniform")

# The graph again shows the reasoning and vocabulary scores increase as we 
# cross the cutoff at 0. 


## End Module 9 ##

# rm(list = ls()) ## <- This code clear all the environment