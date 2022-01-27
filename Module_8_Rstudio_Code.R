
## Module 8 R Tutorial
## Difference in difference

# The data we use is from a study by Jingbo Cui, Junjie Zhang, and Yang Zheng 
# in which they used both a double difference and a triple difference 
# methodology to evaluate the effect of a policy change on firm innovation.  
# Specifically, the study examines the effect of a new emission trading 
# scheme (ETS) in China on the development by firms of low-carbon technologies.  
# The authors use patent application data of publicly-listed Chinese firms 
# between 2003 and 2015 for the study (Cui, Jingbo, Junjie Zhang, and Yang Zheng. 
# 2018. "Carbon Pricing Induces Innovation: Evidence from China's Regional 
# Carbon Market Pilots." AEA Papers and Proceedings, 108 : 453-57.). 
# The data can be downloaded here (https://www.aeaweb.org/doi/10.1257/pandp.20181027.data).

# There are several dta files in the downloaded directory. We will only use 
# "AEA_P&P_table.dta" in this exercise. We will follow the general methodology 
# used in the paper, but with some simplifications to emphasize the concepts 
# presented in Module 8. Hence, results may not always align exactly with those 
# of the study.

# Begin by importing and describing the data to verify that the version you 
# downloaded matches the data used in the tutorial. 

## Part 1. Import the data and show the summary
library(haven)
library(plyr)
library(dplyr)
library(lmtest)
library(sandwich)

AEA_P_P_table <- read_dta("C:/Users/charl/Desktop/Georgetown Ux - Impact Evaluation Methods with Applications in Low- and Middle-Income Countries/4. Stata R Excercises/Excercise 8 (R)/Module 8/Data/Code & Result/AEA_P&P_table.dta")
View(AEA_P_P_table)

summary(AEA_P_P_table)

# Make sure that your data has 18,937 observations and 23 variables. 
# Before proceeding with the exercise, we need to figure out what we want to 
# accomplish. Following the spirit of Jingbo Cui and his co-authors, we aim to 
# determine whether the introduction of the ETS (cap and trade) scheme in China 
# increased innovation. Consistent with the paper, we will use patent applications 
# to measure innovation. Three sources of variation in the implementation of the 
# program allow us to estimate the treatment effect: time, region, and sector. 

# **Time**: The authors collected data on each firm's patent applications before 
# and after the program was implemented. The variable `post` records this and 
# takes on a value of 0 if the observation is before ETS was implemented and 1 
# if it is after. 

# **Region**: The pilot program was implemented in some regions, but not others. 
# The variable `region` records whether the observation is in an area where ETS 
# was implemented. 

# **Industry:** The pilot only covers some industries. The variable `Ind` takes 
# on a value of 0 if the industry is not covered by ETS and 1 if it is covered.

# We can exploit these differences in when, whether, and which firms are covered 
# by the ETS program to construct double-difference and triple-difference estimates 
# of the effect of ETS on innovation. We will first construct a 
# difference-in-difference estimate using `post` and `region`, and then add 
# `Ind` to calculate a triple difference estimate.

# There are two dependent variables that we are interested in examining. 
# First, `logenvrAEW` records the log of the number of patents for low-carbon 
# technologies. Second, `logNon_envrAEW` records the log of the number of 
# patents for non-low-carbon technologies. Begin by calculating the 
# difference-in-difference estimate of the effect of ETS on the number 
# of low-carbon technologies. 

## Part 2: Regress `logenvrAEW` on `post`, `region`, and an interaction between `post` and `region`

model_1 <- lm(AEA_P_P_table$logenvrAEW ~ AEA_P_P_table$post + AEA_P_P_table$region + AEA_P_P_table$post*AEA_P_P_table$region)
model_1_robust <- coeftest(model_1, vcov = vcovHC(model_1, "HC1"))

summary(model_1)
print(model_1_robust)

# We see that the coefficient is about 0.06. Since our dependent variable is 
# the natural logarithm of the number of patents, we can interpret this as 
# indicating that ETS increases the number of patents for low-carbon products 
# by about 6%. This value is statistically significant at the 5% level, but 
# not at the 10% level. 

# When we use OLS, one of the assumptions is that each observation is independent 
# of every other observation. But in this case, we are measuring the same firms 
# multiple times. We can imagine that firms that are innovative tend to have higher 
# than average innovation every year, so they are not statistically independent. 
# To manage that, we used robust regression option, to asses the real std. error
# of the coeficients

# We can imagine that firms with higher assets and revenue may file more patents 
# because they have more resources to devote to research and development. 
# Let's try adding these as control variables. 

## Part 3: Regress `logenvrAEW` on `post`, `region`, and an interaction between
## `post` and `region`. Control for `t_Revenue` and `t_Assets`.

model_2 <- lm(AEA_P_P_table$logenvrAEW ~ AEA_P_P_table$post + AEA_P_P_table$region + AEA_P_P_table$post*AEA_P_P_table$region 
              + AEA_P_P_table$t_Revenue + AEA_P_P_table$t_Assets)
model_2_robust <- coeftest(model_2, vcov = vcovHC(model_2, "HC1"))

summary(model_2)
print(model_2_robust)

# With the addition of the control variables, the difference-in-differences 
# estimate is similar, but the standard errors declined because assets and 
# revenue reduced the amount of noise. Now our estimate of the treatment is 
# significant at the 5% level, suggesting that the implementation of ETS 
# increased the number of environmentally-oriented patents.

# Note: Remember, the coefficient 0.0748521 is the DD impact of the treatment
#       That is, the intervention increased 7.4% the rate of new low carbon patents.

# But perhaps innovation in environmentally-friendly products came at the 
# expense of other forms of innovation. Rerun the model in *Part 3*, 
# but use `logNon_envrAEW` (log count of carbon patents) as the dependent variable. 

## Part 4. Calculate the difference-in-difference estimate of ETS on `logNon_envrAEW`.

model_3 <- lm(AEA_P_P_table$logNon_envrAEW ~  AEA_P_P_table$post + AEA_P_P_table$region + AEA_P_P_table$post*AEA_P_P_table$region 
              + AEA_P_P_table$t_Revenue + AEA_P_P_table$t_Assets)
model_3_robust <- coeftest(model_3, vcov = vcovHC(model_3, "HC1"))

summary(model_3)
print(model_3_robust)

# The coefficient on the interaction term is slightly negative, but very close to 
# zero and not statistically significant (-0.004191 / not significative). Hence, there does not appear to be evidence 
# of a crowd-out of other forms of non-environmental innovation. 

# But what if you are not convinced that the parallel trends assumption is valid? 
# Since there's a third source of difference -- industry -- that we have not 
# yet made use of, we can instead try a triple-difference specification. 

# First, calculate the triple difference estimate of the effect of ETS on 
# patents for low-carbon technology. Continue controlling for assets and revenue.

## Part 5. Calculate the triple difference estimate of the effect of ETS on `logenvrAEW`

model_4 <- lm(AEA_P_P_table$logenvrAEW ~  AEA_P_P_table$post 
              + AEA_P_P_table$region + AEA_P_P_table$post*AEA_P_P_table$region 
              + AEA_P_P_table$Ind + AEA_P_P_table$post*AEA_P_P_table$Ind
              + AEA_P_P_table$region*AEA_P_P_table$Ind
              + AEA_P_P_table$post*AEA_P_P_table$region*AEA_P_P_table$Ind
              + AEA_P_P_table$t_Assets + AEA_P_P_table$t_Revenue)
model_4_robust <- coeftest(model_4, vcov = vcovHC(model_4, "HC1"))

summary(model_4)
print(model_4_robust)

# The triple-difference coefficient that we are interested in is given by 
# `AEA_P_P_table$post:AEA_P_P_table$region:AEA_P_P_table$Ind`. The coefficient 
# is still positive, and now larger, and significant at a 5% significance level.  

# Note: In R there's a difference in the significance of the impact coefficient
# comparing with Stata. Our result found that the coeffcient is significative 
# at 5% of significance, but in Stata, the same regression finds significance
# at 10%. The same coefficient, but less significant.

# Finally, let's test for innovation crowd-out using a triple difference model. 

## Part. 6. Calculate the triple difference estimate of the effect of 
## ETS on `logNon_envrAEW`

model_5 <- lm(AEA_P_P_table$logNon_envrAEW ~  AEA_P_P_table$post 
              + AEA_P_P_table$region + AEA_P_P_table$post*AEA_P_P_table$region 
              + AEA_P_P_table$Ind + AEA_P_P_table$post*AEA_P_P_table$Ind
              + AEA_P_P_table$region*AEA_P_P_table$Ind
              + AEA_P_P_table$post*AEA_P_P_table$region*AEA_P_P_table$Ind
              + AEA_P_P_table$t_Assets + AEA_P_P_table$t_Revenue)
model_5_robust <- coeftest(model_5, vcov = vcovHC(model_5, "HC1"))

summary(model_5)
print(model_5_robust)

# Based on these results, ETS increased non-low-carbon patents (coef: 0.271329, 27%, significant at 1%). So there does 
# not appear to be evidence of crowd-out of other forms of innovation. 
# In short, the results indicate that China's cap and trade 
# program increased innovation. 

## End Module 8 ##

# rm(list = ls()) ## <- This code clear all the environment