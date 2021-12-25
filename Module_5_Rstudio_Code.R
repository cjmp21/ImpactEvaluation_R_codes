## Module 5 - Impact Evaluation: Regressions
## code in R (stata license caducated) // Using Rstudio


## importing data
 library(haven)
 Thornton_HIV_Testing_Data <- read_dta("C:/Users/charl/Desktop/Georgetown Ux - Impact Evaluation Methods with Applications in Low- and Middle-Income Countries/4. Stata Excercises/Excercise 3/Module 5_Stata/Data/Thornton HIV Testing Data.dta")
 View(Thornton_HIV_Testing_Data)

 
 ## Part 1: Getting started
 data<-data.frame(Thornton_HIV_Testing_Data)
 
 ## from the environment we see 4820 obs and 44 variables

 ## now a summary of statistics
  summary(data)
 
 ## Part 2: Regressions
  
##In this section, we will analyze the impact of receiving _any_ monetary incentive on the study participant's decision to learn the results from their HIV test. The variable `tinc` records the total value of the monetary incentive that respondent was offered (in kwacha). We may `tabulate` the variable `tinc` to see what range of incentives were offered. 
## tabulate tinc variable to see what it reffers to (histograma)
  
hist_tinc <-hist(data$tinc)  
View(hist_tinc) ## muestra grafico del histograma
hist_tinc[["breaks"]] ## muestra los rangos de la tabla (valores minimos de los rangos)
hist_tinc[["counts"]] ## muestra la cuenta de observaciones en la tabla
hist_tinc[["density"]] ## muestra las probabilidades que calcula

## Part 3: generate treatment variable (1 if got money in tinc / 0 if doesn´t)
treatment <- ifelse(data$tinc>0,1,ifelse(data$tinc==0,0,ifelse(data$tinc=='NA',0,0)))
treatment[is.na(treatment)] <- 0 ## treatment without nulls

treatment_with_nulls <- ifelse(data$tinc>0,1,ifelse(data$tinc==0,0,ifelse(data$tinc=='NA',0,0)))

## Part 4: Run a regression of `got` on `treatment` (use treatment with nulls variable)
library(MASS, lib.loc = "C:/Program Files/R/R-4.0.5/library")

model_1 <- lm(data$got ~ treatment_with_nulls)
summary(model_1)
confint(object=model_1, parm="treatment_with_nulls", level=0.95)

## interpretation
## the treatment effect is 0.4506, or about 45 percentage points, compared to a 
## control group average, measured by the constant term, of about 34 percent 
## (0.33868).  The treatment effect has a p-value of 0.000, and the 95% confidence 
## interval is from about 41 to 49 percentage points.

## voy por aqui

## Part. 5: Run a robust regression

## in presence of Heterocedastik errors (diferent variance in dependant and independant variables, of in the two samples)
## But if the variances in the treatment and control groups had been different - that is, if the error terms were heteroskedastic - then the formula for the standard error of the difference in means would have been a bit different, and the confidence intervals a bit wider.
## Now remember that a regression of an outcome on treatment group assignment, is simply a way of comparing the mean outcomes in the treatment and control groups.  And if we are concerned that the distributions of outcomes in the two groups might have different variances, then we'll have to adjust the standard errors.
## This intuition carries over to any regression, not just one involving two values of the independent variable, and adding the robust option tells Stata to calculate standard errors that account for the heteroskedastic properties of the data.  The robust option does NOT change the estimate of any parameter in a regression, just the standard errors and confidence intervals.
## Now run a regression of `got` on `treatment` with the `robust` option.

library(zoo)
library(foreign)
library(sandwich)
library(lmtest)

model_1_robust <- coeftest(model_1, vcov = vcovHC(model_1, "HC1"))
coeftest(model_1, vcov = vcovHC(model_1, "HC1"))
summary(model_1) ## approximately same coefficients | Part 4 = Part 5 in coefficients

## The coefficients on `treatment` and the constant term are exactly the same as in Task 5. 
## But observe that both of the standard errors are larger. 

## Part 6. un a regression of `got` on `treatment` without robust standard errors, then execute the command `estat hettest`
## To test the asumption that standard errors are larger in robust regression (taking account of the Heteroskedasticity)

library(lmtest)

bptest(model_1)

# this command  tests the null hypothesis that errors are homoskedastic against 
# the alternative hypothesis that the variance in standard errors either 
# increases or decreases with one or more of the dependent variables. 
# The very low p-value (`Prob > chi`) indicates that we can safely reject the 
# null hypothsis that standard errors are homoskedastic. 

# So, in the model_1 the errors are not homoskedastic, for that reason is better to
# use robust regression to evaluate the Std. Error of the coefficients.
# Be carefull, robust regression doesn't capture all possible heteroskedasticity possible problems.
# The condition that you need to be aware off i that errors should be uniformly distributed

# Part 7. Calculate the mean of `got` among those that were treated and were not treated.
summary(data$got)
summary(data$got[treatment_with_nulls == 0], basic = T)
summary(data$got[treatment_with_nulls == 1], basic = T)

# compare with intercept and treatment_with_nulls coefficients in summary(model_1)
# Note that the average in the control group in this table is indeed the same as the constant term 
# in the earlier regression.  Also, the sum of the constant term and the treatment effect in the 
# regression table is equal to the average in the treatment group.

## Part. 8 Add the control variable `age` to the regression of `got` on `treatment`

model_2 <- lm(data$got ~ treatment_with_nulls + data$age)
model_2_robust <- coeftest(model_2, vcov = vcovHC(model_2, "HC1"))
coeftest(model_2, vcov = vcovHC(model_2, "HC1"))
summary(model_2) ## approximately same coefficients

# The coefficients doesn't change so much, and also the control variable has a very small effect on the output.
# Another remark is that, the "age" coefficient is significant (different from 0) at 5% of error, but not at 1%.

## Part. 9 Regress got on treatment, male, and an interaction between treatment and male.

model_3 <- lm(data$got ~ treatment_with_nulls + data$male + treatment_with_nulls*data$male)
model_3_robust <- coeftest(model_3, vcov = vcovHC(model_3, "HC1"))
coeftest(model_3, vcov = vcovHC(model_3, "HC1"))
summary(model_3) ##  same coefficients

# Coefficients male and interaction treatment and male are not significant at any level of significantly.
# We can't conclude that the effect is different from man and woman.

## Additional:  Regress `got` on `treatment`, `age`, and an interaction between `treatment` and `age`.
model_4 <- lm(data$got ~ treatment_with_nulls + data$age + treatment_with_nulls*data$age)
model_4_robust <- coeftest(model_4, vcov = vcovHC(model_4, "HC1"))
coeftest(model_4, vcov = vcovHC(model_4, "HC1"))
summary(model_4) ##  same coefficients

# looking at the results, there's no different impact from the coefficient of interaction of age (is statistically equal to zero)
# Also, age is not significant with significance levels of 5% and 1%.
#  we cannot conclude that age impacts the effectiveness of the intervention

## End Module 5 ##

## Note: there's some minimal differences between Stata and R commands
## but those are in some cases and at a 4th level of decimals.


