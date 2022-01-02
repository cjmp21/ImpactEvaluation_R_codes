## Module 6 R Tutorial
## Imperfect Compliance and Attrition

## Part 1: Import `Thornton HIV Testing AER.dta` and generate treatment variable
library(haven)
library(plyr)
library(dplyr)

Thornton_HIV_Testing_Data <- read_dta("C:/Users/charl/Desktop/Georgetown Ux - Impact Evaluation Methods with Applications in Low- and Middle-Income Countries/4. Stata R Excercises/Excercise 5 (R)/Module 5_Stata/Data/Thornton HIV Testing Data.dta")
View(Thornton_HIV_Testing_Data)

data<-data.frame(Thornton_HIV_Testing_Data)

# treatment with nulls
treatment_with_nulls <- ifelse(data$tinc>0,1,ifelse(data$tinc==0,0,ifelse(data$tinc=='NA',0,0)))
data$treatment_with_nulls <- treatment_with_nulls

# treatment without nulls
treatment <- ifelse(data$tinc>0,1,ifelse(data$tinc==0,0,ifelse(data$tinc=='NA',0,0)))
treatment[is.na(treatment)] <- 0 ## treatment without nulls
data$treatment <- treatment

rm(treatment,treatment_with_nulls)

## Part 2: Calculate the shares of people in the treatment and control groups who learned their HIV status.

## The objective of this study is to evaluate how learning one's HIV status changes an individual's 
##sexual behavior.  Being "treated" means an individual knows her or his HIV status; for those assigned 
## to the treatment group, a monetary incentive is used to encourage them to obtain this information.

## First, lets calculate the compliance rate for the experiment, which we defined in module 6 as 
## the difference between the share of members of the treatment group who know their HIV status, 
## and the share in the control group who know it.  The variable that indicates whether a respondent 
## learned the results of her/his HIV test is `got`. 

followed_treatment <- ifelse(data$treatment_with_nulls==1,data$got,1-data$got)

treatment_with_nulls <-data$treatment_with_nulls

table1 <-as.data.frame(table(followed_treatment, treatment_with_nulls)) 

follow_treatment_wo_treatment <-as.data.frame(table1[table1[,2] == 0,])
follow_treatment_wo_treatment$Freq <- as.numeric(follow_treatment_wo_treatment$Freq)

follow_treatment_wo_treatment$Percent<-follow_treatment_wo_treatment$Freq/sum(follow_treatment_wo_treatment$Freq)
follow_treatment_wo_treatment$Cum<-cumsum(follow_treatment_wo_treatment$Percent)
total_follow_treatment_wo_t <-sum(follow_treatment_wo_treatment$Freq)
 

follow_treatment_w_treatment <-as.data.frame(table1[table1[,2] == 1,])
follow_treatment_w_treatment$Freq <- as.numeric(follow_treatment_w_treatment$Freq)

follow_treatment_w_treatment <-as.data.frame(table1[table1[,2] == 1,])
follow_treatment_w_treatment$Freq <- as.numeric(follow_treatment_w_treatment$Freq)

follow_treatment_w_treatment$Percent<-follow_treatment_w_treatment$Freq/sum(follow_treatment_w_treatment$Freq)
follow_treatment_w_treatment$Cum<-cumsum(follow_treatment_w_treatment$Percent)
total_follow_treatment_w_t <-sum(follow_treatment_w_treatment$Freq)

follow_treatment_wo_treatment
total_follow_treatment_wo_t

follow_treatment_w_treatment
total_follow_treatment_w_t

## 66.13% of individuals who doesn't had the treatment, followed their results (HIV status). 22.86% didn't.
## Also, 78.92% of the indiduals who had the treatment, followed their results (1745)

## Note: I have 60 nulls comparing with Stata run. Those individuals are in the treatment group 
## (treatment_with_nulls =1) and followed up the treatment (followed_treatment =1)

## One of the outcomes that Thornton considers is the effect of learning one's HIV status on the decision 
## by sexually active, HIV-positive individuals' to purchase.  To address this question, we will need to 
## restrict the sample to sexually active and HIV-positive respondents.
## So to start, lets calculate the compliance rate among this restricted sample, which could be 
## different to the compliance rate for the population at large.


## Part 3: Calculate the compliance rate among the restricted sample. 
## In addition, perform a regression of `got` on `treatment` using the restricted sample. 
## Use the variable `hadsex12` to measure sexual activity and `hiv2004` to determine if the 
## respondent in HIV-positive.

## subsetting the data (1) 

data$row_to_keep_1<-ifelse(data$hadsex12==1 & data$hiv2004==1,1,0)

data$row_to_keep_1[is.na(data$row_to_keep_1)] <- 0

data_2<-data[data$row_to_keep_1==1,c('treatment_with_nulls', 'got', 'hadsex12', 'hiv2004', 'row_to_keep_1','followupsurvey')]

data_2$followed_treatment <- ifelse(data_2$treatment_with_nulls==1,data_2$got,1-data_2$got)

followed_treatment_2 <- data_2$followed_treatment
treatment_with_nulls_2<- data_2$treatment_with_nulls

table2 <-as.data.frame(table( followed_treatment_2, treatment_with_nulls_2)) 

follow_treatment_wo_treatment_2 <-as.data.frame(table2[table2[,2] == 0,])
follow_treatment_wo_treatment_2$Freq <- as.numeric(follow_treatment_wo_treatment_2$Freq)

follow_treatment_wo_treatment_2$Percent<-follow_treatment_wo_treatment_2$Freq/sum(follow_treatment_wo_treatment_2$Freq)
follow_treatment_wo_treatment_2$Cum<-cumsum(follow_treatment_wo_treatment_2$Percent)
total_follow_treatment_wo_t_2 <-sum(follow_treatment_wo_treatment_2$Freq)


follow_treatment_w_treatment_2 <-as.data.frame(table2[table2[,2] == 1,])
follow_treatment_w_treatment_2$Freq <- as.numeric(follow_treatment_w_treatment_2$Freq)

follow_treatment_w_treatment_2 <-as.data.frame(table2[table2[,2] == 1,])
follow_treatment_w_treatment_2$Freq <- as.numeric(follow_treatment_w_treatment_2$Freq)

follow_treatment_w_treatment_2$Percent<-follow_treatment_w_treatment_2$Freq/sum(follow_treatment_w_treatment_2$Freq)
follow_treatment_w_treatment_2$Cum<-cumsum(follow_treatment_w_treatment_2$Percent)
total_follow_treatment_w_t_2 <-sum(follow_treatment_w_treatment_2$Freq)

## result 1 / tables of subsample
follow_treatment_wo_treatment_2 ## subsample sexually active and with HIV that didn't had treatment
total_follow_treatment_wo_t_2

follow_treatment_w_treatment_2 ## subsample sexually active and with HIV that had treatment
total_follow_treatment_w_t_2   ## here I loss 4 points that change to null, comparing with Stata

library(MASS)
library(sandwich)
library(lmtest)

model_1 <- lm(data_2$got ~ data_2$treatment_with_nulls)
model_1_robust <- coeftest(model_1, vcov = vcovHC(model_1, "HC1"))

# result 2 / robust model of subsample
summary(model_1) 
coeftest(model_1, vcov = vcovHC(model_1, "HC1"))

# The sample of respondents in the study who were HIV positive in 2004 and who reported that they were 
# sexually active amounted to just 1 individuals, 24 in the control group and 94 in the treatment 
# group (I loss 4 comparing to Stata sample).  In this smaller sample, the share of treated individuals 
# in the control group is 29.17%, and the share of treated individuals in the treatment group 
#  is 71.28%.

# The compliance rate, amongst the group of sexually active HIV-positive respondents, is thus 
# 71.28 - 29.17 = 42.11%.  This value is of course the same as the coefficient on treatment 
# in the regression (0.4211).  The standard error on the coefficient (0.1047) tells us that the 
# effect of the encouragement on HIV knowledge was highly statistically significant.

##  Intent to Treat Effect and Treatment Effect on the Treated

## Part 4:  Calculate the intent to treat effect of learning that one is HIV positive on the decision 
## to purchase any condoms, restricting the sample to sexually active individuals. Use robust 
## standard errors.

data_3<-data[data$row_to_keep_1==1,c('treatment_with_nulls', 'got', 'hadsex12', 'hiv2004', 'row_to_keep_1', 'anycond')]

data_3$followed_treatment <- ifelse(data_2$treatment_with_nulls==1,data_2$got,1-data_2$got)

model_2 <- lm(data_3$anycond ~ data_3$treatment_with_nulls)
model_2_robust <- coeftest(model_2, vcov = vcovHC(model_2, "HC1"))
summary(model_2) 
coeftest(model_2, vcov = vcovHC(model_2, "HC1"))

# Note: You may notice that there are only 52 observations here, but there were 118 in the prior task. 
# The difference in sample size is due to 66 missing observations in the variable `anycond`. 

# Based on the regression estimates, 20 percent of sexually active and HIV-positive individuals who 
# were not offered a financial incentive to learn their HIV status nonetheless purchased condoms (intercept), 
# while those who were offered the financial incentive were about 23 percentage points
# more likely to purchase condoms. (b1) This suggests that being given a financial incentive to 
# learn one's HIV status increases the willingness to purchase condoms. However, the estimate is not 
# statistically significant, with a p-value of 0.136. The intercept is also statistically insignificant.

# Next, we will calculate the Local Average Treatment Effect (LATE) estimate of learning that 
# one is HIV positive on the decision to purchase condoms. We will first do this manually by using 
# the results from the regression of `anycond` on `treatment` and `got` on `treatment`. 
# We will then use the two-stage least squares (2SLS) method.


## Part 5: Use the results of the regression of `anycond` on `treatment` and `got` on `treatment` 
## to calculate the TOT effect. 

data_3$row_to_keep_2<-ifelse(data_3$anycond>=0,1,0)

data_3$row_to_keep_2[is.na(data_3$row_to_keep_2)] <- 0

data_4<-data_3[data_3$row_to_keep_2==1,] ## remember: you have to create a new object to generate the new data filtered.

model_3 <- lm(data_4$got ~ data_4$treatment_with_nulls)
model_3_robust <- coeftest(model_3, vcov = vcovHC(model_3, "HC1"))
summary(model_3) 
coeftest(model_3, vcov = vcovHC(model_3, "HC1"))

TOT<-data.frame(TOT=model_2$coefficients[2]/model_3$coefficients[2])
TOT$TOT

# Using the fact that the LATE effect is equal to the ITT effect divided by the 
# compliance rate (i.e., the difference in the probably of being treated between
# the treatment group and control group), we estimate that amongst sexually active 
# HIV-positive respondents, learning one's HIV status increases the likelihood of 
# purchasing condoms by over 50 percentage points.

# However, we did not calculate a standard error, so we don't know if this value 
# is statistically significant or not. For that, we will need to use the 2SLS 
# method to calculate the LATE effect.

#  A note on instrumental variable regression: 
# The 2SLS method is a form of _instrumental variable regression_. We will not 
# go into the details of these regressions in this course. However, it is useful 
# to learn some terminology to help navigate the 2SLS Stata command. 
# The variable indicating treatment assignment is called an _instrumental 
# variable_ or _instrument_. The variable indicating if one was actually treated, 
# in this case `got`, is considered an _endogenous variable_. 

## Part 6: Use the `ivregress` command to calculate the LATE effect.

install.packages("ivreg", dependencies = TRUE)
library("ivreg")

two_SLS_model_1 <- ivreg(data_4$anycond ~ data_4$got | data_4$treatment_with_nulls) ## instrumental variable after | symbol
summary(two_SLS_model_1)
coeftest(two_SLS_model_1, vcov = vcovHC(two_SLS_model_1, "HC1")) ##remember: robust standard errors

# Note that the coefficient on `got` is the same as in Task 5 (other than a 
# slight difference created by rounding / remember TOT calculated). However, 
# the results from the 2SLS estimate include a standard error, yielding a 
# p-value of 0.048. Hence, we can conclude that learning that one is HIV 
# positive makes an individual more likely to purchase condoms by a 
# statistically significant margin.

## Atrittion

# Next, we will consider attrition. Since our analysis has only focused on 
# HIV-positive and sexually active individuals, we will consider attrition 
# within this subset of the full sample.

# Before we get into analysis, it is useful to examine the number of people 
# that attrited. Create a table illustrating the overall rate of attrition.

## Part. 7: Tabulate the number of people that attrited.

# use data_2 --> sexually active and HIV-positive

attrited <- 1-data_2$followupsurvey
data_2$attrited <- attrited

rm(attrited)

table_attrited <- as.data.frame(table(data_2$attrited))
table_attrited$Freq <- as.numeric(table_attrited$Freq)

table_attrited$Percent <- table_attrited$Freq/sum(table_attrited$Freq)
table_attrited$Cum <- cumsum(table_attrited$Percent)

total_attrited_not_attrited <- sum(table_attrited$Freq)

table_attrited$Var1 <- ifelse(table_attrited$Var1==1,"Attrited","Did not Attrite")
colnames(table_attrited) <- c("Respondent Attrited","Freq","Percent", "Cum")

table_attrited
total_attrited_not_attrited

# Across the sample of sexually active HIV-positive respondents, 59 percent completed 
# the endline survey, and 41 percent did not - they "attrited".  Such attrition 
# reduces our statistical power, but we really want to know whether it is likely 
# to introduce bias in our estimates of treatment effects.

# Next, we want to test if attrition is random or not. One easy test that we can 
# perform is to regress an indicator for completing the endline survey on 
# treatment - are members of the treatment group more or less likely than 
# members of the control group to complete the survey?
  
## Part 8: Regress `followupsurvey` on `treatment`

data_2$row_to_keep_5 <- ifelse(data_2$attrited == 1 | data_2$attrited == 0, 1,0)
data_2$row_to_keep_5[is.na(data_2$row_to_keep_5)] <-0

data_5 <- data_2[data_2$row_to_keep_5==1, c("followupsurvey","treatment_with_nulls")]

model_4 <- lm (data_5$followupsurvey ~ data_5$treatment_with_nulls)
model_4_robust <- coeftest(model_4, vcov = vcovHC(model_4, "HC1"))

summary(model_4)
model_4_robust

# note: we can see that, as the b1 coefficient is = 0 (fail to reject null hypothesis)
#       then, there is no difference of being in the treatment or control group in 
#       order to fulfill the last survey, i.e., the effect of attrition is random.

# The coefficient on `treatment` is not different from 0 by a statistically significant 
# margin which is a good sign. However, this test does not rule out the possibility 
# of non-random attrition. 

# Another useful test that we can run is to test whether the means of baseline 
# demographic variables are equal among the sample of respondents that completed 
# the follow-up survey. This test is not perfect because even if the variables 
# that we have are similar between the groups, there may be differences in
# characteristics that we cannot observe. In addition, there may be some 
# statistically significant differences even if attrition is random. Nonetheless, 
# the test can still provide useful insight into whether or not there is non-random attrition. 

# This test could easily be done manually by running a regression of each of the 
# variables on treatment, restricting the sample to those who completed the endline survey. 
# But a convenient feature of Stata is that other users have written packages that 
# make certain tasks easier. We will use a package called `orth_out` for this 
# exercise. Before running your .do file, type `ssc install orth_out` to 
# install the package. The package is named `orth_out` because the table that it produces 
# is often called an "orthogonality table." You can then use it like any other Stata command. 

## Part. 9: Restricting the sample to individuals that completed the follow-up survey, 
##          test whether the mean of `age`, `male`, `mar`, `educ2004`, `hadsex12`,
##          `tb`, `land2004`, and `usecondom04`. Use the package `orth_out` for this task.



temporal<-data[data$row_to_keep_1==1,c(  'anycond','hiv2004', 'row_to_keep_1', 'followupsurvey', 'treatment_with_nulls', 'age', 'male', 'mar', 'educ2004', 'hadsex12', 'tb', 'land2004', 'usecondom04')]

temporal$attrited <- 1-temporal$followupsurvey

temporal$row_to_keep_2<-ifelse(temporal$anycond>=0,1,0) 

temporal$row_to_keep_2[is.na(temporal$row_to_keep_2)] <- 0

temporal<-temporal[temporal$row_to_keep_2==1,] 

temporal$row_to_keep_5 <- ifelse(temporal$attrited == 1 | temporal$attrited == 0, 1,0)
temporal$row_to_keep_5[is.na(temporal$row_to_keep_5)] <-0

data_6 <- temporal[temporal$row_to_keep_5==1, c("attrited","followupsurvey","treatment_with_nulls","age", "male", "mar", "educ2004", "hadsex12", "tb", "land2004", "usecondom04")]

rm(temporal)

library(dplyr)

treatment_cond <- group_by(data_6, treatment_with_nulls)

cond_mean <- data_6 %>% 
  group_by(treatment_with_nulls) %>% 
  summarise(across(everything(), list(mean,sd)), n=n())


install.packages("plotrix")
library(plotrix)
library(dplyr)

cond_se <- data_6 %>%
  group_by(treatment_with_nulls) %>% 
  summarise_each(funs(se=std.error), )

# Next, I created a proccess to emulate the table Stata print with the orth_out command

tab_age <- cond_mean$age_1
age_se <-cond_se$age_se
age <- data.frame(tab_age, age_se)
age <- data.frame(t(age))
age_pval <-data.frame(summary(lm(data_6$treatment_with_nulls ~ data_6$age))$coefficients[2,4],0)
age_pval<-data.frame(t(age_pval))
age$p_val <-age_pval
colnames(age) <- c("Control","Treatment","pvalue")

rm( age_pval, tab_age, age_se)


tab_male <- cond_mean$male_1
male_se <-cond_se$male_se
male <- data.frame(tab_male, male_se)
male <- data.frame(t(male))
male_pval <-data.frame(summary(lm(data_6$treatment_with_nulls ~ data_6$male))$coefficients[2,4],0)
male_pval<-data.frame(t(male_pval))
male$p_val <-male_pval
colnames(male) <- c("Control","Treatment","pvalue")

rm( male_pval, tab_male, male_se)


tab_mar <- cond_mean$mar_1
mar_se <-cond_se$mar_se
mar <- data.frame(tab_mar, mar_se)
mar <- data.frame(t(mar))
mar_pval <-data.frame(summary(lm(data_6$treatment_with_nulls ~ data_6$mar))$coefficients[2,4],0)
mar_pval<-data.frame(t(mar_pval))
mar$p_val <-mar_pval
colnames(mar) <- c("Control","Treatment","pvalue")

rm( mar_pval, tab_mar, mar_se)

tab_edu <- cond_mean$educ2004_1
edu_se <-cond_se$educ2004_se
edu <- data.frame(tab_edu, edu_se)
edu <- data.frame(t(edu))
edu_pval <-data.frame(summary(lm(data_6$treatment_with_nulls ~ data_6$educ2004))$coefficients[2,4],0)
edu_pval<-data.frame(t(edu_pval))
edu$p_val <-edu_pval
colnames(edu) <- c("Control","Treatment","pvalue")

rm( edu_pval, tab_edu, edu_se)

tab_sex <- cond_mean$hadsex12_1
sex_se <-cond_se$hadsex12_se
sex <- data.frame(tab_sex, sex_se)
sex <- data.frame(t(sex))
sex_pval <-data.frame(".",0) ## the value of the pval is null
sex_pval<-data.frame(t(sex_pval))
sex$p_val <-sex_pval
colnames(sex) <- c("Control","Treatment","pvalue")

rm( sex_pval, tab_sex, Sex_se)

tab_hiv <- cond_mean$tb_1
hiv_se <-cond_se$tb_se
hiv <- data.frame(tab_hiv, hiv_se)
hiv <- data.frame(t(hiv))
hiv_pval <-data.frame(summary(lm(data_6$treatment_with_nulls ~ data_6$tb))$coefficients[2,4],0)
hiv_pval<-data.frame(t(hiv_pval))
hiv$p_val <-hiv_pval
colnames(hiv) <- c("Control","Treatment","pvalue")

rm( hiv_pval, tab_hiv, hiv_se)

tab_lan <- cond_mean$land2004_1
lan_se <-cond_se$land2004_se
lan <- data.frame(tab_lan, lan_se)
lan <- data.frame(t(lan))
lan_pval <-data.frame(summary(lm(data_6$treatment_with_nulls ~ data_6$land2004))$coefficients[2,4],0)
lan_pval<-data.frame(t(lan_pval))
lan$p_val <-lan_pval
colnames(lan) <- c("Control","Treatment","pvalue")

rm( lan_pval, tab_lan, lan_se)

tab_use <- cond_mean$usecondom04_1
use_se <-cond_se$usecondom04_se
use <- data.frame(tab_use, use_se)
use <- data.frame(t(use))
use_pval <-data.frame(summary(lm(data_6$treatment_with_nulls ~ data_6$usecondom04))$coefficients[2,4],0)
use_pval<-data.frame(t(use_pval))
use$p_val <-use_pval
colnames(use) <- c("Control","Treatment","pvalue")

final_table <- union_all(age,male)
final_table <- union_all(final_table,mar)
final_table <- union_all(final_table,edu)
final_table <- union_all(final_table,sex)
final_table <- union_all(final_table,hiv)
final_table <- union_all(final_table,lan)
final_table <- union_all(final_table,use)
View(final_table)

rm(age,male,mar,edu,sex,hiv,lan,use,cond_mean,cond_se)

# There are statistically significant differences (at the 10% level) in gender (male, 0.06230313 pval) 
# and the percent of people that received an HIV test before the baseline. 
# While this test does not necessarily prove that there is non-random attrition, 
# it does provide some cause for concern. Hence, we may want to perform tests 
# to determine the potential effect of attrition on our results. 

# We will next calculate Manski and Lee bounds to estimate the effect that 
# attrition might have had on our earlier results. Start by calculating the 
# upper and lower Manski bounds.

## Part 10: Calculate the upper and lower Manski bounds of the ITT effect 
## of learning that one is HIV positive on the decision to purchase condoms. 

data_prov<-data[data$row_to_keep_1==1,c('treatment_with_nulls', 'got', 'hadsex12', 'hiv2004', 'row_to_keep_1','followupsurvey','anycond')]

data_prov$attrited <- 1-data_prov$followupsurvey

data_prov$row_to_keep_5 <- ifelse(data_prov$attrited == 1 | data_prov$attrited == 0, 1,0)
data_prov$row_to_keep_5[is.na(data_2$row_to_keep_5)] <-0

data_7 <- data_prov[data_prov$row_to_keep_5==1, c("followupsurvey","treatment_with_nulls","anycond")]

rm(data_prov)

# Generating minimum and maximum values of the outcome
hm_upperbound <- 1
hm_lowerbound <- 0

# To create upper bounds: replace missing outcome values of 
# treatment group with the highest observed outcome and missing 
# outcome values of control group with lowest observed outcome values

data_7$anycond_ubounds <- ifelse(data_7$treatment_with_nulls==1, 
                                 ifelse ( is.na(data_7$anycond),1,data_7$anycond),data_7$anycond)

data_7$anycond_ubounds <- ifelse(data_7$treatment_with_nulls==0, 
                                 ifelse ( is.na(data_7$anycond),0,data_7$anycond_ubounds),data_7$anycond_ubounds)

# To create lower bounds: replace missing outcome values of 
# treatment group with lowest observed and replace missing control 
# outcome values with highest observed outcome

data_7$anycond_lbounds <- ifelse(data_7$treatment_with_nulls==1, 
                                 ifelse ( is.na(data_7$anycond),0,data_7$anycond),data_7$anycond)

data_7$anycond_lbounds <- ifelse(data_7$treatment_with_nulls==0, 
                                 ifelse ( is.na(data_7$anycond),1,data_7$anycond_lbounds),data_7$anycond_lbounds)

library(MASS)
library(sandwich)
library(lmtest)

model_5 <- lm(data_7$anycond_ubounds ~ data_7$treatment_with_nulls)
model_5_robust <- coeftest(model_5, vcov = vcovHC(model_5, "HC1"))
summary(model_5)                                 ## indicators of the model from here 
coeftest(model_5, vcov = vcovHC(model_5, "HC1")) ## coef and robust pvalues from here (upper bounds)

model_6 <- lm(data_7$anycond_lbounds ~ data_7$treatment_with_nulls)
model_6_robust <- coeftest(model_6, vcov = vcovHC(model_6, "HC1"))
summary(model_6)                                 ## indicators of the model from here 
coeftest(model_6, vcov = vcovHC(model_6, "HC1")) ## coef and robust pvalues from here (lower bounds)

# Hence, the upper Manski bound is .546032 and the lower Manski bound is -.29841. Both values are 
# different from 0 by a statistically significant margin (at least 5% of significance), but in 
# different directions.

# If we believe attrition is random, then our original point estimate of the ITT, about 0.229, 
# is our best guess of the effect, although it is not statistically significantly different 
# from zero.  This value lies between the upper and lower Manski bounds.

# But if we are concerned that attrition might be biased one way or the other - that is, 
# attrition is correlated, positively or negatively, with the potential impact of the 
# treatment - then our best guess could be as high as 0.546, or as low as -0.298.  
# Note that both of these estimates are statistically significant, but they are based on 
# extreme assumptions about the missing data.

# Next calculate the upper and lower Lee bounds. This would be tricky to do manually. 

## Part 11: Calculate upper and lower Lee bounds of the ITT effect of learning 
## that one is HIV positive on their decision to purchase condoms. 

install.packages("remotes")
remotes::install_github("vsemenova/leebounds")

install.packages("devtools")
library(devtools) 
install_github("vsemenova/leebounds")
library(leebounds)

data_8 <- as.data.frame(data_7[c("treatment_with_nulls","followupsurvey","anycond")])
colnames(data_8) <- c("d","s","sy")

data_8 <- na.exclude(data_8)

lee_bounds <- leebounds(data_8)

View(lee_bounds)

# So the lower Lee bound is about 21 percentage points, and the upper Lee bound 
# is about 24 percentage points.  These bounds provide a much narrower range of 
# possible effect sizes, a range that again includes the original point estimate 
# of 0.229.  However, like the point estimate, neither of the Lee bounds is 
# statistically significantly different from zero.

## Note: I don't know if the leebounds retrive 0 because of the no significancy of the 
## lee bound estimates, but the R code retreive NA. 
## further evaluation of this part must see the code in the following Url
## https://rdrr.io/github/vsemenova/leebounds/src/R/leebounds.R

## End Module 6 ##

# rm(list = ls()) ## <- This code clear all the environment