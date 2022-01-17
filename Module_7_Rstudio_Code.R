## Module 7 R Tutorial
## Power Calculations

# The exercise covers power calculations for individually randomized and cluster 
# randomized experiments. Unlike in previous Stata tutorials, no data accompanies 
# this document. 

## Individual level randomization 

# First, suppose that you wish to calculate the sample size necessary for a 
# randomized controlled trial. You plan to assign an equal number of people 
# to the treatment and control groups. Assuming a test size of 0.05, calculate 
# the total sample size required to detect a 0.1 standard deviation effect size 
# with 80% power.

## Part 1: Calculate the sample size required to detect a 0.1 standard deviation 
## treatment effect 80% of the time with equal sized treatment and control 
## groups. Set the probability of a Type I error at 5%.

install.packages("pwr")
library(pwr)

test_1 <- pwr.t.test(n = NULL, d = 0.1, sig.level =0.05, power = 0.8
           , type = "two.sample", alternative = c("two.sided", "less", "greater"))
print(test_1)

# Note: look that with a delta (effect size, standard deviation) of 0.1,
# also, 0.8 power percentage, and a significance level of 0.05 (5%),
# for a two sample equally distributed study, we need 1570.7 -> aprox. 1571
# observations (n)

# Maintaining a 0.05 test size, what would the sample sizes need to be in 
# order to detect at least a 0.01, 0.025, 0.05, 0.1, and 0.2 standard deviation 
# effect with 80% power?

## Part 2: Create a table of the sample sizes needed to detect 
## a 0.01, 0.025, 0.05, 0.1, and 0.2 standard deviation effect given 
## the assumptions in *Part 1*.

d_2 <- data.frame(c(0.01, 0.025, 0.05, 0.1, 0.2))
colnames(d_2) <- "d_2"

test_2 <- pwr.t.test(n = NULL, d = d_2[1,1], sig.level =0.05, power = 0.8
           , type = "two.sample", alternative = c("two.sided", "less", "greater"))
test_3 <- pwr.t.test(n = NULL, d = d_2[2,1], sig.level =0.05, power = 0.8
                     , type = "two.sample", alternative = c("two.sided", "less", "greater"))
test_4 <- pwr.t.test(n = NULL, d = d_2[3,1], sig.level =0.05, power = 0.8
                     , type = "two.sample", alternative = c("two.sided", "less", "greater"))
test_5 <- pwr.t.test(n = NULL, d = d_2[5,1], sig.level =0.05, power = 0.8
                     , type = "two.sample", alternative = c("two.sided", "less", "greater"))

test_2_vf <- data.frame(test_2$sig.level, test_2$power, test_2$n*2, test_2$n, test_2$n
, test_2$d, "0", test_2$d, "1")

colnames(test_2_vf) <- c("alpha", "power", "Sample size", "N1", "N2", "delta"
                         , "m1", "m2", "Std. Dev.")

test_3_vf <- data.frame(test_3$sig.level, test_3$power, test_3$n*2, test_3$n, test_3$n
                              , test_3$d, "0", test_3$d, "1")

test_4_vf <- data.frame(test_4$sig.level, test_4$power, test_4$n*2, test_4$n, test_4$n
                              , test_4$d, "0", test_4$d, "1")

test_5_vf <- data.frame(test_1$sig.level, test_1$power, test_1$n*2, test_1$n, test_1$n
                              , test_1$d, "0", test_1$d, "1")

test_6_vf <- data.frame(test_5$sig.level, test_5$power, test_5$n*2, test_5$n, test_5$n
                              , test_5$d, "0", test_5$d, "1")

colnames(test_3_vf) <- c("alpha", "power", "Sample size", "N1", "N2", "delta"
                         , "m1", "m2", "Std. Dev.")
colnames(test_4_vf) <- c("alpha", "power", "Sample size", "N1", "N2", "delta"
                         , "m1", "m2", "Std. Dev.")
colnames(test_5_vf) <- c("alpha", "power", "Sample size", "N1", "N2", "delta"
                         , "m1", "m2", "Std. Dev.")
colnames(test_6_vf) <- c("alpha", "power", "Sample size", "N1", "N2", "delta"
                         , "m1", "m2", "Std. Dev.")

test_2_vf <- rbind(test_2_vf, test_3_vf, test_4_vf, test_5_vf, test_6_vf)

rm(test_2, test_3, test_3_vf, test_4, test_4_vf, test_5, test_5_vf, test_6_vf)

print(test_2_vf)

# As expected, we can see that the required sample size increases non-linearly 
# as the effect size approaches 0. 

# The table is useful, but the calculations might be easier to interpret 
# in a graph. Plot 0.01, 0.02, ..., 0.1 standard deviation treatment 
# effects against sample size. 

## Part 3: Plot 0.01, 0.02, ..., 0.1 standard deviation treatment effects 
## against sample size using the assumptions in *Part 1*. 

lo <- loess(test_2_vf$`Sample size`~test_2_vf$delta )

plot (test_2_vf$delta, test_2_vf$`Sample size`, type = "b", col = "blue"
      , xlab = 'delta', ylab = 'Sample Size', lwd = 3 )
xl <- seq(min(test_2_vf$delta),max(test_2_vf$delta), (max(test_2_vf$delta) - min(test_2_vf$delta))/1000)
lines(xl, predict(lo,xl), col='red', lwd=1)

# Now suppose that you know from a pilot study that the control mean of 
# the outcome variable is 23.7, the experimental mean is 20.3, and the 
# standard deviation is 17. What sample size is required to detect a 
# program effect with 80% power and 95% confidence?

mean_h1 <- 23.7
mean_h0 <- 20.3
sd_h1 <- 17
sd_h0 <- 17  # assuming the same standard deviation

efect_size <- (mean_h1-mean_h0)/sqrt(((sd_h1^2)+(sd_h0^2))/2)

test_3 <- pwr.t.test(n = NULL, d = efect_size, sig.level =0.05, power = 0.8
                     , type = "two.sample", alternative = c("two.sided", "less", "greater"))
print(test_3) # n for each group of inividuals 

TotalN_test_3 <- test_3$n*2
print(TotalN_test_3) # N for the whole experiment = n*2 = 787

# Note: So you would need a sample size of at least 787 individuals. 

# Perhaps instead the director of a program tells you that they will permit 
# you to randomly assign 400 people to receive an intervention out of a total 
# sample of 800. In standard deviations, what is the smallest effect size 
# that you could detect with 80% power and 95% confidence? 

## Part. 5: Calculate the smallest effect size, in standard deviations, that 
## you could calculate with a total sample size of 800.
N <- 800
n <- N/2

test_4 <- pwr.t.test(n = n, d = NULL, sig.level = 0.05, power = 0.8
                     , type = "two.sample", alternative = c("two.sided", "less", "greater"))
print(test_4) # n for each group of inividuals

# So you would be able to detect an effect size just a little under 0.2 
# standard deviations or larger.

## Part 6: Randomized clusters 

# In this section, we perform power calculations in the case of cluster 
# randomized controlled trials. The commands are largely the same as in the 
# case of individual randomization, although several additional parameters must 
# be added. As a result, this section is much shorter. 

# Suppose that you are planning an RCT and due to concerns of spillover 
# effects, you decide to randomize at the village level. What is the total 
# sample size required to detect a 0.1 standard deviation effect 80% of the 
# time with 95% confidence? Assume that you plan to survey 40 people from 
# each village. Suppose that the intra-cluster correlation is 0.1.

## Calculate the sample size needed to detect a 0.1 standard deviation 
## effect with a cluster size of 40 and intra-cluster correlation of 0.1.

install.packages("CRTSize")
library(CRTSize)

clusters_ttm <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.1, AR=1
        , alpha = 0.05, power = 0.8, two.tailed = TRUE)

clusters_cnt <- clusters_ttm
N_ttm <- clusters_ttm$n*40
N_cnt <- clusters_cnt$n*40

number_clusters_ttm <- clusters_ttm$n
number_clusters_cnt <- clusters_cnt$n
test_6 <- rbind(number_clusters_ttm, number_clusters_cnt, N_ttm, N_cnt)
colnames(test_6)<- "Individuals"

print("Estimated number of clusters and Sample Sizes: ")
print(test_6)   
print(paste0("Estimated number of clusters and Sample Sizes: ",N_ttm+N_cnt))

# 192 are the number of clusters in each group 
# 7692 are the total individuals in each group of the experiment
# 15.384 is the total individuals required for this experiment

# Let's next examine how changing our assumption about the intra-cluster 
# correlation alters the required sample size. Construct a graph of the number 
# of villages (in both the treatment and control groups) required to detect 
# an effect for intra-cluster correlations between 0.1 and 0.8.

## Part 7: Construct a graph of the number of clusters required given 
## intra-cluster correlations between 0.1 and 0.8. Use the same assumptions 
## as in *Task 6*. 

cluster_0 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.1, AR=1
                        , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_1 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.15, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_2 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.2, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_3 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.25, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_4 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.3, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_5 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.35, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_6 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.4, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_7 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.45, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_8 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.5, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_9 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.55, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_10 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.6, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_11 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.65, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_12 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.7, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_13 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.75, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_14 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.8, AR=1
                     , alpha = 0.05, power = 0.8, two.tailed = TRUE)

test_7 <- data.frame(rbind(cluster_0$n, cluster_1$n, cluster_2$n, cluster_3$n,cluster_4$n
                , cluster_5$n, cluster_6$n, cluster_7$n, cluster_8$n, cluster_9$n
                , cluster_10$n, cluster_11$n, cluster_12$n, cluster_13$n
                , cluster_14$n))
colnames(test_7) <- "clusters_ttm"


temporal_7 <- data.frame(rbind(cluster_0$ICC, cluster_1$ICC, cluster_2$ICC, cluster_3$ICC,cluster_4$ICC
                               , cluster_5$ICC, cluster_6$ICC, cluster_7$ICC, cluster_8$ICC, cluster_9$ICC
                               , cluster_10$ICC, cluster_11$ICC, cluster_12$ICC, cluster_13$ICC
                               , cluster_14$ICC))
colnames(temporal_7) <- "ICC"

test_7$ICC <- temporal_7$ICC
rm(temporal_7)

test_7$clusters_cnt <- test_7$clusters_ttm
test_7$N_ttm <-test_7$clusters_ttm*40
test_7$N_cnt <-test_7$clusters_cnt*40
test_7$total_N <- test_7$N_ttm+test_7$N_cnt

lo2 <- loess(test_7$total_N~test_7$ICC )

plot (test_7$ICC, test_7$total_N, type = "b", col = "blue"
      , xlab = 'ICC', ylab = 'Total sample size', lwd = 3 )
xl2 <- seq(min(test_7$ICC),max(test_7$ICC), (max(test_7$ICC) - min(test_7$ICC))/1000)
lines(xl2, predict(lo2,xl2), col='red', lwd=1) 


# Note: This plot shows the number of total individuals in the experiment (sample) 

# As the intra-cluster correlation increases, the value of interviewing 
# multiple people in the same village decreases. As a result, with an ICC 
# of 0.8, a total sample size of over 96,000 would be required (101,093.57 ind.). 

# Finally, fix the ICC at 0.1 and examine how the total required sample 
# size changes with cluster sizes of 10, 20, 30, 40, and 50. Present 
# the results in a table. 

## Part 8: Construct a table showing how the required total sample size 
## needed to detect an effect size of 0.1 standard deviations changes as 
## the cluster size is varied between 10 and 50. Use the same 
## assumptions as in *Part 6*. 

library(CRTSize)

cluster_ad_8_1 <- n4means(delta = 0.1, sigma = 1, m = 10, ICC = 0.1, AR=1
                        , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_ad_8_2 <- n4means(delta = 0.1, sigma = 1, m = 20, ICC = 0.1, AR=1
                          , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_ad_8_3 <- n4means(delta = 0.1, sigma = 1, m = 30, ICC = 0.1, AR=1
                          , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_ad_8_4 <- n4means(delta = 0.1, sigma = 1, m = 40, ICC = 0.1, AR=1
                          , alpha = 0.05, power = 0.8, two.tailed = TRUE)
cluster_ad_8_5 <- n4means(delta = 0.1, sigma = 1, m = 50, ICC = 0.1, AR=1
                          , alpha = 0.05, power = 0.8, two.tailed = TRUE)

test_8 <- data.frame(test_5$sig.level, test_5$power, test_5$n*2, test_5$n, test_5$n
                        , test_5$d, "0", test_5$d, "1")

test_8 <- data.frame(cluster_ad_8_1$alpha, cluster_ad_8_1$power
                     , cluster_ad_8_1$nC, cluster_ad_8_1$nE
                     , cluster_ad_8_1$m, cluster_ad_8_1$m
                     , cluster_ad_8_1$delta, "0", cluster_ad_8_1$delta
                     , cluster_ad_8_1$sigma, cluster_ad_8_1$ICC
                     , cluster_ad_8_1$n*2*cluster_ad_8_1$m)
colnames(test_8) <- c("alpha", "power", "nC", "nE", "m", "m", "delta", "m1", "m2"
                      , "sd", "ICC", "N")

temp_2 <- data.frame(cluster_ad_8_2$alpha, cluster_ad_8_2$power
                     , cluster_ad_8_2$nC, cluster_ad_8_2$nE
                     , cluster_ad_8_2$m, cluster_ad_8_2$m
                     , cluster_ad_8_2$delta, "0", cluster_ad_8_2$delta
                     , cluster_ad_8_2$sigma, cluster_ad_8_2$ICC
                     , cluster_ad_8_2$n*2*cluster_ad_8_2$m)
colnames(temp_2) <- c("alpha", "power", "nC", "nE", "m", "m", "delta", "m1", "m2"
                      , "sd", "ICC", "N")

test_8 <- rbind(test_8, temp_2)

temp_2 <- data.frame(cluster_ad_8_3$alpha, cluster_ad_8_3$power
                     , cluster_ad_8_3$nC, cluster_ad_8_3$nE
                     , cluster_ad_8_3$m, cluster_ad_8_3$m
                     , cluster_ad_8_3$delta, "0", cluster_ad_8_3$delta
                     , cluster_ad_8_3$sigma, cluster_ad_8_3$ICC
                     , cluster_ad_8_3$n*2*cluster_ad_8_3$m)
colnames(temp_2) <- c("alpha", "power", "nC", "nE", "m", "m", "delta", "m1", "m2"
                      , "sd", "ICC", "N")
test_8 <- rbind(test_8, temp_2)

temp_2 <- data.frame(cluster_ad_8_4$alpha, cluster_ad_8_4$power
                     , cluster_ad_8_4$nC, cluster_ad_8_4$nE
                     , cluster_ad_8_4$m, cluster_ad_8_4$m
                     , cluster_ad_8_4$delta, "0", cluster_ad_8_4$delta
                     , cluster_ad_8_4$sigma, cluster_ad_8_4$ICC
                     , cluster_ad_8_4$n*2*cluster_ad_8_4$m)
colnames(temp_2) <- c("alpha", "power", "nC", "nE", "m", "m", "delta", "m1", "m2"
                      , "sd", "ICC", "N")
test_8 <- rbind(test_8, temp_2)

temp_2 <- data.frame(cluster_ad_8_5$alpha, cluster_ad_8_5$power
                     , cluster_ad_8_5$nC, cluster_ad_8_5$nE
                     , cluster_ad_8_5$m, cluster_ad_8_5$m
                     , cluster_ad_8_5$delta, "0", cluster_ad_8_5$delta
                     , cluster_ad_8_5$sigma, cluster_ad_8_5$ICC
                     , cluster_ad_8_5$n*2*cluster_ad_8_5$m)
colnames(temp_2) <- c("alpha", "power", "nC", "nE", "m", "m", "delta", "m1", "m2"
                      , "sd", "ICC", "N")
test_8 <- rbind(test_8, temp_2)

rm(temp_2, temporal_x)

print(test_8) # Note: some minor diferences exists between the stata result and
              # the R result. If we round up the R result, is the same quantity
              # of individuals... the total N sample has more diferenes because
              # is the multiplication of Number of cluster * groups of treatment
              # * size of every cluster. If we round up the Number of clusters,
              # then, that difference disappear.

# To conclude, we also note that there are tools outside of Stata (and R) that can 
# simplify power calculations. For instance, [Optimal Design](http://hlmsoft.net/od/) 
# is a commonly used program that is popular because it is freely available 
# and easy to use. In addition, Excel templates are available that allow 
# you to easily vary parameters to see how sample sizes change. 

## End of MOdule 7 ##

# rm(list = ls()) ## <- This code clear all the environment
