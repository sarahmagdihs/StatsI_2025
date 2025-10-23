##########################
# Title:        Problem Set 2
# Description:  Chi Sqaure and Regression
# Author:       Sarah Magdihs
# R version:    R 4.5.1 
#Last modified: 22.10.2025
###########################


# -------------------------------#
# 1. Setup
# -------------------------------#
setwd("~/Documents/GitHub/StatsI_2025/problemSets/PS02/my_answers")

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

##check later: if I need these packages in here 
lapply(c("ggplot2", "stargazer"),  pkgTest)
library(ggplot2)
library(stargazer)

#####################
# Problem 1
#####################

#See Task on PDF
#First: Lets add the contingency table into R

#There are multiple ways to create a matrix in order to fulfil the task in R
#Trying multiple is also just helpful to get more familiar with R, I think. 

#Option A
data_file <- c(14, 6, 7, 7, 7, 1)
df <- matrix(data = data_file, nrow = 2, ncol = 3, byrow =TRUE)
rownames(df) <- c("upper class", "lower class")
colnames(df) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")

print(df)

#Option B
Option_B_df <- matrix(NA, nrow= 2, ncol=3)
rownames(Option_B_df) <- c("upper class", "lower class")
colnames(Option_B_df) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
Option_B_df[1,] <- c(14,6,7)
Option_B_df[2,] <- c(7,7,1)
print(Option_B_df)

#I feel like Option B has the benefit of making it easier to recognise and adjust mistakes
#But since they are both the same, I will use Option A because the name is more efficient 

# Task 1: Calculate the χ2 test statistic (incl. by hand/manually)

#There is a very simple option: R can run a Chi^2 test:
#this creates a dataframe that actually stores a lot of useful information, including the (standardised) residuals
#Importantly, in the console, we also see that R warns us that the approximation may be incorrect. This is because one of the cells contains a value smaller than 5.
#While this normally means that we wouldn't run a Chi^2, we do it anyway for this exercise. 

chi_square_by_R <- chisq.test(df)
print(chi_square_by_R)
#the TS is X-squared = 3.7912. Moreover, R already tells us that it used two degrees of freedom.
#This is because the df = (row - 1) * (column - 1)
#Moreover, it also gives us a p-value. As stated in the second task, we use the significance level alpha = 0.1. 
#Thus, we can already see that we would reject the Nullhypothesis (H_0: the two variables are independent, or f_observed = f_expected) and conclude that there is sufficient evidence for H_A

#additionally, the dataframe stores the standardised residuals:
std_residuals_by_R <- chi_square_by_R$stdres
print(std_residuals_by_R)
# we can then add these into the table in the pdf. 

#regarding interpretation:
#The standardised residuals can help us identify which cells/groups drive the overall difference that we observe 
#The cells with larger standardised residuals contribute more heavily to the Chi^2 statistic 
#If the residual is positive, it means this outcome was observed more often than expected under the Nullhypothesis. Conversely, if the residuals are negative, we actually observed a certain outcome less often than expected under the Nullhypothesis
#(because each residual = observed-expected divided by the SE, which makes it comparable as it is measured in units of the SE)


#now by hand:
#let's use the dataframe we created above
print(df)

#the Chi^2 statistic = the sum of: (observed-expected)^2/expected.
#Thus we first calculate the expected values for each cell. 

#To do this, we need the row/column/grand total: expected = ((row total)+(column total))/grand total 

c_total <- colSums(df)
r_total <- rowSums(df)
grand_total <- sum(df)
#lets check
c_total
r_total
grand_total

#okay lets continue:
#f(expected) =(rowtotal*columntotal)/grandtotal
#lets first figure out how to calculate all the products we need at once
?outer
outer(c_total, r_total) #gives the outer product of the arrays X and Y

#okay, now that I know this works, lets get the expected values for each cell all at once 
expected_values <- outer(r_total, c_total)/grand_total
#make sure the r_total is first, or the form is wrong

#now let's do the actual Chi^2 statistic 
#our elements: 
#observed values: in the dataframe df
#expected values: in expected_values 

chi_sq_by_hand <- sum((df-expected_values)^2/expected_values)

#okay lets compare
chi_sq_by_hand
chi_square_by_R
#it's the same. thats good. 

#now, the rest can also be done by hand:
#lets calculate the p-value 
#first we need the degrees of freedom

#df = (rows−1)(columns−1)
rows <- nrow(df)
columns <- ncol(df)

df <- (rows - 1) * (columns - 1)

p_value_chi_sq <- pchisq(chi_sq_by_hand, df = df, lower.tail = FALSE)
p_value_chi_sq

#as we can see the p-value is equal to roughly 0.15. Thus, since alpha = 0.1 and 0.15 > 0.05, we reject the Nullhypotheses that these two variables are independent

# i tried doing the standardised residuals by hand but i think i messed something up, cause it did NOT want to give me the actual numbers 
#and honestly, this week I don't have time to figure out why 

#####################
# Problem 2
#####################
#The authors hypothesize that female politicians are more likely to support policies female voters want. Researchers found that more women complain about the quality of drinking water than men. You need to estimate the effect of the reservation policy on the number of new or repaired drinking water facilities in the villages.
data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

head(data)
str(data)

#Task 1: State a null and alternative (two-tailed) hypothesis.
# So, as stated in the PS, I need to estimate the effect of the reservation policy on the number of new or repaired drinking water facilities in the villages.
#Thus, plainly speaking, the Nullhypothesis is that the reservation policy as NO impact on the number of new or repaired drinking water facilities in the villages.
#Thus, the Alternative Hypothesis is that there is the reservation policy does have an impact on the number of new or repaired drinking water facilities in the villages.

#Technically, given the description, the hypothesis would be that it has a positive effect, but it is a two-tailed hypothesis test, so it is AN effect. 

#In other words, we expect that in the following regression, beta_1 does not equal zero (Alternative Hypothesis)


#Task 2: Run a bivariate regression to test this hypothesis in R (include your code!).

#simple bivariate regression: 
reg_1 <- lm(data$water ~ data$reserved)
summary(reg_1)

#Task 3: Interpret the coefficient estimate for reservation policy.
# fundamentally, for every one unit increase in X, we expect a beta unit increase in Y
#in this case, the predictor is binary. Our regression shows that based on our data, there is evidence to conclude that the reservation policy has an effect on the number of new or repaired drinking water facilities in the villages (reject H_0).
#We find that, on average, villages with reserved seats for women have 9.25 more new or repaired drinking water facilities than villages without such a policy. 
#Moreover, we beta_0 tells us that if there is no such policy, we expect there to be roughly 15 new or repaired drinking water facilities 

#just to add to this, beta is still an estimator with its own sampling distribution. Our interpretation is based on the sample we have. we could also calculate a CI to establish a 95% CI within which we expect the parameter to fall given repeated sampling
confint(reg_1, level =0.95)

#Here, we see that, given repeated sampling, we are 95% confident that the true parameter falls between 1.485608 and 17.01924. 
#Here, we also see once again, that the estimate of the regression is significant at the sig. level 0.05, since 0 is not included in the intervall. 


#thus, ultimately: As the the coefficient of the reservation policy variable is statistically significant (p = 0.0197) and the 95% confidence interval [1.49, 17.02] does not include zero, we can conclude that the presence of reserved seats for women is associated with a statistically significant positive difference. 
