##########################
# Title:        Problem Set 3
# Description:  Regression
# Author:       Sarah Magdihs
# R version:    R 4.5.1 
#Last modified: 11.11.2025
###########################


# -------------------------------#
# 1. Setup
# -------------------------------#
setwd("~/Documents/GitHub/StatsI_2025/problemSets/PS03/my_answers")

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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
lapply(c("ggplot2", "stargazer"),  pkgTest)
library(ggplot2)
library(stargazer)
if(!require(car)) install.packages("car")
library(car)
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

head(inc.sub)
str(inc.sub)

#####################
# Problem 1
#####################
# interested in knowing how the diﬀerence in campaign spending between incumbent and challenger aﬀects the incumbent’s vote share.

#Task 1: regression where the outcome variable is voteshare and the explanatory variable is difflog.

reg1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(reg1)

#Interpretation?

#Task 2: Scatterplot 
plot(inc.sub$difflog, inc.sub$voteshare,
     main="Relationship Between Difference in Campaign Spending and Incumbents' Vote Share",
     xlab="Difference in Campaign Spending",
     ylab="Incumbents' Vote Share",
     cex.main = 0.95,
     
     abline(lm(voteshare ~ difflog, data = inc.sub), col = "darkred", lwd = 2) )

#eh not that great.. 
#ggplot
ggplot(data=inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point(size=2, shape = 21) +
  geom_smooth(method=lm, color="maroon") + 
  labs(title = "Difference in Campaign Spending and Incumbents' Vote Share",
       x = "Difference in Campaign Spending", y = "Incumbent's Vote Share") +
  theme_bw()
ggsave("plot1.pdf")
# Task 3: Residuals 
residuals1 <- reg1$residuals

#Task 4: Prediction Equation
reg1
#expected vote share (incumbent) = 0.57903 + 0.4167*difference in campaign spending 



#####################
# Problem 2
#####################
#interested in knowing how the diﬀerence between incumbent and challenger’s spending and the vote share of the presidential candidate of the incumbent’s party are related

#Task 1: regression  where the outcome variable is presvote and the explanatory variable is difflog.
reg2 <- lm(presvote ~ difflog, data = inc.sub)
summary(reg2)

#Task 2: Scatterplot 
ggplot(data=inc.sub, aes(x = difflog, y = presvote)) +
  geom_point(size=2, shape = 21) +
  geom_smooth(method=lm, color="maroon") + 
  labs(title = "Difference in Campaign Spending and Vote Share of  Presidential Candidate",
       x = "Difference in Campaign Spending", y = "Vote Share of  Presidential Candidate (of the incumbent’s party)") +
  theme_bw()
ggsave("plot2.pdf")
#Task 3: Residuals 
residuals2 <- reg2$residuals

#Task 4: Prediction Equation
reg2
# expected vote share of incumbent in presidential election = 0.50758 + 0.02384 * difference in campaign spending 

#####################
# Problem 3
#####################
#interested in knowing how the vote share of the presidential candidate of the incumbent’s party is associated with the incumbent’s electoral success.

#Task 1: regression where the outcome variable is voteshare and the explanatory variable is presvote.
reg3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(reg3)

#Task 2: Scatterplot 
ggplot(data=inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point(size=2, shape = 21) +
  geom_smooth(method=lm, color="maroon") + 
  labs(title = "Incumbent Party: Vote Share of the Presidential Candidate and Success of Incumbent", 
       x = "Presidential Candidate's Vote Share", y = "Incumbent's Vote Share") +
  theme_bw() +
theme(
  plot.title = element_text(size = 12) 
)
ggsave("plot3.pdf")
#Task 3: Prediction Equation
reg3
#incumbent’s electoral success =  0.4413 + 0.3880*vote share of the presidential candidate of the incumbent’s party

#####################
# Problem 4
#####################
#residuals from part (a) tell us how much of the variation in voteshare is NOT explained by the diﬀerence in spending between incumbent and challenger
#residuals in part (b) tell us how much of the variation in presvote is not explained by the diﬀerence in spending between incumbent and challenger in the district

#Task 1: regression where the outcome variable is the residuals from Question 1 and the explanatory variable is the residuals from Question 2.
df <- data.frame(residuals_1 = residuals1,
                 residuals_2 = residuals2)

reg4 <- lm(residuals_1 ~ residuals_2, data = df)
summary(reg4)

#Task 2: scatterplot 
ggplot(data =df, aes(x = residuals_2, y = residuals_1)) +
  geom_point(size=2, shape = 21) +
  geom_smooth(method=lm, color="maroon") + 
  labs(title = "Residuals Regression: voteshare and presvote (Controlling for difflog)",
       x = "Residuals of presvote ~ difflog", 
       y = "Residuals of voteshare ~ difflog") +
  theme_bw()
ggsave("plot4.pdf")
#Task 3: prediction equation
reg4
#residuals from Q1 = 1.942e-18 + 0.2569*residuals from Q2

#####################
# Problem 5
#####################
#What if the incumbent’s vote share is aﬀected by both the president’s popularity and the diﬀerence in spending between incumbent and challenger?

#Task 1: regression outcome variable is the incumbent’s voteshare and the
#explanatory variables are difflog and presvote

reg5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(reg5)


#added residuals plot 
pdf("Added_Variable_Plots_Q5.pdf", width = 12, height = 7)
avPlots(reg5, col = "black",col.lines = "maroon", pch = 21, main = "Added Variable Plots for Question 5")
dev.off()

#Task 2: Prediction equation 
#Incumbents vote share = 0.4486442 + 0.0355431*difference in spending + 0.2568770*presvote


#Task 3: 
summary(reg4)
summary(reg5)

#slope of presvote in Q5 and the slope in Q4. Because both ask the same question ("How much does the president’s popularity affect the incumbent’s votes once we account for spending?").
#they just do it using different approaches. 

stargazer(reg1)
stargazer(reg2)
stargazer(reg3)
stargazer(reg4)
stargazer(reg5)

