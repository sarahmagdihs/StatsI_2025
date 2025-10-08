#####################
# Project: Problem Set 1
# Author name: Sarah Magdihs
# last modified: 07.10.25
#####################
#####################
# load libraries
# set wd
# clear global .envir
#####################
setwd("~/Documents/GitHub/StatsI_2025/problemSets/PS01/my_answers")

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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#Task 1 - Short version = t.test; technically does more but will give you the CI
t.test(y, conf.level = .90)

# Task 1 - Long version = step-by-step ; n < 30 --> t-value 
n <- length(y)
mean_y <- mean(y)
sd_y <- sd(y)
se_y <- sd_y/sqrt(n)

#Using t-value 
t_score <- qt(0.05, 24, lower.tail = FALSE)
#or: t_score <- qt(0.95, 24)

#CI 
upper90 <- mean_y + t_score*se_y
lower90 <- mean_y - t_score*se_y

CI90 <- c(lower90, upper90)

#Task 2
#one-sided test: H0: mean =< 100
#Ha: mean > 100

t.test(y, mu = 100, alternative = "greater", conf.level = 0.95)

# longer way
TS <- (mean_y-100)/se_y
1-pt(q=TS, df = 24)



#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
head(expenditure)
str(expenditure)

#Please plot the relationships among Y, X1, X2, and X3 ? What are the correlations among them (you just need to describe the graph and the relationships among them)?
?pairs
expenditure[1,1] #indexing in a matrix = row, column 
expenditure[49,5] #5th element of the 49th row
expenditure[,1] #indexing in a matrix --> all values of the first column
expenditure[,c(2,3,4,5)] # how to do multiple columns 

pairs(expenditure[,c(2,3,4,5)])
#check
plot(x = expenditure$Y, y= expenditure$X1 )
#pairs --> under y --> y-axis?
#explain the ones below the main diagonal acc. to mairi's bf

#task 2
?boxplot
str(expenditure)
#issue region is an integer 
expenditure$Region =as.factor(expenditure$Region)
boxplot(expenditure$Y ~ expenditure$Region)

##check how to get on average 





