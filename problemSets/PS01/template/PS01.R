#####################
# load libraries
# set wd
# clear global .envir
#####################

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

#confidence interval 90
#= mean +- t * s/sqrt(n)
#t-value because n < 30. 

sample_n <- length(y)
sample_mean <- mean(y)
sample_sd <- sd(y)
#standard deviation could also be calculated step-by-step but honestly I dont see the point of doing that, I know the formula is s = sqrt(sum(x_underscore_i-mean)^2/n-1)
t_score <- qt(0.05, 24, lower.tail = FALSE)
# 0-05 is chosen because (1-0.9)/2=0.05 --> two tailed CI
#lower-tail = false --> positive value 

upper_90 <- sample_mean + t_score*(sample_sd/sqrt(sample_n))
lower_90 <- sample_mean - t_score*(sample_sd/sqrt(sample_n))

CI_90 <- c(lower_90, upper_90)
print(CI_90)

#alternatively 
t.test(y, conf.level = 0.90)
# the t-test does more than just a CI but it is a nice way to see whether i did it correctly, i guess. 
# also in this case, both assume a t-distribution

#Next, the school counselor was curious whether the average student IQ in her school
#is higher than the average IQ score (100) among all the schools in the country.
#Using the same sample, conduct the appropriate hypothesis test with α= 0.05.

#wait is it a two-sided or one-sided test? like is the assumption that it is bigger?

t.test(y,alternative = c("greater"), mu =100, conf.level = 0.95 )
?t.test

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)







