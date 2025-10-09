##########################
# Title:        Problem Set 1
# Description:  Confidence Intervals, Hypothesis Testing, Plotting
# Author:       Sarah Magdihs
# R version:    R 4.5.1 
#Last modified: 08.10.2025
###########################

# -------------------------------#
# 1. Setup
# -------------------------------#
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

lapply(c("ggplot2", "stargazer"),  pkgTest)
library(ggplot2)
library(stargazer)

#####################
# Problem 1
#####################

#A school counselor was curious about the average IQ of students in her school and took a random sample of 25 IQ scores.

#Load Data as a vector
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

##Task 1: Find a 90% confidence interval for the average student IQ in the school.

#Firstly, the long version:
n <- length(y)
mean_y <- mean(y)
sd_y <- sd(y)
se_y <- sd_y/sqrt(n)

t_value <- qt((1-0.9)/2, n-1, lower.tail = FALSE)
#Technically, I could also do the following:
#t_value <- qt(0.95, n-1) OR qt(0.05, n-1, lower.tail = FALSE)
#Each of these version will give me the positive value at the upper tail

#CI
upper90 <- mean_y + t_value*se_y
lower90 <- mean_y - t_value*se_y

CI90 <- c(lower90, upper90)
print(CI90)
############################
#Interpretation:
#Thus, the 90% confidence intervall is [93.96;102.92]. 
#This means that - with repeated sampling - the confidence interval contains the true parameter at least 90% of the time. 
#Hence, we can be 90% confident that the interval [93.96;102.92] contains the population mean.
############################

#You can also cross-check this with a t-test:
ttest_1 <- t.test(y, conf.level = .90)

#save
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
# execute function and check ls() to make sure it worked
output_stargazer("ttest_task1.tex", ttest_1)

#As we can see, the t-test also shows that the CI = [93.96;102.92]. 
#(Besides also doing other things). 


##Task 2. Next, the school counselor was curious whether the average student IQ in her school is higher than the average IQ score (100) among all the schools in the country. Using the same sample, conduct the appropriate hypothesis test with α= 0.05.

#Based on the information, this is a one-sided t-test, since the counselor wants to test whether the average student IQ at her school is *higher* than the mean of the population. 

#Thus, the hypotheses are as following:
# H0: mean =< 100 
# Ha: mean > 100
#Since alpha = 0.05, the confidence level is 95%.

#Let's start with the short version again: 
t.test(y, mu = 100, alternative = "greater", conf.level = 0.95)
ttest_task2 <- t.test(y, mu = 100, alternative = "greater", conf.level = 0.95)

#save
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
# execute function and check ls() to make sure it worked
output_stargazer("ttest_task2.tex", ttest_task2)

#Based on this, the Null-Hypothesis cannot be rejected (p=0.7215 > 0.05). 
#Moreover, we can see that the mean of the counselor's students is actually lower than the average for all students in the country (mean= 98.44). 
#This is not only indicated by the 'mean of x' but also by the negative t-value. 

#Furthermore,we can technically also do this step by step ourselves
#I only create mu_0 here, since the other elements where already created for Task 1. 
mu_0 <- 100

#create test statistic
TS <- (mean_y-mu_0)/se_y
#p-value: since it is a one-sided test (on the right side), it needs to be 1-Probability as pt would otherwise give us the probability that T =< TS (so basically the space under the curve to the left of the TS)
p_value <- 1-pt(q=TS, df = n-1)

#data frame 
ttest_by_hand <- c(Mean = mean_y, StdError =se_y, t = TS, df = n-1, p_value = p_value)
print(ttest_by_hand)
#save
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
# execute function and check ls() to make sure it worked
output_stargazer("ttest_by_hand.tex", ttest_by_hand)
#interpretation: The Null-hypothesis cannot be rejected because p = 0.72 > 0.05. 
#The mean is actually slightly lower than 100. 

#####################
# Problem 2
#####################
#Researchers are curious about what affects the amount of money communities spend on addressing homelessness. The following variables constitute our data set about social welfare expenditures in the USA.
#Explore the expenditure data set and import data into R.

#Okay, so let's load and inspect our data first:
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
head(expenditure)
str(expenditure)

#Task 1. Please plot the relationships among Y, X1, X2, and X3 ? What are the correlations among them

#In order to plot the relationships between these variables, I will use a scatterplot. 
#However, I don't want to plot each combination individually, as that is really tedious. 
#Instead, let's use the pairs command. 

pdf("Plot_Matrix.pdf")
pairs(expenditure[,c(2,3,4,5)],
      main = "Scatterplot Matrix of Y, X1, X2, X3")
dev.off()

#So, that covers the plotting. To assess the relationships, looking at data is important, but sometimes correlations (or assosciations) can be hard to gauge visually. So let's use a correlation matrix to make sure.
cor_var <- expenditure[,c(2,3,4,5)]
correlation_matrix <- cor(cor_var)
print(correlation_matrix)
round(correlation_matrix, 2)

#Interpretation: 
#Generally, the created plot includes all possible correlation plot between the four variables. The diagonal only shows the variable names, since each variable obviously perfectly correlates with itself. The correlations above and below the diagonal are mirrored (since it shows, for example, the correlation of Y&X1 and X1&Y). So, I will focus on the plots below the diagonal. 
#There seems to be a moderate positive correlation between Y and X1; Y and X2; Y and X3. Thus, based on the plots and the correlation coefficients, states that have a higher per capita personal income/more financially insecure residence/a higher urban population density, tend to spend more money on shelters/housing assistance. 
#Moreover, X1 and X3 have an r = 0.6, which means that there could be collinearity issues when a regression model uses both variables as predictors. 
#Lastly, X1 and X2 are only weakly correlated (but seem somewhat linear).
#X2 and X3 have a low correlation coefficient (r=0.22) which indicates that there is at best a weak linear correlation. This also makes sense when looking at the graphs: The scatterplot of X2 and X3 looks like their assosciation may be better described by a quadratic function. 

#Task 2.Please plot the relationship between Y and Region? On average, which region has the highest per capita expenditure on housing assistance?

#To look at the relationship between Expenditure and Region, we can use boxplots. However, when I inspected the data, it showed that Region is an integer - which will be an issue for a boxplot. So let's make it a factor. 
str(expenditure)

#Issue: Region is an integer 
expenditure$Region <- factor(expenditure$Region,
                             levels = c(1, 2, 3, 4),
                             labels = c("Northeast", "North Central", "South", "West"))
str(expenditure)

#Now, we can make boxplots:

pdf("boxplot_Y_Region.pdf")
boxplot(expenditure$Y ~ expenditure$Region,
        main="Boxplot of Per Capita Expenditure by Region",
        ylab="Expenditure",
        xlab="Region",
        col = c("blue","green","red","yellow"))
dev.off()

#However, the second question is about the average, but boxplots give us the median. In this case, I think the difference should not be drastic. But since averages our outlier sensitive, let's make sure it actually doesn't make a difference.
#with ggplot

pdf("boxplot_with_average.pdf")
ggplot(data = expenditure, aes(x =Region, y = Y)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y),2)),
               vjust = -0.5, color = "black") +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "darkred") +
  stat_summary(fun = median, geom = "text", aes(label = round(after_stat(y),2)),
               vjust = 1.5, color = "darkred") +
  labs(title = "Per Capita Expenditure by Region",
       x = "Region", y = "Per Capita Expenditure") +
  theme_bw()
dev.off()

#As expected, in this case it doesn't really make a difference regarding the interpretation (despite the mean and median slightly diverging).
#Here, we see that on average states in the Region "West" have the highest per capita expenditure on shelters/housing assistance. 


#Task 3. Please plot the relationship between Y and X1 ? Describe this graph and the relationship. Reproduce the above graph including one more variable Region and display different regions with different types of symbols and colors.

#Let's start with the simple plot:
pdf("Task2_3_basic.pdf")
plot(expenditure$X1, expenditure$Y,
     main="Relationship Between Per Capita Expenditure and Per Capita Personal Income",
     ylab="Expenditure",
     xlab="Personal Income",
     cex.main = 0.95,
     
     abline(lm(Y ~ X1, data = expenditure), col = "red", lwd = 2) )
dev.off()

#Interpretation: 
#As already mentioned during the discussion of the scatterplots and potential correlations, we can see here that there is some kind of positive linear assosciation between per capita personal income and per capita expenditure on shelters/housing assistance. More concretely, states that have higher per capita personal income tend to spend more money on shelters/housing assistance.
#The added regression line helps visualise this relationship more effectively.

#Lastly, let's reproduce the above graph including one more variable Region and display different regions with different types of symbols and colors:

#define colors per region
colors_regions <- c("Northeast" = "lightblue", 
                    "North Central" = "lightpink", 
                    "South" = "lightgreen", 
                    "West" = "purple")

#define symbols per region
symbols_regions <- c("Northeast" = 15,  # square
                     "North Central" = 17,  # triangle
                     "South" = 18,  # diamond
                     "West" = 19)  # circle

#now the graph
pdf("Task2_3_with_Colours_and_Symbols.pdf")
plot(expenditure$X1, expenditure$Y,
     main="Relationship Between Per Capita Expenditure and Per Capita Personal Income",
     ylab="Expenditure",
     xlab="Personal Income",
     cex.main = 0.95,
     col = colors_regions[expenditure$Region],
     pch = symbols_regions[expenditure$Region])

legend("topleft", legend = levels(expenditure$Region), col = colors_regions, pch = symbols_regions, title = "Region")
dev.off()




