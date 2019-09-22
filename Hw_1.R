# Question 2
# (a)
College = read.csv("College.csv")

# (b)
fix(College)
rownames(College) = College[,1]
fix(College)
College<- College[,-1]
fix(College)

# (c)
## (i)
summary(College)
## (ii)
pairs(College[,1:10])
## (iii)
jpeg("Q1C.jpeg")
plot(College$Private,College$Outstate, col = "violet", xlab = "Private", ylab = "Outstate", main = "Plot of Outstate vs Private")
dev.off()

## (iv)
Elite = rep("No", nrow(College))
Elite [College$Top10perc >50]="Yes"
Elite = as.factor(Elite)
College = data.frame(College,Elite)
summary(Elite)

pdf("Outstate vs Elite.pdf")
plot(Elite, College$Outstate, col = 2, xlab = "Elite", ylab = "Outstate", main = "Outstate vs Elite")
dev.off()

## (v)
jpeg("2X2 Histogram Plot.jpeg")
par(mfrow=c(2,2))
hist(College$Apps, col = 2)
hist(College$Accept, col = "blue")
hist(College$Enroll, col = "green")
hist(College$Personal, col = "yellow")
dev.off()

## (vi) - Further Exploration
summary(Elite)
summary(College$Private)
summary(College)
### Plotting more than one variable in a histogram
jpeg("exploration.jpeg")
hist(College$Apps, col = "yellow")
hist(College$Accept, add = T, col = "blue")
dev.off()


# Question 3
Auto = read.csv("Auto.csv", na.strings = "?", header = T)
Auto = na.omit(Auto)
fix(Auto)

## (a) Which predictor is quantitative or qualitative
fix(Auto)
ClassVariables = sapply(Auto, function(x) class(x))
ClassVariables


## (b)
Auto = data.frame(Auto[,-9])
fix(Auto)
data.frame(range=sapply(Auto,range))
#Alternatively (Column format)
t(sapply(Auto, range))

## (c) - mean & standard deviation
t(sapply(Auto, mean))
t(sapply(Auto, sd))

## (d)
Auto = Auto[-c(10:85),]
fix(Auto)
t(sapply(Auto, range))
t(sapply(Auto, mean))
t(sapply(Auto, sd))

## (e) - investigate predictors graphically
fix(Auto)
pairs(Auto)
par(mfrow=c(1,1))
plot(Auto$cylinders,Auto$mpg)
# Continue late in the night


# Question 4 (Boston Dataset)

## (a)
library(MASS)
fix(Boston)
?Boston
nrow(Boston)
ncol(Boston)
#rows represent the observations, while the columns represent the variables or predictors

## (b) - Some pairwise scatterplots
str(Boston)
str
pairs(Boston[c(3,5,8)])
cor(Boston$indus,Boston$nox)
#As an example, the pairwise comparison of Industry and Nitro Oxide (predictors) suggests that a strong relationship exist between them.

## (c)
cor(Boston[-1], Boston$crim)
#index of accessibility to radial highways appears to be the predictor with the strongest correlation with per capita crime rate.

## (d)
t(sapply(Boston, range))
summary(Boston)
#histogram of tax, crime, and pupil-teacher ratio
jpeg("TCP.jpeg")
par(mfrow = c(2,2))
hist(Boston$crim, xlab = "Crime rate", ylab="Number of Suburbs", col = 2)
summary(Boston$crim)
#based on the crime rate summary and histogram, the average crime rate in the entire suburbs is roughly 4% but some suburbs have alarmingly high crime rate of about 89%.

Low_Crime_rate = subset(Boston, Boston$crim > 5)
nrow(Low_Crime_rate)/nrow(Boston)
#Over 20% of the suburbs are faced with crime rate that is beyond 5%.
High_Crime_rate = subset(Boston, Boston$crim > 60)
nrow(High_Crime_rate)/nrow(Boston)
#Approximatly 0.5% of the suburb have a high crime rate.

hist(Boston$tax, xlab = "Full-value property-tax rate /$10,000", ylab="Number of Suburbs", col = "green")
Tax= subset(Boston, Boston$tax< 500)
nrow(Tax)/ nrow(Boston)
#72% of the houses in the Boston suburbs pay less than $500 for property tax

hist(Boston$ptratio, xlab ="Pupil-teacher ratio by town", ylab="Number of Suburbs", col = "Blue")
dev.off()

## (e)
sum(Boston$chas != 0)
# 35 Suburbs

## (f)
median(Boston$ptratio)
# The median is 19

## (g)
Lowest_Median = Boston[order(Boston$medv),]
Lowest_Median[1,]
# Suburb 399 with a median value of $5000
summary(Boston)
# The lowest median value of owner occupied homes in Boston has a fairly high crime rate, no river for serenity, high concentration of dangerous nitrogen oxide, and low property tax.

## (h)
Rooms_7 = subset(Boston, Boston$rm > 7)
nrow(Rooms_7)
# Only 64 suburbs have more than 7 rooms per dwelling

Rooms_8 = subset(Boston, Boston$rm > 8)
nrow(Rooms_8)
# Just 13 suburbs average more than 8 rooms per dwelling

class(Auto$mpg)

sapply(Auto, function(x) class(x))
