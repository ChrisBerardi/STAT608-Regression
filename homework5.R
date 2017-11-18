#Homework 5

#Question 1
data <-read.csv("/users/saistout/desktop/Adrevenue.csv", header=TRUE)
attach(data)
library(MASS)
tCirculation = Circulation^.5

#Y BoxCox transformation
hist(AdRevenue)
lm.notran <- lm(AdRevenue ~ tCirculation)
trans <-boxcox(lm.notran, plotit= TRUE)
max <-max(trans$y)
(lambda <- trans$x[which(trans$y==max)])


lm.notran <- lm(AdRevenue ~ Circulation)
trans <-boxcox(lm.notran, plotit= TRUE)
max <-max(trans$y)
(lambda <- trans$x[which(trans$y==max)])


#Question 2
data <-read.csv("/users/saistout/desktop/pgatour2006.csv", header=TRUE)
library(alr3)

attach(data)

mfull <-lm(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion
+SandSaves + Scrambling + PuttsPerRound)
mmps(mfull)