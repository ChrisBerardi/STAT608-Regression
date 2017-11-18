#Homework 6 Stat608 Summer, 2016

#Question 1
data <-read.csv("/users/saistout/desktop/608/homework/pgatour2006.csv", header=TRUE)
library(alr3)
attach(data)

lmfull <- lm(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion
+SandSaves + Scrambling + PuttsPerRound)

#All possible subsets
#To find the order
X <- cbind(DrivingAccuracy ,GIR ,PuttingAverage ,BirdieConversion,
SandSaves,Scrambling,PuttsPerRound)
install.packages("leaps")
library(leaps)
b <- regsubsets(as.matrix(X),log(PrizeMoney))
rs <- summary(b)
par(mfrow=c(1,2))
plot(1:5,rs$adjr2,xlab="Subset Size",ylab="Adjusted R-squared")
library(car)
subsets(b,statistic=c("adjr2"))

#Now compare
#Calculate adjusted R-squared
rs$adjr2
#Now for the rest
om1 <- lm(log(PrizeMoney)~GIR )
om2 <- lm(log(PrizeMoney)~GIR +PuttsPerRound)
om3 <- lm(log(PrizeMoney)~GIR +BirdieConversion+Scrambling)
om4 <- lm(log(PrizeMoney)~GIR +BirdieConversion+Scrambling+SandSaves)
om5 <- lm(log(PrizeMoney)~GIR +BirdieConversion+Scrambling+SandSaves
+PuttsPerRound)
om6 <-lm(log(PrizeMoney)~GIR +BirdieConversion+Scrambling+SandSaves
+PuttsPerRound+DrivingAccuracy)
om7 <- lmfull
#Subset size=1
n <- length(om1$residuals)
npar <- length(om1$coefficients) +1
#Calculate AIC
extractAIC(om1,k=2)
#Calculate AICc
extractAIC(om1,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om1,k=log(npar))

#Subset size=2
n <- length(om2$residuals)
npar <- length(om2$coefficients) +1
#Calculate AIC
extractAIC(om2,k=2)
#Calculate AICc
extractAIC(om2,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om2,k=log(n))

#Subset size=3
n <- length(om3$residuals)
npar <- length(om3$coefficients) +1
#Calculate AIC
extractAIC(om3,k=2)
#Calculate AICc
extractAIC(om3,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om3,k=log(n))

#Subset size=4
n <- length(om4$residuals)
npar <- length(om4$coefficients) +1
#Calculate AIC
extractAIC(om4,k=2)
#Calculate AICc
extractAIC(om4,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om4,k=log(n))

#Subset size=5
n <- length(om5$residuals)
npar <- length(om5$coefficients) +1
#Calculate AIC
extractAIC(om5,k=2)
#Calculate AICc
extractAIC(om5,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om6,k=log(n))

#Calculate AIC
n <- length(om6$residuals)
npar <- length(om6$coefficients) +1
extractAIC(om6,k=2)
#Calculate AICc
extractAIC(om6,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om6,k=log(n))

n <- length(om7$residuals)
npar <- length(om7$coefficients) +1
#Calculate AIC
extractAIC(om7,k=2)
#Calculate AICc
extractAIC(om7,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om7,k=log(n))


#Backward AIC selection
step(lmfull,data=data)
#Backward BIC
step(lmfull,data=data, k=log(n))


#Forward AIC Selection
inter <- lm(log(PrizeMoney)~1)
step(inter, direction=cbind("forward"),scope=list(lower=~1,
upper=~DrivingAccuracy + GIR + PuttingAverage + BirdieConversion
+SandSaves + Scrambling + PuttsPerRound), data=data)
#Backward BIC Selection
step(inter, direction=cbind("forward"),scope=list(lower=~1,
upper=~DrivingAccuracy + GIR + PuttingAverage + BirdieConversion
+SandSaves + Scrambling + PuttsPerRound), data=data, k=log(n))

#Final Model

#Question 2
play <-read.table("/users/saistout/desktop/608/homework/playoff.txt", header=TRUE, sep="\t")
attach(play)
plot(Population, Proportion)
playlogit <- glm(cbind(PlayoffAppearances,n-PlayoffAppearances) ~ Population, family = binomial)
summary(playlogit)
