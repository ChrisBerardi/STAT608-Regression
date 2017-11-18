#Homework 3

data <-read.csv("/users/saistout/desktop/pgatour2006.csv", header=TRUE)
library(alr3)

attach(data)

#prize money is highly left right skewed -> log transformation
hist(PrizeMoney)
hist(log(PrizeMoney))

hist(DrivingAccuracy)
hist(GIR)
hist(PuttingAverage)
hist(BirdieConversion)
hist(SandSaves)
hist(Scrambling )
hist(PuttsPerRound)

#looks good, nothing out of ordinary
pairs(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion
+SandSaves + Scrambling + PuttsPerRound)

#high value for lambda=1, don't do anything
X <-cbind(DrivingAccuracy,GIR,PuttingAverage,BirdieConversion,SandSaves,
Scrambling,PuttsPerRound)
tranx<-powerTransform(X) #Searches for multivariate normality.
summary(tranx)

#high value for lambda=0 -> log transform
lm.notrans <- lm(PrizeMoney ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion
+SandSaves + Scrambling + PuttsPerRound)
tranmod <- powerTransform(lm.notrans)
summary(tranmod)


#lamdba =0 looks the best of all of the fits
inverseResponsePlot(lm.notrans,key=TRUE)



notiger = data[which (TigerWoods !=1),]



mfull <-lm(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion
+SandSaves + Scrambling + PuttsPerRound)


#One leverage points ->Tiger woods, 185
par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(2,2))
plot(mfull)

identify(Name)

#Both sets look random, like they should
par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(2,2))
plot(mfull$fitted, mfull$residuals)
plot(DrivingAccuracy, mfull$residuals)
plot(GIR , mfull$residuals)
plot(BirdieConversion, mfull$residuals)

par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(2,2))
plot(SandSaves , mfull$residuals)
plot(Scrambling , mfull$residuals)
plot(PuttsPerRound, mfull$residuals)
plot(PuttingAverage , mfull$residuals)

#marginals: driving accuracy, putting average and sand saves are problematic
mmps(mfull)

#The more accurate you are, the less money you get?
#Rest look good putting average/putts get round up, money down
#Everything else is positive as it should be
summary(mfull)


#AvPlots
#Driving accuracy and putting average don't seem to add much
par(cex.axis=1.5,cex.lab=1.5, mar=c(6,7,4,4),lwd=2, pch=19, mfcol=c(3,2))
avPlots(mfull)

#Correlation
cor(X)

#variance inflation should be less than 5
#Big issues with putting averge and putts per round, less so with GIR
vif(mfull)
