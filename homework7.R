#Homework 7 Stat608, Summer 2016

#Question 1
oral <-read.csv("/users/saistout/desktop/608/homework/oral.csv", header=TRUE)
attach(oral)

#Take only the data that exists
total <- TOTALCW6 != "missing"
data <- oral[total,]
detach(oral)
attach(data)

lmfull <- lm(as.numeric(TOTALCW6)~ STAGE + STAGE*TRT-1)
summary(lmfull)
confint(lmfull)

#Question 3
book <-read.table("/users/saistout/desktop/608/homework/book.txt", header=TRUE, sep="\t")
attach(book)


lmnaive <- lm(Sales ~ Time)
par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(2,2))
plot(lmnaive)
stanres <-rstandard(lmnaive)
acf(stanres)
plot(Time, stanres)

lmbetter <- lm(Sales ~ Month_2 + Month_3 + Month_4 + Month_5 + Month_6 +
Month_7 + Month_8 + Month_9 + Month_10+ Month_11+ Month_12)
summary(lmbetter )

par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(2,2))
plot(lmbetter )
stanres <-rstandard(lmbetter )
acf(stanres)
plot(Time, stanres)

lmbetter <- lm(Sales ~ + Month_3 + Month_4 + Month_5 + Month_6 +
 Month_8 + Month_9 + Month_10+ Month_11+ Month_12)
summary(lmbetter )

par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfcol=c(2,2))
plot(lmbetter )
stanres <-rstandard(lmbetter )
acf(stanres)
plot(Time, stanres)
