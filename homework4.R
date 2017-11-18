#Assignment 4

y <- rbind(5,6,8,9,11)
x1 <- rbind(1,200,-50,909,506)
x2 <- rbind(1004,806,1058,100,505)
x3 <- rbind(6,7.3,11,13,13.1)



model <- lm(y ~ x1+x2+x3)
summary(model)
X <- cbind(x1,x2,x3)
cor(X)
