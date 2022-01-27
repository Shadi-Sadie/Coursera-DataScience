## statistical  infrence
#1 
n <- 40
Lambda <- 0.2

## Theorytical Values

tMean<-1/Lambda
tSd<-tMean/sqrt(n)
tVar<-tSd^2


## 1. Show the sample mean and compare it to the theoretical mean distribution

SampleMean <- NULL
for(i in 1:1000) {
    SampleMean <- c(SampleMean, mean(rexp(n, Lambda)))
}
sMean<-mean(SampleMean)

## graph for showing that theortical and sample mean are close
hist(SampleMean, breaks = n, prob = T, col="Yellow", xlab = "Mean Average", main="Distribution of Exponential Average")
abline(v = tMean, col="red", lwd=2)
abline(v = sMean, col="green", lwd=2)


## 2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution


sVar <- var(SampleMean)
# graph for showing diffrence in theortical Variance and sample Variance.

hist(SampleMean, breaks = n,prob=T,col="Yellow", xlab = "Mean Average", main="Distribution of Exponential Average")
x <- seq(min(SampleMean), max(SampleMean), length = 100)
lines(x, dnorm(x, mean = tMean , sd = tSd), pch = 35, col = "green")
lines(x, dnorm(x, mean = sMean , sd = sqrt(sVar)), pch = 28, col = "red")



##3. Show that the distribution is approximately normal.

hist(SampleMean, breaks = n, prob = T, col = "Yellow", xlab = "Means", main="Distribution of Exponential Average")
x <- seq(min(SampleMean), max(SampleMean), length = 100)
lines(x, dnorm(x, mean = tMean, sd = tSd), pch = 25, col = "green")









