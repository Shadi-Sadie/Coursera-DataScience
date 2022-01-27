---
title: "Statistical Inference Course Project part 1- Simulation Exercise"
author: "Shadi Seyedi"
date: "5/1/2020"
output: pdf_document
---

Synopsis

This is part 1 of the course project of statistical inference course in this part our goal is to check if the mean and variance of simutlated exponentional distribuation that we are making is near the theortialc  value of both so that or say we are to compare our simulated result with the Central Limit Theorem. 

We start with loading required packages and continue with defining the variable that we know

```{r setup, echo=TRUE}
library(knitr)


n <- 40
Lambda <- 0.2
tMean<-1/Lambda
tSd<-tMean/sqrt(n)
tVar<-tSd^2


```

1. Question 1 asks us to show the sample mean and compare it to the theoretical mean distribution, I already defiend the the theortical mean here I build the simulation and calcluate the simulated mean 
```{r mean, echo=TRUE}

SampleMean <- NULL
for(i in 1:1000) {
    SampleMean <- c(SampleMean, mean(rexp(n, Lambda)))
}
sMean<-mean(SampleMean)


```

Now we look at the both theorytical and simulation value 


```{r mean compare, echo=TRUE}
sMean
tMean
```
  as we can see they are close to each other we could look at the histogram plot of the simulated data and draw mean as we can see means are so closes to each other that lines can not be distingueshed
  
```{r mean plot, echo=TRUE}  
  
  hist(SampleMean, breaks = n, prob = T, col="Yellow", xlab = "Mean Average", main="Distribution of Exponential Average")
abline(v = tMean, col="red", lwd=2)
abline(v = sMean, col="green", lwd=2)

```
  
  2. In this qustion we want to compare Variances, Theortical Variance is already calculated here we calculate the simulated variance an compare them
  
```{r var compare, echo=TRUE}  
  
  sVar <- var(SampleMean)
   sVar
   tVar
```   

We can conclude the both var are also close to each other, looking at the plot is also confirm this.

```{r var plot, echo=TRUE}  
hist(SampleMean, breaks = n,prob=T,col="Yellow", xlab = "Mean Average", main="Distribution of Exponential Average")
x <- seq(min(SampleMean), max(SampleMean), length = 100)
lines(x, dnorm(x, mean = tMean , sd = tSd), pch = 35, col = "green")
lines(x, dnorm(x, mean = sMean , sd = sqrt(sVar)), pch = 28, col = "red")


```


Finally in question three we want to confirm that the our simulated distribuation is approximatly normal 
drawing a normal dirstribution line on our simulated distribution we can see it is near normal.

```{r normal plot, echo=TRUE}  

hist(SampleMean, breaks = n, prob = T, col = "Yellow", xlab = "Means", main="Distribution of Exponential Average")
x <- seq(min(SampleMean), max(SampleMean), length = 100)
lines(x, dnorm(x, mean = tMean, sd = tSd), pch = 25, col = "green")

```
