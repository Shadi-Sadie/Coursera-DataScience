---
title: "Statistical Infrence_Project_part2"
author: "Shadi Seyedi"
date: "5/1/2020"
output: pdf_document
---

Synopsis

Now in the second portion of the project, we're going to analyze the ToothGrowth data in the R datasets package.




Load the ToothGrowth data and perform some basic exploratory data analyses

#exploratory analyses
```{r exploratory, echo=TRUE}
library(knitr)
library(ggplot2)
library(datasets)
data(ToothGrowth)

str(ToothGrowth)
head(ToothGrowth)
dim(ToothGrowth)

```

#Provide a basic summary of the data.

```{r summary, echo=TRUE}
summary(ToothGrowth)

```

```{r plot, echo=TRUE}

ggplot(data = ToothGrowth, aes(x = as.factor(dose), y = len, fill = supp)) + geom_bar(stat="identity") + facet_grid(supp~.) + xlab("Dose Type") + ylab("Length of Tooth") +
    ggtitle("Bar chart Representing effect of Dose on Tooth Length")

```


Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. 

# confidence intervals
                                                                                            
```{r t-test-1, echo=TRUE}
t.test(len ~ supp, data = ToothGrowth)


```
T-test for dose 0.5 mg:

```{r t-test-2, echo=TRUE}
t.test(len ~ supp, data = subset(ToothGrowth, dose == 0.5))



```
T-test for dose 1 mg:

```{r t-test-3, echo=TRUE}
t.test(len ~ supp, data = subset(ToothGrowth, dose == 1))


```
T-test for dose 2 mg:

```{r t-test-4, echo=TRUE}
t.test(len ~ supp, data = subset(ToothGrowth, dose == 2))



```

# conclusions and Assumption

For all dosages, the p-value of this test is is less than 0.5, So we can reject the null hypothesis.which is there is no diffrence between the teeth length among diffrent group of dosege.

For the entire trail we cannot conclude OJ is more effective that VC for all scenarios.

