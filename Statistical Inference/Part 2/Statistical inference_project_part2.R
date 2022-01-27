library(datasets)
library(ggplot2)
data(ToothGrowth)


str(ToothGrowth)
head(ToothGrowth)
dim(ToothGrowth)

## summary
summary(ToothGrowth)

ggplot(data = ToothGrowth, aes(x = as.factor(dose), y = len, fill = supp)) + geom_bar(stat="identity") + facet_grid(supp~.) + xlab("Dose Type") + ylab("Length of Tooth") +
    ggtitle("Bar chart Representing effect of Dose on Tooth Length")


## hyphot testing 
t.test(len ~ supp, data = ToothGrowth)
t.test(len ~ supp, data = subset(ToothGrowth, dose == 0.5))
