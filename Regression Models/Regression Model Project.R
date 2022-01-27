##You work for Motor Trend, a magazine about the automobile industry. 
#Looking at a data set of a collection of cars, they are interested in exploring 
#the relationship between a set of variables and miles per gallon (MPG) (outcome). 
#They are particularly interested in the following two questions:
    
##  “Is an automatic or manual transmission better for MPG (mile per gallon) ”
## " Quantify the MPG difference between automatic and manual transmissions "

## Transmission (0 = automatic, 1 = manual)
## exploratory data analyses

library(ggplot2)
library(ggcorrplot)
library(printr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(jtools)
tab_model(LM)
summ(LM)
# 1. cleaning data
data("mtcars")
X<-head(mtcars)
str(mtcars)
summary(mtcars)


#kable(head(mtcars),align = 'c')

cleanedmtcars<-subset(mtcars,select=c(mpg,am,gear,cyl,disp,hp,wt))

colnames(cleanedmtcars)<-c("MPG","Transmission","gear","cylinder","displacment","GrossHorsepower","Weight")

sum(is.na(mtcars)) 
cleanedmtcars$Transmission<-factor(cleanedmtcars$Transmission)

#2 head of data
table(mtcars$am)
table(mtcars$mpg)

str(cleanedmtcars)
summary(cleanedmtcars$mpg)
table(cleanedmtcars$cylinder)
table(cleanedmtcars$gear)
table(cleanedmtcars$displacment)

#3 plot
?qplot
qplot(x=mpg,data=mtcars,fill= I("yellow") ,colour="white")
qplot(y = mpg, data = mtcars)
qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = I("red"))

qplot(mpg, wt, data = mtcars, geom = "path")
qplot(factor(cyl), wt, data = mtcars, geom = c("boxplot", "jitter"))
qplot(mpg, data = mtcars, geom = "dotplot")

qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = cyl)
qplot(mpg, wt, data = mtcars, size = cyl)
qplot(mpg, wt, data = mtcars, facets = vs ~ am)


qplot(1:10, rnorm(10), colour = runif(10))
qplot(1:10, letters[1:10])
mod <- lm(mpg ~ wt, data = mtcars)
qplot(resid(mod), fitted(mod))



g<-ggplot (data = mtcars,aes(factor(am), mpg, fill=factor(am)))+ geom_violin(trim=FALSE)
   # +labs(title="Plot of mpg by transmission",x= "Transmission", y = "Mile per Gallon")
b<-g+geom_boxplot(width=0.1,fill="white")
b+ scale_fill_manual(values=c("yellow", "green"),name="Transmission", breaks= c("0","1"),labels=c("automatic", "manual")) 

# heatmap

corr <- round(cor(mtcars), 1)
ggcorrplot(corr,colors = c("chartreuse4", "greenyellow", "khaki1"), lab=TRUE)

?ggcorrplot
# possible models 

t.test(mtcars$mpg~mtcars$am)
test<-t.test(mpg ~ am, data= mtcars, var.equal = FALSE, paired=FALSE ,conf.level = .95)
result <- data.frame( "t-statistic"  = test$statistic, 
                      "df" = test$parameter,
                      "p-value"  = test$p.value,
                      "lower CL" = test$conf.int[1],
                      "upper CL" = test$conf.int[2],
                      "automatic mean" = test$estimate[1],
                      "manual mean" = test$estimate[2],
                      row.names = "")
print(result)

LM <- lm(mpg ~ am, data=mtcars)
plot(LM)
summary(LM)

Multi = lm(data = mtcars, mpg~.)
Best <- (step(Multi,trace=0))


mfit <- lm(mpg~ factor(am) + cyl +disp+ wt + hp, data = mtcars)
summary(mfit)
summary(Best)
multiLM <- lm(MPG ~ Transmission +cylinder+displacment+gear, data=cleanedmtcars)
plot(multiLM)
summary(multilm)
class(anova(LM,Best))

cyl disp hp wt have high correlatio

library(huxtable)
export_summs(Best, LM, scale = TRUE,robust = TRUE)

?export_summs

wholeLM <- lm(MPG ~ Transmission +cylinder+displacment+gear+GrossHorsepower + Weight, data = cleanedmtcars) 
plot(wholeLM)

summary(wholeLM)

  #  Did the student fit multiple models and detail their strategy for model selection?
   # Did the student do a residual plot and some diagnostics?
  
# diagnosis  
        
         
# comparing models
 anova(LM,Best)

 par(mfrow=c(2,2))    
 plot(Best)
 
 ?summ 
 ?jtools
 
 
 
 fit <- lm(mpg ~ cyl, data = mtcars)
 get_formula(fit)
 
 md_table(X)
 ?num_print
 plot <- ggplot(mpg, aes(cty, hwy)) +
   geom_jitter() + theme_nice()
 plot
 ?stargazer
 