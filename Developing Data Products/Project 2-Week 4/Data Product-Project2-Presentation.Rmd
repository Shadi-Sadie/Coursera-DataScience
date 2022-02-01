---
title: "Shiny Application and Reproducible Pitch
"
author: "Shadi"
date: "5/29/2020"
output: ioslides_presentation
---


## About


 <p style="color:grey"> This is a presentation for the coursera final course project of Shiny Application and  Reproducible Pitch.</p>

 <p style="color:grey">  The goal of this project is to prepare simple Shiny application and provide required information regarding it.</p>

##  Application overview 


 <p style="color:grey"> This application helps you to figure out how rich you are compared to people in the world and USA!</p>

* The data for  US  income distribution is  from 

[US DATA](https://dqydj.com/average-median-top-household-income-percentiles/)

* and data for world income distribution is  from

[World data](https://docs.google.com/spreadsheets/d/1OSiA2dnbvZ5pUti2DO_HJU8phyfCDC-hNz5McTVcEnM/edit#gid=2139097862)


## Links to github and app


&nbsp;



* [Github](https://github.com/Shadi-Sadie/Coursera-DataScience/tree/main/Developing%20Data%20Products/Project%202-Week%204/IncomeApp)


* [Application](https://shadis.shinyapps.io/dataprojectApp/)


## Slide with Plot

<p style="color:grey"> The follwing plot shows the income distribution for quantile population</p>

```{r, include = FALSE}

library(plotly)
 income <-read.csv(url("https://raw.githubusercontent.com/Shadi-Sadie/Coursera-DataScience/main/Developing%20Data%20Products/Project%202-Week%204/IncomeApp/Income.csv"),header= TRUE)
 colnames(income)<-c("Pop","World.Incom","USIncome")
    income$Pop<-income$Pop*100

```


```{r, echo=FALSE}
plot_ly(data=income,x=~Pop,name="Income distribtuion") %>%
    add_lines(y = ~World.Incom, name= "World income distribution",color = I("Green") ) %>%
    add_lines(y = ~USIncome, name= "US income distribution", color = I("Blue") ) %>%
layout(title ="Income Distrbution", xaxis= list( title = "Percentage population"),
       yaxis = list( title = "Income"))%>%
layout(plot_bgcolor='rgba(245, 245, 245,1)') 
       
```
