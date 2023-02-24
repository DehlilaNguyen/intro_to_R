---
title: "Nguyen.Assignment_01"
author: "DEHLILA HOA NGUYEN"
date: "2023-02-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

knitr::opts_knit$set(root.dir = "/path/to/working/directory")
# Project/Homework 2

install.packages("readr")

library(readxl)

library(readr)

library(tibble)

install.packages("tidyverse")

install.packages("rlang")

```{r}

# Read data on tibble format 

library(tidyverse)

Project02_SuperStoreOrders<- read_csv("Project02_SuperStoreOrders.csv")

```

##I tried to do this code on my co-worker's work desktop and it worked for the read_csv, but on my laptop, I'm #experiencing issues with read_csv, so the only code that work on my laptop is if I do it to read(xlsm). If I run this code on my laptop, then it will work. But I tried to do the majority of my hw at work this week bc we had some issue with out site, so my code run find under read.csv since I dont have that issue. This upcomming week I will be st office hours for your help in helping me fix this issue. 

#Project02_SuperStoreOrders <- read_xlsx(paste0(getwd(), #"/../Input/Project02_SuperStoreOrders.xlsx"),
                        sheet = "Sheet1")

# Question 1:

# Create a summary statistic and provide youranalysis/observations.

```{r}

summary(Project02_SuperStoreOrders)

```

The database represented the sales data of a company over the period from 2011 to 2014. It presents 51290 observations and 21 variables. Above are the values of the descriptive statistics for each of the variables (average, median or mode, quantile etc..)

# Question 2:

# Analyze how the orders are made over the years and by segments. Plot the below graph. Make sure the name of "X-axis" should match. Colors could vary.

```{r}
library(ggplot2)
library(dplyr)

df <- Project02_SuperStoreOrders %>%
  group_by(year,segment) %>% 
  summarise(total_count=n(),.groups = 'drop') %>% 
  as.data.frame()

ggplot(df, aes(x = year, y = total_count, fill =
                 segment)) + geom_bar(stat = "identity", position = "stack") + labs(x = "Year of Order", y = "Count", fill = "segment")
```

Based on the graph generated, orders have increased each year in all segments. 

# Question 3:

# Which segment is the best seller in the entire dataset? How do you compare the segments? Provide a plot to confirm your claim about the best seller.

```{r}

total_orders <-
  aggregate(Project02_SuperStoreOrders$quantity, by=list(segment=Project02_SuperStoreOrders$segment),
            FUN=sum) 

colnames(total_orders) <- c("segment", "quantity")

best_seller <- total_orders[which.max(total_orders$quantity),"segment"]

ggplot(total_orders, aes(x=segment, y=quantity, fill=segment))+geom_bar(stat="identity")

```

The best segment was generated using the amount of product ordered per
segment. Basically, we made a sum per segment of the quantity, this allowed us
to conclude that the best sales segment is "Consumer". The graph above
generated this conclusion.

# Question 4:

# Create a bar chart of regional orders. Provide your analysis of which region receives the most orders and which region receives the fewest. Ensure you provide the numbers to validate that the longer bar corresponds to the maximum #number of orders. Invert the axis coordinates and display the bars in the reverse order of the number of orders by region. #Replace the bars' color with the color of your choosing. I used the color blue in the graph I made. Additionally, ensure that the axis is labeled "Region."

```{r}
order_count <- as.data.frame(table(Project02_SuperStoreOrders$region))
colnames(order_count) <- c("region", "count")
#order_count <- order_count[order(order_count$count),]

ggplot(order_count, aes(y=reorder(region, count), x=count))+
  geom_bar(stat="identity",fill="blue")+
  geom_text(aes(label = count), hjust = -0.50)+
  labs(y = "Region", x = "Count")

```

Based on the information generated, the region with the most sales is "Central" with 1111 orders and the one with the least sales is "Canada" with 384 orders.

#Question 5:

#Use the below code snippet and complete the rest to create the profits graph by region.
# *Hint*: You are creating a bar plot in a polar coordinate system.

```{r}
library(dplyr) 
Project02_SuperStoreOrders %>% 
  mutate(total_profit = sum(profit)) %>% 
  group_by(region) %>% 
  summarize(total_profit = sum(profit)) %>%
  ggplot(., aes(x=region, y=total_profit, fill=region)) + 
  geom_bar(stat="identity") + 
  coord_polar()

```

# Question 6:

# Include a density plot to infer the total profit made. Include the orders that have profits within the range of [-125, 125].

```{r}
library(dplyr) 
Project02_SuperStoreOrders %>%
  filter(profit<125 , profit>-125) %>%
  select("profit") %>%
  ggplot(., aes(x=profit)) + geom_density()
```


#Density function represented the realized profit over a certain period of time and the observed peak represents my profit #concentration area. The area below the density function curve represents the total realized profit.

#Honors Pledge:
#As a student of the Dr. Robert B. Pamplin Jr. School of Business I have read and strive to uphold the University’s Code of Academic #Integrity and promote ethical behavior. In doing so, I pledge on my honor that I have not given, received, or used any unauthorized #materials or assistance on this examination or assignment.  I further pledge that I have not engaged in cheating, forgery, or #plagiarism and I have cited all appropriate sources.


#Student Signature:  ___DN_____________________________________





