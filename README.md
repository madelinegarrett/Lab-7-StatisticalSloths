# Lab7-StatisticalSloths
---
title: "Lab7-StatsSloths"
output: html_document
---

The data set we will be using is called the “Individual household electric power consumption data set”. It consists of measurements of electric power consumption and several electrical quantities from one household with a one-minute sampling rate over a period of almost 4 years. Our team imported the data by placing headers as true, the seperators as ; and the stringsAsFactors = FALSE. We placed NA.strings equal to NA, " ", and ?. That way we can get all the missing values that the data may have. 

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
install.packages("data.table")
library(data.table)
```

```{r}
dataf <- read.table("household_power_consumption 3.txt", header = TRUE, sep = ";", na.strings = c( "NA", " ", "?"), stringsAsFactors = FALSE)

dataf
str(data)

power <- read.table("household_power_consumption.txt", sep=";", header=T, na.strings=c("NA", "", "?"), stringsAsFactors = FALSE) %>%
  separate(Time, into = c("Hour", "Minute", "Second"), sep = ":")


```
* Madeline's Question:
* Katie's Question: How does energy in the kitchen (sub metering no.1) vary by month in the year of 2009?
* Kevin's Question: How does the energy consumption in the heating and cooling systems (sub metering 3) change according to the time of year?
```{r}
power <- read.table("household_power_consumption.txt", sep=";", header=T, na.strings=c("NA", "", "?"), stringsAsFactors = FALSE) %>%
  separate(Time, into = c("Hour", "Minute", "Second"), sep = ":") %>%
  separate(Date, into = c("Day", "Month", "Year"), sep = "/")
  
season <- power%>%
  mutate(season=case_when((Month == 1 | Month == 2 | Month == 3)~"Winter", (Month == 4 | Month == 5 | Month == 6)~"Spring", (Month == 7 | Month == 8 | Month == 9)~"Summer", (Month == 10 | Month == 11 | Month == 12)~"Fall")) %>%
  filter(Sub_metering_3>0) %>%
  group_by(season) %>%
  mutate(total=sum(Sub_metering_3)) %>%
  distinct(total)

options(scipen=500)
ggplot(data = season) + 
  geom_bar(stat = "identity", mapping = aes(season, total, fill=season)) +
  theme(legend.position = "none")

month <- power %>%
  filter(Sub_metering_3>0) %>%
  group_by(Month) %>%
  mutate(total=sum(Sub_metering_3)) %>%
  distinct(total)

ggplot(data = month) + 
  geom_bar(stat = "identity", mapping = aes(Month, total, fill=Month))
```

* Zandy's Question: How much more or the much less power is consumed between the different seasons in a year and does that posiible extra consumption or less consumption lead to wasted power and or money?
