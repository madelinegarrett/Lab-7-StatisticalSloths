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
Madeline's Question:
Katie's Question: How does energy in the kitchen (sub metering no.1) vary by month in the year of 2009?
Kevin's Question:
Zandy's Question:
