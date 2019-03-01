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
### Madeline's Question:
### Katie's Question: How does energy in the kitchen (sub metering no.1) vary by month in the year of 2009?
Answer: There does not seem to be a strict pattern month by month. However, January has the highest amount of usage for the year of 2009. The lowest amount of usage in the kitchen for this year is in July. I created a data set using filter and mutate for Sub_metering_1 in the year of 2009 to create a graph and determine which month had the highest usage and which had the lowest.
Results for Monthly Usage in the Kitchen in 2009:
* January: 5143
* February: 3376
* March: 4506
* April: 3897
* May: 3514
* June: 3025
* July: 1763
* August: 2996
* September: 4502
* October: 4168
* November: 4215
* December: 3864
```{r}

power <- read.table("household_power_consumption.txt", sep=";", header=T, na.strings=c("NA", "", "?"), stringsAsFactors = FALSE) %>%
  separate(Date, into = c("Day", "Month", "Year"), sep = "/", convert = TRUE)

power
```
```{r}
year <- power %>%
  filter(Sub_metering_1>0) %>%
  filter(Year == 2009) %>%
  mutate(total = sum(Sub_metering_1))
year
```
```{r}
ggplot(data = year)+
  geom_bar(mapping=aes(x=as.factor(Month), fill=Sub_metering_1), color = "blue")+
  labs(title = "Energy Consumption in the Kitchen by Month in 2009", x = "Month", y = "Voltage")
```
* I found this question interesting because I believed kitchen energy consumption would vary slightly. However, I am suprised that the usage has no trend from month to month.
* In order to conduct my exploratory data analysis I began by reading the descriptions of each variable in the data set. I then decided to use the seperate function in order to seperate the date into three new columns: Month, Day and Year. I then decided that it would be interesting to see if energy usage in any of the Sub_metering zones varied by month. I decided to look into energy usage by month in the kitchen for the year 2009. I chose 2009 mainly because it was 10 years ago and I was curious. I created a data set to reflect the year of 2009 and used mutate to create a total usage column. For the plot I used a geom_bar function that shows the energy consumption by month in the kitchen for the year of 2009. I added a title as well as changing the names of the x and y axis. 
### Kevin's Question: How does the energy consumption in the heating and cooling systems (sub metering 3) change according to the time of year?
Answer: Of the four seasons, winter saw the highest level of energy consumption put towards heating and cooling, while summer had the lowest level. I found this by adding the values from the Sub_metering_3 column, the one about heating and cooling energy usage, from each season. I used the values from Jan, Feb, and Mar for winter, Apr, May, and Jun for spring, Jul, Aug, and Sept for summer, and Oct, Nov, and Dec for fall. I also found and included the results for energy usage by month to add support to my findings.

Results for Energy Usage: 
* Winter - 3824424
* Spring - 3370710
* Summer - 2485316
* Fall - 3554717

```{r}
power <- read.table("household_power_consumption.txt", sep=";", header=T, na.strings=c("NA", "", "?"), stringsAsFactors = FALSE) %>%
  separate(Time, into = c("Hour", "Minute", "Second"), sep = ":") %>%
  separate(Date, into = c("Day", "Month", "Year"), sep = "/", convert = TRUE)
  
season <- power%>%
  mutate(season=case_when((Month == 1 | Month == 2 | Month == 3)~"Winter", (Month == 4 | Month == 5 | Month == 6)~"Spring", (Month == 7 | Month == 8 | Month == 9)~"Summer", (Month == 10 | Month == 11 | Month == 12)~"Fall")) %>%
  filter(Sub_metering_3>0) %>%
  group_by(season) %>%
  mutate(total=sum(Sub_metering_3)) %>%
  distinct(total)

options(scipen=500)
ggplot(data = season) + 
  geom_bar(stat = "identity", mapping = aes(season, total, fill=as.factor(season))) +
  theme(legend.position = "none") +
  labs(title = "Energy Consumption on Temp by Season", x = "Season", y = "Energy")

month <- power %>%
  filter(Sub_metering_3>0) %>%
  group_by(Month) %>%
  mutate(total=sum(Sub_metering_3)) %>%
  distinct(total)

ggplot(data = month) + 
  geom_bar(stat = "identity", mapping = aes(Month, total, fill=as.factor(Month))) + 
  scale_fill_discrete(name = "Month", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  theme(legend.position = "bottom") +
  labs(title = "Energy Consumption on Temp by Month", x = "Month", y = "Energy")
```
* This question is interesting because it allows you to see what time of year you use the most energy for heating and cooling your home. This can be helpful because it can show you when you spend the most on heating and cooling.

* One of the data analysis techniques I used was the separate function. I used this to separate the date from one column with the day, month, and year all included to three columns, one for each value. I then used mutate to split the months and their values into the seasons using the technique I described above in my answer. Then I grouped by the season and mutated a new column that showed the total usage per season. I did much of the same to find the values per month only I did not mutate a column for season. I used bar graphs to display my results visually. I used the fill to change the color of the bar for each season/month and changed the plot and axes titles to more clearly display what they represent. For the season graph, I hid the legend as it was redundant. For the month graph, I removed the x axis ticks and text and moved the legend to the bottom and changed the legend labels to the corresponding months.

### Zandy's Question: How much more or how much less power is consumed between the different seasons in a year and does that posiible extra consumption or less consumption lead to wasted power and or money?
Answer: This household in France was very efficient in their energy consumption. While a typical household wastes about 30% of the energy they use, this household uses 64.2% of the energy energy of a typical household and so yearly saves about $903 on their energy consumption which is far less than a typical household.

Average energy consumed per season in kilowatts per minute:
* Winter = 1.4176230
* Spring = 1.1031940
* Summer = 0.7266815
* Fall = 1.1346098

Total energy consumed per season and yearly in kilowatts per minute:
* Winter = 700,181
* Spring = 578,245.7
* Summer = 376,631.8
* Fall = 581,966.3

Magnitude of energy consumed in Winter vs other seasons:
* Winter vs. Spring = 1.4176230/1.1031940 = 1.285 = 128.5% higher
* Winter vs. Summer = 1.4176230/0.7266815 = 1.95 = 195% higher
* Winter vs. Fall = 1.4176230/1.1346098 = 1.249 = 124.9% higher


Other Statistics:
* total typical French household energy consumption yearly = 14,520 KW/h * 60 = 871,200
* households yearly consumption = (700,181+578,245.7+376,631.8+581,966.3)/4 = 559,256.2
* percentage of yearly consumption = 559,256.2/871,200 = 0.642 = 64.2%
* french yearly household expenditure = $210.27 per month * 12 = $2523.24
* households yearly expenditure = $1620

```{r}
power <- read.table("file:///C:/Users/zandy/Downloads/household_power_consumption/household_power_consumption.txt", sep=";", header=T, na.strings=c("NA", "", "?"), stringsAsFactors = FALSE) %>%
  separate(Time, into = c("Hour", "Minute", "Second"), sep = ":") %>%
  separate(Date, into = c("Day", "Month", "Year"), sep = "/", convert = TRUE)
  
season <- power %>%
  mutate(season=case_when((Month == 12 | Month == 1 | Month == 2)~"Winter", (Month == 3 |     Month == 4 | Month == 5)~"Spring", (Month == 6 | Month == 7 | Month == 8)~"Summer", (Month   == 9 | Month == 10 | Month == 11)~"Fall")) %>%
  filter(Global_active_power>0) %>%
  group_by(season) %>%
  mutate(average=mean(Global_active_power)) %>%
  distinct(average)

ggplot(data = season) +
  geom_bar(stat = "identity", mapping = aes(x = season, y = average, fill =as.factor(season))) +
  labs(tile = "Average Energy consumption in a minute(in kilowatts)", x = "season", y = "Average")


season_total <- power %>%
  mutate(season=case_when((Month == 12 | Month == 1 | Month == 2)~"Winter", (Month == 3 |     Month == 4 | Month == 5)~"Spring", (Month == 6 | Month == 7 | Month == 8)~"Summer", (Month   == 9 | Month == 10 | Month == 11)~"Fall")) %>%
  filter(Global_active_power>0) %>%
  group_by(season) %>%
  mutate(total=sum(Global_active_power)) %>%
  distinct(total)

  ```
* Why this question is important: This question is imortant because the answer to this question can help people to either become more efficient with their energy consumption and cut back or to cut back on energy consumption to save money.
* What I did: I used the seperate function to determine different time frames by splitting time into years, months, days, hours, minutes, and seconds. I used the mutate function to split the months into seasons with Winter being months 12, 1,2: Spring being months 3,4,5: Summer being months 6,7,8: and Fall being months 9,10,11. I then grouped the seasons together and determined the average and total energy consumption per season and added the total energy consumption up per season and divided by 4  to get a total yearly average energy consumption. I used a bar graph of average energy consumption per minutes in kilowatts per season to visually depict my data with the colors being connected to a different season and labels and a tile to more clearly show what each axis represents and what the graph represents. I finally used statistics to determine how much more or how much less energy was used in the different seasons and used some outside information to determine a typical house's energy consumption and expenditure to determine how efficient this house was with consuming energy.
