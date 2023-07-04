---
title: "Cyclistic"
author: "Jun Sung Son"
date: "2022-08-21"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Introduction and background
Cyclistic is bike share company in Chicago. Lily Moreno, director of marketing, believes growth of number of annual members will be key to future growth. Moreno want to design the strategies to convert casual rider to annual members. The purpose of this analysis is to gain insight how annual member and casual riders use Cyclistic bike differently. 
### ASK
Lily Moreno wants to find the answers for three questions for future marketing program which are
1. How do annual members and casual riders use Cyclistic bikes differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?

Lily Moreno has assigned to answer first question: How do annual members and casual riders use Cyclistic bike differently?

Objective of this analysis is to answer how casual members and annual members use Cyclistic differently and design the strategies to convert casual rider to annual members.

Stakeholders for the analysis are Lily Moreno, the director of marketing team, Cyclistic marketing analysis team, and Cyclistic executive team.

## Prepare
I will use the Cyclistic's historical trip data to analyze and identify trends. The data has been  made available by Motivate International Inc. under this [license](https://ride.divvybikes.com/data-license-agreement "license"). I downloaded the data and stored to separate folder in csv(Comma-separated values) format and the data includes columns ride_id,rideable_type,started_at,ended_at, start_station_name,start_station_id,end_station_name,end_station_id,start_lat,start_lng,end_lat,end_lng,and member_casual. I downloaded only the data that Cyclistic collected from August 2021 to July 2022 for the analysis because they were most recent data. I downloaded the data from Cyclistic's historical data. The data-privacy issues prohibit from using riders’ personally identifiable information. I will use the R programming language to prepare,process, analyze, and share. 

## Process
Insatall and load necesary packages.
```{r}
library(tidyverse) 
library(lubridate)  
library(ggplot2)
getwd() 

```
Import Cyclistic data to R studio.
```{r}
Aug_2021<-read.csv("C:/Users/eagls/Desktop/Cyclistic/202108-divvy-tripdata/August_2021.csv")
Sep_2021<-read.csv("C:/Users/eagls/Desktop/Cyclistic/202109-divvy-tripdata/September_2021.csv")
Oct_2021<-read.csv("C:/Users/eagls/Desktop/Cyclistic/202110-divvy-tripdata/October_2021.csv")
Nov_2021<-read.csv("C:/Users/eagls/Desktop/Cyclistic/202111-divvy-tripdata/November_2021.csv")
Dec_2021<-read.csv("C:/Users/eagls/Desktop/Cyclistic/202112-divvy-tripdata/December_2021.csv")
Jan_2022<-read.csv("C:/Users/eagls/Desktop/Cyclistic/202201-divvy-tripdata/January_2022.csv")
Feb_2022<-read.csv("C:/Users/eagls/Desktop/Cyclistic/202202-divvy-tripdata/February_2022.csv")
Mar_2022<-read.csv("C:/Users/eagls/Desktop/Cyclistic/202203-divvy-tripdata/March_2022.csv")
Apr_2022<-read.csv("C:/Users/eagls/Desktop/Cyclistic/202204-divvy-tripdata/April_2022.csv")
May_2022<-read.csv("C:/Users/eagls/Desktop/Cyclistic/202205-divvy-tripdata/May_2022.csv")
Jun_2022<-read.csv("C:/Users/eagls/Desktop/Cyclistic/202206-divvy-tripdata/June_2022.csv")
Jul_2022<-read.csv("C:/Users/eagls/Desktop/Cyclistic/202207-divvy-tripdata/July_2022.csv")
```
Merge previous 12 month data into one data frame.
```{r}
all_trip<-bind_rows(Aug_2021,Sep_2021,Oct_2021,Nov_2021,Dec_2021,Jan_2022,Feb_2022,Mar_2022,Apr_2022,May_2022,Jun_2022,Jul_2022)
```
Remove the columns that unnecessary for analysis.
```{r}
all_trip<-all_trip %>%  
  select(-c(start_station_name,start_station_id,end_station_name,end_station_id,start_lat, start_lng, end_lat,end_lng,X))
```
Clean up and add data to prepare for analysis.
```{r}
colnames(all_trip)
nrow(all_trip)  
dim(all_trip)  
head(all_trip)  
str(all_trip)  
summary(all_trip)  

```
Convert type of started_at and ended_at columns in to POSIXct.
```{r}
all_trip$started_at<-as.POSIXct(x=c(all_trip$started_at), format = "%m/%d/%Y %H:%M", tz = "GMT")
all_trip$ended_at<-as.POSIXct(x=c(all_trip$ended_at), format = "%m/%d/%Y %H:%M", tz = "GMT")

```
Add columns date,month,day,year, day_of week.
```{r}
all_trip$date <- as.Date(all_trip$started_at)
all_trip$month <- format(as.Date(all_trip$started_at), "%m")
all_trip$day <- format(as.Date(all_trip$started_at), "%d")
all_trip$year <- format(as.Date(all_trip$started_at), "%Y")
all_trip$day_of_week <- format(as.Date(all_trip$started_at), "%A")

```

Add column ride_length to calculate length of ride.
```{r}
all_trip$ride_length <- difftime(all_trip$ended_at,all_trip$started_at)
```
Convert into numeric
```{r}
all_trip$ride_length <- as.numeric(as.character(all_trip$ride_length))
is.numeric(all_trip$ride_length)
```

Remove the data which rideable type is other than electric bike or classic bike or ride length is less than 0 second.
```{r}
all_trip_v2 <- all_trip[!(all_trip$rideable_type == "docked_bike" | all_trip$ride_length<=0),]
```
## Analyze

Find mean,median,maximum,and minimum ride length
```{r}
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual, FUN = mean)
```
```{r}
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual, FUN = median)
```
```{r}
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual, FUN = max)
```
```{r}
aggregate(all_trip_v2$ride_length ~ all_trip_v2$member_casual, FUN = min)

```
## Share

Share graphs what I found from Analyze section.
```{r}
all_trip_v2$day_of_week <- ordered(all_trip_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```
```{r}
all_trip_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)		
```
```{r}
all_trip_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

```

From this graph, we know that there are more number of rides for Cyclistic members on week days. However, there are more number of rides for casual riders on weekends.
```{r}
all_trip_v2%>%
  group_by(member_casual)%>%
  summarise(average_duration = mean(ride_length))%>%
  ggplot(aes(x=member_casual,y =     average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
  
```

We can observe that casual riders have longer average ride length compare to Cyclistic members.
```{r}
all_trip_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

```
From above graph, we can gain information that casual riders' average of ride length of each day is longer than Cyclistic memeber. In addition, the average of duration is higher on weekends. 

## Act

1.Cyclistic members use more Cyclistic bikes compare to casual riders on weekdays. However, The casual riders use more Cyclistic bikes on the weekend.
2.Casual riders use Cyclistic bikes longer than members
3.The average duration of casual riders is longer on weekend.

Top three recommendations based on analysis are
1.Provide time coupons for members who reach specific duration.
2.Separate the membership such as week day plans, weekend plans, and everyday plans so that customers can use Cyclistic bikes for various purposes.
3.Open the join riding bike clubs service for members.
