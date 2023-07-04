이 분석은 coursera에 case 스터디로 제공된 프로젝트입니다
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
### 소개 및 배경
Cyclistic은 미국 시카고에 있는 공유자전거 회사입니다. 마케팅부 책임자 Lily Moreno는 미래의 성장은 연간회원수에 걸렸다고 생각합니다. 
Moreno는 일반 라이더들을 연간 회원으로 전환시킬 전략을 세우기를 원합니다. 이 분석의 목적은 연간회원과 일반 라이더들이 Cylistic 자전거를 다르게 사용하는지 인사이트를 얻기 위함입니다

### 질문
Lily Moreno는 
연간 회원과 일반 라이더들이 어떡해 다르게 Cyclistic 자전거를 다르게 사용하는지, 
왜 일반 라이더들이 Cyclistic 연간 회원권을 사야하는지,
어떡해하면 디지털 미디어를 사용하여 일반라이들에게 회원이 될수 있을정도의 영향을 줄수있을지
에 대한 대답을 미래의 마케팅을 위해 원합니다.

그래서 Lily Moreno는 첫 번째 질문인 연간 회원과 일반 라이더들이 어떡해 다르게 Cyclistic 자전거를 다르게 사용하는지에 대한 답을 찾기를 원합니다

이 분석의 목표는 연간 회원과 일반 라이더들이 어떡해 다르게 Cyclistic 자전거를 다르게 사용하는지 대한 대답과 그에 맞는 전략을 짜서 일반 라이더들을 연간회원으로 이끄는 것입니다.

이 분석의 이해 관계자는 마케팅팀 책임자인 Lily Moreno, Cyclistic 마케팅 분석 팀 및 Cyclistic 경영진입니다.

### 준비
저는 Cyclistic의 사용기록 데이터를 이용해서 트렌드를 나타내고 분석할것입니다. 사용기록 데이터는 이 [license](https://ride.divvybikes.com/data-license-agreement "license")를 통해 Motivation International Inc.에서 받았습니다. 
데이터를 제 컴퓨터에 csv파일 형태로 다운로드 받았고 이 파일의 형태는 ride_id,rideable_type,started_at,ended_at, start_station_name,start_station_id,end_station_name,end_station_id,start_lat,start_lng,end_lat,end_lng,and member_casual행들로 구성되어있습니다. 
저는 데이터를 Cyclistic이 모은 제일 최근 데이터인 2021년 8월 부터 2022년 7월까지의 데이터만 가지고 분석하였습니다. 
데이터 개인정보 보호 문제로 인해 라이더들의 개인 식별 정보를 사용할 수 없습니다. 이번 분석을 위해 저는 R 프로그래밍 언어를 사용하여 데이터를 준비, 처리, 분석 및 공유할 것입니다.

### 처리
필요 R패키지를 설치를 하고 불러줍니다
```{r}
library(tidyverse) 
library(lubridate)  
library(ggplot2)
getwd() 

```
다운받은 Cyclistic 데이터들을 R studio로 불러옵니다
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
최근 1년간의데이터를 하나로 합쳐줍니다
```{r}
all_trip<-bind_rows(Aug_2021,Sep_2021,Oct_2021,Nov_2021,Dec_2021,Jan_2022,Feb_2022,Mar_2022,Apr_2022,May_2022,Jun_2022,Jul_2022)
```
필요없다고 판단되는 행들을 지워줍니다
```{r}
all_trip<-all_trip %>%  
  select(-c(start_station_name,start_station_id,end_station_name,end_station_id,start_lat, start_lng, end_lat,end_lng,X))
```
start_at과 ended_at행을 POSTIXct타입으로 변경해줍니다
```{r}
all_trip$started_at<-as.POSIXct(x=c(all_trip$started_at), format = "%m/%d/%Y %H:%M", tz = "GMT")
all_trip$ended_at<-as.POSIXct(x=c(all_trip$ended_at), format = "%m/%d/%Y %H:%M", tz = "GMT")

```
날짜,월,일,년,요일 행을 추가해 줍니다
```{r}
all_trip$date <- as.Date(all_trip$started_at)
all_trip$month <- format(as.Date(all_trip$started_at), "%m")
all_trip$day <- format(as.Date(all_trip$started_at), "%d")
all_trip$year <- format(as.Date(all_trip$started_at), "%Y")
all_trip$day_of_week <- format(as.Date(all_trip$started_at), "%A")

```
ride_length 행을 추가해줍니다
```{r}
all_trip$ride_length <- difftime(all_trip$ended_at,all_trip$started_at)
```
ride_length 행을 숫자형태로 바꿔줍니다
```{r}
all_trip$ride_length <- as.numeric(as.character(all_trip$ride_length))
is.numeric(all_trip$ride_length)
```
전기자전거 일반자전거 혹은 라이드 시간이 0초 이하로 측정된 데이터를 지워줍니다
```{r}
all_trip_v2 <- all_trip[!(all_trip$rideable_type == "docked_bike" | all_trip$ride_length<=0),]
```
### 분석
연간회원과 일반 라이더들의 이용시간 평균,중간값,최대값,최소값을 구해줍니다
평균값
<img width="446" alt="image" src="https://github.com/json9101/R-Programming/assets/57518426/bd53eafa-eed8-4234-b78a-fa043a642c6d">
중간값
<img width="434" alt="image" src="https://github.com/json9101/R-Programming/assets/57518426/aec38359-007e-4fab-9e48-9fdec7205332">
최대값
<img width="457" alt="image" src="https://github.com/json9101/R-Programming/assets/57518426/db7a36fd-d5c8-417e-a52b-592c0e98e7e0">
최소값
<img width="460" alt="image" src="https://github.com/json9101/R-Programming/assets/57518426/31bfcd49-d707-4c8a-aea7-505208fb70e7">
각 요일의 라이드 숫자와 평균시간
<img width="834" alt="image" src="https://github.com/json9101/R-Programming/assets/57518426/2b2bcc97-fec4-48e0-91ca-bc219bdf3a4c">

### 공유
제가 분석섹션에서 구한값을 그래프형식으로 보여줍니다

<img width="494" alt="image" src="https://github.com/json9101/R-Programming/assets/57518426/2a0b4fc5-4976-4e79-bbe1-5236d8864663">

이 그래프를 통해 평일에는 Cyclistic회원들의 사용율이 높지만 주말에는 일반 라이더들의 이용율이 더 높습니다

<img width="662" alt="image" src="https://github.com/json9101/R-Programming/assets/57518426/ab2829a9-34e7-45f0-a3c0-00381960b58c">

위의 그래프를 통해 일반 라이더들이 평균적으로 연간회원보더 더 운행한다는것을 알수있습니다

<img width="503" alt="image" src="https://github.com/json9101/R-Programming/assets/57518426/8747b4be-d21d-424f-8d63-9bc8c311dc8b">

위 그래프에서 우리는 일반 이용자들의 평균 주행 시간이 Cyclistic 회원보다 길다는 정보를 얻을 수 있습니다. 또한, 평균 주행 시간은 주말에 더 높습니다.

### 행동
이번 분석을 통해 인사이트를 얻은것은 
1.주중에는 Cyclistic 회원들이 일반 이용자들보다 더 많이 Cyclistic 자전거를 이용합니다. 그러나 주말에는 일반 이용자들이 더 많이 Cyclistic 자전거를 이용합니다.
2.일반 이용자들은 회원들보다 Cyclistic 자전거를 더 오래 이용합니다.
3.일반 이용자들의 평균 이용 시간은 주말에 더 깁니다.

위의 분석을 기반한 세가지 권고사항은
1.특정 이용 시간을 달성한 회원들을 위한 시간 쿠폰을 제공하기.
2.주중 플랜, 주말 플랜 및 매일 플랜과 같이 회원십을 구분하여 고객들이 다양한 목적으로 Cyclistic 자전거를 이용할 수 있도록 하기.
3.연간 회원가입시에 20 퍼센트 할인 이벤트 진행하기
