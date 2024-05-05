#Load Packages
library(ggplot2)
library(shiny)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(hms)

#Set Working Directory
setwd('H:\\Desktop Folder\\R Shit\\Uber')

#Read Data
uber_april <- read.csv("uber-raw-data-apr14.csv")
uber_may <- read.csv("uber-raw-data-may14.csv")
uber_june <- read.csv("uber-raw-data-jun14.csv")
uber_july <- read.csv("uber-raw-data-jul14.csv")
uber_august <- read.csv("uber-raw-data-aug14.csv")
uber_september <- read.csv("uber-raw-data-sep14.csv")

#Bind Data Together
uber <- bind_rows(uber_april, uber_may, uber_june, uber_july, uber_august, uber_september)

#Cleaning Data
uber <- separate(uber, Date.Time, into = c("Date", "Time"), sep = ' ')
uber$Date <- as.Date(uber$Date, format = "%m/%d/%Y")
uber$Time <- as_hms(uber$Time)

uber$weekday <- weekdays(uber$Date)
uber$hour <- hour(uber$Time)
uber$month <- month(uber$Date)
uber$day <- day(uber$Date)
uber$week <- week(uber$Date) - week(floor_date(uber$Date, "month")) + 1

#Create RDS File
saveRDS(uber, "uber_all.rds")
