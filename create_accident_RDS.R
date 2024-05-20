library(readxl)
library(ggplot2)
library(rsconnect)
library(shiny)
library(DT)
library(dplyr)
library(lubridate)
library(readxl)

setwd("C:/Users/kobem/Documents/GitHub/DATA-332/Crime Final")

#Read in data sets
df_accident <- read.csv("accident_data.csv")
df_weather <- read.csv("weather_data.csv")

#Combine data sets
combined_data <- merge(df_accident, df_weather, by = "ST_CASE")

# Delete columns STATE.y and STATENAME.y
combined_data <- subset(combined_data, select = -c(STATE.y, STATENAME.y, PERNOTMVIT, VE_FORMS, PVH_INVL, PERMVIT, TWAY_ID2, RD_OWNER,
                                                   RD_OWNER, RD_OWNERNAME, NHS, NHSNAME, SP_JUR, SP_JURNAME, MILEPT, MILEPTNAME, LATITUDENAME, 
                                                   LONGITUDNAME, RELJCT1, RELJCT1NAME, RELJCT2, RELJCT2NAME, WRK_ZONENAME, RAIL, RAILNAME, 
                                                   NOT_HOURNAME, NOT_MINNAME, ARR_HOURNAME, ARR_MINNAME, HOSP_HRNAME, HOSP_MNNAME))

saveRDS(combined_data, "data.rds")
