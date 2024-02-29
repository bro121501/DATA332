library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

rm(list = ls()) #Clears variables

setwd('C:\\Users\\bro12\\Desktop\\Desktop\\Data332\\In Class') #Sets working directory
#Must change \ to \\ otherwise get an error

df_truck <- read_excel('data/NP_EX_1-2.xlsx', 
                       sheet = 2, skip = 3, .name_repair = 'universal')
                       #df = dataframe, sheet = sheet number, skip = amount of rows skipped.

#Selecting columns
df <- df_truck[, c(4:15)]
#Deselect Columns
df <- subset(df, select = -c(...10))

#Getting difference in days within a column
date_min <- min(df$Date)
date_max <- max(df$Date)

number_of_days_on_the_road <- date_max - date_min
print(number_of_days_on_the_road)

days <- difftime(max(df$Date), min(df$Date))
print(days)

print(difftime(max(df$Date), min(df$Date)))

number_of_days_recorded <- nrow(df)
total_hours <- sum(df$Hours)
average_hours_per_day_recorded <- round(total_hours / number_of_days_recorded, digits = 3) #digits = rounding to the amount of digits

print(average_hours_per_day_recorded)

df$fuel_cost <- df$Gallons * df$Price.per.Gallon

df$total_cost <- round(df$Tolls + df$Misc + df$fuel_cost, digits = 2)

df[c("warehouse", 'starting_city_state')] <- str_split_fixed(df$Starting.Location, ',', 2) #string split or text to collumns in excel

total_expenses <- sum(df$total_cost)

total_fuel_cost <- round(sum(df$fuel_cost), digits = 2)

total_other_expenses <- round(sum(df$Misc), digits = 2)

total_gallons_consumed <- round(sum(df$Gallons), digits = 3)

df$miles <- df$Odometer.Ending - df$Odometer.Beginning

total_miles <- sum(df$miles)

miles_per_gallon <- round(total_miles / total_gallons_consumed, digits = 3)

total_cost_per_mile <- round(total_expenses / total_miles, digits = 2)

df$starting_city_state <- gsub(',', "", df$starting_city_state) #Deletes , from city_state column

# df[c("col1", 'ccol2')] <- str_split_fixed(df$city_state, '', 2) #string split or text to collumns in excel

df_starting_pivot <- df %>%
  group_by(starting_city_state) %>%
  summarize(count = n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE), #na.rm ignores null values
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
            )
  #%>% creates a pipe

ggplot(df_starting_pivot, aes(x = starting_city_state, y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))

df[c("store", 'ending_city_state')] <- str_split_fixed(df$Delivery.Location, ',', 2)
df$ending_city_state <- gsub(',', "", df$ending_city_state)
df$ending_city_state <- str_trim(df$ending_city_state, side = c("left"))

df_ending_pivot <- df %>%
  group_by(ending_city_state) %>%
  summarize(count = n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE), #na.rm ignores null values
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )
#%>% creates a pipe

ggplot(df_ending_pivot, aes(x = ending_city_state, y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))
