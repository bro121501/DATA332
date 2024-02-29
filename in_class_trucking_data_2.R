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

df_truck_0001 <- read_excel('data/truck data 0001.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_0369 <- read_excel('data/truck data 0369.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1226 <- read_excel('data/truck data 1226.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1442 <- read_excel('data/truck data 1442.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1478 <- read_excel('data/truck data 1478.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1539 <- read_excel('data/truck data 1539.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1769 <- read_excel('data/truck data 1769.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_pay <- read_excel('data/Driver Pay Sheet.xlsx', .name_repair = 'universal')

df <- rbind(df_truck_0001, df_truck_0369, df_truck_1226, df_truck_1442, df_truck_1478, 
            df_truck_1539, df_truck_1769)

df_starting_pivot <- df %>%
  group_by(Truck.ID) %>%
  summarize(count = n())

df <- left_join(df, df_pay, by = c('Truck.ID'))

df <- subset(df, select = -c(...2))
df <- subset(df, select = -c(Summary, ...10))

df$trip_cost <- (df$Odometer.Ending - df$Odometer.Beginning) * df$labor_per_mil

df_total_pay_per_driver <- df %>%
  group_by(Truck.ID) %>%
  summarize(
    total_pay = sum(trip_cost), 
    first = first(first), 
    last = last(last)
  )


df_total_pay_per_driver <- unite(df_total_pay_per_driver, name, first, last, sep = " ")

custom_colors <- c("red", "blue", "green", "orange", "purple", "cyan", "magenta")

ggplot(df_total_pay_per_driver, aes(x = name, y = total_pay, fill = Truck.ID)) +
  geom_col() +
  scale_fill_manual(values = custom_colors) +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))



# Create a ggplot object
ggplot(df_total_pay_per_driver, aes(x = name, y = total_pay, fill = name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +  # Set the custom colors
  theme_minimal()

graph <- data.frame(
  name = c(df_total_pay_per_driver$name),
  total_pay_axis = df_total_pay_per_driver$total_pay)
)

ggplot(graph, aes(x = name, y = total_pay_axis, fill = name)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = total_pay_axis), vjust = -.5, color = "black", size = 3) +
  labs(x = "Name of Driver", y = "Total Pay") +
  theme_minimal() +
  theme(legend.position = "none")
