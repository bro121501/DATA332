library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(here)
library(skimr)
library(kableExtra)
library(fastmap)
library(knitr)
library(reshape2)

rm(list = ls()) #Clears variables

setwd('H:\\Desktop Folder\\R Shit\\Student') #Sets working directory
#Must change \ to \\ otherwise get an error

student <- read_excel('data/Student.xlsx', .name_repair = 'universal')
registration <- read_excel('data/Registration.xlsx', .name_repair = 'universal')
course <- read_excel('data/Course.xlsx', .name_repair = 'universal')

joined_data <- left_join(student, registration, by = "Student.ID")
joined_data <- left_join(joined_data, course, by = "Instance.ID")

joined_data_pivot <- joined_data %>%
  group_by(Title) %>%
  summarize(count_by_major = n())

color = c("red", "orange", "yellow", "green", "blue", "violet")

{plot <- ggplot(joined_data_pivot, aes(x = Title)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Number of Majors", x = "Major", y = "Count") +
  theme_minimal()
plot}

ggsave("H:\\Desktop Folder\\R Shit\\Student\\Graphs\\Count of Majors.png", plot, width = 6, height = 4, dpi = 300)

{plot <- ggplot(joined_data_pivot, aes(x = Title, y = count_by_major, fill = color)) +
  geom_col() +
  labs(x = "Major", y = "Amount of Students in Major") +
  theme(legend.position = "none")

plot}

ggsave("H:\\Desktop Folder\\R Shit\\Student\\Graphs\\Students per Majors.png", plot, width = 6, height = 4, dpi = 300)

joined_data <- separate(joined_data, Birth.Date, into = c("year", "month", "day"), sep = "-")

birth_year_pivot <- joined_data %>%
  group_by(year) %>%
  summarize(count_by_year = n())

{plot <- ggplot(birth_year_pivot, aes(x = year)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Birth Year of the Students", x = "Birth Year", y = "Amount of Students Born") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1))
plot}

ggsave("H:\\Desktop Folder\\R Shit\\Student\\Graphs\\Count of Birth Years.png", plot, width = 6, height = 4, dpi = 300)

{plot <- ggplot(birth_year_pivot, aes(x = year, y = count_by_year)) +
  geom_col() +
  labs(title = "Amount of Students born per year", x = "Birth Year", y = "Amount of Students Born") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1))
plot}

ggsave("H:\\Desktop Folder\\R Shit\\Student\\Graphs\\Amount of students born per year.png", plot, width = 6, height = 4, dpi = 300)

total_cost_per_major <- joined_data %>%
  group_by(Payment.Plan, Title) %>%
  summarize(sum_column = sum(Cost))

data <- data.frame(
  Category = total_cost_per_major$Title[1:6],
  Segment1 = total_cost_per_major$sum_column[1:6],
  Segment2 = total_cost_per_major$sum_column[7:12]
)

# Melt the data frame to long format

data_melted <- melt(data, id.vars = "Category")
data_melted$variable <- as.character(data_melted$variable)

data_melted$variable[data_melted$variable == "Segment1"] <- "No Payment Plan"
data_melted$variable[data_melted$variable == "Segment2"] <- "Payment Plan"

{plot <- ggplot(data_melted, aes(x = Category, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(position = position_stack(vjust = 0.5), aes(label = value), 
            color = "red", fontface = "bold", size = 5, show.legend = FALSE) +
  labs(title = "Total Cost per Major Segmented by Payment Plan",
       x = "Majors",
       y = "Total Cost",
       fill = "Key") +
  scale_fill_manual(values = c("No Payment Plan" = "blue", "Payment Plan" = "gold")) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_minimal()
plot}

ggsave("H:\\Desktop Folder\\R Shit\\Student\\Graphs\\Cost per Major.png", plot, width = 6, height = 4, dpi = 300)

balance_due_per_major <- joined_data %>%
  group_by(Payment.Plan, Title) %>%
  summarize(sum_column = sum(Balance.Due))

data <- data.frame(
  Category = balance_due_per_major$Title[1:6],
  Segment1 = balance_due_per_major$sum_column[1:6],
  Segment2 = balance_due_per_major$sum_column[7:12]
)

data_melted <- melt(data, id.vars = "Category")
data_melted$variable <- as.character(data_melted$variable)

data_melted$variable[data_melted$variable == "Segment1"] <- "No Payment Plan"
data_melted$variable[data_melted$variable == "Segment2"] <- "Payment Plan"

{plot <- ggplot(data_melted, aes(x = Category, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(position = position_stack(vjust = 0.5), aes(label = value), 
            color = "red", fontface = "bold", size = 5, show.legend = FALSE) +
  labs(title = "Total Balance due per Major Segmented by Payment Plan",
       x = "Majors",
       y = "Total Cost",
       fill = "Key") +
  scale_fill_manual(values = c("No Payment Plan" = "blue", "Payment Plan" = "gold")) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_minimal()
plot}
ggsave("H:\\Desktop Folder\\R Shit\\Student\\Graphs\\Balance per major.png", plot, width = 6, height = 4, dpi = 300)
