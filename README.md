# Counting Cars All Data
## Contributors
<p>Blake Oliver</p>
<p>Kevin Akins</p>
<p>Arnav Shretha</p>

## Introduction
<p>We will analyze the speed of cars passing the Speed Radar Sign heading South on 30th St past 24th Ave in Rock Island Il to determine if the Speed Radar Sign actually invokes drivers to slow down.</p>

## Dictionary
1. Speed MPH: The speed at which the vehicle was traveling in Miles per Hour
2. Time: Time of day
3. Temperature: The temperature during the time of recording
4. Group: Group who collected the Data

## 1. Data Collection
1.1 Each of our three group memebers went to the 30th st and 24th ave and recorded 50 cars passing through the radar zone.

1.2 We recorded, in an Excel file:
1) The speed of the vehicle
2) If the light on the radar sign flashed
3) The Vehicle Color
4) The Vehicle Manufacturer
5) The Vehicle Type
6) The Day of the Week
7) The Time of Recording
8) The Weather during the Recording
9) The Temperature during the Recording
10) The collectors Name

1.2 We then combined our data with the data from the other groups.

## 2. Data Manipulation and Cleaning
2.1 Calling the Necessary Packages
```
library(shiny)
library(ggplot2)
library(DT)
library(readxl)
library(dplyr)
library(tidyr)
```

2.2 Reading the Excel files into R
```
group1 <- read_excel('Car (1).xlsx', .name_repair = 'universal')
group2 <- read_excel('Car Data Excel.xlsx', .name_repair = 'universal')
group3 <- read_excel('CarData  (2).xlsx', .name_repair = 'universal')
group4 <- read_excel('counting_cars.xlsx', .name_repair = 'universal')
group5 <- read.csv('IRL_Car_Data.csv')
group6 <- read_excel('MergedCarData.xlsx', .name_repair = 'universal')
group7 <- read_excel('Speed analyst 332 Car Data.xlsx', .name_repair = 'universal')
group8 <- read.csv('UpdatedCarTracking.csv')
```

2.3 Determine which columns all the group's data have in common.

2.4 Rename Columns to be standardized.
```
colnames(group8)[colnames(group8) == "Weather"] <- "Temperature"
colnames(group8)[colnames(group8) == "Speed..mph."] <- "Speed"
colnames(group8)[colnames(group8) == "Time.of.Day"] <- "Time"
colnames(group7)[colnames(group7) == "MPH"] <- "Speed"
colnames(group7)[colnames(group7) == "Time.of.Day"] <- "Time"
colnames(group5)[colnames(group5) == "MPH"] <- "Speed"
colnames(group5)[colnames(group5) == "Time.of.Day"] <- "Time"
colnames(group4)[colnames(group4) == "Temp"] <- "Temperature"
colnames(group4)[colnames(group4) == "MPH"] <- "Speed"
colnames(group1)[colnames(group1) == "Speed.MPH"] <- "Speed"
```

2.5 Remove the date from columns with both time and date in the same column.

```
group1 <- separate(group1, Time, into = c("date", "Time"), sep = ' ')
group2 <- separate(group2, Time, into = c("date", "Time"), sep = ' ')
group3 <- separate(group3, Time, into = c("date", "Time"), sep = ' ')
group4 <- separate(group4, Time, into = c("date", "Time"), sep = ' ')
group6 <- separate(group6, Time, into = c("date", "Time"), sep = ' ')
group7 <- separate(group7, Time, into = c("date", "Time"), sep = ' ')
```

2.6 Remove unnessesary columns.
```
group1 <- select(group1, Time, Speed, Temperature)
group2 <- select(group2, Time, Speed, Temperature)
group3 <- select(group3, Time, Speed, Temperature)
group4 <- select(group4, Time, Speed, Temperature)
group5 <- select(group5, Time, Speed, Temperature)
group6 <- select(group6, Time, Speed, Temperature)
group7 <- select(group7, Time, Speed, Temperature)
group8 <- select(group8, Time, Speed, Temperature)
```

2.7 Add a column to show which group recorded which data.
```
group1$Group <- 1
group2$Group <- 2
group3$Group <- 3
group4$Group <- 4
group5$Group <- 5
group6$Group <- 6
group7$Group <- 7
group8$Group <- 8
```

2.8 Combine the data into one dataset.
```
dataset <- rbind(group1, group2, group3, group4, group5, group6, group7, group8)
```

2.9 Create Variables used for later analysis.

```
mean_speed <- mean(dataset$Speed, na.rm = TRUE)
median_speed <- median(dataset$Speed, na.rm = TRUE)
correlation_coefficient <- cor(dataset$Temperature, dataset$Speed)
linear_model <- lm(Speed ~ Temperature, data = dataset)
```

3. Creating Shiny App
3.1 Create Fluid ggplot for the first page of the Shiny App
```
my_theme <- function() {
  theme_minimal() +
    theme(
      text = element_text(color = "#333333"),  # Set text color
      plot.background = element_rect(fill = "#F5F5F5", color = NA),  # Set plot background color and remove border
      panel.background = element_rect(fill = "#FFFFFF"),  # Set panel background color
      panel.border = element_blank(),  # Remove panel border
      axis.line = element_line(color = "#333333"),  # Set axis line color
      axis.text = element_text(color = "#333333"),  # Set axis text color
      legend.text = element_text(color = "#333333")  # Set legend text color
    )
}
```

3.2 Define UI for Page 2
```
ui2 <- fluidPage(
  titlePanel("Second Page"),
  mainPanel(
    h4("Essay Content:"),
    # Custom output for essay content
    uiOutput("essay_output")
  )
)
```

3.3 Define UI for Page 3
```
ui3 <- fluidPage(
  titlePanel("Third Page"),
  mainPanel(
    h4("Static Graphs:"),
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3"),
    plotOutput("plot4")
  )
)
```

3.4 Define UI for Page 1
```
ui <- fluidPage(
  titlePanel("Explore Car Speed Dataset"),
  tags$head(
    tags$style(HTML("
    .names-panel {
      position: absolute;
      bottom: 10px;
      left: 10px;
    }
  "))
  ),
  tabsetPanel(
    tabPanel("First Page",
             sidebarLayout(
               sidebarPanel(
                 h4('Cars passing near 30th St and 24th Avenue heading south in Rock Island IL'),
                 selectInput('X', 'Choose X', choices = column_names, selected = column_names[1]),
                 selectInput('Y', 'Choose Y', choices = column_names, selected = column_names[3]),
                 selectInput('Splitby', 'Split By', choices = column_names, selected = column_names[3])
               ),
               mainPanel(
                 plotOutput('plot_01'),
                 DTOutput("table_01"),
                 absolutePanel(bottom = 100, left = -280,
                               class = "names-pannel",
                               h5("Names:"),
                               p("Blake Oliver"),
                               p("Arnav Shrestha"),
                               p("Kevin Akins")
                               
                               )
               )
             )),
    tabPanel("Second Page", ui2),
    tabPanel("Third Page", ui3)
  )
)
```

3.5 Define server function for all 3 pages
```
server <- function(input, output, session) {
```

3.6 Define server logic for page 1
```
output$plot_01 <- renderPlot({
    ggplot(dataset, aes_string(x = input$X, y = input$Y, colour = input$Splitby)) +
      geom_point() +
      labs(x = input$X, y = input$Y) +  # Set axis labels dynamically
      my_theme()  # Apply custom theme to the plot
  })
  
  output$table_01 <- renderDT({
    dataset[, c(input$X, input$Y, input$Splitby)]
  }, options = list(pageLength = 10))  # Increase page length for better display
```

3.7 Define server logic for page 2
```
output$essay_output <- renderUI({
    # Read the content of the text file
    text_content <- readLines("essay.txt")
    # Combine the lines into a single string
    essay_text <- paste(text_content, collapse = "\n")
    # Create custom output with pre tag for styling
    tags$pre(style = "white-space: pre-wrap; width: 100%;", essay_text)
  })
```
3.8 Define server logic for page 3 and close the server function
```
output$plot1 <- renderPlot({
    ggplot(dataset, aes(x = Speed)) +
      geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
      labs(
        title = "Density Plot of Speed",
        x = "Speed",
        y = "Density"
      ) +
      theme_minimal() + 
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10) 
      ) +
      scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
      scale_y_continuous(labels = scales::percent_format())
  })
  
  output$plot2 <- renderPlot({
    ggplot(dataset, aes(x = Speed)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
      labs(
        title = "Distribution of Speed",
        x = "Speed",
        y = "Frequency"
      ) +
      theme_minimal() +  
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
        axis.title = element_text(size = 12),  
        axis.text = element_text(size = 10)  
      ) +
      scale_x_continuous(breaks = seq(0, 100, by = 10)) +  
      scale_y_continuous(labels = scales::comma_format())
  })
  
  output$plot3 <- renderPlot({
    ggplot(dataset, aes(x = Temperature, y = Speed)) +
      geom_point(color = "skyblue", alpha = 0.7) +
      labs(
        title = "Scatter Plot of Temperature vs. Speed",
        x = "Temperature",
        y = "Speed"
      ) +
      theme_minimal() + 
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
  
  output$plot4 <- renderPlot({
    ggplot(dataset, aes(x = Temperature, y = Speed)) +
      geom_point(color = "skyblue", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add linear regression line
      labs(
        title = "Scatter Plot of Temperature vs. Speed with Linear Regression",
        x = "Temperature",
        y = "Speed"
      ) +
      theme_minimal() + 
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
}
```

3.9 Create Shiny Application
```
shinyApp(ui = ui, server = server)
```

## Data Analysis

1. Density Plot of Speed
   <div align = "center">
   <img src = "https://github.com/bro121501/DATA332/blob/Counting-Cars-All-Data/Density.png" width = "700")>
   </div>

2. Distribution of Speed
   <div align = "center">
   <img src = "https://github.com/bro121501/DATA332/blob/Counting-Cars-All-Data/Speed.png" width = "700")>
   </div>

3. Scatter Plot of Temperature vs Speed
   <div align = "center">
   <img src = "" width = "700")>
   </div>

4. Scatter Plot of Temperature vs Speed with Linear Regression
   <div align = "center">
   <img src = "https://github.com/bro121501/DATA332/blob/Counting-Cars-All-Data/Regression.png" width = "700")>
   </div>

## Outcome

We believe the evidence we collected, verifies the research we have done and proves the Speed Radar Signs do cause drivers to slow down overall.

The full essay is on the second page here: https://blakeoliver20.shinyapps.io/Cars/
It is also here: https://github.com/bro121501/DATA332/blob/Counting-Cars-IRL/essay.txt
