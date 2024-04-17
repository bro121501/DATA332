# Counting Cars
## Contributors
<p>Blake Oliver</p>
<p>Kevin Akins</p>
<p>Arnav Shretha</p>

## Introduction
<p>We will analyze the speed of cars passing the Speed Radar Sign heading South on 30th St past 24th Ave in Rock Island Il to determine if the Speed Radar Sign actually invokes drivers to slow down.</p>

## Dictionary
1. Speed MPH: The speed at which the vehicle was traveling in Miles per Hour
2. Flashing Light: If the light attached to the radar sign flashed. The light only flashed if a vehicle was traveling 10 MPH over the speed limit, or 40 MPH and above
3. Vehicle Color: The color of the vehicle recorded
4. Manufacturer: The creater of the vehicle recorded
5. Vehicle Type: The type of vehicle recorded
6. Day: Day of the week
7. Time: Time of day
8. Weather: The weather during the time of recording
9. Temperature: The temperature during the time of recording
10. Collector Name: The person who recorded the interaction

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


## 2. Data Manipulation
2.1 Calling the Necessary Packages
```
library(shiny)
library(ggplot2)
library(DT)
library(readxl)
library(dplyr)
library(tidyr)
```

2.2 Reading the Excel file into R
```
dataset <- read_excel('Car.xlsx', .name_repair = 'universal')
```

2.3 Create Variables to test if Vehicles travel slower because of the Radar Sign
```
manufacturer_freq <- dataset %>%
  group_by(Manufacturer) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

top_10_fastest <- dataset %>%
  group_by(Manufacturer, Vehicle.Color, Vehicle.Type) %>%
  summarise(Max_Speed = max(Speed.MPH)) %>%
  arrange(desc(Max_Speed)) %>%
  head(10)

avg_speed <- dataset %>%
  group_by(Vehicle.Type) %>%
  summarise(Avg_Speed = mean(Speed.MPH, na.rm = TRUE))

daily_stats <- dataset %>%
  group_by(Day) %>%
  summarise(
    Highest_Speed = max(Speed.MPH),
    Slowest_Speed = min(Speed.MPH),
    Median_Speed = median(Speed.MPH),
    Mean_Speed = mean(Speed.MPH)
  )

daily_stats_long <- daily_stats %>%
  pivot_longer(cols = c(Highest_Speed, Slowest_Speed, Median_Speed, Mean_Speed),
               names_to = "Statistic", values_to = "Value")

avg_speed_manufacturer <- dataset %>%
  group_by(Manufacturer) %>%
  summarise(Avg_Speed = mean(Speed.MPH, na.rm = TRUE))
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
    plotOutput("plot3")
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
    ggplot(avg_speed_manufacturer, aes(x = Manufacturer, y = Avg_Speed)) +
      geom_col(fill = "skyblue", width = 0.7) +
      labs(
        title = "Average Speed by Manufacturer",
        x = "Manufacturer",
        y = "Average Speed (MPH)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1), 
            axis.title = element_text(face = "bold")) +
      coord_flip()
  })
  
  output$plot2 <- renderPlot({
    ggplot(daily_stats_long, aes(x = Day, y = Value, fill = Statistic)) +
      geom_col(position = "dodge", width = 0.7) +
      labs(
        title = "Speed Statistics by Day",
        x = "Day",
        y = "Speed (MPH)",
        fill = "Statistic"
      ) +
      scale_fill_manual(
        values = c("Highest_Speed" = "red", "Slowest_Speed" = "purple", "Median_Speed" = "green", "Mean_Speed" = "blue")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))  # Rotate x-axis labels for better readability
  })
  
  output$plot3 <- renderPlot({
    ggplot(avg_speed, aes(x = Vehicle.Type, y = Avg_Speed)) +
      geom_col(fill = "skyblue", width = 0.5) +  # Use geom_col for vertical bars
      labs(
        title = "Average Speed by Vehicle Type",
        x = "Vehicle Type",
        y = "Average Speed (MPH)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      coord_flip()# Rotate x-axis labels for better readability
  })
}
```

3.9 Create Shiny Application
```
shinyApp(ui = ui, server = server)
```

## Data Analysis

1. Average Speed by Manufacturer
   <div align = "center">
   <img src = "https://github.com/bro121501/DATA332/blob/Counting-Cars-IRL/Speed%20by%20Manu.png" width = "700")>
   </div>

- Cadillac was the only manufacturer above 40 mph on average, but we only recorded two Cadillacs in our sample.

2. Speed Statistics by Day
   <div align = "center">
   <img src = "https://github.com/bro121501/DATA332/blob/Counting-Cars-IRL/Speed%20by%20Day.png" width = "700")>
   </div>

- Wednesday was the fastest day.

3. Speed by Vehicle Type
   <div align = "center">
   <img src = "https://github.com/bro121501/DATA332/blob/Counting-Cars-IRL/Speed%20by%20Type.png" width = "700")>
   </div>

## Outcome

We believe the evidence we collected, verifies the research we have done and proves the Speed Radar Signs do cause drivers to slow down overall.

The full essay is on the second page here: https://blakeoliver20.shinyapps.io/Cars/
It is also here: https://github.com/bro121501/DATA332/blob/Counting-Cars-IRL/essay.txt
