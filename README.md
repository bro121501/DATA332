# Uber Project
## Contributors
<p>Blake Oliver</p>

## Introduction
<p>We will analyze Uber rides from April through September 2014 to gain insights.</p>

## Dictionary
1. Lat: Latitude
2. Lon: Longitude
3. Base: The base that received the Uber request
4. Hour: The hour the request was made
5. Week: The week of the month the request was made

## 1. Data Loading, Cleaning, and Optimizing
1.1 Load Packages
```
library(ggplot2)
library(shiny)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(hms)
```

1.2 Read CSV into RStudio
```
uber_april <- read.csv("uber-raw-data-apr14.csv")
uber_may <- read.csv("uber-raw-data-may14.csv")
uber_june <- read.csv("uber-raw-data-jun14.csv")
uber_july <- read.csv("uber-raw-data-jul14.csv")
uber_august <- read.csv("uber-raw-data-aug14.csv")
uber_september <- read.csv("uber-raw-data-sep14.csv")
```

1.3 Bind all dataframes together
```
uber <- bind_rows(uber_april, uber_may, uber_june, uber_july, uber_august, uber_september)
```

1.4 Clean the Data
```
uber <- separate(uber, Date.Time, into = c("Date", "Time"), sep = ' ')
uber$Date <- as.Date(uber$Date, format = "%m/%d/%Y")
uber$Time <- as_hms(uber$Time)

uber$weekday <- weekdays(uber$Date)
uber$hour <- hour(uber$Time)
uber$month <- month(uber$Date)
uber$day <- day(uber$Date)
uber$week <- week(uber$Date) - week(floor_date(uber$Date, "month")) + 1
```

1.5 Create an RDS file to optimize data load times
```
saveRDS(uber, "uber_all.rds")
```

## 2. Data Manipulation
2.1 Create Pivot Tables
```
trips_by_hour_month <- uber %>%
  group_by(hour, month) %>%
  summarize(trips = n())

trips_by_hour <- uber %>%
  group_by(hour) %>%
  summarize(trips = n())

trips_by_day <- uber %>%
  group_by(day) %>%
  summarize(trips = n())

trips_by_month_by_weekday <- uber %>%
  group_by(month, weekday) %>%
  summarize(trips = n())
trips_by_month_by_weekday$month <- factor(trips_by_month_by_weekday$month)
trips_by_month_by_weekday$weekday <- factor(trips_by_month_by_weekday$weekday, 
                                            levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                                       "Thursday", "Friday", "Saturday"))

trips_by_month <- uber %>%
  group_by(month) %>%
  summarize(trips = n())

trips_by_base_month <- uber %>%
  group_by(Base, month) %>%
  summarize(trips = n())

trips_by_weekday <- uber %>%
  group_by(weekday) %>%
  summarize(trips = n())

trips_by_hour_day <- uber %>%
  group_by(day, hour) %>%
  summarize(trips = n())

trips_by_month_day <- uber %>%
  group_by(month, day) %>%
  summarize(trips = n())

trips_by_base_weekday <- uber %>%
  group_by(Base, weekday) %>%
  summarize(trips = n())

trips_by_month_week <- uber %>%
  group_by(month, week) %>%
  summarize(trips = n())
```

## 3. Shiny
3.1 Create Server Backend for each page
```
server <- function(input, output, session) {

probability <- reactive({
    req(input$day_of_week, input$time)
    
    probability_pivot <- uber %>%
      filter(hour == input$time)
    
    denom <- as.numeric(nrow(probability_pivot))
    
    probability_pivot <- probability_pivot %>%
      filter(weekday == input$day_of_week)
    
    numerator <- as.numeric(nrow(probability_pivot))
    
    # Calculate probability
    probability_value <- round(numerator / denom, digits = 2)
    
    probability_value <- probability_value * 100
    
    probability_value
  })
  
  output$probability_output <- renderText({
    paste("Probability: ", probability(), "%")
  })

output$plot1 <- renderPlot({
    ggplot(trips_by_hour_month, aes(x = hour, y = trips, fill = factor(month))) +
      geom_bar(stat = 'identity', position = "dodge") +
      labs(x = "Hour of Day", y = "Number of Trips", fill = "Month") +
      ggtitle("Trips by Hour of Day per Month") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0, 23, by = 1), labels = sprintf("%02d", seq(0, 23, by = 1))) +
      scale_fill_brewer(palette = "Set1")
  })
  
  output$plot2 <- renderPlot({
    ggplot(trips_by_hour, aes(x = hour, y = trips)) +
      geom_line() +
      geom_point() +
      labs(x = "Hour of Day", y = "Number of Trips") +
      ggtitle("Trips Every Hour") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0, 23, by = 1), labels = sprintf("%02d", seq(0, 23, by = 1))) +
      scale_y_continuous(labels = scales::comma) +
      theme(panel.grid.minor.x = element_blank())
  })
  
  output$plot3 <- renderPlot({
    ggplot(trips_by_day, aes(x = day, y = trips)) +
      geom_line() +
      geom_point() +
      labs(x = "Day of Month", y = "Number of Trips") +
      ggtitle("Trips per Day") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0, 31, by = 1), labels = sprintf("%02d", seq(0, 31, by = 1))) +
      scale_y_continuous(labels = scales::comma) +
      theme(panel.grid.minor.x = element_blank())
  })

output$plot4 <- renderPlot({
  ggplot(trips_by_month_by_weekday, aes(x = month, y = trips, fill = factor(weekday))) +
    geom_bar(stat = 'identity', position = "dodge") +
    labs(x = "Month", y = "Number of Trips", fill = "Weekday") +
    ggtitle("Trips by Month per Weekday") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
})

output$plot5 <- renderPlot({
  ggplot(trips_by_month, aes(x = month, y = trips)) +
    geom_line() +
    geom_point() +
    labs(x = "Month", y = "Number of Trips") +
    ggtitle("Trips Every Month") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 23, by = 1), labels = sprintf("%02d", seq(0, 23, by = 1))) +
    scale_y_continuous(labels = scales::comma) +
    theme(panel.grid.minor.x = element_blank())
})

output$plot6 <- renderPlot({
  ggplot(trips_by_base_month, aes(x = Base, y = trips, fill = factor(month))) +
    geom_bar(stat = 'identity', position = "dodge") +
    labs(x = "Base", y = "Number of Trips", fill = "Month") +
    ggtitle("Trips by Base and Month") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(labels = scales::comma) +
    theme(panel.grid.minor.x = element_blank())
})

output$plot01 <- renderPlot({
  ggplot(trips_by_hour_day, aes(x = hour, y = day, fill = trips)) +
    geom_tile() +
    scale_fill_gradient(low = "lightblue", high = "darkred") +  # Adjust color gradient
    labs(x = "Hour of the Day", y = "Day of the Month", fill = "Number of Trips") +
    ggtitle("Trips by Hour and Day") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 23, by = 1), labels = sprintf("%02d", seq(0, 23, by = 1))) +
    scale_y_continuous(breaks = seq(1, 31, by = 1), labels = sprintf("%02d", seq(1, 31, by = 1))) +
    theme(panel.grid.minor.x = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    guides(fill = "none")
})

output$plot02 <- renderPlot({
  ggplot(trips_by_month_day, aes(x = month, y = day, fill = trips)) +
    geom_tile() +
    scale_fill_gradient(low = "lightblue", high = "darkred") +  # Adjust color gradient
    labs(x = "Month", y = "Day of the Month", fill = "Number of Trips") +
    ggtitle("Trips by Month and Day") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 23, by = 1), labels = sprintf("%02d", seq(0, 23, by = 1))) +
    scale_y_continuous(breaks = seq(1, 31, by = 1), labels = sprintf("%02d", seq(1, 31, by = 1))) +
    theme(panel.grid.minor.x = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    guides(fill = "none")
})

output$plot03 <- renderPlot({
  ggplot(trips_by_month_week, aes(x = month, y = week, fill = trips)) +
    geom_tile() +
    scale_fill_gradient(low = "lightblue", high = "darkred") +  # Adjust color gradient
    labs(x = "Month", y = "Week of the Month", fill = "Number of Trips") +
    ggtitle("Trips by Month and Week") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 23, by = 1), labels = sprintf("%02d", seq(0, 23, by = 1))) +
    scale_y_continuous(breaks = seq(1, 31, by = 1), labels = sprintf("%02d", seq(1, 31, by = 1))) +
    theme(panel.grid.minor.x = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    guides(fill = "none")
})

output$plot04 <- renderPlot({
  ggplot(trips_by_base_weekday, aes(x = Base, y = weekday, fill = trips)) +
    geom_tile() +
    scale_fill_gradient(low = "lightblue", high = "darkred") +  # Adjust color gradient
    labs(x = "Base", y = "Day of the Week", fill = "Number of Trips") +
    ggtitle("Trips by Base and Weekday") +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    guides(fill = "none")
})

output$table3 <- DT::renderDataTable({
  trips_by_hour
})

output$table1 <- DT::renderDataTable({
  trips_by_day
})

output$table2 <- DT::renderDataTable({
  trips_by_weekday
})

uber_subset_sampled <- sample_n(uber, 500)

output$map <- renderLeaflet({
  leaflet(uber_subset_sampled) %>%
    addTiles() %>%
    addMarkers(lng = ~Lon, lat = ~Lat,
               popup = paste("Date:", uber_subset_sampled$Date, "<br>",
                             "Time:", uber_subset_sampled$Time))
})
}
```

3.2 Create UI's for the different pages
```
ui2 <- fluidPage(
  titlePanel("Graphs"),
  mainPanel(
    tabsetPanel(
      tabPanel("Trips by Hour of the Day per Month",
               plotOutput("plot1")),
      tabPanel("Trips by Hour",
               plotOutput("plot2")),
      tabPanel("Trips per Day",
               plotOutput("plot3")),
      tabPanel("Trips by Month per Weekday",
               plotOutput("plot4")),
      tabPanel("Trips per Month",
               plotOutput("plot5")),
      tabPanel("Trips by Base per Month",
               plotOutput("plot6"))
    )
  )
)

ui3 <- fluidPage(
  titlePanel("Heatmaps"),
  mainPanel(
    tabsetPanel(
      tabPanel("Trips by Hour and Day",
               plotOutput("plot01")),
      tabPanel("Trips by Month and Day",
               plotOutput("plot02")),
      tabPanel("Trips by Month and Week",
               plotOutput("plot03")),
      tabPanel("Trips by Bases and Day of Week",
               plotOutput('plot04'))
    )
  )
)

ui4 <- fluidPage(
  titlePanel("Tables"),
  mainPanel(
    tabsetPanel(
    tabPanel("Trips by Hour",
             dataTableOutput("table3")),
    tabPanel("Trips by Day of the Month",
                dataTableOutput("table1")),
    tabPanel("Trips by Weekday",
                dataTableOutput("table2"))
    )
  )
)

ui5 <- fluidPage(
  titlePanel("Leaflet"),
  leafletOutput("map")
)

ui <- fluidPage(
  titlePanel("Explore Uber Data"),
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
    tabPanel("Ride Prediction",
             sidebarLayout(
               sidebarPanel(
                 h4('Predict whether you will get a ride depending on the Day!'),
                 selectInput('day_of_week', 'Day of the Week', choices = unique(uber$weekday)),
                 selectInput("time", "Hour", choices = unique(uber$hour))
               ),
               mainPanel(
                 textOutput("probability_output")
               )
             )),
    tabPanel("Graphs", ui2),
    tabPanel("Heatmaps", ui3),
    tabPanel("Tables", ui4),
    tabPanel("Leaflet", ui5)
  )
)
```

3.3 Run Shiny
```
shinyApp(ui = ui, server = server)
```

## Links
Shiny: https://blakeoliver20.shinyapps.io/Uber/
