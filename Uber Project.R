#Load Packages
library(ggplot2)
library(shiny)
library(dplyr)
library(lubridate)
library(tidyr)
library(hms)
library(DT)
library(leaflet)

#Set Working Directory
setwd('H:\\Desktop Folder\\R Shit\\Uber')

#Load RDS
uber <- readRDS("uber_all.rds")

#Creating Pivot Tables
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

# Define UI for the second page
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

#Define UI for the Third Page
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

#Define UI for the fourth page
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

#Define UI for the Fifth page
ui5 <- fluidPage(
  titlePanel("Leaflet"),
  leafletOutput("map")
)

# Define UI for the first page
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

# Define server logic for all pages
server <- function(input, output, session) {
  
  # Server logic for the first page
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
  
  # Server logic for the Second page
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

#Server Logic for Page 3
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

#Server Logic for Page 4
output$table3 <- DT::renderDataTable({
  trips_by_hour
})

output$table1 <- DT::renderDataTable({
  trips_by_day
})

output$table2 <- DT::renderDataTable({
  trips_by_weekday
})

#Server Logic for Page 5

uber_subset_sampled <- sample_n(uber, 500)

output$map <- renderLeaflet({
  leaflet(uber_subset_sampled) %>%
    addTiles() %>%
    addMarkers(lng = ~Lon, lat = ~Lat,
               popup = paste("Date:", uber_subset_sampled$Date, "<br>",
                             "Time:", uber_subset_sampled$Time))
})

}
# Combine both UIs into a single application
shinyApp(ui = ui, server = server)

