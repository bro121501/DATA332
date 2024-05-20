# Final Project
## Contributors
<p>Blake Oliver and Kobe Magee</p>

## Introduction
<p>We are comparing accident data and weather data for the year 2021 to determine the conditions that produce the most accidents.</p>

## Dictionary
1. Fatality: A motor vehicle accident resulting in a death
1.1. Fatal: Another name for a fatality
2. WEATHERNAME.x: The weather condition present during the time of the accident
3. HARM_EVNAME: The description of the person(s) involved in the fatality
4. STATENAME.x: The name of the state the fatality took place
5. CITYNAME: The name of the city the fatality took place
6. MONTHNAME: The name of the Month the fatality took place
7. ROUTENAME: The type of road the fatality took place on
8. ST_CASE: Unique case number for each fatality

## 1. Data Loading, Cleaning, and Optimizing
1.1 Load Packages
```
library(readxl)
library(ggplot2)
library(rsconnect)
library(shiny)
library(DT)
library(dplyr)
library(lubridate)
library(readxl)
library(leaflet)
library(leaflet.extras)
```

1.2 Read CSV into RStudio
```
df_accident <- read.csv("accident_data.csv")
df_weather <- read.csv("weather_data.csv"
```

1.3 Combine dataframes together
```
combined_data <- merge(df_accident, df_weather, by = "ST_CASE")
```
**ST_CASE is a unique key**

1.4 Create RDS
```
saveRDS(combined_data, "data.rds")
```

## 2. Data Manipulation
2.1 Create Filters and Pivot Tables
```
weather_fatalities <- combined_data %>%
  group_by(WEATHERNAME.x) %>%
  summarise(total_fatalities = sum(FATALS)) %>%
  arrange(desc(total_fatalities)) %>%
  top_n(5)

event_fatalities <- combined_data %>%
  group_by(HARM_EVNAME) %>%
  summarise(total_fatalities = sum(FATALS)) %>%
  arrange(desc(total_fatalities)) %>%
  top_n(10)

filtered_combined_data <- combined_data %>%
  filter(STATENAME.x != "Not Reported" & STATENAME.x != "NOT APPLICABLE")

state_fatalities <- filtered_combined_data %>%
  group_by(STATENAME.x) %>%
  summarise(total_fatalities = sum(FATALS)) %>%
  arrange(desc(total_fatalities)) %>%
  top_n(15)

filtered_combined_data <- combined_data %>%
  filter(CITYNAME != "Not Reported" & CITYNAME != "NOT APPLICABLE")

city_fatalities <- filtered_combined_data %>%
  group_by(CITYNAME) %>%
  summarise(total_fatalities = sum(FATALS)) %>%
  arrange(desc(total_fatalities)) %>%
  top_n(15)

# Filter out rows with NA values in the MONTHNAME column
monthly_fatalities <- combined_data[complete.cases(combined_data$MONTHNAME), ]

monthly_fatalities <- monthly_fatalities %>%
  group_by(MONTHNAME) %>%
  summarise(total_fatalities = sum(FATALS)) %>%
  arrange(desc(total_fatalities))

# Reorder levels of MONTHNAME to start from January
monthly_fatalities$MONTHNAME <- factor(monthly_fatalities$MONTHNAME, levels = c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
))

# Aggregate data by route name and calculate total fatalities
route_fatalities <- combined_data %>%
  group_by(ROUTENAME) %>%
  summarise(total_fatalities = sum(FATALS)) %>%
  arrange(desc(total_fatalities)) %>%
  top_n(8)

transport_weather <- combined_data %>%
  filter(HARM_EVNAME == "Motor Vehicle In-Transport")

transport_weather <- transport_weather %>%
  group_by(WEATHERNAME.x) %>%
  summarize(total_fatalities = sum(FATALS)) %>%
  arrange(desc(total_fatalities)) %>%
  top_n(10)

texas_weather <- combined_data %>%
  filter(STATENAME.x == "Texas")

texas_weather <- texas_weather %>%
  group_by(WEATHERNAME.x) %>%
  summarize(total_fatalities = sum(FATALS)) %>%
  arrange(desc(total_fatalities))

houston_weather <- combined_data %>%
  filter(CITYNAME == "HOUSTON")

houston_weather <- houston_weather %>%
  group_by(WEATHERNAME.x) %>%
  summarize(total_fatalities = sum(FATALS)) %>%
  arrange(desc(total_fatalities))

state_highway_weather <- combined_data %>%
  filter(ROUTENAME == "State Highway")

state_highway_weather <- state_highway_weather %>%
  group_by(WEATHERNAME.x) %>%
  summarize(total_fatalities = sum(FATALS)) %>%
  arrange(desc(total_fatalities))

october_weather <- combined_data %>%
  filter(MONTHNAME == "October")

october_weather <- october_weather %>%
  group_by(WEATHERNAME.x) %>%
  summarize(total_fatalities = sum(FATALS)) %>%
  arrange(desc(total_fatalities))

```

## 3. Shiny
3.1 Create Server Backend for each page
```
server <- function(input, output, session) {
  
  # Server logic for the first page

  # Server logic for the Second page
  output$weathercond <- renderPlot({
    ggplot(weather_fatalities, aes(x = reorder(WEATHERNAME.x, total_fatalities), y = total_fatalities, fill = WEATHERNAME.x)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 5 Weather Conditions by Fatalities",
           x = "Weather Conditions",
           y = "Number of Fatalities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
 
  output$motor_transport_weather <- renderPlot({
    ggplot(transport_weather, aes(x = reorder(WEATHERNAME.x, total_fatalities), y = total_fatalities, fill = WEATHERNAME.x)) +
      geom_bar(stat = "identity") +
      labs(title = "Weather Conditions by Motor Vehicle in Transport Fatalities",
           x = "Weather Conditions",
           y = "Number of Fatalities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$texas_weather_fatal <- renderPlot({
    ggplot(texas_weather, aes(x = reorder(WEATHERNAME.x, total_fatalities), y = total_fatalities, fill = WEATHERNAME.x)) +
      geom_bar(stat = "identity") +
      labs(title = "Weather Conditions by Texas Fatalities",
           x = "Weather Conditions",
           y = "Number of Fatalities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$houston_weather_fatal <- renderPlot({
    ggplot(houston_weather, aes(x = reorder(WEATHERNAME.x, total_fatalities), y = total_fatalities, fill = WEATHERNAME.x)) +
      geom_bar(stat = "identity") +
      labs(title = "Weather Conditions by Houston Fatalities",
           x = "Weather Conditions",
           y = "Number of Fatalities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$highway_weather_fatal <- renderPlot({
    ggplot(state_highway_weather, aes(x = reorder(WEATHERNAME.x, total_fatalities), y = total_fatalities, fill = WEATHERNAME.x)) +
      geom_bar(stat = "identity") +
      labs(title = "Weather Conditions by Houston Fatalities",
           x = "Weather Conditions",
           y = "Number of Fatalities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$october_weather_fatalities <- renderPlot({
    ggplot(october_weather, aes(x = reorder(WEATHERNAME.x, total_fatalities), y = total_fatalities, fill = WEATHERNAME.x)) +
      geom_bar(stat = "identity") +
      labs(title = "Weather Conditions by Houston Fatalities",
           x = "Weather Conditions",
           y = "Number of Fatalities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #Server Logic for Page 3
  output$eventsbyfat <- renderPlot({
    ggplot(event_fatalities, aes(x = reorder(HARM_EVNAME, total_fatalities), y = total_fatalities, fill = HARM_EVNAME)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 Types of Events by Fatalities",
           x = "Type of Event",
           y = "Number of Fatalities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #Server Logic for Page 4
  output$statesfat <- renderPlot({
    ggplot(state_fatalities, aes(x = reorder(STATENAME.x, total_fatalities), y = total_fatalities, fill = STATENAME.x)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 15 States with the Most Fatalities",
           x = "State",
           y = "Number of Fatalities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$citiesfat <- renderPlot({
    ggplot(city_fatalities, aes(x = reorder(CITYNAME, total_fatalities), y = total_fatalities, fill = CITYNAME)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 15 Cities with the Most Fatalities",
           x = "City",
           y = "Number of Fatalities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$routefat <- renderPlot({
    ggplot(route_fatalities, aes(x = reorder(ROUTENAME, total_fatalities), y = total_fatalities, fill = ROUTENAME)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 5 Route Type by Total Fatalities",
           x = "Route Type",
           y = "Number of Fatalities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #Server logic for Page 5
  output$monthfat <- renderPlot({
    ggplot(monthly_fatalities, aes(x = MONTHNAME, y = total_fatalities, fill = MONTHNAME)) +
      geom_bar(stat = "identity") +
      labs(title = "Fatalities by Month",
           x = "Month",
           y = "Number of Fatalities") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #Server Logic for Page 6
  output$map <- renderLeaflet({
    # Create Leaflet map
    m <- leaflet(options = leafletOptions(minZoom = 2)) %>% 
      setView(lng = -95.7129, lat = 37.0902, zoom = 4) %>% 
      addTiles()
    
    # Add heatmap layer
    m <- m %>% 
      addHeatmap(data = combined_data, 
                 lng = ~LONGITUD, 
                 lat = ~LATITUDE, 
                 radius = 10,
                 blur = 20)
    
    # Return the map
    m
  })
}
```

3.2 Create UI's for the different pages
```
weather <- fluidPage(
  titlePanel("Graphs"),
  mainPanel(
    tabsetPanel(
      tabPanel("Weather Conditions by Fatalities",
               plotOutput("weathercond")),
      tabPanel("Weather Conditions by Motor Vehicle in Transport Fatalities",
               plotOutput("motor_transport_weather")),
      tabPanel("Weather Conditions by Texas Fatalities",
               plotOutput("texas_weather_fatal")),
      tabPanel("Weather Conditions by Houston Fatalities",
               plotOutput("houston_weather_fatal")),
      tabPanel("Weather Conditions by State Highway Fatalities",
               plotOutput("highway_weather_fatal")),
      tabPanel("Weather Conditions by October Fatalities",
               plotOutput("october_weather_fatalities"))
    )
  )
)

events <- fluidPage(
  titlePanel("Graphs"),
  mainPanel(
    tabsetPanel(
      tabPanel("Events by Fatalities",
               plotOutput("eventsbyfat"))
    )
  )
)

location <- fluidPage(
  titlePanel("Graphs"),
  mainPanel(
    tabsetPanel(
      tabPanel("States with the Most Fatalities",
               plotOutput("statesfat")),
      tabPanel("Cities with the Most Fatalities",
               plotOutput("citiesfat")),
      tabPanel("Route Types with the Most Fatalities",
               plotOutput("routefat"))
    )
  )
)

time <- fluidPage(
  titlePanel("Graphs"),
  mainPanel(
    tabsetPanel(
      tabPanel("Fatalities by Month",
               plotOutput("monthfat"))
    )
  )
)

heat_map <- fluidPage(
  titlePanel("Heat Map"),
  leafletOutput("map")
)

# Define the UI
ui <- fluidPage(
  # Add some CSS for styling
  tags$head(
    tags$style(HTML("
      .title-page {
        text-align: center;
        margin-top: 100px;
      }
      .title {
        font-size: 48px;
        font-weight: bold;
      }
      .subtitle {
        font-size: 24px;
        color: #666;
      }
      .description {
        margin-top: 20px;
        font-size: 18px;
      }
      .paragraph {
        font-size: 12px;
      }
    "))
  ),
  
  # Main UI layout with tabs
  tabsetPanel(
    # Tab for Title Page
    tabPanel(
      "Title Page",
      div(class = "title-page",
          div(class = "title", "2021 Accident Data and Weather Data"),
          div(class = "subtitle", "Blake Oliver, Kobe Magee"),
          div(class = "description", 
              "We are comparing accident data and weather data for the year 2021 to determine 
              the conditions that produce the most accidents."),
          div(class = "paragraph",
              "We chose this data because we believe it is relevant to us. As drivers we want to know what conditions produce the most accidents so we know to stay off the road during those conditions."),
          div(class = "paragraph", 
              "The requirments for this project were to find two data sets and try to find correlations between them. We were to create
              quality charts, a Kanban Board in GitHub, Document our code within a GitHub ReadME, present our findings in a Google Meet, 
              and create a shiny applications with our findings posted to Shiny.io.")
      )
    ),
    tabPanel("Weather", weather),
    tabPanel("Events", events),
    tabPanel("Location", location),
    tabPanel("Time", time),
    tabPanel("Heat Map", heat_map),
  
  )
)
```

3.3 Run Shiny
```
shinyApp(ui = ui, server = server)
```

## Summary
We determined most fatal accidents happen on Clear, Cloudy, and Rainy days in that order.

## Links
Shiny: https://blakeoliver20.shinyapps.io/Accident_Weather/
