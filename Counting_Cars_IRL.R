library(shiny)
library(ggplot2)
library(DT)
library(readxl)
library(dplyr)
library(tidyr)

# Set working directory and read dataset
setwd('H:\\Desktop Folder\\R Shit\\Car')
dataset <- read_excel('Car.xlsx', .name_repair = 'universal')
dataset <- na.omit(dataset)
column_names <- colnames(dataset)  # Get column names for input selection

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

# Define a custom theme for ggplot
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

# Define UI for the second page
ui2 <- fluidPage(
  titlePanel("Second Page"),
  mainPanel(
    h4("Essay Content:"),
    # Custom output for essay content
    uiOutput("essay_output")
  )
)

# Define UI for the third page
ui3 <- fluidPage(
  titlePanel("Third Page"),
  mainPanel(
    h4("Static Graphs:"),
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3")
  )
)

# Define UI for the first page
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

# Define server logic for all pages
server <- function(input, output, session) {
  
  # Server logic for the first page
  output$plot_01 <- renderPlot({
    ggplot(dataset, aes_string(x = input$X, y = input$Y, colour = input$Splitby)) +
      geom_point() +
      labs(x = input$X, y = input$Y) +  # Set axis labels dynamically
      my_theme()  # Apply custom theme to the plot
  })
  
  output$table_01 <- renderDT({
    dataset[, c(input$X, input$Y, input$Splitby)]
  }, options = list(pageLength = 10))  # Increase page length for better display
  
  # Server logic for the second page
  # Render essay content
  output$essay_output <- renderUI({
    # Read the content of the text file
    text_content <- readLines("essay.txt")
    # Combine the lines into a single string
    essay_text <- paste(text_content, collapse = "\n")
    # Create custom output with pre tag for styling
    tags$pre(style = "white-space: pre-wrap; width: 100%;", essay_text)
  })
  
  
  # Server logic for the third page
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


# Combine both UIs into a single application
shinyApp(ui = ui, server = server)
