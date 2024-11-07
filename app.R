# Shiny App
# Valerie Janis
# stat 545
#hwk 10

library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)

# Load data
accident <- read_csv("accident1.csv")
View(accident)

# Define server
server <- function(input, output) {
  
  # Line plot for traffic data analysis
  output$fatalPlot <- renderPlot({
    # Filter data for selected states
    filtered_data <- accident %>%
      filter(STATENAME %in% c(input$state1, input$state2))
    
    # Create line plot comparing weather condition to fatalities per 100,000
    ggplot(filtered_data, aes(x = WEATHERNAME, y = FATALS, color = STATENAME, group = STATENAME)) +
      geom_line(stat = "summary", fun = mean) +
      labs(title = "Fatalities per 100,000 by Weather Condition",
           x = "Weather Condition", y = "Fatalities per 100,000") +
      scale_color_manual(values = c("blue", "green"))  # Use different colors for each state
  })
  
  # Pie chart for accident data analysis with colors and legend
  output$distPieChart <- renderPlot({
    # Filter data for selected state and count the frequency of fatality numbers
    pie_data <- accident %>%
      filter(STATENAME == input$statename) %>%
      count(FATALS) %>%
      filter(FATALS %in% 1:5)  # Only keep frequencies 1 to 5
    
    # Create pie chart with colors
    ggplot(pie_data, aes(x = "", y = n, fill = factor(FATALS))) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      labs(fill = "Number of People in Crash", title = paste("Frequency of People in Crashes for", input$statename)) +
      theme_void() +
      scale_fill_manual(values = c("1" = "red", "2" = "orange", "3" = "yellow", "4" = "green", "5" = "blue"))  # Customize colors
  })
  
  # Render the map with crash locations
  output$collisionMap <- renderLeaflet({
    # Filter data based on selected state and collision type
    data <- accident %>%
      filter(STATENAME == input$state_map, MAN_COLLNAME == input$collision_type)
    
    # Check if there's data available
    if (nrow(data) == 0) {
      showNotification("No data available for the selected state and collision type.", type = "warning")
      return(NULL)
    }
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LONGITUDNAME, lat = ~LATITUDENAME,
        popup = ~paste("Collision Type:", MAN_COLLNAME, "<br>Fatalities:", FATALS),
        radius = 5, color = "red", fillOpacity = 0.7
      ) %>%
      setView(lng = mean(data$LONGITUDNAME, na.rm = TRUE), lat = mean(data$LATITUDENAME, na.rm = TRUE), zoom = 6)
  })
  
  # Summary table of collision types for selected state
  output$collisionSummary <- renderTable({
    accident %>%
      filter(STATENAME == input$state_map) %>%
      count(MAN_COLLNAME) %>%
      rename(`Collision Type` = MAN_COLLNAME, `Total Incidents` = n)
  })
}

# Define UI
ui <- fluidPage(
  titlePanel("Analysis of Traffic and Accident Data"),
  sidebarLayout(
    sidebarPanel(
      h3("Select States for Comparison"),
      selectInput("state1", "Select First State:", choices = unique(accident$STATENAME)),
      selectInput("state2", "Select Second State:", choices = unique(accident$STATENAME)),
      
      h3("Select State for Accident Data Analysis"),
      selectInput("statename", "State:", choices = unique(accident$STATENAME)),
      
      h3("Map Filters"),
      selectInput("state_map", "Select State for Map:", choices = unique(accident$STATENAME)),
      selectInput("collision_type", "Collision Type:", 
                  choices = c("Front-to-Front", "Front-to-Rear", "Angle", 
                              "Sideswipe - Same Direction", "Sideswipe - Opposite Direction"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Traffic Data Analysis", 
                 plotOutput("fatalPlot", height = "300px", width = "100%")),
        tabPanel("Accident Data Analysis", 
                 plotOutput("distPieChart", height = "300px", width = "100%")),
        tabPanel("Collision Map", 
                 leafletOutput("collisionMap", height = "500px"),
                 tableOutput("collisionSummary"))  # Summary table for collision types
      )
    )
  )
)

# Run the application
shinyApp(ui = ui, server = server)


