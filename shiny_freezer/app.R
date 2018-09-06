# load libraries
library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(sf)

# run code in preparation for shiny app (file: run_shiny_freezer)
source("data/run_shiny_freezer.R")

# Add basemap
leaflet.tile <- "Esri.OceanBasemap" 

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Groundfish Analysis Team Freezer Inventory"),
   
   # Sidebar with a select box for selecting a specific species  
   sidebarLayout(
      sidebarPanel(
        selectInput("select", label = h3("Select species"), 
          choices = inventory_sf$species, 
          selected = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("mymap", height=1000)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$value <- renderPrint({ input$select })
  
   
   output$mymap <- renderLeaflet({
     m <- leaflet() %>% 
       addProviderTiles(leaflet.tile) %>% 
       addMarkers(data = filter(inventory_sf, species %in% input$select), popup = ~as.character(inv_no), label = ~as.character(year), clusterOptions = markerClusterOptions())
       #setView(lng = -73.935242, lat= 40.730610, zoom = 10)
     m
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

