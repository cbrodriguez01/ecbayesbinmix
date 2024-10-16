
library(tidyverse)
library(shiny)
library(leaflet)
library(tigris)
library(sp)

# Load Massachusetts county shapefiles
tracts <- tracts(state = "MA", class = "sp", year = 2019)

#Merge with profiles
profiles<-readRDS("./Data/mbmm_clusters_19eth_sized.rds") #add the names of the counties and in the map -- add county:profile
profiles <- profiles %>% select("GEOID","nsdop_profiles")
map19<- merge(tracts, profiles, by = "GEOID")

# Create a color palette  for profiles
pal <- colorFactor(
  palette = c("#d62728","salmon1",  
              "#9467bd","#2ca02c",
              "#e377c2",  "#17becf",
              "#1f77b4", "#ffff00"),   # Colors corresponding to factor levels
  domain = map19@data$nsdop_profiles
)



# Define UI for application
ui <- fluidPage(
  titlePanel("Massachusetts NSDoH Profiles"),
  leafletOutput("map", width = "100%", height = "600px")
)

# Define server logic required to draw the map
server <- function(input, output, session) {
  
  # Define Massachusetts bounding box (Longitude and Latitude coordinates)
  massachusetts_bounds <- list(
    lng1 = -73.508142, lat1 = 41.237964,  # Southwest corner of Massachusetts
    lng2 = -69.928393, lat2 = 42.886589   # Northeast corner of Massachusetts
  )
  
  output$map <- renderLeaflet({
    leaflet(map19) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(nsdop_profiles),  # Set the fill color to profile colors
        color = "white", weight = 0.5, opacity = 1, fillOpacity = 0.5,  # No fill color, transparent polygons
        highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.3),  # Highlight on hover
        label =~paste(NAME, ": ", nsdop_profiles), 
        labelOptions = labelOptions(direction = "auto") 
      ) %>%
      setMaxBounds(
        lng1 = massachusetts_bounds$lng1, lat1 = massachusetts_bounds$lat1,
        lng2 = massachusetts_bounds$lng2, lat2 = massachusetts_bounds$lat2
      ) %>%
      setView(lng = -71.3824, lat = 42.4072, zoom = 8)  # Center the map on Massachusetts
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


#County Name
#Towns in county














install.packages("rmapshaper")
library(shiny)
library(leaflet)
library(tigris)
library(sp)
library(rmapshaper)  # To simplify the boundaries

# Load Massachusetts county shapefiles
counties <- counties(state = "MA", class = "sp")

# Simplify the shapefiles to remove coastal lines
counties_simplified <- ms_simplify(counties, keep = 0.5)  # Adjust 'keep' for the level of simplification

# Add a hypothetical factor variable (population category for demonstration)
counties_simplified@data$pop_category <- factor(sample(c("Low", "Medium", "High"), nrow(counties_simplified@data), replace = TRUE))

# Create a color palette based on the factor variable 'pop_category'
pal <- colorFactor(
  palette = c("green", "yellow", "red"),   # Colors corresponding to factor levels
  domain = counties_simplified@data$pop_category
)

# Define UI for application
ui <- fluidPage(
  titlePanel("Massachusetts Counties Map (Simplified)"),
  leafletOutput("map", width = "100%", height = "600px")
)

# Define server logic
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(counties_simplified) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(pop_category),  # Set the fill color based on the factor variable
        color = "white", weight = 2, opacity = 1, fillOpacity = 0.7,  # Adjust polygon properties
        highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7),
        label = ~paste(NAME, ": ", pop_category),  # Show county name and population category
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      setView(lng = -71.3824, lat = 42.4072, zoom = 7)  # Center the map on Massachusetts
  })
}

# Run the application 
shinyApp(ui = ui, server = server)









library(shiny)
library(leaflet)
library(tigris)
library(sp)
library(rmapshaper)
library(dplyr)  # For data manipulation

# Load Massachusetts census tract shapefiles
census_tracts <- tracts(state = "MA", class = "sp")

# Simplify the census tract shapefiles to remove unnecessary details (including coastal lines)
census_tracts_simplified <- ms_simplify(census_tracts, keep = 0.05)

# Add a hypothetical factor variable (e.g., population category) for the census tracts
census_tracts_simplified@data$pop_category <- factor(sample(c("Low", "Medium", "High"), nrow(census_tracts_simplified@data), replace = TRUE))

# Create a lookup table mapping census tracts to counties and towns (replace with real data if available)
lookup_table <- data.frame(
  tract = census_tracts_simplified@data$NAME,  # Census tract ID
  county = sample(c("County A", "County B", "County C"), nrow(census_tracts_simplified@data), replace = TRUE),  # Hypothetical counties
  towns = sample(c("Town X, Town Y", "Town Z, Town W", "Town Q, Town R"), nrow(census_tracts_simplified@data), replace = TRUE)  # Hypothetical towns
)

# Create a color palette based on the factor variable 'pop_category'
pal <- colorFactor(
  palette = c("green", "yellow", "red"),   # Colors corresponding to factor levels
  domain = census_tracts_simplified@data$pop_category
)

# Define UI for application
ui <- fluidPage(
  titlePanel("Massachusetts Census Tracts Map (Simplified)"),
  sidebarLayout(
    sidebarPanel(
      textInput("tract_input", "Enter Census Tract Number:", ""),
      actionButton("search_btn", "Search"),
      br(),
      h4("Search Result"),
      verbatimTextOutput("search_result")
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet(census_tracts_simplified) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(pop_category),  # Set the fill color based on the factor variable
        color = "white", weight = 1, opacity = 1, fillOpacity = 0.7,  # Adjust polygon properties
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7),
        label = ~paste(NAME, ": ", pop_category),  # Show tract name (or GEOID) and population category
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      setView(lng = -71.3824, lat = 42.4072, zoom = 8)  # Center the map on Massachusetts
  })
  
  # Perform search when the user clicks the search button
  observeEvent(input$search_btn, {
    search_tract <- input$tract_input
    
    # Find the county and towns corresponding to the input census tract
    result <- lookup_table %>%
      filter(tract == search_tract)
    
    if (nrow(result) == 0) {
      output$search_result <- renderText("No matching census tract found.")
    } else {
      output$search_result <- renderText({
        paste("Census Tract:", result$tract, "\nCounty:", result$county, "\nTowns:", result$towns)
      })
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




