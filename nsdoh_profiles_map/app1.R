
library(tidyverse)
library(shiny)
library(leaflet)
library(tigris)
library(sp)

# Load Massachusetts county shapefiles
#tracts <- tigris::tracts(state = "MA", class = "sp", year = 2019)
#Use tidycensus instead so that we don't need to zoom
tracts<-get_acs(state = "MA", geography = "tract",  variables = "B03002_001", year = 2019, geometry = TRUE, cache_table = TRUE)
tracts1<- tracts %>% select(GEOID,NAME, geometry)
tracts1<-tracts1 %>% mutate(NAME1 = sub(",?\\s*Massachusetts", "", NAME))

#Merge with profiles
profiles<-readRDS("./nsdoh_profiles_map/nsdoh_data.rds") # data containig profiles for each census tracts. Also, added county labels
profiles <- profiles %>% select("GEOID","nsdop_profiles")
                                #, "CountyName",)
#profiles1 <- profiles %>% mutate(countyname = sub(",.*", "", CountyName))


map19<- merge(tracts1, profiles, by = "GEOID")

# Create a color palette  for profiles
pal <- colorFactor(
  palette = c("#d62728","salmon1",  
              "#9467bd","#2ca02c",
              "#e377c2",  "#17becf", 
              "#1f77b4", "#ffff00"),   # Colors corresponding to factor levels
  domain = map19$nsdop_profiles
)


#mbmm<-readRDS("./nsdoh_profiles_map/mbmm_result.rds")

# Define UI for application
ui <- fluidPage(
  titlePanel("Massachusetts NSDoH Profiles"),
  fluidRow(
    column(
      width = 8,
      leafletOutput("map", width = "100%", height = "600px")
    ),
    column(
      width = 4,
      plotOutput("barplot", height = "400px")
    )
  )
)

# Define server logic required to draw the map
server <- function(input, output, session) {
  
  # # Define Massachusetts bounding box (Longitude and Latitude coordinates)
  # massachusetts_bounds <- list(
  #   lng1 = -73.508142, lat1 = 41.237964,  # Southwest corner of Massachusetts
  #   lng2 = -69.928393, lat2 = 42.886589   # Northeast corner of Massachusetts
  # )
  # 
  output$map <- renderLeaflet({
    leaflet(map19 %>% sf::st_transform('+proj=longlat +datum=WGS84')) %>%
      addTiles() %>% 
      addPolygons(fillColor = ~pal(nsdop_profiles),  # Set the fill color to profile colors
        color = "white", weight = 0.5, opacity = 1, fillOpacity = 0.5,  # No fill color, transparent polygons
        highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.3),  # Highlight on hover
        label =~paste(NAME1, ":", nsdop_profiles), #show county name: census tract: profile assignment
        labelOptions = labelOptions(direction = "auto")) %>%
     # setMaxBounds(
      #  lng1 = massachusetts_bounds$lng1, lat1 = massachusetts_bounds$lat1,
     #   lng2 = massachusetts_bounds$lng2, lat2 = massachusetts_bounds$lat2) %>%
      # Center the map on Massachusetts 
      setView(lng = -71.3824, lat = 42.4072, zoom = 8)  %>% 
      addLegend(
      position = "topright",
      pal = pal,
      values = map19$nsdop_profiles,
      title = "NSDoH Profile")
  })
  
  # Observe hover events to update the bar plot
  observeEvent(input$map_shape_mouseover, {
    censustract_name <- input$map_shape_mouseover$id  # Get the hovered name
    
    # Filter the data for the selected census tract
    selected_ct<- map19[map19$NAME1 == censustract_name, ]
    
    # Extract the profile for the bar plot
    if (nrow(selected_ct) > 0) {
      profile_ct <- unlist(strsplit(selected_county$age_group, ","))
      age_data <- as.data.frame(table(age_groups))
      
      # Render the bar plot
      output$barplot <- renderPlot({
        ggplot(age_data, aes(x = age_groups, y = Freq, fill = selected_county$pop_category)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = c("Low" = "green", "Medium" = "yellow", "High" = "red")) +
          labs(title = paste("Age Group Distribution in", county_name),
               x = "Age Group", y = "Frequency") +
          theme_minimal() +
          theme(legend.position = "none")
      })
    }
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

addProviderTiles(providers$Stamen.TonerLite)

# Things to add
# - panel with the barplot with posterior probability estimates
# - consider making a column instead of a tab
# - make a data frame with model results and paste code from ggplot to generate model results
# - match colors in the map to the barplot


