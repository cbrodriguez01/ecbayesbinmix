
library(tidyverse)
library(tidycensus)
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
profiles <- profiles %>% select("GEOID","nsdoh_profiles")
                                #, "CountyName",)
#profiles1 <- profiles %>% mutate(countyname = sub(",.*", "", CountyName))


map19<- merge(tracts1, profiles, by = "GEOID")

# Create a color palette  for profiles
pal <- colorFactor(
  palette = c("#d62728","salmon1",  
              "#9467bd","#2ca02c",
              "#e377c2",  "#17becf", 
              "#1f77b4", "#ffff00"),   # Colors corresponding to factor levels
  domain = map19$nsdoh_profiles
)


#mbmm<-readRDS("./nsdoh_profiles_map/mbmm_result.rds")

# Define UI for application
ui <- fluidPage(
  titlePanel("Massachusetts NSDoH Profiles"),
  leafletOutput("map", width = "100%", height = "800px")
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
      addPolygons(fillColor = ~pal(nsdoh_profiles),  # Set the fill color to profile colors
        color = "white", weight = 0.5, opacity = 1, fillOpacity = 0.5,  # No fill color, transparent polygons
        highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.3),  # Highlight on hover
        label =~paste(NAME1, ":", nsdoh_profiles), #show county name: census tract: profile assignment
        labelOptions = labelOptions(direction = "auto")) %>%
     # setMaxBounds(
      #  lng1 = massachusetts_bounds$lng1, lat1 = massachusetts_bounds$lat1,
     #   lng2 = massachusetts_bounds$lng2, lat2 = massachusetts_bounds$lat2) %>%
      # Center the map on Massachusetts 
      setView(lng = -71.3824, lat = 42.4072, zoom = 8)  %>% 
      addLegend(
      position = "topright",
      pal = pal,
      values = map19$nsdoh_profiles,
      title = "NSDoH Profile")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


# Things to add
# - panel with the barplot with posterior probability estimates
# - consider making a column instead of a tab
# - match colors in the map to the barplot


