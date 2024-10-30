#Latest Version with text
library(tidyverse)
library(tidycensus)
library(shiny)
library(leaflet)
library(tigris)
library(sp)


#------ Load Massachusetts county shapefiles
#relative paths
#tracts <- tigris::tracts(state = "MA", class = "sp", year = 2019)
#Because tidycensus requires an API key--save this dataframe as .rds
#tracts<-get_acs(state = "MA", geography = "tract",  variables = "B03002_001", year = 2019, geometry = TRUE, cache_table = TRUE)
#tracts1<- tracts %>% select(GEOID,NAME, geometry)
#tracts1<-tracts1 %>% mutate(NAME1 = sub(",?\\s*Massachusetts", "", NAME))
#saveRDS(tracts1, "./nsdoh_profiles_app/tracts.rds")
tracts<-readRDS("tracts.rds")


#----Merge with profiles
profiles<-readRDS("nsdoh_data.rds") #data containig profiles for each census tracts. Also, added county labels
profiles <- profiles %>% select("GEOID","nsdoh_profiles")

#profiles1 <- profiles %>% mutate(countyname = sub(",.*", "", CountyName))

map19<- merge(tracts, profiles, by = "GEOID")

# Create a color palette  for profiles
pal <- colorFactor(
  palette = c("#d62728","salmon1",  
              "#9467bd","#2ca02c",
              "#e377c2",  "#17becf", 
              "#1f77b4", "#ffff00"),   # Colors corresponding to factor levels
  domain = map19$nsdoh_profiles
)

##Prepare results from mbmm 
mbmm<-readRDS("mbmm_result.rds")
#remember that we organized clusters by size
mbmm$nsdoh_profile<- factor(mbmm$cluster, levels = 1:8,
                            labels = c("Profile 1", "Profile 8", "Profile 2",
                                       "Profile 6", "Profile 5", "Profile 7",
                                       "Profile 4", "Profile 3"))


# Define UI for application
ui <- fluidPage(
  titlePanel("Massachusetts Neighborhood Social Determinants of Health (NSDoH) Profiles"),
  
  # Top panel with two columns of text
  fluidRow(
   column(
     width = 10, 
      tags$p(
        "Our study used 2015-2019 census tract-level social determinants of health (SDoH) data 
        from the American Community Survey (ACS) to create neighborhood profiles.",
        style = "font-size: 20px;"  # Increased font size and bold text
      ),
      tags$p(
        "The data presented here are the results of a fully Bayesian Multivariate Bernoulli Mixture Model (MBMM).",
        style = "font-size: 20px;"  # Increased font size and bold text
      )
    ),
    # #Added the specified bullet points using tags$ul() and tags$li() for an unordered list.
    # column(
    #   width = 6,
    #   tags$h4("Additional Information",
    #           style = "font-size: 18px; font-weight: bold;" ), # Increased font size and bold text
    #   tags$ul(
    #     tags$li("Zoom in to your desired area and hover over a location to view information on the census tract, county, and profile assignment.",
    #             style = "font-size: 18px;"),
    #     tags$li("The bar plot on the right shows the estimated posterior probability (from the MBMM) of high exposure to each of 
    #     the 18 SDoH variables, grouped by domain, 
    #             based on the assigned NSDoH profile.",
    #             style = "font-size: 18px;")
    #   )
    # )
#https://shiny.posit.co/r/articles/build/tag-glossary/
   column(
     width = 2,
     tags$a(
       href = "https://www.google.com",  # Replace with your study's link
       target = "_blank",  # Opens in a new tab
       class = "btn btn-primary",  # Bootstrap styling for a button
       "View the Study",
       
     ),
     style = "text-align: right; padding-top: 20px;"  # Align the button to the right
   )
  ),
  
  # Add space between the text and the map
  tags$hr(style = "margin-top: 20px; margin-bottom: 20px;"),
  
  # Main content with map and bar plot
  fluidRow(
    column(
      width = 8,
      leafletOutput("map", width = "100%", height = "500px")
    ),
    column(
      width = 4,
      plotOutput("barplot", height = "400px"),
      tags$h4("Additional Information",
              style = "font-size: 12px; font-weight: bold;" ), # Increased font size and bold text
      tags$ul(
        tags$li("Zoom in to your desired area and hover over a location to view information on the census tract, county, and profile assignment.",
                style = "font-size: 11px;"),
        tags$li("Mouse hover to see the bar plot on the right with the estimated posterior probability (from the MBMM) of high exposure to each of the 
        SDoH indicators included in the model, grouped by domain, 
                based on the assigned NSDoH profile.",
                style = "font-size: 11px;")
      )
    )
  )
)

# Define server logic required to draw the map
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(map19 %>% sf::st_transform('+proj=longlat +datum=WGS84')) %>%
      addTiles() %>% 
      addPolygons(fillColor = ~pal(nsdoh_profiles),  # Set the fill color to profile colors
                  color = "white", weight = 0.5, opacity = 1, fillOpacity = 0.5,  # No fill color, transparent polygons
                  highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.3),  # Highlight on hover
                  layerId = ~NAME1,  # Set layerId to NAME1 to ensure that input$map_shape_mouseover$id captures the ID of the hovered census tract.
                  label =~paste(NAME1, ":", nsdoh_profiles), #show county name: census tract: profile assignment
                  labelOptions = labelOptions(direction = "auto")) %>%
      # Center the map on Massachusetts 
      setView(lng = -71.3824, lat = 42.4072, zoom = 8)  %>% 
      addLegend(
        position = "topright",
        pal = pal,
        values = map19$nsdoh_profiles,
        title = "NSDoH Profile")
  })
  
  # Observe hover events to update the bar plot
  observeEvent(input$map_shape_mouseover, {
    censustract_name <- input$map_shape_mouseover$id  # Get the hovered name
    #print(input$map_shape_mouseover$id)
    
    # Filter the data for the selected census tract
    selected_tract <- map19 %>% filter(NAME1 == censustract_name)
    selected_ct_profile<- selected_tract$nsdoh_profiles
    #print(selected_ct_profile)
    
    
    # Check if a valid census tract was found
    if (nrow(selected_tract) > 0) {
      tract_profile_mbmm <- mbmm %>% filter(nsdoh_profile == selected_ct_profile)
      
      # Render the bar plot
      output$barplot <- renderPlot({
        group_cols<-c("#1f77b4","#2ca02c","salmon1", "#e377c2")
        ggplot(tract_profile_mbmm,aes(x = NSES_VARS, y = theta_kj, fill = NSES_group)) +
          geom_col()  +
          scale_fill_manual(values = group_cols) +
          labs(title = selected_ct_profile, x= "", y = "Probability",fill = "Neighborhood SDoH Domains") +
          theme_classic()+
          theme(strip.text = element_text(size = 10),
                text = element_text(size = 10),
                axis.text.x = element_text(size=10, angle=90, vjust = 0.88, hjust = 0.88),
                axis.title.y = element_text(size = 10, color = "black", face = "bold"),
                axis.text.y = element_text(size=10),
                legend.title = element_text(size = 8, color = "black", face = "bold"),
                legend.text = element_text(size = 8, color = "black"),
                legend.position = "bottom",
                legend.direction = "vertical",
                plot.title = element_text(hjust = 0.5)) +
          guides(fill = guide_legend(nrow = 2))
      })
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

#library(rsconnect)
#rsconnect::deployApp("/Users/carmenrodriguez/OneDrive - Harvard University/nsdoh_profiles_app")
