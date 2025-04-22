#Latest Version with text
library(tidyverse)
library(tidycensus)
library(shiny)
library(leaflet)
library(tigris)
library(sp)
library(sf)


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
profiles<-readRDS("nsdoh_data_042025.rds") #data containig profiles for each census tracts. Also, added county labels
profiles <- profiles %>% select("GEOID","nsdoh_profiles")


map19<- merge(tracts, profiles, by = "GEOID")
map19$nsdoh_profiles <- factor(map19$nsdoh_profiles, labels = c("Profile 1", "Profile 2", "Profile 3",
                                                                              "Profile 4", "Profile 5"))


# Create a color palette  for profiles
pal <- colorFactor(
  palette = c("#d62728","salmon1",  
              "#2ca02c", "#e377c2",  "#1f77b4"),   # Colors corresponding to factor levels
  domain = map19$nsdoh_profiles
)



##Prepare results from mbmm 
mbmm<-readRDS("mbmm_results_042025.rds")
#remember that we organized clusters by size
mbmm$nsdoh_profile<- factor(mbmm$cluster_size, 
                            labels = c("Profile 1", "Profile 2", "Profile 3",
                                       "Profile 4", "Profile 5"))
#write.csv(mbmm, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Tables/theta_kj_042025.csv")
#2020 Urban boundaries for MA from MA GIS Data Hub (https://gis.data.mass.gov)
urban_boundaries <- st_read("./Urban_Boundaries_2020/Urban_Boundaries_2020.shp") %>% select(geometry)
urban_boundaries1 <- urban_boundaries %>% sf::st_transform('+proj=longlat +datum=WGS84')
#urban2<-urban_areas(year = 2019) --how GEOID10?


#MassGIS Data: 2020 Environmental Justice Populations--BLOCK GROUP LEVEL!
# (i) the annual median household income is not more than 65 percent of the statewide annual median household income; 
# (ii) minorities comprise 40 percent or more of the population; 
# (iii) 25 percent or more of households lack English language proficiency; 
# (iv) minorities comprise 25 percent or more of the population and the annual median household income 
# of the municipality in which the neighborhood is located does not exceed 150 percent of the statewide annual median household income.

EJP<-st_read("./ej2020/EJ_POLY.shp") 
EJP1<-EJP %>% filter(EJ_CRITE_1 %in% c(2,3)) %>% select(geometry)  #filter to include BGs that meet all 3 criteria
EJP1 <- EJP1 %>% sf::st_transform('+proj=longlat +datum=WGS84')

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
        href = "http://arxiv.org/abs/2412.07134",  # Replace with your study's link
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
      leafletOutput("map", width = "100%", height = "500px"),
      checkboxInput("show_urban", "Show 2020 Urban Boundaries", value = FALSE),
      checkboxInput("show_EJP", "Show 2020 Environmental Justice Populations", value = FALSE)
    ),
    column(
      width = 4,
      plotOutput("barplot", height = "400px"),
      tags$h4("Additional Information",
              style = "font-size: 12px; font-weight: bold;" ), # Increased font size and bold text
      tags$ul(
        tags$li("Zoom in to your desired area and hover over a location to view information on the census tract, county, and profile assignment.",
                style = "font-size: 11px;"),
        tags$li("Click to see the bar plot on the right with the estimated posterior probability (from the MBMM) of high exposure to each of the 
        SDoH indicators included in the model, grouped by domain, 
                based on the assigned NSDoH profile.",
                style = "font-size: 11px;"),
        tags$li("Use check boxes to layer the 2020 urban boundaries or block groups boundaries for  Environmental Justice Populations 
                (defined as: limited English household(E) + minority population (M) AND  M + E + median household income < 65% of the statewide annual median household income) for Massachusetts (MassGIS Data Hub).",
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
                  color = "white", weight = 0.5, opacity = 1, fillOpacity = 0.4,  # No fill color, transparent polygons
                  highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.6),  # Highlight on hover
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
  
  # Observe the checkbox input to add/remove urban boundaries layer
  observe({
    proxy <- leafletProxy("map")  # Use leafletProxy to modify the existing map
    
    if (input$show_urban) {
      # Add urban boundaries layer
      proxy %>%
        addPolygons(
          data = urban_boundaries1,
          color = "black", weight = 2, opacity = 0.7, fill = FALSE,
          group = "Urban Boundaries"
        ) #the name of the group the newly created layers should belong to
    } else {
      # Remove urban boundaries layer
      proxy %>% clearGroup("Urban Boundaries")
    }
  })
  # Observe the checkbox input to add/remove EJP layer
  observe({
    proxy <- leafletProxy("map")  # Use leafletProxy to modify the existing map
    
    if (input$show_EJP) {
      # Add urban boundaries layer
      proxy %>%
        addPolygons(
          data = EJP1, color = "orangered",
          weight = 2, opacity = 0.9, 
          fillColor = "orangered",
          group = "Environmental Justice Populations"
        ) #the name of the group the newly created layers should belong to
    } else {
      # Remove  layer
      proxy %>% clearGroup("Environmental Justice Populations")
    }
  })
  # Observe hover events to update the bar plot
  observeEvent(input$map_shape_click, {
    censustract_name <- input$map_shape_click$id  # Get the hovered name
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
        ggplot(tract_profile_mbmm,aes(x = NSDOH_VARS, y = theta_kj, fill = NSDOH_group)) +
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
