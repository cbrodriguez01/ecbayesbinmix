#=======================================================
# ACS wave 2015-2019 MAPS
#We want to map profiles assignments
#Author: Carmen Rodriguez
#Last Updated: 10/13/24
#=======================================================
##----Load packages----
library(tidyverse)
library(ggplot2)
library(tidycensus)
library(sf)
library(tmap)
library(RColorBrewer)
library(cartogram)
library(geodaData)

##-----------Old maps from ISBA's poster----

data19<-readRDS("./Data/mbmm_clusters_19eth_sized.rds")
data19$census_tract<-str_sub(data19$GEOID,start=6, end = 11) 
data19$countycode<-str_sub(data19$GEOID,start=3, end = 5)

### Map showing the distribution 
temp<-get_acs(state = "MA", geography = "tract", 
              variables = "B03002_001", year = 2019, geometry = TRUE, cache_table = TRUE)

temp1<- temp %>% select(GEOID, NAME, geometry, estimate)

dat19map<-data19 %>% select(GEOID,nsdop_profiles)

#join datasets
map19<- merge(temp1, dat19map, by = "GEOID")
map19a<-map19 %>%  select(GEOID, NAME, nsdop_profiles,geometry) 
map19a<- map19a %>% mutate(`NSDoH profile` = nsdop_profiles)
#map19a$Cluster<-as.factor(map19a$Cluster)

#colors to match the bar plot
custom_colors <- custom_colors <- c("#d62728","salmon1",  
                                    "#9467bd","#2ca02c",
                                    "#e377c2",  "#17becf",
                                    "#1f77b4", "#ffff00")

map19_clust<-tm_shape(st_make_valid(map19a)) +
  tm_fill("NSDoH profile",style="cat",palette=custom_colors,legend.hist=TRUE,alpha=1)  +
  #tm_borders(col = "white") +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right",scale =1.2) 

tmap_save(tm = map19_clust, filename = "./Figures/map19_8clust.png")
#jpeg("/Users/carmenrodriguez/Desktop/Research Projects/Conferences/ISBA 2024/Poster/map19.jpeg", width = 500, height = 500)
#map19_clust
#dev.off()

#census tracts in cluster 1-- what counties do they belong to?
checks<-map19a %>% filter(nsdop_profiles == "Profile 1") %>% select("GEOID", "NAME")

##-----------SHINY APP-------------












