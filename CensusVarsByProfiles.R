library(tidyverse)
library(table1)
library(tidycensus)

### Looking at the distribution of other census variables across the NSDoH Profiles

#Load
load("/Users/carmenrodriguez/Desktop/Research Projects/Census Data/Census Data Revisions/censusdata_updated_acs2019.RData")
load("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Data/census_age_dist.RData")
mbmm_clust<-readRDS("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Model_run_updates/mbmm_clustersassign_042025.rds") %>% select(GEOID,nsdoh_profiles, assignment_ecr)


#Keep only age and race/ethnicity variables
raceth<-acs5_2019_wide %>% select(GEOID,NonHispanicWhite_2019,NonHispanicBlack_2019,NonHispanicAsian_2019,
                                        Hispanic_or_Latino_2019)
acs19_dat<-left_join(agedist19_wide, raceth, by = "GEOID")
acs19_dat<-left_join(acs19_dat,mbmm_clust, by = "GEOID")



table1::table1(~  age_median + age_20_24 + age_25_34 + age_35_44 + age_45_54 + age_55_59 + age_60_64 + age_65_74 + age_75_84 + age_85_more+ total_ct_pop +
               NonHispanicWhite_2019 + NonHispanicBlack_2019 + NonHispanicAsian_2019 + Hispanic_or_Latino_2019 |nsdoh_profiles, data= acs19_dat, overall = F)


#Look at the variables in the model 
# censusdata_bin <- readRDS("./Data/censusdata_bin_raceeth_042524.rds")
# names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
# bin19<-censusdata_bin$acs5_2019_bin
# bin19$GEOID <- row.names(bin19)
# acs19_dat<-left_join(bin19,mbmm_clust, by = "GEOID")
# names(acs19_dat)
# acs19_dat<- acs19_dat %>% 
#   mutate_if(is.numeric, as.factor)
# 
# table1::table1(~. |nsdop_profiles, data= acs19_dat, overall = F)


#Data for Urban vs. Rural census tracts--
#https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural.html
#Massachusetts has 23 urban areas (2020); 9 urbanized areas and 11 urban clusters: https://gis.data.mass.gov
# 
# library(sf)
# library(ggplot2)
# shapefile <- st_read("./nsdoh_profiles_app_urban/Urban_Boundaries_2020/Urban_Boundaries_2020.shp")
# 
# ggplot(data = shapefile) +
#   geom_sf() +
#   theme_minimal() +
#   labs(title = "Map from Shapefile")
