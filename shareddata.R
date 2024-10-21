library(tidyverse)
library(tidycensus)

#------CENSUS DATA  with clusters only-------
data19<-readRDS("./Data/mbmm_clusters_19eth_sized.rds")
data19_n<-data19 %>% select(GEOID, nsdop_profiles)
data19_n$countycode<-str_sub(data19$GEOID,start=3, end = 5)

temp<-get_acs(state = "MA", geography = "county", 
              variables = "B03002_001", year = 2019, geometry = F)
temp$countycode<-str_sub(temp$GEOID,start=3, end = 5)
temp1<-temp %>% rename(CountyName = NAME)
temp2<-temp1 %>% select(CountyName, countycode)

#get NAME variable from age dist datasets
load("./Data/census_age_dist.RData")
ct_name<-agedist19_wide %>% select(GEOID, NAME)
data19_n1<-merge(data19_n, ct_name, by = "GEOID")

#merge
nsdohdat<-merge(data19_n1, temp2, by = "countycode")
nsdohdat<-nsdohdat %>% rename(nsdoh_profiles = nsdop_profiles) %>% select(GEOID, countycode,NAME, nsdoh_profiles)

saveRDS(nsdohdat, "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/nsdoh_profiles_map/nsdoh_data.rds")
write.csv(nsdohdat, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/nsdoh_profiles_map/nsdoh_data.csv")


