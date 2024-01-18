## Census data set for BayesBinMix

library(tidycensus)
library(tidyverse)
library(Hmisc)
library(reshape2)
library(ggplot2)
library(psych)
library(table1)
library(Hmisc)
getwd()

load("/Users/carmenrodriguez/Desktop/Research Projects/Census Data/censusdata.RData")


###-------DATASET SET UP ----------###

#----------------Make all variable binary for each dataset (ACS 2010, 2015 and 2019)
##2006-2010
names(acs5_2010_wide)

#Add income and two or more rooms (0 or above) to the bin dataset
#variable:two or more room == Households with more than one person per room (Percent!!OCCUPANTS PER ROOM)

#Subset variables to only keep percentages
acs5_2010_wide1<-acs5_2010_wide %>%  select("Two_or_more_rooms_2010", "Lackplumbing_2010","medianincome_2010",
"BachelorHigherP_2010", "BelowPovertyLineP_2010","Education9years_2010",    
"Femalehousehold_2010","HighSchoolHigherP_2010","Malehoushold_2010", "No_vehicle_2010",      
"OwnerOccupiedUnitP_2010","RenterOccupiedUnitP_2010",    
 "UnemployementP_2010", "white_collar_occupation_2010", "NonHispanicWhiteP_2010","NonHispanicBlackP_2010",     
"NonHispanicAsianP_2010", "Hispanic_or_LatinoP_2010")
  
acs5_2010_wide1<- as.data.frame(apply(acs5_2010_wide1, 2, function(x) as.numeric(x)))
#Check variable two or more rooms
median(acs5_2010_wide1$Two_or_more_rooms_2010, na.rm = T)
summary(acs5_2010_wide1$Two_or_more_rooms_2010) #zeroes
table(acs5_2010_wide1$Two_or_more_rooms_2010) #1048 zeroes, i.e, 1048 census tracts for which households did not have more than one person per room
##So we classify this variable as 0 = no, 1 = yes 
tpp<- ifelse(is.na(acs5_2010_wide1$Two_or_more_rooms_2010), NA, ifelse(acs5_2010_wide1$Two_or_more_rooms_2010 == 0, 0, 1))

#check lack of plumbing
summary(acs5_2010_wide1$Lackplumbing_2010) #-zeroes mean they have complete plumbing--we classify this as 0, and 1 as lack of
table(acs5_2010_wide1$Lackplumbing_2010)
lcp<-ifelse(is.na(acs5_2010_wide1$Lackplumbing_2010), NA, ifelse(acs5_2010_wide1$Lackplumbing_2010 == 0, 0, 1))


acs_2010_medians<- cbind(names(acs5_2010_wide1[3:length(acs5_2010_wide1)]),sapply(3:length(acs5_2010_wide1), function(i) median(acs5_2010_wide1[,i], na.rm = TRUE)))


#Create the binary variables, 1 if above or equal to median for MA and 0 if below (vars 2:20)  and then we append Two_or_more_rooms_2010
acs5_2010_bin<- as.data.frame(sapply(3:length(acs5_2010_wide1), function(i) {ifelse(is.na(acs5_2010_wide1[,i]), NA, ifelse(as.numeric(acs5_2010_wide1[,i] >= acs_2010_medians[i-2,2]),1,0))}))


acs5_2010_bin<-cbind(tpp, lcp, acs5_2010_bin)
colnames(acs5_2010_bin)<-names(acs5_2010_wide1)

##2011-2015-- follow same steps as above
names(acs5_2015_wide)
median(acs5_2015_wide$Two_or_more_rooms_2015, na.rm = T)
table(acs5_2015_wide$Two_or_more_rooms_2015) #zeroes

#Subset variables to only keep percentages
acs5_2015_wide1<-acs5_2015_wide %>%  select("Two_or_more_rooms_2015","Lackplumbing_2015", "medianincome_2015",
  "BachelorHigherP_2015", "BelowPovertyLineP_2015","Education9years_2015",    
  "Femalehousehold_2015","HighSchoolHigherP_2015",    
  "Malehoushold_2015", "No_vehicle_2015",      
  "OwnerOccupiedUnitP_2015","RenterOccupiedUnitP_2015",    
  "UnemployementP_2015", "white_collar_occupation_2015", "NonHispanicWhiteP_2015","NonHispanicBlackP_2015",     
  "NonHispanicAsianP_2015", "Hispanic_or_LatinoP_2015")

acs5_2015_wide1<- as.data.frame(apply(acs5_2015_wide1, 2, function(x) as.numeric(x)))
tpp<- ifelse(is.na(acs5_2015_wide1$Two_or_more_rooms_2015), NA, ifelse(acs5_2015_wide1$Two_or_more_rooms_2015 == 0, 0, 1))
lcp<-ifelse(is.na(acs5_2015_wide1$Lackplumbing_2015), NA, ifelse(acs5_2015_wide1$Lackplumbing_2015 == 0, 0, 1))


acs_2015_medians<- cbind(names(acs5_2015_wide1[3:length(acs5_2015_wide1)]),sapply(3:length(acs5_2015_wide1), function(i) median(acs5_2015_wide1[,i], na.rm = TRUE)))

#Create the binary variables, 1 if above or equal to median for MA and 0 if below
acs5_2015_bin<- as.data.frame(sapply(3:length(acs5_2010_wide1), function(i) {ifelse(is.na(acs5_2015_wide1[,i]), NA, ifelse(as.numeric(acs5_2015_wide1[,i] >= acs_2015_medians[i-2,2]),1,0))}))

acs5_2015_bin<-cbind(tpp,lcp, acs5_2015_bin)
colnames(acs5_2015_bin)<-names(acs5_2015_wide1)



##2015-2019
names(acs5_2019_wide)
median(acs5_2019_wide$Two_or_more_rooms_2019, na.rm = T)
table(acs5_2019_wide$Two_or_more_rooms_2019) #zeroes

#Subset variables to only keep percentages
acs5_2019_wide1<-acs5_2019_wide %>%  select("Two_or_more_rooms_2019", "Lackplumbing_2019","medianincome_2019",
  "BachelorHigherP_2019", "BelowPovertyLineP_2019","Education9years_2019",    
  "Femalehousehold_2019","HighSchoolHigherP_2019",    
  "Malehoushold_2019", "No_vehicle_2019",      
  "OwnerOccupiedUnitP_2019","RenterOccupiedUnitP_2019",    
  "UnemployementP_2019", "white_collar_occupation_2019", "NonHispanicWhiteP_2019","NonHispanicBlackP_2019",     
  "NonHispanicAsianP_2019", "Hispanic_or_LatinoP_2019")

acs5_2019_wide1<- as.data.frame(apply(acs5_2019_wide1, 2, function(x) as.numeric(x)))
tpp<-ifelse(is.na(acs5_2019_wide1$Two_or_more_rooms_2019), NA, ifelse(acs5_2019_wide1$Two_or_more_rooms_2019 == 0, 0, 1))
lcp<-ifelse(is.na(acs5_2019_wide1$Lackplumbing_2019), NA, ifelse(acs5_2019_wide1$Lackplumbing_2019 == 0, 0, 1))

acs_2019_medians<- cbind(names(acs5_2019_wide1[3:length(acs5_2019_wide1)]),sapply(3:length(acs5_2019_wide1), function(i) median(acs5_2019_wide1[,i], na.rm = TRUE)))

#Create the binary variables, 1 if above or equal to median for MA and 0 if below
acs5_2019_bin<- as.data.frame(sapply(3:length(acs5_2019_wide1), function(i) {ifelse(is.na(acs5_2019_wide1[,i]), NA, ifelse(as.numeric(acs5_2019_wide1[,i] >= acs_2019_medians[i-2,2]),1,0))}))

acs5_2019_bin<-cbind(tpp,lcp, acs5_2019_bin)
colnames(acs5_2019_bin)<-names(acs5_2019_wide1)


## Save binary dataset
dats<-list(acs5_2010_bin,acs5_2015_bin,acs5_2019_bin)
saveRDS(dats, "/Users/carmenrodriguez/Desktop/Research Projects/Census Data/censusdata_bin.rds")

