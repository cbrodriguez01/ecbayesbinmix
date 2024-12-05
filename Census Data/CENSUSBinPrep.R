## Census data set for BayesBinMix
#Last updated: 01/23/24

library(tidycensus)
library(tidyverse)
library(Hmisc)
library(reshape2)
library(ggplot2)
library(psych)
library(table1)
library(Hmisc)
getwd()

load("/Users/carmenrodriguez/Desktop/Research Projects/Census Data/censusdata_updated.RData")


###-------DATASET SET UP ----------###

#----------------Make all variable binary for each dataset (ACS 2010, 2015 and 2019)
##2006-2010
names(acs5_2010_wide)

#Add income and two or more rooms (0 or above) to the bin dataset
#variable:two or more room == Households with more than one person per room (Percent!!OCCUPANTS PER ROOM)

#Subset variables to only keep percentages
acs5_2010_wide1<-acs5_2010_wide %>%  select("Two_or_more_rooms_2010_P", "Lackplumbing_2010","medianincome_2010",
"BachelorHigherP_2010", "BelowPovertyLineP_2010","Education9years_2010","Femalehousehold_2010_P","HighSchoolHigherP_2010", "No_vehicle_2010",      
"OwnerOccupiedUnitP_2010","RenterOccupiedUnitP_2010", "UnemployementP_2010", "white_collar_occupation_2010", "SNAP_2010_P")

names(acs5_2010_wide1)

acs5_2010_wide1<- as.data.frame(apply(acs5_2010_wide1, 2, function(x) as.numeric(x)))

#Check variable two or more rooms
median(acs5_2010_wide1$Two_or_more_rooms_2010_P, na.rm = T)
median(acs5_2010_wide1$Crowding_housing_2010, na.rm = T)
summary(acs5_2010_wide1$Two_or_more_rooms_2010_P) #zeroes



table(acs5_2010_wide1$Two_or_more_rooms_2010_P) #1048 zeroes, i.e, 1048 census tracts for which households did not have more than one person per room
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
rownames(acs5_2010_bin)<- acs5_2010_wide$GEOID



##2011-2015-- follow same steps as above
names(acs5_2015_wide)
median(acs5_2015_wide$Two_or_more_rooms_2015_P, na.rm = T)
table(acs5_2015_wide$Two_or_more_rooms_2015_P) #zeroes

#Subset variables to only keep percentages
acs5_2015_wide1<-acs5_2015_wide %>%  select("Two_or_more_rooms_2015_P", "Lackplumbing_2015","medianincome_2015",
                                            "BachelorHigherP_2015", "BelowPovertyLineP_2015","Education9years_2015","Femalehousehold_2015_P","HighSchoolHigherP_2015", "No_vehicle_2015",      
                                            "OwnerOccupiedUnitP_2015","RenterOccupiedUnitP_2015", "UnemployementP_2015", "white_collar_occupation_2015", "SNAP_2015_P")


acs5_2015_wide1<- as.data.frame(apply(acs5_2015_wide1, 2, function(x) as.numeric(x)))
names(acs5_2015_wide1)

#Check variable two or more rooms
median(acs5_2015_wide1$Two_or_more_rooms_2015_P, na.rm = T)
median(acs5_2015_wide1$Crowding_housing_2015, na.rm = T)
summary(acs5_2015_wide1$Two_or_more_rooms_2015_P) #zeroes


tpp<- ifelse(is.na(acs5_2015_wide1$Two_or_more_rooms_2015), NA, ifelse(acs5_2015_wide1$Two_or_more_rooms_2015 == 0, 0, 1))
lcp<-ifelse(is.na(acs5_2015_wide1$Lackplumbing_2015), NA, ifelse(acs5_2015_wide1$Lackplumbing_2015 == 0, 0, 1))


acs_2015_medians<- cbind(names(acs5_2015_wide1[3:length(acs5_2015_wide1)]),sapply(3:length(acs5_2015_wide1), function(i) median(acs5_2015_wide1[,i], na.rm = TRUE)))

#Create the binary variables, 1 if above or equal to median for MA and 0 if below
acs5_2015_bin<- as.data.frame(sapply(3:length(acs5_2010_wide1), function(i) {ifelse(is.na(acs5_2015_wide1[,i]), NA, ifelse(as.numeric(acs5_2015_wide1[,i] >= acs_2015_medians[i-2,2]),1,0))}))

acs5_2015_bin<-cbind(tpp,lcp, acs5_2015_bin)
colnames(acs5_2015_bin)<-names(acs5_2015_wide1)
rownames(acs5_2015_bin)<- acs5_2015_wide$GEOID

##2015-2019
names(acs5_2019_wide)
median(acs5_2019_wide$Two_or_more_rooms_2019, na.rm = T)
table(acs5_2019_wide$Two_or_more_rooms_2019) #zeroes

#Subset variables to only keep percentages
acs5_2019_wide1<-acs5_2019_wide %>%   select("Two_or_more_rooms_2019_P", "Lackplumbing_2019","medianincome_2019",
                                             "BachelorHigherP_2019", "BelowPovertyLineP_2019","Education9years_2019","Femalehousehold_2019_P","HighSchoolHigherP_2019", "No_vehicle_2019",      
                                             "OwnerOccupiedUnitP_2019","RenterOccupiedUnitP_2019", "UnemployementP_2019", "white_collar_occupation_2019", "SNAP_2019_P")

acs5_2019_wide1<- as.data.frame(apply(acs5_2019_wide1, 2, function(x) as.numeric(x)))

tpp<-ifelse(is.na(acs5_2019_wide1$Two_or_more_rooms_2019), NA, ifelse(acs5_2019_wide1$Two_or_more_rooms_2019 == 0, 0, 1))
lcp<-ifelse(is.na(acs5_2019_wide1$Lackplumbing_2019), NA, ifelse(acs5_2019_wide1$Lackplumbing_2019 == 0, 0, 1))

acs_2019_medians<- cbind(names(acs5_2019_wide1[3:length(acs5_2019_wide1)]),sapply(3:length(acs5_2019_wide1), function(i) median(acs5_2019_wide1[,i], na.rm = TRUE)))

#Create the binary variables, 1 if above or equal to median for MA and 0 if below
acs5_2019_bin<- as.data.frame(sapply(3:length(acs5_2019_wide1), function(i) {ifelse(is.na(acs5_2019_wide1[,i]), NA, ifelse(as.numeric(acs5_2019_wide1[,i] >= acs_2019_medians[i-2,2]),1,0))}))

acs5_2019_bin<-cbind(tpp,lcp, acs5_2019_bin)
colnames(acs5_2019_bin)<-names(acs5_2019_wide1)
rownames(acs5_2019_bin)<- acs5_2019_wide$GEOID

## Save binary dataset
dats<-list(acs5_2010_bin,acs5_2015_bin,acs5_2019_bin)
saveRDS(dats, "/Users/carmenrodriguez/Desktop/Research Projects/Census Data/censusdata_bin_rownames.rds")

#Export medians as a table
medians<- data.frame(varname = acs_2010_medians[,1],acs2010 = acs_2010_medians[,2] ,acs2015 = acs_2015_medians[,2],acs2019 = acs_2019_medians[,2])
write.table(medians, file = "ACS_medians.txt",sep = ",", quote = FALSE, row.names = F)





### Get distribution of binarized data
#Load census data-- make sure this has the row names so that we can get CTs
censusdata_bin <- readRDS("/Users/carmenrodriguez/Desktop/Research Projects/Census Data/censusdata_bin_rownames.rds")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")


#Rename variables and add survey indicator
acs10<-censusdata_bin$acs5_2010_bin
colnames(acs10)<- c("Two_or_more_rooms", "Lackplumbing","medianincome","BachelorHigher",        
                    "BelowPovertyLineP","Education9years","Femalehousehold","HighSchoolHigherP",      
                    "No_vehicle","OwnerOccupiedUnitP","RenterOccupiedUnitP","UnemployementP",         
                    "white_collar_occupation", "SNAP") 
acs10$survey_year<- "ACS2010"
                 
  
acs15<-censusdata_bin$acs5_2015_bin
colnames(acs15)<- c("Two_or_more_rooms", "Lackplumbing","medianincome","BachelorHigher",        
                    "BelowPovertyLineP","Education9years","Femalehousehold","HighSchoolHigherP",      
                    "No_vehicle","OwnerOccupiedUnitP","RenterOccupiedUnitP","UnemployementP",         
                    "white_collar_occupation", "SNAP")     
acs15$survey_year<- "ACS2015"

acs19<-censusdata_bin$acs5_2019_bin
colnames(acs19)<- c("Two_or_more_rooms", "Lackplumbing","medianincome","BachelorHigher",        
                    "BelowPovertyLineP","Education9years","Femalehousehold","HighSchoolHigherP",      
                    "No_vehicle","OwnerOccupiedUnitP","RenterOccupiedUnitP","UnemployementP",         
                    "white_collar_occupation", "SNAP")   
acs19$survey_year<- "ACS2019"


allwaves<-rbind(acs10, acs15, acs19)
allwaves<-allwaves %>% mutate_if(is.numeric, as.factor)
allwaves<- allwaves %>% mutate(survey_year_fct = case_when(
  survey_year == "ACS2010" ~ 1,
  survey_year == "ACS2015" ~ 2,
  survey_year == "ACS2019" ~ 3,
))
table(allwaves$survey_year_fct)
allwaves$survey_year_fct<- factor(allwaves$survey_year_fct, levels = 1:3, labels = c("ACS2010", "ACS2015", "ACS2019"))

tab<-table1::table1( ~ Two_or_more_rooms + Lackplumbing+medianincome + BachelorHigher+        
                BelowPovertyLineP + Education9years + Femalehousehold + HighSchoolHigherP +
                  No_vehicle + OwnerOccupiedUnitP + RenterOccupiedUnitP + UnemployementP +         
                white_collar_occupation + SNAP| survey_year_fct, data = allwaves, overall = F )
library(flextable)
t1flex(tab) %>% 
  save_as_docx(path="/Users/carmenrodriguez/Desktop/Research Projects/Census Data/censusbindist.docx")



#Next, we create a function to compute the p-value for continuous or categorical variables.
#https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}
tab1<-table1::table1( ~ Two_or_more_rooms + Lackplumbing+medianincome + BachelorHigher+        
                       BelowPovertyLineP + Education9years + Femalehousehold + HighSchoolHigherP +
                       No_vehicle + OwnerOccupiedUnitP + RenterOccupiedUnitP + UnemployementP +         
                       white_collar_occupation + SNAP| survey_year_fct, data = allwaves, overall = F, extra.col=list(`P-value`=pvalue))
tab1
