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

load("/Users/carmenrodriguez/Desktop/Research Projects/Census Data/censusdata_updated_newvars.RData")


###-------DATASET SET UP ----------###

#----------------Make all variable binary for each dataset (ACS 2010, 2015 and 2019)
##2006-2010
#Subset variables 
acs5_2010_wide1 <- acs5_2010_wide  %>% select( !c( "Femalehousehold_2010_E","SNAP_2010_E","Two_or_more_rooms_2010_E",  
                                                   paste0("ms", 1:8), "ms10", "ms_to", paste0("wc", 1:14), "wc_to",
                                                   "pov_to", paste0("pov", 2:5), "white_collar_occupation_2010", "HouseIncBlowPovLineP_2010", "Lessthan2rooms_2010_E", 
                                                   "Two_or_more_rooms_2010_P", "Total_occhousing_2010", "NonHispanicAsian_2010" ,    "NH_some_other_race_2010",
                                                   "NH_two_or_more_races_2010", "Hispanic_or_Latino_2010",   "NonHispanicWhite_2010" ,    "NonHispanicBlack_2010" ))
names(acs5_2010_wide1)


acs5_2010_wide1<- as.data.frame(apply(acs5_2010_wide1, 2, function(x) as.numeric(x)))

#Check variable two or more rooms
median(acs5_2010_wide1$Crowding_housing_2010, na.rm = T)
#check lack of plumbing
summary(acs5_2010_wide1$Lackplumbing_2010) #-zeroes mean they have complete plumbing--we classify this as 0, and 1 as lack of
table(acs5_2010_wide1$Lackplumbing_2010)
lcp<-ifelse(is.na(acs5_2010_wide1$Lackplumbing_2010), NA, ifelse(acs5_2010_wide1$Lackplumbing_2010 == 0, 0, 1))

#Take out Lack of plumbing and race 
acs5_2010_wide2<- acs5_2010_wide1 %>% select(!c("Lackplumbing_2010"))
acs_2010_medians<- cbind(names(acs5_2010_wide2[2:length(acs5_2010_wide2)]),sapply(2:length(acs5_2010_wide2), function(i) median(acs5_2010_wide2[,i], na.rm = TRUE)))

#Create the binary variables, 1 if above or equal to median for MA and 0 if below (vars 2:20)  and then we append lack of p.
acs5_2010_bin<- as.data.frame(sapply(2:length(acs5_2010_wide2), function(i) {ifelse(is.na(acs5_2010_wide2[,i]), NA, ifelse(as.numeric(acs5_2010_wide2[,i] >= acs_2010_medians[i-1,2]),1,0))}))


colnames(acs5_2010_bin)<-names(acs5_2010_wide2[2:length(acs5_2010_wide2)])
rownames(acs5_2010_bin)<- acs5_2010_wide$GEOID
acs5_2010_bin$Lackplumbing_2010<-lcp
rows_with_all_missing <- which(rowSums(is.na(acs5_2010_bin)) == ncol(acs5_2010_bin))
#[1] "25001990000" "25005990000" "25007990000" "25009990100" "25017980000" "25019990000" "25023990003" "25025980700" "25025981000" "25025981201" "25025981202" "25025981501" "25025981600" "25025981700"


##2011-2015-- follow same steps as above

#Subset variables 
acs5_2015_wide1 <- acs5_2015_wide  %>% select( !c( "Femalehousehold_2015_E","SNAP_2015_E","Two_or_more_rooms_2015_E",  
                                                   paste0("ms", 1:8), "ms10", "ms_to", paste0("wc", 1:14), "wc_to",
                                                   "pov_to", paste0("pov", 2:5), "white_collar_occupation_2015", "HouseIncBlowPovLineP_2015", "Lessthan2rooms_2015_E", 
                                                   "Two_or_more_rooms_2015_P", "Total_occhousing_2015", "NonHispanicAsian_2015" ,    "NH_some_other_race_2015",
                                                   "NH_two_or_more_races_2015", "Hispanic_or_Latino_2015",   "NonHispanicWhite_2015" ,    "NonHispanicBlack_2015" ))
names(acs5_2015_wide1)


acs5_2015_wide1<- as.data.frame(apply(acs5_2015_wide1, 2, function(x) as.numeric(x)))

#Check variable two or more rooms
median(acs5_2015_wide1$Crowding_housing_2015, na.rm = T)
#check lack of plumbing
summary(acs5_2015_wide1$Lackplumbing_2015) #-zeroes mean they have complete plumbing--we classify this as 0, and 1 as lack of
table(acs5_2015_wide1$Lackplumbing_2015)
lcp<-ifelse(is.na(acs5_2015_wide1$Lackplumbing_2015), NA, ifelse(acs5_2015_wide1$Lackplumbing_2015 == 0, 0, 1))

#Take out Lack of plumbing and race 
acs5_2015_wide2<- acs5_2015_wide1 %>% select(!c("Lackplumbing_2015"))
acs_2015_medians<- cbind(names(acs5_2015_wide2[2:length(acs5_2015_wide2)]),sapply(2:length(acs5_2015_wide2), function(i) median(acs5_2015_wide2[,i], na.rm = TRUE)))

#Create the binary variables, 1 if above or equal to median for MA and 0 if below (vars 2:20)  and then we append lack of p.
acs5_2015_bin<- as.data.frame(sapply(2:length(acs5_2015_wide2), function(i) {ifelse(is.na(acs5_2015_wide2[,i]), NA, ifelse(as.numeric(acs5_2015_wide2[,i] >= acs_2015_medians[i-1,2]),1,0))}))


colnames(acs5_2015_bin)<-names(acs5_2015_wide2[2:length(acs5_2015_wide2)])
rownames(acs5_2015_bin)<- acs5_2015_wide$GEOID
acs5_2015_bin$Lackplumbing_2015<-lcp
rows_with_all_missing15 <- which(rowSums(is.na(acs5_2015_bin)) == ncol(acs5_2015_bin))
# [1] "25001990000" "25005990000" "25007990000" "25009990100" "25017980000" "25019990000" "25023990003" "25025981000" "25025981201" "25025981501" "25025981502" "25025981600" "25025981700" "25025990101"


##2015-2019
#Subset variables 
acs5_2019_wide1 <- acs5_2019_wide  %>% select( !c( "Femalehousehold_2019_E","SNAP_2019_E","Two_or_more_rooms_2019_E",  
                                                   paste0("ms", 1:8), "ms10", "ms_to", paste0("wc", 1:14), "wc_to",
                                                   "pov_to", paste0("pov", 2:5), "white_collar_occupation_2019", "HouseIncBlowPovLineP_2019", "Lessthan2rooms_2019_E", 
                                                   "Two_or_more_rooms_2019_P", "Total_housing_2019", "NonHispanicAsian_2019" ,    "NH_some_other_race_2019",
                                                   "NH_two_or_more_races_2019", "Hispanic_or_Latino_2019",   "NonHispanicWhite_2019" ,    "NonHispanicBlack_2019" ))
names(acs5_2019_wide1)


acs5_2019_wide1<- as.data.frame(apply(acs5_2019_wide1, 2, function(x) as.numeric(x)))

#Check variable two or more rooms
median(acs5_2019_wide1$Crowding_housing_2019, na.rm = T)
#check lack of plumbing
summary(acs5_2019_wide1$Lackplumbing_2019) #-zeroes mean they have complete plumbing--we classify this as 0, and 1 as lack of
table(acs5_2019_wide1$Lackplumbing_2019)
lcp<-ifelse(is.na(acs5_2019_wide1$Lackplumbing_2019), NA, ifelse(acs5_2019_wide1$Lackplumbing_2019 == 0, 0, 1))

#Take out Lack of plumbing and race 
acs5_2019_wide2<- acs5_2019_wide1 %>% select(!c("Lackplumbing_2019"))
acs_2019_medians<- cbind(names(acs5_2019_wide2[2:length(acs5_2019_wide2)]),sapply(2:length(acs5_2019_wide2), function(i) median(acs5_2019_wide2[,i], na.rm = TRUE)))

#Create the binary variables, 1 if above or equal to median for MA and 0 if below (vars 2:20)  and then we append lack of p.
acs5_2019_bin<- as.data.frame(sapply(2:length(acs5_2019_wide2), function(i) {ifelse(is.na(acs5_2019_wide2[,i]), NA, ifelse(as.numeric(acs5_2019_wide2[,i] >= acs_2019_medians[i-1,2]),1,0))}))


colnames(acs5_2019_bin)<-names(acs5_2019_wide2[2:length(acs5_2019_wide2)])
rownames(acs5_2019_bin)<- acs5_2019_wide$GEOID
acs5_2019_bin$Lackplumbing_2019<-lcp
rows_with_all_missing19 <- which(rowSums(is.na(acs5_2019_bin)) == ncol(acs5_2019_bin))
#[1] "25001990000" "25005990000" "25007990000" "25009990100" "25017980000" "25019990000" "25023990003" "25025980700" "25025981000" "25025981201" "25025981501" "25025981502" "25025981600" "25025981700"




## Save binary dataset
dats<-list(acs5_2010_bin,acs5_2015_bin,acs5_2019_bin)
saveRDS(dats, "/Users/carmenrodriguez/Desktop/Research Projects/Census Data/censusdata_bin_rownames_032924.rds")

#Export medians as a table
medians<- data.frame(varname = acs_2010_medians[,1],acs2010 = acs_2010_medians[,2] ,acs2015 = acs_2015_medians[,2],acs2019 = acs_2019_medians[,2])
write.table(medians, file = "ACS_medians2.txt",sep = ",", quote = FALSE, row.names = F)



### Get distribution of binarized data
#Load census data-- make sure this has the row names so that we can get CTs
censusdata_bin <- readRDS("/Users/carmenrodriguez/Desktop/Research Projects/Census Data/censusdata_bin_rownames_032924.rds")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")


#Rename variables and add survey indicator
acs10<-acs5_2010_bin
colnames(acs10)<- c("medianincome"  , "Femalehousehold" ,"Education9years",        
                    "HighSchoolHigherP", "BachelorHigherP", "ENProficiency", "UnemployementP"  ,"SNAP" ,                
                    "OwnerOccupiedUnitP" , "RenterOccupiedUnitP" , "No_vehicle", "ethnicminority", "crowded_housing", "Belowpl", "working_class", "Lackplumbing") 


acs10$survey_year<- "ACS2010"
                 
  
acs15<-acs5_2015_bin
colnames(acs15)<- c("medianincome"  , "Femalehousehold" ,"Education9years",        
                    "HighSchoolHigherP", "BachelorHigherP", "ENProficiency", "UnemployementP"  ,"SNAP" ,                
                    "OwnerOccupiedUnitP" , "RenterOccupiedUnitP" , "No_vehicle", "ethnicminority", "crowded_housing", "Belowpl", "working_class", "Lackplumbing") 

acs15$survey_year<- "ACS2015"

acs19<-acs5_2019_bin
colnames(acs19)<- c("medianincome"  , "Femalehousehold" ,"Education9years",        
                    "HighSchoolHigherP", "BachelorHigherP", "ENProficiency", "UnemployementP"  ,"SNAP" ,                
                    "OwnerOccupiedUnitP" , "RenterOccupiedUnitP" , "No_vehicle", "ethnicminority", "crowded_housing", "Belowpl", "working_class", "Lackplumbing") 

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

tab<-table1::table1( ~ Lackplumbing + medianincome + BachelorHigherP +        
                       Education9years + Femalehousehold + HighSchoolHigherP +
                       No_vehicle + OwnerOccupiedUnitP + RenterOccupiedUnitP + UnemployementP +         
                       SNAP + working_class + Belowpl + crowded_housing + ethnicminority + ENProficiency  | survey_year_fct, data = allwaves, overall = F  )
library(flextable)
t1flex(tab) %>% 
  save_as_docx(path="/Users/carmenrodriguez/Desktop/Research Projects/Census Data/censusbindist_032924.docx")



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
tab1<-table1::table1( ~ Lackplumbing + medianincome + BachelorHigherP +        
                        Education9years + Femalehousehold + HighSchoolHigherP +
                        No_vehicle + OwnerOccupiedUnitP + RenterOccupiedUnitP + UnemployementP +         
                        SNAP + working_class + Belowpl + crowded_housing + ethnicminority + ENProficiency  | survey_year_fct, data = allwaves, overall = F, extra.col=list(`P-value`=pvalue))
tab1
