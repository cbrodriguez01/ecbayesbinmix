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

##  Correlations  between potential SES variables
acs5_2010_corr1<-acs5_2010_wide %>% select(HighSchoolHigherP_2010,BachelorHigherP_2010, Education9years_2010,
                                           RenterOccupiedUnitP_2010, OwnerOccupiedUnitP_2010,
                                           UnemployementP_2010, pov_2010, medianincome_2010,
                                           Lackplumbing_2010,Femalehousehold_2010_P,
                                           No_vehicle_2010,Crowding_housing_2010,working_class_2010, lang_home_EN_notwell_2010,
                                           SNAP_2010_P, working_class_2010, Hispanic_or_Latino_2010, NonHispanicBlack_2010, NonHispanicAsian_2010) %>% rename(
                                             `>= High School`= HighSchoolHigherP_2010,
                                             `>= Bachelors` = BachelorHigherP_2010,
                                             `< High School` =  Education9years_2010, 
                                             `Renter-occupied housing` = RenterOccupiedUnitP_2010,
                                             `Owner-occupied housing`= OwnerOccupiedUnitP_2010,
                                             `Unemployment`= UnemployementP_2010, 
                                             `Below 150% poverty`= pov_2010,
                                             `Lack complete plumbing` = Lackplumbing_2010,
                                             `Female household` = Femalehousehold_2010_P,
                                             `No vehicle in household` = No_vehicle_2010,
                                             `Crowded housing`= Crowding_housing_2010,
                                             `Working class` = working_class_2010,
                                             `SNAP benefits`=SNAP_2010_P,
                                             `Median income`= medianincome_2010,
                                              `Limited EN proficiency` = lang_home_EN_notwell_2010,
                                             `Hispanic` = Hispanic_or_Latino_2010,
                                              `NH Black`= NonHispanicBlack_2010,
                                               `NH Asian` = NonHispanicAsian_2010)

acs5_2015_corr1<-acs5_2015_wide %>% select(HighSchoolHigherP_2015,BachelorHigherP_2015, Education9years_2015,
                                           RenterOccupiedUnitP_2015, OwnerOccupiedUnitP_2015,
                                           UnemployementP_2015, pov_2015, medianincome_2015,
                                           Lackplumbing_2015,Femalehousehold_2015_P,
                                           No_vehicle_2015,Crowding_housing_2015,working_class_2015, lang_home_EN_notwell_2015,
                                           SNAP_2015_P, working_class_2015, Hispanic_or_Latino_2015, NonHispanicBlack_2015, NonHispanicAsian_2015) %>% rename(
                                             `>= High School`= HighSchoolHigherP_2015,
                                             `>= Bachelors` = BachelorHigherP_2015,
                                             `< High School` =  Education9years_2015, 
                                             `Renter-occupied housing` = RenterOccupiedUnitP_2015,
                                             `Owner-occupied housing`= OwnerOccupiedUnitP_2015,
                                             `Unemployment`= UnemployementP_2015, 
                                             `Below 150% poverty`= pov_2015,
                                             `Lack complete plumbing` = Lackplumbing_2015,
                                             `Female household` = Femalehousehold_2015_P,
                                             `No vehicle in household` = No_vehicle_2015,
                                             `Crowded housing`= Crowding_housing_2015,
                                             `Working class` = working_class_2015,
                                             `SNAP benefits`=SNAP_2015_P,
                                             `Median income`= medianincome_2015,
                                             `Limited EN proficiency` = lang_home_EN_notwell_2015,
                                             `Hispanic` = Hispanic_or_Latino_2015,
                                             `NH Black`= NonHispanicBlack_2015,
                                             `NH Asian` = NonHispanicAsian_2015)

acs5_2019_corr1<-acs5_2019_wide %>% select(HighSchoolHigherP_2019,BachelorHigherP_2019, Education9years_2019,
                                           RenterOccupiedUnitP_2019, OwnerOccupiedUnitP_2019,
                                           UnemployementP_2019, pov_2019, medianincome_2019,
                                           Lackplumbing_2019,Femalehousehold_2019_P,
                                           No_vehicle_2019,Crowding_housing_2019,working_class_2019, lang_home_EN_notwell_2019,
                                           SNAP_2019_P, working_class_2019, Hispanic_or_Latino_2019, NonHispanicBlack_2019, NonHispanicAsian_2019) %>% rename(
                                             `>= High School`= HighSchoolHigherP_2019,
                                             `>= Bachelors` = BachelorHigherP_2019,
                                             `< High School` =  Education9years_2019, 
                                             `Renter-occupied housing` = RenterOccupiedUnitP_2019,
                                             `Owner-occupied housing`= OwnerOccupiedUnitP_2019,
                                             `Unemployment`= UnemployementP_2019, 
                                             `Below 150% poverty`= pov_2019,
                                             `Lack complete plumbing` = Lackplumbing_2019,
                                             `Female household` = Femalehousehold_2019_P,
                                             `No vehicle in household` = No_vehicle_2019,
                                             `Crowded housing`= Crowding_housing_2019,
                                             `Working class` = working_class_2019,
                                             `SNAP benefits`=SNAP_2019_P,
                                             `Median income`= medianincome_2019,
                                             `Limited EN proficiency` = lang_home_EN_notwell_2019,
                                             `Hispanic` = Hispanic_or_Latino_2019,
                                             `NH Black`= NonHispanicBlack_2019,
                                             `NH Asian` = NonHispanicAsian_2019)


corrmatrix_2010<-round(rcorr(as.matrix(acs5_2010_corr1))$r,2)
corrmatrix_2015<-round(rcorr(as.matrix(acs5_2015_corr1))$r,2)
corrmatrix_2019<-round(rcorr(as.matrix(acs5_2019_corr1))$r,2)



## New correlation plots-- MADE FOR ENAR
#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
library(corrplot)
library(RColorBrewer)
#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

# jpeg("/Users/carmenrodriguez/Desktop/Research Projects/EC optimal care data/ECBayesBinMix Manuscript/corr10_3.29.24.jpg", width = 950, height = 950)
# corrplot(corrmatrix_2010, method = "circle", type = "lower",
#          col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, tl.cex = 1.8, cl.ratio = 0.08,  mar=c(0,0,1,0))
# dev.off()
# 
# jpeg("/Users/carmenrodriguez/Desktop/Research Projects/EC optimal care data/ECBayesBinMix Manuscript/corr15_3.29.24.jpg", width = 950, height = 950)
# corrplot(corrmatrix_2015, method = "circle", type = "lower",
#          col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, tl.cex = 1.8, cl.ratio = 0.08,  mar=c(0,0,1,0))
# dev.off()
# 
# jpeg("/Users/carmenrodriguez/Desktop/Research Projects/EC optimal care data/ECBayesBinMix Manuscript/corr19_3.29.24.jpg", width = 950, height = 950)
# corrplot(corrmatrix_2019, method = "circle", type = "lower",
#          col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, tl.cex = 1.8, cl.ratio = 0.08,  mar=c(0,0,1,0))
# dev.off()
# 
# #New corrplots 3/29/24
# 
# jpeg("/Users/carmenrodriguez/Desktop/Research Projects/EC optimal care data/ECBayesBinMix Manuscript/Aux Files/corr10_3.29.24.jpg", width = 850, height = 850)
# corrplot(corrmatrix_2010, type = "lower",
#          col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, tl.cex = 1.2, cl.ratio = 0.08,  mar=c(0,0,1,0), cl.pos ="b")
# dev.off()
# 
# jpeg("/Users/carmenrodriguez/Desktop/Research Projects/EC optimal care data/ECBayesBinMix Manuscript/Aux Files/corr15_3.29.24.jpg", width = 850, height = 850)
# corrplot(corrmatrix_2015, method = "circle", type = "lower",
#          col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, tl.cex = 1.2, cl.ratio = 0.08,  mar=c(0,0,1,0), cl.pos ="b")
# dev.off()
# 
# jpeg("/Users/carmenrodriguez/Desktop/Research Projects/EC optimal care data/ECBayesBinMix Manuscript/Aux Files/corr19_3.29.24.jpg", width = 850, height = 850)
# corrplot(corrmatrix_2019, method = "circle", type = "lower",
#          col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, tl.cex = 1.2, cl.ratio = 0.08,  mar=c(0,0,1,0), cl.pos ="b")
# dev.off()


#New corrplots 4/14/24
jpeg("/Users/carmenrodriguez/Desktop/Research Projects/EC optimal care data/ECBayesBinMix Manuscript/Aux Files/corr10_4.14.24.jpg", width = 850, height = 850)
corrplot(corrmatrix_2010, type = "lower",
         col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, tl.cex = 1.1, cl.ratio = 0.08,  mar=c(0,0,1,0), cl.pos ="b")
dev.off()

jpeg("/Users/carmenrodriguez/Desktop/Research Projects/EC optimal care data/ECBayesBinMix Manuscript/Aux Files/corr15_4.14.24.jpg", width = 850, height = 850)
corrplot(corrmatrix_2015, method = "circle", type = "lower",
         col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, tl.cex = 1.1, cl.ratio = 0.08,  mar=c(0,0,1,0), cl.pos ="b")
dev.off()

jpeg("/Users/carmenrodriguez/Desktop/Research Projects/EC optimal care data/ECBayesBinMix Manuscript/Aux Files/corr19_4.14.24.jpg", width = 850, height = 850)
corrplot(corrmatrix_2019, method = "circle", type = "lower",
         col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, tl.cex = 1.1, cl.ratio = 0.08,  mar=c(0,0,1,0), cl.pos ="b")
dev.off()

jpeg("/Users/carmenrodriguez/Desktop/Research Projects/EC optimal care data/ECBayesBinMix Manuscript/Aux Files/corr19_6.8.24.jpg", width = 850, height = 850)
corrplot(corrmatrix_2019, method = "circle", type = "lower",
         col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, tl.cex = 1.7, cl.ratio = 0.08,  mar=c(0,0,1,0), cl.pos ="b")
dev.off()

jpeg("/Users/carmenrodriguez/Desktop/Research Projects/EC optimal care data/ECBayesBinMix Manuscript/Aux Files/corr19_6.8.24_upper.jpg", width = 850, height = 850)
corrplot(corrmatrix_2019, method = "circle", type = "upper",
         col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, tl.cex = 1.7, cl.ratio = 0.08,  mar=c(0,0,1,0), cl.pos ="b")
dev.off()


## Descriptive statistics-- rename variables to be same across and add survey identifier
acs5_2010_wide1 <- acs5_2010_wide  %>% select( !c( "Femalehousehold_2010_E","SNAP_2010_E","Two_or_more_rooms_2010_E",  
                                                   paste0("ms", 1:8), "ms10", "ms_to", paste0("wc", 1:14), "wc_to",
                                                 "pov_to", paste0("pov", 2:5), "white_collar_occupation_2010", "HouseIncBlowPovLineP_2010", "Lessthan2rooms_2010_E", "Two_or_more_rooms_2010_P", "Total_occhousing_2010"))
names(acs5_2010_wide1)

colnames(acs5_2010_wide1)<- c("GEOID" , "medianincome"  , "Femalehousehold" ,"Education9years",        
"HighSchoolHigherP", "BachelorHigherP", "ENProficiency", "UnemployementP"  ,"SNAP" ,                
"OwnerOccupiedUnitP" , "RenterOccupiedUnitP" , "No_vehicle" ,            
"Lackplumbing","Hispanic_or_Latino" ,    
"NonHispanicWhite", "NonHispanicBlack" ,"NonHispanicAsian", "NH_some_other_race" ,"NH_two_or_more_races", "ethnicminority", "crowded_housing", "Belowpl", "working_class")

acs5_2010_wide1$survey_year<- "ACS2010"                             


acs5_2015_wide1 <- acs5_2015_wide  %>% select(!c( "Femalehousehold_2015_E","SNAP_2015_E","Two_or_more_rooms_2015_E",  
                                                   paste0("ms", 1:8), "ms10", "ms_to", paste0("wc", 1:14), "wc_to",
                                                   "pov_to", paste0("pov", 2:5), "white_collar_occupation_2015", "HouseIncBlowPovLineP_2015", "Lessthan2rooms_2015_E", "Two_or_more_rooms_2015_P", "Total_occhousing_2015"))

colnames(acs5_2015_wide1)<- c("GEOID" , "medianincome"  , "Femalehousehold" ,"Education9years",        
                              "HighSchoolHigherP", "BachelorHigherP", "ENProficiency", "UnemployementP"  ,"SNAP" ,                
                              "OwnerOccupiedUnitP" , "RenterOccupiedUnitP" , "No_vehicle" ,            
                              "Lackplumbing","Hispanic_or_Latino" ,    
                              "NonHispanicWhite", "NonHispanicBlack" ,"NonHispanicAsian", "NH_some_other_race" ,"NH_two_or_more_races", "ethnicminority", "crowded_housing", "Belowpl", "working_class")
acs5_2015_wide1$survey_year<- "ACS2015" 

acs5_2019_wide1 <- acs5_2019_wide  %>% select(!c("Femalehousehold_2019_E","SNAP_2019_E","Two_or_more_rooms_2019_E",  
                                                  paste0("ms", 1:8), "ms10", "ms_to", paste0("wc", 1:14), "wc_to",
                                                  "pov_to", paste0("pov", 2:5), "white_collar_occupation_2019", "HouseIncBlowPovLineP_2019", "Lessthan2rooms_2019_E", "Two_or_more_rooms_2019_P", "Total_housing_2019"))


colnames(acs5_2019_wide1)<- c("GEOID" , "medianincome"  , "Femalehousehold" ,"Education9years",        
                              "HighSchoolHigherP", "BachelorHigherP", "ENProficiency", "UnemployementP"  ,"SNAP" ,                
                              "OwnerOccupiedUnitP" , "RenterOccupiedUnitP" , "No_vehicle" ,            
                              "Lackplumbing","Hispanic_or_Latino" ,    
                              "NonHispanicWhite", "NonHispanicBlack" ,"NonHispanicAsian", "NH_some_other_race" ,"NH_two_or_more_races", "ethnicminority", "crowded_housing", "Belowpl", "working_class")
acs5_2019_wide1$survey_year<- "ACS2019"      


#Add survey identifier
allwaves_props<-rbind(acs5_2010_wide1, acs5_2015_wide1, acs5_2019_wide1)
#allwaves_props<-allwaves_props %>% mutate_if(is.numeric, as.factor)
allwaves_props<- allwaves_props %>% mutate(survey_year_fct = case_when(
  survey_year == "ACS2010" ~ 1,
  survey_year == "ACS2015" ~ 2,
  survey_year == "ACS2019" ~ 3,
))
allwaves_props$survey_year_fct<- factor(allwaves_props$survey_year_fct, levels = 1:3, labels = c("ACS2010", "ACS2015", "ACS2019"))
table(allwaves_props$survey_year_fct)


tab<-table1::table1( ~  Lackplumbing + medianincome + BachelorHigherP +        
                        Education9years + Femalehousehold + HighSchoolHigherP +
                       No_vehicle + OwnerOccupiedUnitP + RenterOccupiedUnitP + UnemployementP +         
                        SNAP + working_class + Belowpl + crowded_housing + ethnicminority + ENProficiency + Hispanic_or_Latino +  NonHispanicWhite + NonHispanicBlack + NonHispanicAsian + NH_some_other_race + 
                      NH_two_or_more_races | survey_year_fct, data = allwaves_props, overall = F )
tab




# BIG NOTE: DO NOT PANICK IF INCOME IS DIFFERENT WHEN OUTPUT TAB-- SOMETHING IS WEIRD WITH THE TABLE BUT IF TOU DO EACH DATASET INDIVIDUALLY YOU 
#GET THE RIGHT VALUES-- ALL OTHER VARIBALES ARE OKAY- I did checks--see below!
library(flextable)
t1flex(tab) %>% 
  save_as_docx(path="/Users/carmenrodriguez/Desktop/Research Projects/Census Data/censusvarsdist_withRace032924.docx")




#Another way-- to check the above!
#descriptives<-psych::describe(allwaves_props %>% filter(survey_year_fct == "ACS2010"))
# new<-descriptives %>% filter(vars %in% c(12,47,82))

# acs5_2010_long$year <-2010
# acs5_2015_long$year <-2015
# acs5_2019_long$year <-2019
# acs_all_long<-rbind(acs5_2010_long, acs5_2015_long, acs5_2019_long) %>% arrange(GEOID, year)
# acs_all_long$year<- as.factor(acs_all_long$year)
# head(acs_all_long)


# acs_all1<-merge(acs5_2010_wide,acs5_2015_wide, by = c("GEOID"))
# acs_all<-merge(acs_all1, acs5_2019_wide, by = c("GEOID"))
# acs_all<-acs_all %>% select(-c("NAME.y", "NAME")) %>% rename( NAME = NAME.x)
# descriptives<-psych::describe(acs_all)
# new<-descriptives %>% filter(vars %in% c(12,47,82))

