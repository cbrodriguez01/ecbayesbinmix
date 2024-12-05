library(stringr)
library(lubridate)
#library(table1)
library(flextable)
library(magrittr)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(readxl)
library(gmodels)
library(furniture)
library(tableone)
`%!in%` <- Negate(`%in%`)
source("pvaluescal.R")

#For more detail information see "merged_census_ehr_enar.Rmd" and email from July 20th 2022
# - In the census data we found 1411 census tracts that are contained in the EHR data, however there were a few cases 
# where we had same census tract but a different county. 
# - To make sure we merge correctly, we requested county codes for participants so that we can merge using 
# both CTs and county fips


#------EHR DATA-----------
ehrdata<-readRDS("/Users/carmenrodriguez/Library/CloudStorage/OneDrive-HarvardUniversity/EC Data/R files/MCRdat_optimalcarevar.rds")
str(ehrdata)
table(ehrdata$race_eth)

#Unique census tracts
u<-unique(ehrdata$census_tract) 
#length(u)--1402 unique cts in this dataset

# In the EHR data I need to add county code for 94 participants that have these census tracts so that we can match them correctly- 
#Briana requested this data:stephenson_county_codes.txt. See emails from July 20 
county_codes<- read.table("/Users/carmenrodriguez/Library/CloudStorage/OneDrive-HarvardUniversity/EC Data/stephenson_county_codes.txt",
                          header = T,colClasses = c("character","character","character", "character", "character"))
county_codes<-county_codes %>% rename(Patient_ID_num1 = display_id)

#Merge with EHR-- remember this file is how we link participants IDs
ptids_crosswalks <- read_excel("/Users/carmenrodriguez/Library/CloudStorage/OneDrive-HarvardUniversity/EC Data/pt_id_crosswalk copy.xls") 

ehrdata_wids<-merge(ptids_crosswalks, ehrdata , 
                    by = "Patient_ID_Num")
ehrdata_countyfips<-right_join(county_codes, ehrdata_wids, by = "Patient_ID_num1") %>% arrange(Patient_ID_Num)

#3/26/24: Before splitting add new race/ehtnicity variable and exclusions
#Drop missing/unknown race/ethnicity category
#Recode- race/eth---- merge NHA WITH OTHER
ehrdata_countyfips<- ehrdata_countyfips %>% mutate(race_eth_recode = case_when(
  race_eth %in% c("Non-Hispanic Asian", "Other") ~ 4,
  race_eth == "Non-Hispanic White" ~ 1,
  race_eth == "Non-Hispanic Black" ~ 2,
  race_eth == "Hispanic" ~ 3,
  race_eth == "Missing/unknown" ~ NA,
))

ehrdata_countyfips$race_eth_recode<- factor(ehrdata_countyfips$race_eth_recode, levels = 1:4, labels = c("Non-Hispanic White", "Non-Hispanic Black","Hispanic", "Other"))  
table(ehrdata_countyfips$race_eth_recode)

table(ehrdata_countyfips$race_eth_recode, ehrdata_countyfips$nativity)


#Other pre-processing
table(ehrdata_countyfips$RX_Summ_Surg_Primary_Site_c)
ehrdata_countyfips$type_surg_received<-ifelse(ehrdata_countyfips$RX_Summ_Surg_Primary_Site_c %in% c("Other Surgery","Unknown"), 4, ehrdata_countyfips$RX_Summ_Surg_Primary_Site_c)
table(ehrdata_countyfips$type_surg_received)

ehrdata_countyfips$type_surg_received<-factor(ehrdata_countyfips$type_surg_received, levels = 1:4, labels = c("None", "Tumor destruction","Resection", "Other/Unknown"))

ehrdata_countyfips$facility_type1_cat_1<-ifelse(ehrdata_countyfips$facility_type1_cat%in% c("Unknown"), NA, ehrdata_countyfips$facility_type1_cat)
ehrdata_countyfips$facility_type1_cat_1<-factor(ehrdata_countyfips$facility_type1_cat_1, levels = 1:4, labels = c("Academic Medical Centers" ,"Community","Specialty","Teaching"))

ehrdata_countyfips$yeardx_fct<-as.factor(ehrdata_countyfips$yeardx)



#Fix table 1-- row percentages
variables1<- c("race_eth_recode", "nativity", "age_dx_cat","yeardx_fct","insurance_status","trt_summary_overall",
              "type_surg_received", "radiation_yn", "Rad_Reg_Rx_Mod_cat", "chemo_yn",
              "FIGOStage", "Grade_cat", "facility_type1_cat_1", "facility_size1",
              "facility_docspecialty1")

table1paper<-CreateTableOne(vars = variables1,strata = "optimal_care", data = ehrdata_countyfips, factorVars = variables1, includeNA= T, addOverall = T )
for (i in 1:length(variables1)) {
  sum = table1paper$CatTable[[2]][[i]]$freq + table1paper$CatTable[[3]][[i]]$freq
  table1paper$CatTable[[2]][[i]]$percent = (table1paper$CatTable[[2]][[i]]$freq / sum)*100
  table1paper$CatTable[[3]][[i]]$percent = (table1paper$CatTable[[3]][[i]]$freq / sum)*100
}

table1export<-print(table1paper, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(table1export, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/myTable1_full.csv")
#Export


# Split data by range of years of diagnosis to match census datasets
ehrdata_countyfips <- ehrdata_countyfips %>% rename( countycode = county_at_dx)
# 2010 ACS
ehrdata_10<- ehrdata_countyfips %>% filter(yeardx <= 2010) 
table(ehrdata_10$yeardx)
# 2015 ACS
ehrdata_15<- ehrdata_countyfips %>% filter(yeardx > 2010 & yeardx < 2015)
table(ehrdata_15$yeardx)
# 2019 ACS
ehrdata_19<- ehrdata_countyfips %>% filter(yeardx >= 2015)
table(ehrdata_19$yeardx)


#------CENSUS DATA  with clusters only & CENSUS DATA with Yost NSES Index only-------
load("./bayesbinmix_clustered_tracts.RData")
load("./YostIndexDat.RData")
acs2010<-readRDS("./dat10_temp.rds")
### Census data by survey wave
str(acs2010)
dat10<-merge(acs2010,yostindex10, by ="GEOID")
dat10<-as.data.frame(dat10)
dat10$census_tract<-str_sub(dat10$GEOID,start=6, end = 11) 
dat10$countycode<-str_sub(dat10$GEOID,start=3, end = 5)
dat10<-dat10 %>% rename(yostindex = yostquintiles10)


dat15<-merge(dat15,yostindex15, by ="GEOID")
dat15<-as.data.frame(dat15)
dat15$census_tract<-str_sub(dat15$GEOID,start=6, end = 11) 
dat15$countycode<-str_sub(dat15$GEOID,start=3, end = 5)
dat15<-dat15 %>% rename(yostindex = yostquintiles15)

dat19<-merge(dat19,yostindex19, by ="GEOID")
dat19<-as.data.frame(dat19)
dat19$census_tract<-str_sub(dat19$GEOID,start=6, end = 11) 
dat19$countycode<-str_sub(dat19$GEOID,start=3, end = 5)
dat19<-dat19 %>% rename(yostindex = yostquintiles19)


#---Merging datasets-----
ehrcensus10<- left_join(ehrdata_10, dat10, by= "census_tract", relationship = "many-to-many") #we only want 3787
# Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 76 of `x` matches multiple rows in `y`.
# ℹ Row 482 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
#####THIS IS BECAUSE WE NEED TO THEN KEEP COUNTIES AT DIAGNOSIS


#Now we only keep the counties at diagnosis when the cts are duplicated
table(ehrcensus10$countycode.x) # out of these 70, we only need to keep 34
table(ehrcensus10$countycode.y)

#ehrcensus10 %>% filter(!is.na(countycode.x)) %>% select(Patient_ID_Num, census_tract, countycode.x,countycode.y, NAME, race_eth, surgery_yn)

#Rows to be removed
rmv<- which(!(ehrcensus10$countycode.x == ehrcensus10$countycode.y))
#final
ehrcensus10<- ehrcensus10 %>% filter(row_number() %!in%  rmv)
#length(unique(ehrcensus10$Patient_ID_Num))


### added 4/30/24-- to be removed later
cts10<- unique(ehrcensus10$census_tract)



ehrcensus15<- left_join(ehrdata_15, dat15, by=  "census_tract", relationship = "many-to-many") #3119
#Now keep the counties at diagnosis
table(ehrcensus15$countycode.x) # out of these 68, we only need to keep 34
table(ehrcensus15$countycode.y)

# ehrcensus15 %>% filter(!is.na(countycode.x)) %>% select(Patient_ID_Num, census_tract, countycode.x,countycode.y, NAME, optimal_care)
# 
# ehrcensus15 %>% filter(!(countycode.x == countycode.y))%>% select(Patient_ID_Num, census_tract, countycode.x,countycode.y, NAME)

#Rows to be removed
rmv<- which(!(ehrcensus15$countycode.x == ehrcensus15$countycode.y))
#final
ehrcensus15<- ehrcensus15 %>% filter(row_number() %!in%  rmv)


ehrcensus19<- left_join(ehrdata_19, dat19, by=  "census_tract", relationship = "many-to-many") #2412
#Now keep the counties at diagnosis
table(ehrcensus19$countycode.x) # out of these 36, we only need to keep 16
table(ehrcensus19$countycode.y)

ehrcensus19 %>% filter(!is.na(countycode.x)) %>% select(Patient_ID_Num, census_tract, countycode.x,countycode.y, NAME, optimal_care)

ehrcensus19 %>% filter(!(countycode.x == countycode.y))%>% select(Patient_ID_Num, census_tract, countycode.x,countycode.y, NAME)

#Rows to be removed
rmv<- which(!(ehrcensus19$countycode.x == ehrcensus19$countycode.y))
#final
ehrcensus19<- ehrcensus19 %>% filter(row_number() %!in%  rmv)

#----------------------------------------------------------------------#
# LOOK AT DISTRIBUTIONS ACROSS DIFFERENT ACS SURVEY WAVES BEFORE MODEL #
#----------------------------------------------------------------------#

#---ACS 2006-2010----
####----Look at distribution of cluster assignment by optimal care and other variables

# table1::table1(~ race_eth + nativity  +  age_dx_cat  + Grade_cat + trt_summary_overall+ 
#                  RX_Summ_Surg_Primary_Site_c + radiation_yn + Rad_Reg_Rx_Mod_cat + chemo_yn + 
#                  FIGOStage + insurance_status + facility_type1_cat+ facility_size1  +  
#                  facility_docspecialty1 + optimal_care |ClusterAssignment, data= ehrcensus10, overall=F, extra.col=list(`P-value`=pvalue))
# 

#To get row%
library(furniture)
#https://cran.r-project.org/web/packages/furniture/vignettes/Table1.html
t1<-table1(ehrcensus10, race_eth_recode, nativity, age_dx_cat, trt_summary_overall,
          type_surg_received, radiation_yn , Rad_Reg_Rx_Mod_cat , chemo_yn ,
          FIGOStage, insurance_status, facility_type1_cat_1,facility_size1, 
          facility_docspecialty1, ClusterAssignment, splitby = ~ optimal_care, row_wise = TRUE, export = "tab1", NAkeep = T)

t1a<-table1(ehrcensus10, race_eth_recode, nativity, age_dx_cat, trt_summary_overall,
            type_surg_received, radiation_yn , Rad_Reg_Rx_Mod_cat , chemo_yn ,
            FIGOStage, insurance_status, facility_type1_cat_1,facility_size1, 
            facility_docspecialty1, optimal_care, splitby = ~ ClusterAssignment, row_wise = TRUE, export = "tab1a")


#Fix tables
variables<- c("race_eth_recode", "nativity", "age_dx_cat","insurance_status","trt_summary_overall",
               "type_surg_received", "radiation_yn", "Rad_Reg_Rx_Mod_cat", "chemo_yn",
               "FIGOStage", "Grade_cat", "facility_type1_cat_1", "facility_size1",
               "facility_docspecialty1", "ClusterAssignment","yostindex")

table2paper<-CreateTableOne(vars = variables,strata = "optimal_care", data = ehrcensus10, factorVars = variables, includeNA= T, addOverall = T )
for (i in 1:length(variables)) {
  sum = table2paper$CatTable[[2]][[i]]$freq + table2paper$CatTable[[3]][[i]]$freq
  table2paper$CatTable[[2]][[i]]$percent = (table2paper$CatTable[[2]][[i]]$freq / sum)*100
  table2paper$CatTable[[3]][[i]]$percent = (table2paper$CatTable[[3]][[i]]$freq / sum)*100
}

table2export<-print(table2paper, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(table2export, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab1.csv")
#Export

variablesn<- c("race_eth_recode", "nativity", "age_dx_cat","insurance_status","trt_summary_overall",
              "type_surg_received", "radiation_yn", "Rad_Reg_Rx_Mod_cat", "chemo_yn",
              "FIGOStage", "Grade_cat", "facility_type1_cat_1", "facility_size1",
              "facility_docspecialty1", "optimal_care", "yostindex")

table2papera<-CreateTableOne(vars = variablesn,strata = "ClusterAssignment", data = ehrcensus10, factorVars = variablesn, includeNA= T, addOverall = T)
for (i in 1:length(variablesn)) {
  sum = table2papera$CatTable[[1]][[i]]$freq 
  for(j in 2:10){
  table2papera$CatTable[[j]][[i]]$percent = (table2papera$CatTable[[j]][[i]]$freq / sum)*100
  }}
table2exporta<-print(table2papera, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
write.csv(table2exporta, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab1a.csv")



#---ACS 2011-2015----
####----Look at distribution of cluster assignment by optimal care and other variables

# table1::table1(~ race_eth + nativity  +  age_dx_cat  + Grade_cat + trt_summary_overall+ 
#                  RX_Summ_Surg_Primary_Site_c + radiation_yn + Rad_Reg_Rx_Mod_cat + chemo_yn + 
#                  FIGOStage + insurance_status + facility_type1_cat+ facility_size1  +  
#                  facility_docspecialty1 + optimal_care |ClusterAssignment_new, data= ehrcensus15, overall=F, extra.col=list(`P-value`=pvalue))


t2<-table1(ehrcensus15, race_eth_recode, nativity, age_dx_cat, trt_summary_overall,
           type_surg_received , radiation_yn , Rad_Reg_Rx_Mod_cat , chemo_yn ,
           FIGOStage, insurance_status, facility_type1_cat_1,facility_size1, 
           facility_docspecialty1, ClusterAssignment_new, splitby = ~ optimal_care, row_wise = TRUE, export = "tab2")

t2a<-table1(ehrcensus15, race_eth_recode, nativity, age_dx_cat, trt_summary_overall,
            type_surg_received, radiation_yn , Rad_Reg_Rx_Mod_cat , chemo_yn ,
            FIGOStage, insurance_status, facility_type1_cat_1,facility_size1, 
            facility_docspecialty1, optimal_care, splitby = ~ ClusterAssignment_new, row_wise = TRUE, export = "tab2a")


table3paper<-CreateTableOne(vars = variables,strata = "optimal_care", data = ehrcensus15, factorVars = variables, includeNA= T, addOverall = T )
for (i in 1:length(variables)) {
  sum = table3paper$CatTable[[2]][[i]]$freq + table3paper$CatTable[[3]][[i]]$freq
  table3paper$CatTable[[2]][[i]]$percent = (table3paper$CatTable[[2]][[i]]$freq / sum)*100
  table3paper$CatTable[[3]][[i]]$percent = (table3paper$CatTable[[3]][[i]]$freq / sum)*100
}

table3export<-print(table3paper, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(table3export, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab2.csv")
#Export

table3papera<-CreateTableOne(vars = variablesn,strata = "ClusterAssignment", data = ehrcensus15, factorVars = variablesn, includeNA= T, addOverall = T)
for (i in 1:length(variablesn)) {
  sum = table3papera$CatTable[[1]][[i]]$freq 
  for(j in 2:9){
    table3papera$CatTable[[j]][[i]]$percent = (table3papera$CatTable[[j]][[i]]$freq / sum)*100
  }}
table3exporta<-print(table3papera, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
write.csv(table3exporta, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab2a.csv")



#---ACS 2015-2019----
####----Look at distribution of cluster assignment by optimal care and other variables
# 
# table1::table1(~ race_eth + nativity  +  age_dx_cat  + Grade_cat + trt_summary_overall+ 
#                  RX_Summ_Surg_Primary_Site_c + radiation_yn + Rad_Reg_Rx_Mod_cat + chemo_yn + 
#                  FIGOStage + insurance_status + facility_type1_cat+ facility_size1  +  
#                  facility_docspecialty1 + optimal_care |ClusterAssignment_new, data= ehrcensus19, overall=F, extra.col=list(`P-value`=pvalue))
# 

t3<-table1(ehrcensus19, race_eth_recode, nativity, age_dx_cat, trt_summary_overall,
           type_surg_received , radiation_yn , Rad_Reg_Rx_Mod_cat , chemo_yn ,
           FIGOStage, insurance_status, facility_type1_cat,facility_size1, 
           facility_docspecialty1, ClusterAssignment, splitby = ~ optimal_care, row_wise = TRUE, export = "tab3")

t3a<-table1(ehrcensus19, race_eth_recode, nativity, age_dx_cat, trt_summary_overall,
            type_surg_received , radiation_yn , Rad_Reg_Rx_Mod_cat , chemo_yn ,
           FIGOStage, insurance_status, facility_type1_cat_1,facility_size1, 
           facility_docspecialty1, optimal_care, splitby = ~ ClusterAssignment, row_wise = TRUE, export = "tab3a")




table4paper<-CreateTableOne(vars = variables,strata = "optimal_care", data = ehrcensus19, factorVars = variables, includeNA= T, addOverall = T )
for (i in 1:length(variables)) {
  sum = table4paper$CatTable[[2]][[i]]$freq + table4paper$CatTable[[3]][[i]]$freq
  table4paper$CatTable[[2]][[i]]$percent = (table4paper$CatTable[[2]][[i]]$freq / sum)*100
  table4paper$CatTable[[3]][[i]]$percent = (table4paper$CatTable[[3]][[i]]$freq / sum)*100
}

table4export<-print(table4paper, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(table4export, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab3.csv")
#Export

table4papera<-CreateTableOne(vars = variablesn,strata = "ClusterAssignment", data = ehrcensus19, factorVars = variablesn, includeNA= T, addOverall = T)
for (i in 1:length(variablesn)) {
  sum = table4papera$CatTable[[1]][[i]]$freq 
  for(j in 2:7){
    table4papera$CatTable[[j]][[i]]$percent = (table4papera$CatTable[[j]][[i]]$freq / sum)*100
  }}
table4exporta<-print(table4papera, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
write.csv(table4exporta, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab3a.csv")



table(ehrcensus10$ClusterAssignment, ehrcensus10$yostindex)



#----------------------------------------------------------------------#
#---------- LOGISTIC REGRESSION MODELS BY SURVEY WAVE----------------- #
#----------------------------------------------------------------------#

#####------ 2006-2010-----
#We will model the probability of not receiving optimal care
#Set cluster with largest n as the reference
ehrcensus10$ClusterAssignmentr <- relevel(ehrcensus10$ClusterAssignment_final , ref= 9)
ehrcensus10$notoptimal<- ifelse(ehrcensus10$optimal_care == "Not optimal", 1, 0)

#check<-ehrcensus10 %>% filter(cluster==4) %>% select("census_tract","NAME", "race_eth",  "optimal_care")

#Unadjusted
mod1<-glm(optimal_care ~ ClusterAssignmentr,family=binomial(link='logit'),data= ehrcensus10)
##The odds of not receiving optimal care are 0.63 lower

#Adjust for socio-demographic characteristics
mod2<- glm(optimal_care ~ ClusterAssignmentr  + yeardx + race_eth_recode + age_dx_cat + insurance_status,family=binomial(link='logit'),data= ehrcensus10)
summary(mod2)
#Based on distribution of age by census tracts, population in census tracts in cluster 4 are older-- highest median age compared to other clusters
#Facility information 
mod3<-glm(optimal_care ~ ClusterAssignmentr + yeardx + facility_type1_cat_1,family=binomial(link='logit'),data= ehrcensus10)
summary(mod3)
#Adjust for  tumor info
mod4<- glm(optimal_care ~ ClusterAssignmentr + yeardx + FIGOStage + Grade_cat  ,family=binomial(link='logit'),data= ehrcensus10)
summary(mod4)

#Full model
mod5<- glm(optimal_care ~ ClusterAssignmentr  + yeardx  + age_dx_cat + FIGOStage + Grade_cat + insurance_status  + facility_type1_cat_1 ,family=binomial(link='logit'),data= ehrcensus10)
summary(mod5)

p<-glm(optimal_care ~  FIGOStage,family=binomial(link='logit'),data= ehrcensus15)
summary(p)
exp(coef(p))


#---Models with Yost index--- direction we expect based on prev literature
mod6<- glm(notoptimal ~ yostindex,family=binomial(link='logit'),data= ehrcensus10)
summary(mod6)
mod7<- glm(optimal_care ~ yostindex + race_eth_recode + yeardx  + age_dx_cat + FIGOStage + Grade_cat + insurance_status  + facility_type1_cat_1 ,family=binomial(link='logit'),data= ehrcensus10)
summary(mod7)





#mod5a<- glm(optimal_care ~ ClusterAssignmentr + race_eth_recode + as.factor(yeardx)  + age_dx_cat + FIGOStage + Grade_cat + insurance_status  + facility_type1_cat_1 ,family=binomial(link='logit'),data= ehrcensus10)
#Full model with interaction between race/eth and cluster-- small cell counts -- i dont thin this is feasible
#unstable estimates of model parameters, including interaction terms.  large standard errors or inflated estimates of effect sizes.
#mod6<- glm(notoptimal ~ ClusterAssignmentr*race_eth_recode + as.factor(yeardx)  + age_dx_cat + FIGOStage + Grade_cat + insurance_status  + facility_type1_cat_1 ,family=binomial(link='logit'),data= ehrcensus10)
#summary(mod6)


#Nice output
#https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html
library(sjPlot)
library(sjmisc)
library(sjlabelled)
            
# #pred.labels1<-c(paste0("NSES:Cluster", sep= " ", c(1:5, 7:9)),"YearDx:2007", "YearDx:2008", "YearDx:2009","YearDx:2010",
#                 "NHB", "Hispanic", "Other","Age: 50-64", "Age:65+","Insurance:Medicare","Insurance:Public", "Insurance: Other", "Insurance:not insured",
#                  "Facility: Community", "Facility: Specialty", "Facility: Teaching", "Stage:II", "Stage:III", "Stage: IV","Grade:II", "Grade:III")



tab_model(mod1, 
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F, digits = 3)

tab_model(mod5,  p.style = "stars",
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F, digits = 3)


pred.labels2<-c("NSES:Yost:HM","NSES:Yost:M","NSES:Yost:LM", "NSES:Yost:L","YearDx",
                "NHB", "Hispanic", "Other","Age: 50-64", "Age:65+","Insurance:Medicare","Insurance:Public", "Insurance: Other", "Insurance:not insured",
                "Facility: Community", "Facility: Specialty", "Facility: Teaching", "Stage:II", "Stage:III", "Stage: IV","Grade:II", "Grade:III")

tab_model(mod7, p.style = "stars",
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F, digits = 3)



#Plot models: https://strengejacke.github.io/sjPlot/articles/blackwhitefigures.html
# set variable label for response
set_label(ehrcensus10$notoptimal) <- "Did not receive optimal care"
plot_models(mod5, vline.color = "black",legend.title = "", m.labels = "Outcome: Optimal Care")

#####------ 2011-2015-----
ehrcensus15$ClusterAssignmentr <- relevel(ehrcensus15$ClusterAssignment , ref= 6)
ehrcensus15$notoptimal<- ifelse(ehrcensus15$optimal_care == "Not optimal", 1, 0)

#Unadjusted
mod1_2<-glm(notoptimal ~ ClusterAssignmentr,family=binomial(link='logit'),data= ehrcensus15)
##The odds of not receiving optimal care are 0.63 lower

#Adjust for socio-demographic characteristics
mod2_2<- glm(notoptimal ~ ClusterAssignmentr + yeardx + race_eth_recode + age_dx_cat + insurance_status,family=binomial(link='logit'),data= ehrcensus15)
summary(mod2_2)
#Facility information 
mod3_2<-glm(notoptimal ~ ClusterAssignmentr + yeardx + facility_type1_cat_1,family=binomial(link='logit'),data= ehrcensus15)
summary(mod3_2)
#Adjust for  tumor info
mod4_2<- glm(notoptimal ~ ClusterAssignmentr + yeardx + FIGOStage + Grade_cat  ,family=binomial(link='logit'),data= ehrcensus15)
summary(mod4_2)

#Full model
mod5_2<- glm(notoptimal ~ ClusterAssignmentr + race_eth_recode + yeardx  + age_dx_cat + FIGOStage + Grade_cat + insurance_status  
           + facility_type1_cat_1,family=binomial(link='logit'),data= ehrcensus15)
summary(mod5_2)

mod7_2<- glm(notoptimal ~ yostindex + race_eth_recode + yeardx  + age_dx_cat + FIGOStage + Grade_cat + insurance_status  + facility_type1_cat_1 ,family=binomial(link='logit'),data= ehrcensus15)
summary(mod7_2)



# pred.labels2<-c(paste0("NSES:Cluster", sep= " ", c(1:5, 7:8)),"YearDx:2012", "YearDx:2013", "YearDx:2014",
#                 "NHB", "Hispanic", "Other","Age: 50-64", "Age:65+","Insurance:Medicare","Insurance:Public", "Insurance: Other", "Insurance:not insured",
#                 "Facility: Community", "Facility: Specialty", "Facility: Teaching", "Stage:II", "Stage:III", "Stage: IV","Grade:II", "Grade:III")

#tab_model(mod1_2, mod2_2, mod3_2, mod4_2, mod5_2, pred.labels = pred.labels2, p.style = "stars", dv.labels = paste0("Model", sep=" ", 1:5),
 #         string.pred = "Coeffcient",
#          string.ci = "95% CI", show.intercept = F)


tab_model(mod5_2,  p.style = "stars",
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F, digits = 3)


tab_model(mod7_2, p.style = "stars",
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F, digits = 3)


#####------ 2015-2019-----
ehrcensus19$ClusterAssignment<-factor(ehrcensus19$ClusterAssignment, levels = c(1,2,3,5,6,8), labels= c("cluster1","cluster2", "cluster3", "cluster5",
                                                                                                       "cluster6", "cluster8"))
ehrcensus19$ClusterAssignmentr <- relevel(ehrcensus19$ClusterAssignment , ref= 5)
ehrcensus19$notoptimal<- ifelse(ehrcensus19$optimal_care == "Not optimal", 1, 0)

#Unadjusted
mod1_3<-glm(notoptimal ~ ClusterAssignmentr,family=binomial(link='logit'),data= ehrcensus19)
##The odds of not receiving optimal care are 0.63 lower

#Adjust for socio-demographic characteristics
mod2_3<- glm(notoptimal ~ ClusterAssignmentr + yeardx + race_eth_recode + age_dx_cat + insurance_status,family=binomial(link='logit'),data= ehrcensus19)
summary(mod2_3)
#Facility information 
mod3_3<-glm(notoptimal ~ ClusterAssignmentr + yeardx + facility_type1_cat_1,family=binomial(link='logit'),data= ehrcensus19)
summary(mod3_3)
#Adjust for  tumor info
mod4_3<- glm(notoptimal ~ ClusterAssignmentr + yeardx + FIGOStage + Grade_cat  ,family=binomial(link='logit'),data= ehrcensus19)
summary(mod4_3)

#Full model
mod5_3<- glm(notoptimal ~ ClusterAssignmentr + race_eth_recode + yeardx  + age_dx_cat + FIGOStage + Grade_cat + insurance_status  
             + facility_type1_cat_1,family=binomial(link='logit'),data= ehrcensus19)
summary(mod5_3)

mod7_3<- glm(notoptimal ~ yostindex + race_eth_recode + yeardx  + age_dx_cat + FIGOStage + Grade_cat + insurance_status  + facility_type1_cat_1 ,family=binomial(link='logit'),data= ehrcensus19)
summary(mod7_3)

# pred.labels2<-c(paste0("NSES:Cluster", sep= " ", c(1,2,3,5,8)),"YearDx:2016", "YearDx:2017",
#                 "NHB", "Hispanic", "Other","Age: 50-64", "Age:65+","Insurance:Medicare","Insurance:Public", "Insurance: Other", "Insurance:not insured",
#                 "Facility: Community", "Facility: Specialty", "Facility: Teaching", "Stage:II", "Stage:III", "Stage: IV","Grade:II", "Grade:III")
# 
# tab_model(mod1_3, mod2_3, mod3_3, mod4_3, mod5_3,  p.style = "stars", pred.labels =  pred.labels2, dv.labels = paste0("Model", sep=" ", 1:5),
#           string.pred = "Coeffcient",
#           string.ci = "95% CI", show.intercept = F)



tab_model(mod5_3,  p.style = "stars",
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F, digits = 3)


tab_model(mod7_3, p.style = "stars",
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F, digits = 3)





# 
# #Using cluster colors-- unadjusted
# mod1a<- glm(notoptimal ~ cluster_color,family=binomial(link='logit'), data= ehrcensus10)
# summary(mod1a)
# 
# mod1b<- glm(notoptimal ~ cluster_color,family=binomial(link='logit'), data= ehrcensus15)
# summary(mod1b)
# 
# mod1c<- glm(notoptimal ~ cluster_color, family=binomial(link='logit'),data= ehrcensus19)
# summary(mod1c)
# 
# tab_model(mod1a, mod1b, mod1c, p.style = "stars", dv.labels = paste0("ACS", sep=" ", 1:3),
#           string.pred = "Coeffcient",
#           string.ci = "95% CI", show.intercept = F)
# 
# 
# 
# #Crosstabs
# table(dat10$cluster_color, dat15$cluster_color)
# 
# 
# #Sanity checks
# ehrdata_countyfips$notoptimal<- ifelse(ehrdata_countyfips$optimal_care == "Not optimal", 1, 0)
# moda<- glm(optimal_care ~ race_eth_recode + as.factor(yeardx)  + age_dx_cat + FIGOStage + Grade_cat + insurance_status  
#              + facility_type1_cat_1,family=binomial(link='logit'),data= ehrdata_countyfips)
# summary(moda)
# 
# moda1<- glm(notoptimal ~  FIGOStage,family=binomial(link='logit'),data= ehrdata_countyfips)
# summary(moda1)
# exp(coef(moda1))
# 
# 
# CrossTable(ehrdata_countyfips$FIGOStage,ehrdata_countyfips$optimal_care, prop.c = F,  chisq = T,  digits = 3)
