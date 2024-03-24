library(stringr)
library(lubridate)
library(table1)
library(flextable)
library(magrittr)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(readxl)
library(gmodels)
`%!in%` <- Negate(`%in%`)

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


#------CENSUS DATA  with clusters only------
load("./bayesbinmix_clustered_tracts.RData")
### Census data by survey wave
str(dat10)
dat10<-as.data.frame(dat10)
dat10$census_tract<-str_sub(dat10$GEOID,start=6, end = 11) 
dat10$countycode<-str_sub(dat10$GEOID,start=3, end = 5)

dat15<-as.data.frame(dat15)
dat15$census_tract<-str_sub(dat15$GEOID,start=6, end = 11) 
dat15$countycode<-str_sub(dat15$GEOID,start=3, end = 5)

dat19<-as.data.frame(dat19)
dat19$census_tract<-str_sub(dat19$GEOID,start=6, end = 11) 
dat19$countycode<-str_sub(dat19$GEOID,start=3, end = 5)


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


#---ACS 2006-2010----
####----Look at distribution of cluster assignment by optimal care and other variables
#Re-assign labels lost during merging
label(ehrcensus10$Age_Dx)<- "Age at diagnosis"
label(ehrcensus10$yeardx)<- "Year of diagnosis"
label(ehrcensus10$Grade_cat)<- "Grade at diagnosis"
label(ehrcensus10$Stage) <- "Stage at diagnosis (in progress)"
label(ehrcensus10$RX_Dt_Surg_formatted)<- "Surgery date"
label(ehrcensus10$RX_Dt_Rad_formatted)<- "Radiation first date"
label(ehrcensus10$RX_Dt_Chemo_formatted)<- "Chemotherapy first date"
label(ehrcensus10$RX_Summ_Surg_Primary_Site_c)<- "Type of surgery"
label(ehrcensus10$Reason_No_Surg)<- "Reason for no surgery"
label(ehrcensus10$RX_Summ_Surg_Rad_Seq)<- "Surgery-radiation sequence"
label(ehrcensus10$RX_Summ_Chemo_f)<- "Chemotherapy summary/reasons no chemo"
label(ehrcensus10$Rad_Reg_Rx_Mod_cat) <- "Type of radiation administered"
label(ehrcensus10$Reason_No_Radiation) <- "Reason no radiation"
label(ehrcensus10$RX_Dt_Most_Defin_Surg_formatted)<-"Date of most recent surgery"
label(ehrcensus10$facility_size1)<- "Size of facility 1"
label(ehrcensus10$facility_type1_cat)<-"Facility 1 type"
label(ehrcensus10$facility_docspecialty1)<- "Specialty of doctor in facility 1"
label(ehrcensus10$radiation_yn)<- "Radiation status"
label(ehrcensus10$chemo_yn)<- "Chemotherapy status"
label(ehrcensus10$trt_summary_overall) <- "Overall Treatment Status"
label(ehrcensus10$earliestdate)<- "Date of first treatment"
label(ehrcensus10$year1stTRT)<- "Year of first treatment"
label(ehrcensus10$yeardx)<- "Year of diagnosis"
label(ehrcensus10$nativity)<-"Birthplace"
label(ehrcensus10$race_eth)<-"Race-Ethnicity"
label(ehrcensus10$RX_Summ_Surg_Primary_Site_c) <- "Type of surgery recieved"
label(ehrcensus10$age_dx_cat)<-"Age at diagnosis (y)"
label(ehrcensus10$insurance_status)<-"Insurance Status at Diagnosis"
label(ehrcensus10$optimal_care)<-"Optimal care"
label(ehrcensus10$age_dx_cat)<-"Age at diagnosis (y)"

#Add p-value
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

table1::table1(~ race_eth + nativity  +  age_dx_cat  + Grade_cat + trt_summary_overall+ 
                 RX_Summ_Surg_Primary_Site_c + radiation_yn + Rad_Reg_Rx_Mod_cat + chemo_yn + 
                 FIGOStage + insurance_status + facility_type1_cat+ facility_size1  +  
                 facility_docspecialty1 + optimal_care |ClusterAssignment, data= ehrcensus10, overall=F, extra.col=list(`P-value`=pvalue))


##MAKE THIS MORE EFFICIENT BUT WILL KEEP LIKE THIS FOR NOW

set.seed(1234)
CrossTable(ehrcensus10$race_eth, ehrcensus10$ClusterAssignment, prop.c = F,  chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$nativity, ehrcensus10$ClusterAssignment, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$age_dx_cat, ehrcensus10$ClusterAssignment, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$insurance_status, ehrcensus10$ClusterAssignment, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$Grade_cat, ehrcensus10$ClusterAssignment, prop.c = F,  chisq = T, digits = 3)
CrossTable(ehrcensus10$RX_Summ_Surg_Primary_Site_c, ehrcensus10$ClusterAssignment, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$trt_summary_overall, ehrcensus10$ClusterAssignment, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$Rad_Reg_Rx_Mod_cat, ehrcensus10$ClusterAssignment, prop.c = F,chisq = T, digits = 3,simulate.p.value=TRUE)
CrossTable(ehrcensus10$chemo_yn, ehrcensus10$ClusterAssignment, prop.c = F, chisq =T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$FIGOStage, ehrcensus10$ClusterAssignment, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$facility_type1_cat, ehrcensus10$ClusterAssignment, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$facility_size1, ehrcensus10$ClusterAssignment, prop.c = F, chisq = T, digits = 3,simulate.p.value=TRUE)
CrossTable(ehrcensus10$facility_docspecialty1, ehrcensus10$ClusterAssignment, prop.c = F, prop.t = F, chisq = T, digits = 3, expected = F,simulate.p.value=TRUE)
CrossTable(ehrcensus10$optimal_care, ehrcensus10$ClusterAssignment, prop.c = F, prop.t = F, chisq = T, digits = 3, expected = F)


#---ACS 2011-2015----
####----Look at distribution of cluster assignment by optimal care and other variables
#Re-assign labels lost during merging
label(ehrcensus15$Age_Dx)<- "Age at diagnosis"
label(ehrcensus15$yeardx)<- "Year of diagnosis"
label(ehrcensus15$Grade_cat)<- "Grade at diagnosis"
label(ehrcensus15$Stage) <- "Stage at diagnosis (in progress)"
label(ehrcensus15$RX_Dt_Surg_formatted)<- "Surgery date"
label(ehrcensus15$RX_Dt_Rad_formatted)<- "Radiation first date"
label(ehrcensus15$RX_Dt_Chemo_formatted)<- "Chemotherapy first date"
label(ehrcensus15$RX_Summ_Surg_Primary_Site_c)<- "Type of surgery"
label(ehrcensus15$Reason_No_Surg)<- "Reason for no surgery"
label(ehrcensus15$RX_Summ_Surg_Rad_Seq)<- "Surgery-radiation sequence"
label(ehrcensus15$RX_Summ_Chemo_f)<- "Chemotherapy summary/reasons no chemo"
label(ehrcensus15$Rad_Reg_Rx_Mod_cat) <- "Type of radiation administered"
label(ehrcensus15$Reason_No_Radiation) <- "Reason no radiation"
label(ehrcensus15$RX_Dt_Most_Defin_Surg_formatted)<-"Date of most recent surgery"
label(ehrcensus15$facility_size1)<- "Size of facility 1"
label(ehrcensus15$facility_type1_cat)<-"Facility 1 type"
label(ehrcensus15$facility_docspecialty1)<- "Specialty of doctor in facility 1"
label(ehrcensus15$radiation_yn)<- "Radiation status"
label(ehrcensus15$chemo_yn)<- "Chemotherapy status"
label(ehrcensus15$trt_summary_overall) <- "Overall Treatment Status"
label(ehrcensus15$earliestdate)<- "Date of first treatment"
label(ehrcensus15$year1stTRT)<- "Year of first treatment"
label(ehrcensus15$yeardx)<- "Year of diagnosis"
label(ehrcensus15$nativity)<-"Birthplace"
label(ehrcensus15$race_eth)<-"Race-Ethnicity"
label(ehrcensus15$RX_Summ_Surg_Primary_Site_c) <- "Type of surgery recieved"
label(ehrcensus15$age_dx_cat)<-"Age at diagnosis (y)"
label(ehrcensus15$insurance_status)<-"Insurance Status at Diagnosis"
label(ehrcensus15$optimal_care)<-"Optimal care"
label(ehrcensus15$age_dx_cat)<-"Age at diagnosis (y)"


table1::table1(~ race_eth + nativity  +  age_dx_cat  + Grade_cat + trt_summary_overall+ 
                 RX_Summ_Surg_Primary_Site_c + radiation_yn + Rad_Reg_Rx_Mod_cat + chemo_yn + 
                 FIGOStage + insurance_status + facility_type1_cat+ facility_size1  +  
                 facility_docspecialty1 + optimal_care |ClusterAssignment_new, data= ehrcensus15, overall=F, extra.col=list(`P-value`=pvalue))


set.seed(1234)
CrossTable(ehrcensus15$race_eth, ehrcensus15$ClusterAssignment_new, prop.c = F,  chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus15$nativity, ehrcensus15$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus15$age_dx_cat, ehrcensus15$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus15$insurance_status, ehrcensus15$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus15$Grade_cat, ehrcensus15$ClusterAssignment_new, prop.c = F,  chisq = T, digits = 3)
CrossTable(ehrcensus15$RX_Summ_Surg_Primary_Site_c, ehrcensus15$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus15$trt_summary_overall, ehrcensus15$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus15$Rad_Reg_Rx_Mod_cat, ehrcensus15$ClusterAssignment_new, prop.c = F,chisq = T, digits = 3,simulate.p.value=TRUE)
CrossTable(ehrcensus15$chemo_yn, ehrcensus15$ClusterAssignment_new, prop.c = F, chisq =T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus15$FIGOStage, ehrcensus15$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus15$facility_type1_cat, ehrcensus15$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus15$facility_size1, ehrcensus15$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3,simulate.p.value=TRUE)
CrossTable(ehrcensus15$facility_docspecialty1, ehrcensus15$ClusterAssignment_new, prop.c = F, prop.t = F, chisq = T, digits = 3, expected = F,simulate.p.value=TRUE)
CrossTable(ehrcensus15$optimal_care, ehrcensus15$ClusterAssignment_new, prop.c = F, prop.t = F, chisq = T, digits = 3, expected = F)



#---ACS 2015-2019----
####----Look at distribution of cluster assignment by optimal care and other variables
#Re-assign labels lost during merging
label(ehrcensus19$Age_Dx)<- "Age at diagnosis"
label(ehrcensus19$yeardx)<- "Year of diagnosis"
label(ehrcensus19$Grade_cat)<- "Grade at diagnosis"
label(ehrcensus19$Stage) <- "Stage at diagnosis (in progress)"
label(ehrcensus19$RX_Dt_Surg_formatted)<- "Surgery date"
label(ehrcensus19$RX_Dt_Rad_formatted)<- "Radiation first date"
label(ehrcensus19$RX_Dt_Chemo_formatted)<- "Chemotherapy first date"
label(ehrcensus19$RX_Summ_Surg_Primary_Site_c)<- "Type of surgery"
label(ehrcensus19$Reason_No_Surg)<- "Reason for no surgery"
label(ehrcensus19$RX_Summ_Surg_Rad_Seq)<- "Surgery-radiation sequence"
label(ehrcensus19$RX_Summ_Chemo_f)<- "Chemotherapy summary/reasons no chemo"
label(ehrcensus19$Rad_Reg_Rx_Mod_cat) <- "Type of radiation administered"
label(ehrcensus19$Reason_No_Radiation) <- "Reason no radiation"
label(ehrcensus19$RX_Dt_Most_Defin_Surg_formatted)<-"Date of most recent surgery"
label(ehrcensus19$facility_size1)<- "Size of facility 1"
label(ehrcensus19$facility_type1_cat)<-"Facility 1 type"
label(ehrcensus19$facility_docspecialty1)<- "Specialty of doctor in facility 1"
label(ehrcensus19$radiation_yn)<- "Radiation status"
label(ehrcensus19$chemo_yn)<- "Chemotherapy status"
label(ehrcensus19$trt_summary_overall) <- "Overall Treatment Status"
label(ehrcensus19$earliestdate)<- "Date of first treatment"
label(ehrcensus19$year1stTRT)<- "Year of first treatment"
label(ehrcensus19$yeardx)<- "Year of diagnosis"
label(ehrcensus19$nativity)<-"Birthplace"
label(ehrcensus19$race_eth)<-"Race-Ethnicity"
label(ehrcensus19$RX_Summ_Surg_Primary_Site_c) <- "Type of surgery recieved"
label(ehrcensus19$age_dx_cat)<-"Age at diagnosis (y)"
label(ehrcensus19$insurance_status)<-"Insurance Status at Diagnosis"
label(ehrcensus19$optimal_care)<-"Optimal care"
label(ehrcensus19$age_dx_cat)<-"Age at diagnosis (y)"

table1::table1(~ race_eth + nativity  +  age_dx_cat  + Grade_cat + trt_summary_overall+ 
                 RX_Summ_Surg_Primary_Site_c + radiation_yn + Rad_Reg_Rx_Mod_cat + chemo_yn + 
                 FIGOStage + insurance_status + facility_type1_cat+ facility_size1  +  
                 facility_docspecialty1 + optimal_care |ClusterAssignment_new, data= ehrcensus19, overall=F, extra.col=list(`P-value`=pvalue))


##MAKE THIS MORE EFFICIENT BUT WILL KEEP LIKE THIS FOR NOW
set.seed(1234)
CrossTable(ehrcensus19$race_eth, ehrcensus19$ClusterAssignment_new, prop.c = F,  chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus19$nativity, ehrcensus19$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus19$age_dx_cat, ehrcensus19$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus19$insurance_status, ehrcensus19$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus19$Grade_cat, ehrcensus19$ClusterAssignment_new, prop.c = F,  chisq = T, digits = 3)
CrossTable(ehrcensus19$RX_Summ_Surg_Primary_Site_c, ehrcensus19$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus19$trt_summary_overall, ehrcensus19$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus19$Rad_Reg_Rx_Mod_cat, ehrcensus19$ClusterAssignment_new, prop.c = F,chisq = T, digits = 3,simulate.p.value=TRUE)
CrossTable(ehrcensus19$chemo_yn, ehrcensus19$ClusterAssignment_new, prop.c = F, chisq =T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus19$FIGOStage, ehrcensus19$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus19$facility_type1_cat, ehrcensus19$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus19$facility_size1, ehrcensus19$ClusterAssignment_new, prop.c = F, chisq = T, digits = 3,simulate.p.value=TRUE)
CrossTable(ehrcensus19$facility_docspecialty1, ehrcensus19$ClusterAssignment_new, prop.c = F, prop.t = F, chisq = T, digits = 3, expected = F,simulate.p.value=TRUE)
CrossTable(ehrcensus19$optimal_care, ehrcensus19$ClusterAssignment_new, prop.c = F, prop.t = F, chisq = T, digits = 3, expected = F)


#--------LOGISTIC REGRESSION MODELS BY SURVEY WAVE----------

#####------ 2006-2010-----
#We will model the probability of not receiving optimal care
#I want cluster 4 to be reference
ehrcensus10$ClusterAssignmentr <- relevel(ehrcensus10$ClusterAssignment , ref= 4)
ehrcensus10$notoptimal<- ifelse(ehrcensus10$optimal_care == "Not optimal", 1, 0)

check<-ehrcensus10 %>% filter(cluster==4) %>% select("census_tract","NAME", "race_eth",  "optimal_care")

#Unadjusted
mod1<-glm(notoptimal ~ ClusterAssignmentr,family=binomial(link='logit'),data= ehrcensus10)
##The odds of not receiving optimal care are 0.63 lower

table1(~ race_eth + nativity  +  age_dx_cat  + Grade_cat + trt_summary_overall+ 
         RX_Summ_Surg_Primary_Site_c + radiation_yn + Rad_Reg_Rx_Mod_cat + chemo_yn + 
         FIGOStage + insurance_status + facility_type1_cat+ facility_size1  +  
         facility_docspecialty1 + ClusterAssignment| optimal_care, data= ehrcensus10, overall=F, extra.col=list(`P-value`=pvalue))
# 
# #Fix p-values-- TBF
# set.seed(1234)
# variables<- c("optimal_care", "race_eth ","nativity", "age_dx_cat", "Grade_cat", "trt_summary_overall", 
#                 "RX_Summ_Surg_Primary_Site_c", "radiation_yn", "Rad_Reg_Rx_Mod_cat" , "chemo_yn", 
#                 "FIGOStage", "insurance_status", "facility_type1_cat", "facility_size1",
#                 "facility_docspecialty1", "ClusterAssignment")
# p_values_check<-function(variables, dataset){
#   pvalues<-matrix(0, nrow = length(variables), ncol = 3)
#   colnames(pvalues)<-c("varname", "chisq", "fisher")
#   r<-1
#   for( v in variables){
#     tests<-CrossTable(dataset$v, dataset$variables[1], prop.c = F,  chisq = T, fisher = T, digits = 3, simulate.p.value=TRUE)
#     chiq<-tests$chisq$p.value
#     fisher<-tests$fisher.ts$p.value
#     pvalues[r,]<- c(v, chiq, fisher)
#     r<-r+1
# }
# 
# }
set.seed(1234)
CrossTable(ehrcensus10$race_eth, ehrcensus10$optimal_care, prop.c = F,  chisq = T, fisher = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$nativity, ehrcensus10$optimal_care, prop.c = F,  chisq = T, fisher = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$age_dx_cat, ehrcensus10$optimal_care, prop.c = F,  chisq = T, fisher = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$Grade_cat, ehrcensus10$optimal_care, prop.c = F,  chisq = T, fisher = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$trt_summary_overall, ehrcensus10$optimal_care, prop.c = F,  chisq = T, fisher = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$RX_Summ_Surg_Primary_Site_c, ehrcensus10$optimal_care, prop.c = F,  chisq = T, fisher = T, digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$Rad_Reg_Rx_Mod_cat, ehrcensus10$optimal_care, prop.c = F,  chisq = T,  digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$radiation_yn, ehrcensus10$optimal_care, prop.c = F,  chisq = T,  digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$chemo_yn, ehrcensus10$optimal_care, prop.c = F,  chisq = T,  digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$insurance_status, ehrcensus10$optimal_care, prop.c = F,  chisq = T,  digits = 3, simulate.p.value=TRUE)
CrossTable(ehrcensus10$facility_type1_cat, ehrcensus10$optimal_care, prop.c = F,  chisq = T,  digits = 3,simulate.p.value=TRUE)
CrossTable(ehrcensus10$facility_size1, ehrcensus10$optimal_care, prop.c = F,  chisq = T,  digits = 3)
CrossTable(ehrcensus10$ClusterAssignment, ehrcensus10$optimal_care, prop.c = F,  chisq = T,  digits = 3)



#Adjust for socio-demographic characteristics
#Recode- race/eth---- not merge other wiht missing
ehrcensus10<- ehrcensus10 %>% mutate( race_eth_recode = case_when(
  race_eth %in% c("Non-Hispanic Asian", "Other", "Missing/unknown") ~ 4,
  race_eth == "Non-Hispanic White" ~ 1,
  race_eth == "Non-Hispanic Black" ~ 2,
  race_eth == "Hispanic" ~ 3,
))
                          
ehrcensus10$race_eth_recode<- factor(ehrcensus10$race_eth_recode, levels = 1:4, labels = c("Non-Hispanic White", "Non-Hispanic Black","Hispanic", "Other"))  
  

mod2<- glm(notoptimal ~ ClusterAssignmentr + as.factor(yeardx) + race_eth_recode + age_dx_cat + insurance_status,family=binomial(link='logit'),data= ehrcensus10)
summary(mod2)
#Facility information 
mod3<-glm(notoptimal ~ ClusterAssignmentr + as.factor(yeardx) + facility_type1_cat + facility_size1 +  facility_docspecialty1,family=binomial(link='logit'),data= ehrcensus10)
summary(mod3)
#Adjust for  tumor info
mod4<- glm(notoptimal ~ ClusterAssignmentr + as.factor(yeardx) + FIGOStage + Grade_cat  ,family=binomial(link='logit'),data= ehrcensus10)
summary(mod4)

#Full model
mod5<- glm(notoptimal ~ ClusterAssignmentr + race_eth_recode + as.factor(yeardx)  + age_dx_cat + FIGOStage + Grade_cat + insurance_status  + facility_type1_cat + facility_size1 +  facility_docspecialty1 ,family=binomial(link='logit'),data= ehrcensus10)
summary(mod5)

#Nice output
#https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html
library(sjPlot)
library(sjmisc)
library(sjlabelled)
pred.labels <-c(paste0("NSES:Cluster", sep= " ", 1:9),"YearDx:2006","YearDx:2007", "YearDx:2008", "YearDx:2009","YearDx:2010","NHW",
                 "NHB", "Hispanic", "Other", "Age:<50","Age: 50-64", "Age:65+","Insurance:Private", "Insurance:Medicare","Insurance:Public", "Insurance: Other", "Insurance:not insured",
                 "Facility: academic", "Facility: Community", "Facility: Specialty", "Facility: Teaching", "Facility: Teaching", "Facility: Unknown","Size:small", "Size:Medium",
                "Size: Large","Doc:IntMed", "Doc: Hematology", "Doc: Gyno", "Doc:Oncology", "Doc: Rad", "Doc:missing", "Doc:other", "Stage: I","Stage:II", "Stage:III", "Stage: IV", "Grade:I","Grade:II", "Grade:III")
            
pred.labels2<-c(paste0("NSES:Cluster", sep= " ", c(1:3, 5:9)),"YearDx:2007", "YearDx:2008", "YearDx:2009","YearDx:2010",
                "NHB", "Hispanic", "Other","Age: 50-64", "Age:65+","Insurance:Medicare","Insurance:Public", "Insurance: Other", "Insurance:not insured",
                 "Facility: Community", "Facility: Specialty", "Facility: Teaching", "Facility: Unknown","Size:Medium",
                "Size: Large", "Doc: Hematology", "Doc: Gyno", "Doc:Oncology", "Doc: Rad", "Doc:missing", "Doc:other", "Stage:II", "Stage:III", "Stage: IV","Grade:II", "Grade:III")

tab_model(mod1, mod2, mod3, mod4, mod5,  p.style = "stars", pred.labels =  pred.labels2, dv.labels = paste0("Model", sep=" ", 1:5),
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F)


#Plot models: https://strengejacke.github.io/sjPlot/articles/blackwhitefigures.html
# set variable label for response
set_label(ehrcensus10$notoptimal) <- "Did not receive optimal care"
plot_models(mod1, mod5)



#####------ 2011-2015-----

ehrcensus15$ClusterAssignmentr <- relevel(ehrcensus15$ClusterAssignment_new , ref= 2)
ehrcensus15$notoptimal<- ifelse(ehrcensus15$optimal_care == "Not optimal", 1, 0)

#Unadjusted
mod1_2<-glm(notoptimal ~ ClusterAssignmentr,family=binomial(link='logit'),data= ehrcensus15)
##The odds of not receiving optimal care are 0.63 lower

table1(~ race_eth + nativity  +  age_dx_cat  + Grade_cat + trt_summary_overall+ 
         RX_Summ_Surg_Primary_Site_c + radiation_yn + Rad_Reg_Rx_Mod_cat + chemo_yn + 
         FIGOStage + insurance_status + facility_type1_cat+ facility_size1  +  
         facility_docspecialty1 + ClusterAssignment_new| optimal_care, data= ehrcensus15, overall=F, extra.col=list(`P-value`=pvalue))



#Adjust for socio-demographic characteristics
#Recode- race/eth
ehrcensus15<- ehrcensus15 %>% mutate( race_eth_recode = case_when(
  race_eth %in% c("Non-Hispanic Asian", "Other", "Missing/unknown") ~ 4,
  race_eth == "Non-Hispanic White" ~ 1,
  race_eth == "Non-Hispanic Black" ~ 2,
  race_eth == "Hispanic" ~ 3,
))

ehrcensus15$race_eth_recode<- factor(ehrcensus15$race_eth_recode, levels = 1:4, labels = c("Non-Hispanic White", "Non-Hispanic Black","Hispanic", "Other"))  


mod2_2<- glm(notoptimal ~ ClusterAssignmentr + as.factor(yeardx) + race_eth_recode + age_dx_cat + insurance_status,family=binomial(link='logit'),data= ehrcensus15)
summary(mod2_2)
#Facility information 
mod3_2<-glm(notoptimal ~ ClusterAssignmentr + as.factor(yeardx) + facility_type1_cat + facility_size1 +  facility_docspecialty1,family=binomial(link='logit'),data= ehrcensus15)
summary(mod3_2)
#Adjust for  tumor info
mod4_2<- glm(notoptimal ~ ClusterAssignmentr + as.factor(yeardx) + FIGOStage + Grade_cat  ,family=binomial(link='logit'),data= ehrcensus15)
summary(mod4_2)

#Full model
mod5_2<- glm(notoptimal ~ ClusterAssignmentr + race_eth_recode + as.factor(yeardx)  + age_dx_cat + FIGOStage + Grade_cat + insurance_status  
           + facility_type1_cat + facility_size1 +  facility_docspecialty1 ,family=binomial(link='logit'),data= ehrcensus15)
summary(mod5_2)

pred.labels2<-c(paste0("NSES:Cluster", sep= " ", c(1, 3:8)),"YearDx:2012", "YearDx:2013", "YearDx:2014",
                "NHB", "Hispanic", "Other","Age: 50-64", "Age:65+","Insurance:Medicare","Insurance:Public", "Insurance: Other", "Insurance:not insured",
                "Facility: Community", "Facility: Specialty", "Facility: Teaching", "Facility: Unknown","Size:Medium",
                "Size: Large", "Doc: Hematology", "Doc: Gyno", "Doc:Oncology", "Doc: Rad", "Doc:missing", "Doc:other", "Stage:II", "Stage:III", "Stage: IV","Grade:II", "Grade:III")

tab_model(mod1_2, mod2_2, mod3_2, mod4_2, mod5_2,  p.style = "stars", pred.labels =  pred.labels2, dv.labels = paste0("Model", sep=" ", 1:5),
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F)


set_label(ehrcensus15$notoptimal) <- "Did not receive optimal care"
plot_models(mod1_2, mod5_2)

#####------ 2015-2019-----

ehrcensus19$ClusterAssignmentr <- relevel(ehrcensus19$ClusterAssignment_new , ref= 4)
ehrcensus19$notoptimal<- ifelse(ehrcensus19$optimal_care == "Not optimal", 1, 0)

#Unadjusted
mod1_3<-glm(notoptimal ~ ClusterAssignmentr,family=binomial(link='logit'),data= ehrcensus19)
##The odds of not receiving optimal care are 0.63 lower

table1(~ race_eth + nativity  +  age_dx_cat  + Grade_cat + trt_summary_overall+ 
         RX_Summ_Surg_Primary_Site_c + radiation_yn + Rad_Reg_Rx_Mod_cat + chemo_yn + 
         FIGOStage + insurance_status + facility_type1_cat+ facility_size1  +  
         facility_docspecialty1 + ClusterAssignment_new| optimal_care, data= ehrcensus19, overall=F, extra.col=list(`P-value`=pvalue))



#Adjust for socio-demographic characteristics
#Recode- race/eth
ehrcensus19<- ehrcensus19 %>% mutate( race_eth_recode = case_when(
  race_eth %in% c("Non-Hispanic Asian", "Other", "Missing/unknown") ~ 4,
  race_eth == "Non-Hispanic White" ~ 1,
  race_eth == "Non-Hispanic Black" ~ 2,
  race_eth == "Hispanic" ~ 3,
))

ehrcensus19$race_eth_recode<- factor(ehrcensus19$race_eth_recode, levels = 1:4, labels = c("Non-Hispanic White", "Non-Hispanic Black","Hispanic", "Other"))  


mod2_3<- glm(notoptimal ~ ClusterAssignmentr + as.factor(yeardx) + race_eth_recode + age_dx_cat + insurance_status,family=binomial(link='logit'),data= ehrcensus19)
summary(mod2_3)
#Facility information 
mod3_3<-glm(notoptimal ~ ClusterAssignmentr + as.factor(yeardx) + facility_type1_cat + facility_size1 +  facility_docspecialty1,family=binomial(link='logit'),data= ehrcensus19)
summary(mod3_3)
#Adjust for  tumor info
mod4_3<- glm(notoptimal ~ ClusterAssignmentr + as.factor(yeardx) + FIGOStage + Grade_cat  ,family=binomial(link='logit'),data= ehrcensus19)
summary(mod4_3)

#Full model
mod5_3<- glm(notoptimal ~ ClusterAssignmentr + race_eth_recode + as.factor(yeardx)  + age_dx_cat + FIGOStage + Grade_cat + insurance_status  
             + facility_type1_cat + facility_size1 +  facility_docspecialty1 ,family=binomial(link='logit'),data= ehrcensus19)
summary(mod5_3)

pred.labels2<-c(paste0("NSES:Cluster", sep= " ", c(1:3, 5:7)),"YearDx:2016", "YearDx:2017",
                "NHB", "Hispanic", "Other","Age: 50-64", "Age:65+","Insurance:Medicare","Insurance:Public", "Insurance: Other", "Insurance:not insured",
                "Facility: Community", "Facility: Specialty", "Facility: Teaching", "Facility: Unknown","Size:Medium",
                "Size: Large", "Doc: Hematology", "Doc: Gyno", "Doc:Oncology", "Doc: Rad", "Doc:missing", "Doc:other", "Stage:II", "Stage:III", "Stage: IV","Grade:II", "Grade:III")

tab_model(mod1_3, mod2_3, mod3_3, mod4_3, mod5_3,  p.style = "stars", pred.labels =  pred.labels2, dv.labels = paste0("Model", sep=" ", 1:5),
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F)




#Using cluster colors-- unadjusted
mod1a<- glm(notoptimal ~ cluster_color,family=binomial(link='logit'), data= ehrcensus10)
summary(mod1a)

mod1b<- glm(notoptimal ~ cluster_color,family=binomial(link='logit'), data= ehrcensus15)
summary(mod1b)

mod1c<- glm(notoptimal ~ cluster_color, family=binomial(link='logit'),data= ehrcensus19)
summary(mod1c)

tab_model(mod1a, mod1b, mod1c, p.style = "stars", dv.labels = paste0("ACS", sep=" ", 1:3),
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F)



#Crosstabs

table(dat10$cluster_color, dat15$cluster_color)


