#=======================================================
#EC data and ACS wave 2015-2019, 
#Author: Carmen Rodriguez
#Last Updated: 4/22/25
#=======================================================
library(tidyverse)
library(ggplot2)
library(psych)
library(table1)
library(tableone)
library(Hmisc)
library(MCMCvis)
library(knitr)
library(gmodels)
library(viridis)
library(tidycensus)
library(stringr)
library(readxl)
library(openxlsx)
library(modelr)
`%!in%` <- Negate(`%in%`)

#------MCR DATA-----------
ehrdata<-readRDS("/Users/carmenrodriguez/Library/CloudStorage/OneDrive-HarvardUniversity/EC Data/R files/MCRdat_optimalcarevar.rds")
str(ehrdata)
table(ehrdata$race_eth)

#Unique census tracts
#u<-unique(ehrdata$census_tract) 
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

table(ehrdata_countyfips$race_eth_recode)

#Other pre-processing
table(ehrdata_countyfips$RX_Summ_Surg_Primary_Site_c)
ehrdata_countyfips<- ehrdata_countyfips %>% mutate(type_surg_received = case_when(
    RX_Summ_Surg_Primary_Site_c == "None" ~ 1,
    RX_Summ_Surg_Primary_Site_c == "Resection" ~ 2,
    RX_Summ_Surg_Primary_Site_c %in% c("Tumor destruction","Other Surgery","Unknown") ~3))
  
table(ehrdata_countyfips$type_surg_received)
ehrdata_countyfips$type_surg_received<-factor(ehrdata_countyfips$type_surg_received, levels = 1:3, labels = c("None","Resection", "Other/Unknown"))

ehrdata_countyfips$facility_type1_cat_1<-ifelse(ehrdata_countyfips$facility_type1_cat%in% c("Unknown"), NA, ehrdata_countyfips$facility_type1_cat)
ehrdata_countyfips$facility_type1_cat_1<-factor(ehrdata_countyfips$facility_type1_cat_1, levels = 1:4, labels = c("Academic Medical Centers" ,"Community","Specialty","Teaching"))

ehrdata_countyfips$yeardx_fct<-as.factor(ehrdata_countyfips$yeardx)


# Split data by range of years of diagnosis to match census datasets
# 2015-2019 ACS
ehrdata_countyfips <- ehrdata_countyfips %>% rename( countycode = county_at_dx)
ehrdata_19<- ehrdata_countyfips %>% filter(yeardx >= 2015)
table(ehrdata_19$yeardx)


#------CENSUS DATA  with clusters only-------
data19<-readRDS("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Model_run_updates/mbmm_clustersassign_042025.rds")

ehrcensus19<- left_join(ehrdata_19, data19, by=  "census_tract", relationship = "many-to-many") #want 2412
str(ehrcensus19)
#Now keep the counties at diagnosis
table(ehrcensus19$countycode.x) # out of these 36, we only need to keep 16
table(ehrcensus19$countycode.y)
ehrcensus19 %>% filter(!is.na(countycode.x)) %>% select(Patient_ID_Num, census_tract, countycode.x,countycode.y, optimal_care)
ehrcensus19 %>% filter(!(countycode.x == countycode.y))%>% select(Patient_ID_Num, census_tract, countycode.x,countycode.y)

#Rows to be removed
rmv<- which(!(ehrcensus19$countycode.x == ehrcensus19$countycode.y))

#Final
ehrcensus19a<- ehrcensus19 %>% filter(row_number() %!in%  rmv)



#Tables
variables<- c("race_eth_recode", "nativity","yeardx_fct", "age_dx_cat","insurance_status","trt_summary_overall",
              "type_surg_received", "radiation_yn", "Rad_Reg_Rx_Mod_cat", "chemo_yn",
              "FIGOStage", "Grade_cat", "facility_type1_cat_1", "facility_size1",
              "facility_docspecialty1", "nsdoh_profiles")

variablesn<- c("optimal_care","race_eth_recode", "nativity", "yeardx_fct", "age_dx_cat","insurance_status","trt_summary_overall",
               "type_surg_received", "radiation_yn", "Rad_Reg_Rx_Mod_cat", "chemo_yn",
               "FIGOStage", "Grade_cat", "facility_type1_cat_1", "facility_size1",
               "facility_docspecialty1")

table4paper<-CreateTableOne(vars = variables,strata = "optimal_care", data = ehrcensus19a, factorVars = variables, includeNA= T, addOverall = T)

#For row %
for (i in 1:length(variables)) {
  sum = table4paper$CatTable[[2]][[i]]$freq + table4paper$CatTable[[3]][[i]]$freq
  table4paper$CatTable[[2]][[i]]$percent = (table4paper$CatTable[[2]][[i]]$freq / sum)*100
  table4paper$CatTable[[3]][[i]]$percent = (table4paper$CatTable[[3]][[i]]$freq / sum)*100
}
table4export_row<-print(table4paper, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

##Table B2-column percentages again
table4export_col<-print(table4paper, showAllLevels = TRUE, formatOptions = list(big.mark = ","))


table4papera<-CreateTableOne(vars = variablesn,strata = "nsdoh_profiles", data = ehrcensus19a, factorVars = variablesn, includeNA= T, addOverall = T)

#Export
write.csv(table4export_row, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Tables/tab1_042225_row.csv")
write.csv(table4export_col, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Tables/tab1_042225_col.csv")
#write.csv(table4exporta, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab3a.csv")

#keep column percentages for Table 2
table4export_cols<-print(table4papera, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
write.csv(table4export_cols, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Tables/tab042225_cols.csv")


#---LOGISTIC REGRESSION MODELS-----#
# Load packages
library(rstanarm)
library(brms)
library(bayesplot)
library(broom)
library(broom.mixed)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

ehrcensus19a$nsdoh_profiles<-factor(ehrcensus19a$nsdoh_profiles, labels = c("Profile 1", "Profile 2", "Profile 3",
                                        "Profile 4", "Profile 5"))
ehrcensus19a$nsdoh_profilesr <- relevel(ehrcensus19a$nsdoh_profiles , ref= "Profile 1")

model_freq<-glm(optimal_care ~ nsdoh_profiles ,family=binomial(link='logit'),data= ehrcensus19a)
summary(model_freq)
# + yeardx  + age_dx_cat + insurance_status  + facility_type1_cat_1  + facility_size1 + facility_docspecialty1 
tab_model(model_freq,  p.style = "stars",
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F, digits = 3)


ehrcensus19a$optimal_care_recode<- as.numeric(ehrcensus19a$optimal_care) #1/2 ; here 2 is optimal (=1)
ehrcensus19a$optimal_care_recode <-ifelse(ehrcensus19a$optimal_care_recode == 1, 0,1)

#----Bayesian binary logistic regression model
#https://www.rensvandeschoot.com/tutorials/generalised-linear-models-with-brms/
model_bayes<-brm(optimal_care_recode ~ nsdoh_profilesr + yeardx  + age_dx_cat + 
                            insurance_status  + facility_type1_cat_1 + facility_size1 +
                 facility_docspecialty1,
                 data = ehrcensus19a,
                 family = bernoulli(link='logit'), 
                 warmup = 500, 
                 iter = 2000, 
                 chains = 2, 
                 init= "0", 
                 cores=2,
                 seed = 2004) 

#Updated 11/14: remove doc specialty from model and 
mult1<-brm(optimal_care_recode ~ nsdoh_profilesr + yeardx  + age_dx_cat + 
                   insurance_status  + facility_type1_cat_1,
                 data = ehrcensus19a,
                 family = bernoulli(link='logit'), 
                 warmup = 500, 
                 iter = 2000, 
                 chains = 2, 
                 init= "0", 
                 cores=2,
                 seed = 2004) 
# 
# mult2<-brm(optimal_care_recode ~ nsdoh_profilesr + yeardx  + age_dx_cat + 
#              insurance_status  + facility_type1_cat_1 + FIGOStage,
#            data = ehrcensus19a,
#            family = bernoulli(link='logit'), 
#            warmup = 500, 
#            iter = 2000, 
#            chains = 2, 
#            init= "0", 
#            cores=2,
#            seed = 2004) 

#---Model convergence
###The plot only shows the iterations after the burn-in period. The two chains mix well 
#for all of the parameters and therefore, we can conclude no evidence of non-convergence.
mcmc_plot(mult1, 
         type = "trace")

##We can also check autocorrelation, considering that the presence of strong autocorrelation would bias variance estimates.
mcmc_plot(mult1, 
          type = "acf_bar")

##Model summary -Bayesian binary logistic regression model--FINAL MODEL!
#Model 1
summary(mult1)
est1<-round(exp(fixef(mult1)[-1,-2]), 3)
est1_dat<- as.data.frame(est1[1:4,])
est1_dat$NSDOH_prof<-2:5
est1_dat$NSDOH_prof<-factor(est1_dat$NSDOH_prof, levels = 2:5, labels = paste0("Profile",sep= " ", 2:5))



##Plot
model1_plot<-ggplot(est1_dat, aes(x = Estimate, y = NSDOH_prof)) +
  geom_point(size = 4, color = "blue") +  # Odds ratios as points
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2, color = "black", linewidth = 0.75) +  # Credible intervals
  theme_minimal() +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  labs(y = "NSDoH Profiles",x = "Odds Ratio") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red")  # Reference line at OR = 1


# #Model 2
# summary(mult2)
# est2<-round(exp(fixef(mult2)[-1,-2]), 3)
# est2_dat<- as.data.frame(est2[1:7,])
# est2_dat$NSDOH_prof<-2:8
# est2_dat$NSDOH_prof<-factor(est2_dat$NSDOH_prof, levels = 2:8, labels = paste0("Profile",sep= " ", 2:8))
# 
# ##Plot
# model2_plot<-ggplot(est2_dat, aes(x = Estimate, y = NSDOH_prof)) +
#   geom_point(size = 4, color = "blue") +  # Odds ratios as points
#   geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2, color = "black", size = 0.75) +  # Credible intervals
#   theme_minimal() +
#  theme_minimal() +
#   theme(axis.text = element_text(size = 14),
#     axis.title = element_text(size = 16),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
#   labs(y = "NSDoH Profiles",x = "Odds Ratio") +
#   geom_vline(xintercept = 1, linetype = "dashed", color = "red")  # Reference line at OR = 1
# 


#Export plots
#ggsave(filename = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/model_plot.svg")
jpeg("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Model_run_updates/ECmodel1_plot.jpeg", width = 500, height = 500)
model1_plot
dev.off()

# jpeg("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/model2_plot_ordered.jpeg", width = 500, height = 500)
# model2_plot
# dev.off()

#Export results 
#write.csv(est2,file = "./Table1/modelout.csv")


####Additional models as per Dr. Alimena's suggestions
#---Model among patients with Stage II or higher
table(ehrcensus19a$FIGOStage)

#subset
stage2<-ehrcensus19a %>% filter(FIGOStage %in% c("Stage II", "Stage III",  "Stage IV")) #n=409

mod1_stage2<-brm(optimal_care_recode ~ nsdoh_profilesr ,
                        data = stage2,
                        family = bernoulli(link='logit'), 
                        warmup = 500, 
                        iter = 2000, 
                        chains = 2, 
                        init= "0", 
                        cores=2,
                        seed = 2004) 

summary(mod1_stage2)
est_stage2_uni<-round(exp(fixef(mod1_stage2)[-1,-2]), 3)

#Adjusted
mod2_stage2<-brm(optimal_care_recode ~ nsdop_profilesr + yeardx  + age_dx_cat + 
                              insurance_status  + facility_type1_cat_1,
                            data = stage2,
                            family = bernoulli(link='logit'), 
                            warmup = 500, 
                            iter = 2000, 
                            chains = 2, 
                            init= "0", 
                            cores=2,
                            seed = 2004) 

summary(mod2_stage2)
est_stage2_mult<-round(exp(fixef(mod2_stage2)[-1,-2]), 3)

write.csv(est_stage2_mult,file = "./Table1/modelout.csv")


#----Facility type as outcome variable:
table(ehrcensus19a$nsdoh_profilesr)
table(ehrcensus19a$facility_type1_cat_1)

#Facility type as outcome variable:
ehrcensus19a <-ehrcensus19a %>% mutate(facilitybin = ifelse( facility_type1_cat == "Academic Medical Centers", 1, 0))


model_facility_uni<-brm(facilitybin ~ nsdoh_profilesr,
                 data = ehrcensus19a,
                 family = bernoulli(link='logit'), 
                 warmup = 500, 
                 iter = 2000, 
                 chains = 2, 
                 init= "0", 
                 cores=2,
                 seed = 2004) 

model_facility_adj1<-brm(facilitybin ~ nsdoh_profilesr + yeardx  + age_dx_cat + 
                          insurance_status ,
                        data = ehrcensus19a,
                        family = bernoulli(link='logit'), 
                        warmup = 500, 
                        iter = 2000, 
                        chains = 2, 
                        init= "0", 
                        cores=2,
                        seed = 2004) 


model_facility_adj2<-brm(facilitybin ~ nsdoh_profilesr + yeardx  + age_dx_cat + 
                           insurance_status  + FIGOStage,
                         data = ehrcensus19a,
                         family = bernoulli(link='logit'), 
                         warmup = 500, 
                         iter = 2000, 
                         chains = 2, 
                         init= "0", 
                         cores=2,
                         seed = 2004) 
## Summarize model

est_fc<-round(exp(fixef(model_facility_uni)[-1,-2]), 3)
est_fc_m1<-round(exp(fixef(model_facility_adj1)[-1,-2]), 3)
est_fc_m2<-round(exp(fixef(model_facility_adj2)[-1,-2]), 3)

write.xlsx(list(est_fc, est_fc_m1, est_fc_m2),file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Tables/modelout_042225.csv")


# #Plot
# model_plot1<-ggplot(est_fc_dat, aes(x = Estimate, y = NSDOH_prof)) +
#   geom_point(size = 4, color = "blue") +  # Odds ratios as points
#   geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2, color = "black", size = 0.75) +  # Credible intervals
#   theme_minimal() +
#   theme_minimal() +
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 16),
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
#   labs(y = "NSDoH Profiles",x = "Odds Ratio") +
#   geom_vline(xintercept = 1, linetype = "dashed", color = "red")  # Reference line at OR = 1
# 


#ADDED ON 1/26/26 BY Carmen Rodriguez (as per reviewers suggestions)




#---Models with Yost index--- direction we expect based on prev literature

#------CENSUS DATA  with clusters only & CENSUS DATA with Yost NSES Index only-------
#load("./bayesbinmix_clustered_tracts.RData")
load("./Census Data/YostIndexDat.RData")
#Yost index data 2015-2019
yostindex19$census_tract<-str_sub(yostindex19$GEOID,start=6, end = 11) 
yostindex19$countycode<-str_sub(yostindex19$GEOID,start=3, end = 5)
yostindex19<-yostindex19 %>% rename(yostindex = yostquintiles19)

#Merge with EHR data 2015-2019
str(ehrcensus19a)
ehrcensus19b<- left_join(ehrcensus19a,yostindex19, by= "census_tract",  relationship = "many-to-many") #2412
#Final
ehrcensus19b_clean <- ehrcensus19b %>%
  distinct(Patient_ID_Num, census_tract, countycode.x, countycode.y, optimal_care, .keep_all = TRUE)

#sum(ehrcensus19a$Patient_ID_Num == ehrcensus19b_clean$Patient_ID_Num) #TRUE


CreateTableOne(vars = "yostindex", strata = "nsdoh_profilesr", data = ehrcensus19b_clean,includeNA= T, addOverall = T)

table(ehrcensus19b_clean$yostindex, ehrcensus19b_clean$nsdoh_profiles)



#Logistic regression models with Yost index
model_yost1<-brm(optimal_care_recode ~ yostindex,
           data = ehrcensus19b_clean,
           family = bernoulli(link='logit'), 
           warmup = 500, 
           iter = 2000, 
           chains = 2, 
           init= "0", 
           cores=2,
           seed = 2004) 

#Adjusted model
model_yost2<-brm(optimal_care_recode ~ yostindex + yeardx  + age_dx_cat + 
                   insurance_status  + facility_type1_cat_1,
                 data = ehrcensus19b_clean,
                 family = bernoulli(link='logit'), 
                 warmup = 500, 
                 iter = 2000, 
                 chains = 2, 
                 init= "0", 
                 cores=2,
                 seed = 2004) 




#Model 1
summary(model_yost1)
est1<-round(exp(fixef(model_yost1)[-1,-2]), 3)
est1_dat<- as.data.frame(est1[1:4,])


summary(model_yost2)
est2<-round(exp(fixef(model_yost2)[-1,-2]), 3)
est2_dat<- as.data.frame(est2[1:4,])


est1_dat
est2_dat


