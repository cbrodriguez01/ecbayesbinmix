#=======================================================
#EC data and ACS wave 2015-2019, 
#but will also look at 2006-2010 and explore changes
#Author: Carmen Rodriguez
#Last Updated: 10/13/24
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
ehrdata_countyfips$type_surg_received<-ifelse(ehrdata_countyfips$RX_Summ_Surg_Primary_Site_c %in% c("Other Surgery","Unknown"), 4, ehrdata_countyfips$RX_Summ_Surg_Primary_Site_c)
table(ehrdata_countyfips$type_surg_received)

ehrdata_countyfips$type_surg_received<-factor(ehrdata_countyfips$type_surg_received, levels = 1:4, labels = c("None", "Tumor destruction","Resection", "Other/Unknown"))

ehrdata_countyfips$facility_type1_cat_1<-ifelse(ehrdata_countyfips$facility_type1_cat%in% c("Unknown"), NA, ehrdata_countyfips$facility_type1_cat)
ehrdata_countyfips$facility_type1_cat_1<-factor(ehrdata_countyfips$facility_type1_cat_1, levels = 1:4, labels = c("Academic Medical Centers" ,"Community","Specialty","Teaching"))

ehrdata_countyfips$yeardx_fct<-as.factor(ehrdata_countyfips$yeardx)


# Split data by range of years of diagnosis to match census datasets
# 2015-2019 ACS
ehrdata_countyfips <- ehrdata_countyfips %>% rename( countycode = county_at_dx)
ehrdata_19<- ehrdata_countyfips %>% filter(yeardx >= 2015)
table(ehrdata_19$yeardx)


#------CENSUS DATA  with clusters only-------
data19<-readRDS("./Data/mbmm_clusters_19eth_sized.rds")
data19$census_tract<-str_sub(data19$GEOID,start=6, end = 11) 
data19$countycode<-str_sub(data19$GEOID,start=3, end = 5)


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
              "facility_docspecialty1", "nsdop_profiles")

variablesn<- c("optimal_care","race_eth_recode", "nativity", "yeardx_fct", "age_dx_cat","insurance_status","trt_summary_overall",
               "type_surg_received", "radiation_yn", "Rad_Reg_Rx_Mod_cat", "chemo_yn",
               "FIGOStage", "Grade_cat", "facility_type1_cat_1", "facility_size1",
               "facility_docspecialty1")

table4paper<-CreateTableOne(vars = variables,strata = "optimal_care", data = ehrcensus19a, factorVars = variables, includeNA= T, addOverall = T )

#For row %
for (i in 1:length(variables)) {
  sum = table4paper$CatTable[[2]][[i]]$freq + table4paper$CatTable[[3]][[i]]$freq
  table4paper$CatTable[[2]][[i]]$percent = (table4paper$CatTable[[2]][[i]]$freq / sum)*100
  table4paper$CatTable[[3]][[i]]$percent = (table4paper$CatTable[[3]][[i]]$freq / sum)*100
}
table4export_row<-print(table4paper, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

##Table B1-column percentages again
table4export_col<-print(table4paper, showAllLevels = TRUE, formatOptions = list(big.mark = ","))


table4papera<-CreateTableOne(vars = variablesn,strata = "nsdop_profiles", data = ehrcensus19a, factorVars = variablesn, includeNA= T, addOverall = T)
##uncomment for row
# for (i in 1:length(variablesn)) {
#   sum = table4papera$CatTable[[1]][[i]]$freq 
#   for(j in 2:9){
#     table4papera$CatTable[[j]][[i]]$percent = (table4papera$CatTable[[j]][[i]]$freq / sum)*100
#   }}
#table4exporta<-print(table4papera, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

#Export
write.csv(table4export_row, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab3_row.csv")
write.csv(table4export_col, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab3_col.csv")
#write.csv(table4exporta, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab3a.csv")

#keep column percentages for Table 2
table4export_cols<-print(table4papera, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
write.csv(table4export_cols, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab3a_cols_10.13.csv")


##Table B1-column percentages again
#tabb1<-table1::table1( ~ race_eth_recode +  nativity + yeardx_fct + age_dx_cat + insurance_status +
#                        type_surg_received + Rad_Reg_Rx_Mod_cat + chemo_yn +
#                        FIGOStage+Grade_cat+facility_type1_cat_1+facility_size1 +
#                        facility_docspecialty1 |optimal_care, data = ehrcensus19a, overall = F)
# tabb1



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

ehrcensus19a$nsdop_profilesr <- relevel(ehrcensus19a$nsdop_profiles , ref= "Profile 1")

model_freq<-glm(optimal_care ~ nsdop_profilesr  + yeardx  + age_dx_cat + 
                  insurance_status  + facility_type1_cat_1  + facility_size1 +
                  facility_docspecialty1 ,family=binomial(link='logit'),data= ehrcensus19a)
summary(model_freq)

tab_model(model_freq,  p.style = "stars",
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F, digits = 3)


#model_bayes <- stan_glm(optimal_care ~ ClusterAssignmentr  + yeardx  + age_dx_cat + 
#                          insurance_status  + facility_type1_cat_1, data = ehrcensus19a,
#                  family = binomial(link = "logit"), seed = 2004)


ehrcensus19a$optimal_care_recode<- as.numeric(ehrcensus19a$optimal_care) #1/2 ; here 2 is optimal (=1)
ehrcensus19a$optimal_care_recode <-ifelse(ehrcensus19a$optimal_care_recode == 1, 0,1)

model_bayes<-brm(optimal_care_recode ~ nsdop_profilesr + yeardx  + age_dx_cat + 
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
#https://www.rensvandeschoot.com/tutorials/generalised-linear-models-with-brms/

##Model convergence
###The plot only shows the iterations after the burn-in period. The two chains mix well 
#for all of the parameters and therefore, we can conclude no evidence of non-convergence.
mcmc_plot(model_bayes, 
         type = "trace")

##We can also check autocorrelation, considering that the presence of strong autocorrelation would bias variance estimates.
mcmc_plot(model_bayes, 
          type = "acf_bar")

## Interpret model
#Bayesian binary logistic regression model
summary(model_bayes)
est<-round(exp(fixef(model_bayes)[-1,-2]), 3)
est_dat<- as.data.frame(est[1:7,])
est_dat$NSDOH_prof<-2:8
est_dat$NSDOH_prof<-factor(est_dat$NSDOH_prof, levels = 2:8, labels = paste0("Profile",sep= " ", 2:8))


#est_dat$NSDOH_prof <- relevel(est_dat$NSDOH_prof, ref =  "All Low")


#Plot
model_plot<-ggplot(est_dat, aes(x = Estimate, y = NSDOH_prof)) +
  geom_point(size = 4, color = "blue") +  # Odds ratios as points
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2, color = "black", size = 0.75) +  # Credible intervals
  theme_minimal() +
 theme_minimal() +
  theme(axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  labs(y = "NSDoH Profiles",x = "Odds Ratio") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red")  # Reference line at OR = 1






ggsave(filename = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/model_plot.svg")
jpeg("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/model_plot_ordered.jpeg", width = 500, height = 500)
model_plot
dev.off()