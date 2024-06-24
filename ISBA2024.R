###########################################################################################
###########################################################################################
#-------------------------ISBA MEETING 2024 POSTER----------------------------------------
#Carmen Rodriguez
#6/13/2024
#We will use model pi ~ Dirichlet(1,...,1) and K ~ Truncated Poisson(1) at Kmax = 50
#Results we want to show:
#1. Bar plots showing the probabilities of high exposure to NSDoH variable  for a census tract given assignment to cluster/mixture component k. 
#2. Map showing the cluster assignment for census tracts
#3. Forest plot
###########################################################################################
###########################################################################################
library(tidyverse)
library(ggplot2)
library(psych)
library(table1)
library(tableone)
library(Hmisc)
library(BayesBinMix)
library(coda)
library(DescTools)
library(MCMCvis)
library(knitr)
library(gmodels)
library(viridis)
library(tidycensus)
library(sf)
library(tmap)
library(RColorBrewer)
library(cartogram)
library(geodaData)
library(stringr)
library(readxl)
library(openxlsx)
`%!in%` <- Negate(`%in%`)
source("Utils.R") #Load aux functions
#Includes ethnic minorities variables
res_all2<-readRDS("./Model Outputs/acs19_raceeth_priors.rds")
censusdata_bin <- readRDS("./Data/censusdata_bin_raceeth_042524.rds")
censusdata<- load("./Data/censusdata_updated_newvars.RData")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
res19_poi<-res_all2[[1]][["res_acs19_deltatemp1_poisson"]]
print(res19_poi) 
mapK_poi<-Mode(res19_poi$K.mcmc)[1]

mix_wts_poi<-cbind(summary(res19_poi$parameters.ecr.mcmc)$statistics[,c(1,2)], summary(res19_poi$parameters.ecr.mcmc)$quantiles[,c(1,5)])
tail(mix_wts_poi, mapK_poi) %>% kable()

classificationprobs_poi<-res19_poi$classificationProbabilities.ecr
# Add cluster classification based on maximum probability 
maxProb<-apply(classificationprobs_poi, 1, max)
ClusterAssignment_highest<-apply(classificationprobs_poi,1, highest_index)
classificationprobs_poi$maxProb<-maxProb
classificationprobs_poi$ClusterAssignment_ecr<-res19_poi$clusterMembershipPerMethod[,2]
classificationprobs_poi$ClusterAssignment_highest<-ClusterAssignment_highest
#Prep data
classificationprobs_poi$GEOID<-row.names(censusdata_bin$acs5_2019_bin)
classificationprobs_poi$ClusterAssignment_final<- factor(classificationprobs_poi$ClusterAssignment_highest,
                                                         levels = 1:9, labels = paste0("Cluster",sep= " ", 1:9))

dat19_clusters_poi<- classificationprobs_poi %>% select(GEOID,ClusterAssignment_final, maxProb)

#Generate bar plots using estimated theta_kj and observed data
#Need to be in order
#names(censusdata_bin$acs5_2019_bin)
sesvars<-c("Median income", "Female household", "< HS", " >= HS", " >= Bacherlors", "Limited EN Proficiency", "Unemployment", "SNAP benefits", "Owner-occupied", "Renter-occupied", 
           "No vehicle", "Hispanic or Latino", "NH Black", "NH Asian", "Crowded housing", "Below 150% poverty", "Working class", "Lack complete plumbing")

group_cols<-c("#1f77b4","#2ca02c","salmon1", "#bcbd22", "#e377c2")


# Generate 14 pastel colors for ungrouped graph
pastel_palette1 <- generate_pastel_colors(18)


#fig_title = paste("Success Probabilities when mapK=", mapK, sep="")
fig_title = ""
dat_19<-preparedat_fig(res19_poi,mapK_poi,sesvars)
dat_19$cluster <- factor(dat_19$cluster, levels = 1:9, labels = c(
  "Cluster 1: 401", "Cluster 2: 74", "Cluster 3: 378", "Cluster 4: 94",
  "Cluster 5: 26", "Cluster 6: 109", "Cluster 7: 86", "Cluster 8: 143", "Cluster 9: 167"
))

# Generate plot
barplot<-dat_19 %>% ggplot(aes(x = NSES_VARS, y = theta_kj, fill = NSES_group)) +
  geom_col()  +
  facet_wrap(~cluster, nrow = 9, ncol = 1) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = fig_title, x= "",
       y = "Probability",
       fill = "Neighborhood SES Variables") +
  theme(strip.text = element_text(size = 25),
        text = element_text(size = 25),
        axis.text.x = element_text(size=25, angle=45, vjust = 0.88, hjust = 0.88), 
        #axis.title.x = element_text(size = 18, color = "black", face = "bold"),
        axis.title.y = element_text(size = 28, color = "black", face = "bold"),
        axis.text.y = element_text(size=22), 
        legend.title = element_text(size = 28, color = "black", face = "bold"),
        legend.text = element_text(size = 25, color = "black"),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5))

barplot1<-dat_19 %>% ggplot(aes(x = NSES_VARS, y = theta_kj, fill = NSES_group)) +
  geom_col()  +
  facet_wrap(~cluster, nrow = 5, ncol = 2) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = fig_title, x= "",
       y = "Probability",
       fill = "Neighborhood SES Variables") +
  theme(strip.text = element_text(size = 14),
        text = element_text(size = 18),
        axis.text.x = element_text(size=16, angle=90, vjust = 0.88, hjust = 0.88), 
        #axis.title.x = element_text(size = 18, color = "black", face = "bold"),
        axis.title.y = element_text(size = 18, color = "black", face = "bold"),
        axis.text.y = element_text(size=12), 
        legend.title = element_text(size = 18, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5))


jpeg("/Users/carmenrodriguez/Desktop/Research Projects/Conferences/ISBA 2024/Poster/barplot19.jpeg", width = 1500, height = 1800)
barplot
dev.off()
jpeg("/Users/carmenrodriguez/Desktop/Research Projects/Conferences/ISBA 2024/Poster/barplot19_2cols.jpeg", width = 1100, height = 1000)
barplot1
dev.off()
png("/Users/carmenrodriguez/Desktop/Research Projects/Conferences/ISBA 2024/Poster/barplot19_2cols.png", width = 1100, height = 1000)
barplot1
dev.off()




### Map showing the distribution 
temp<-get_acs(state = "MA", geography = "tract", 
              variables = "B03002_001", year = 2019, geometry = TRUE, cache_table = TRUE)

temp1<- temp %>% select(GEOID, NAME, geometry, estimate)

dat19map<-dat19_clusters_poi %>% select(GEOID,ClusterAssignment_final)
#dat19map$ClusterAssignment_num<-as.numeric(dat19map$ClusterAssignment_final)

#join datasets
map19<- merge(temp1, dat19map, by = "GEOID")
map19a<-map19 %>%  select(GEOID, NAME, ClusterAssignment_final,geometry) 
map19a<- map19a %>% mutate(`NSDoH profile` = ClusterAssignment_final)
#map19a$Cluster<-as.factor(map19a$Cluster)

#colors to match the bar plot
custom_colors <- custom_colors <- c("#d62728","salmon1", "#2ca02c",  
                                    "#9467bd", "#8c564b",
                                    "#e377c2",  "#17becf",
                                    "#1f77b4", "#ffff00")
map19_clust<-tm_shape(st_make_valid(map19a)) +
  tm_fill("NSDoH profile",style="cat",palette=custom_colors,legend.hist=TRUE,alpha=1)  +
  #tm_borders(col = "white") +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right",scale =1.2) 

tmap_save(tm = map19_clust, filename = "/Users/carmenrodriguez/Desktop/Research Projects/Conferences/ISBA 2024/Poster/map19.png")
#jpeg("/Users/carmenrodriguez/Desktop/Research Projects/Conferences/ISBA 2024/Poster/map19.jpeg", width = 500, height = 500)
#map19_clust
#dev.off()

###############################################################################################
###############################################################################################



#------EHR DATA-----------
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
data19<-dat19_clusters_poi
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
#final
ehrcensus19a<- ehrcensus19 %>% filter(row_number() %!in%  rmv)
variables<- c("race_eth_recode", "nativity", "age_dx_cat","insurance_status","trt_summary_overall",
              "type_surg_received", "radiation_yn", "Rad_Reg_Rx_Mod_cat", "chemo_yn",
              "FIGOStage", "Grade_cat", "facility_type1_cat_1", "facility_size1",
              "facility_docspecialty1", "ClusterAssignment_final")
variablesn<- c("race_eth_recode", "nativity", "age_dx_cat","insurance_status","trt_summary_overall",
               "type_surg_received", "radiation_yn", "Rad_Reg_Rx_Mod_cat", "chemo_yn",
               "FIGOStage", "Grade_cat", "facility_type1_cat_1", "facility_size1",
               "facility_docspecialty1", "optimal_care")
##Tables
table4paper<-CreateTableOne(vars = variables,strata = "optimal_care", data = ehrcensus19a, factorVars = variables, includeNA= T, addOverall = T )
for (i in 1:length(variables)) {
  sum = table4paper$CatTable[[2]][[i]]$freq + table4paper$CatTable[[3]][[i]]$freq
  table4paper$CatTable[[2]][[i]]$percent = (table4paper$CatTable[[2]][[i]]$freq / sum)*100
  table4paper$CatTable[[3]][[i]]$percent = (table4paper$CatTable[[3]][[i]]$freq / sum)*100
}

table4export<-print(table4paper, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(table4export, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab3.csv")
#Export

table4papera<-CreateTableOne(vars = variablesn,strata = "ClusterAssignment_final", data = ehrcensus19a, factorVars = variablesn, includeNA= T, addOverall = T)
for (i in 1:length(variablesn)) {
  sum = table4papera$CatTable[[1]][[i]]$freq 
  for(j in 2:7){
    table4papera$CatTable[[j]][[i]]$percent = (table4papera$CatTable[[j]][[i]]$freq / sum)*100
  }}
table4exporta<-print(table4papera, showAllLevels = TRUE, formatOptions = list(big.mark = ","))
write.csv(table4exporta, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/tab3a.csv")

#LOGISTIC REGRESSION MODELS#
#https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html
library(sjPlot)
library(sjmisc)
library(sjlabelled)


#Set cluster with largest n as the reference
ehrcensus19a$ClusterAssignmentr <- relevel(ehrcensus19a$ClusterAssignment_final , ref= 1)

#Unadjusted
mod1<-glm(optimal_care ~ ClusterAssignmentr,family=binomial(link='logit'),data= ehrcensus19a)

#Adjust for socio-demographic characteristics
mod2<- glm(optimal_care ~ ClusterAssignmentr  + yeardx + race_eth_recode + age_dx_cat + insurance_status,family=binomial(link='logit'),data= ehrcensus19a)
summary(mod2)
#Facility information 
mod3<-glm(optimal_care ~ ClusterAssignmentr + yeardx + facility_type1_cat_1,family=binomial(link='logit'),data= ehrcensus19a)
summary(mod3)
#Adjust for  tumor info
mod4<- glm(optimal_care ~ ClusterAssignmentr + yeardx + FIGOStage + Grade_cat  ,family=binomial(link='logit'),data= ehrcensus19a)
summary(mod4)

#Full model
mod5<- glm(optimal_care ~ ClusterAssignmentr  + yeardx  + age_dx_cat + 
             insurance_status  + facility_type1_cat_1 ,family=binomial(link='logit'),data= ehrcensus19a)
summary(mod5)

tab_model(mod1,  p.style = "stars",
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F, digits = 3)

tab_model(mod5,  p.style = "stars",
          string.pred = "Coeffcient",
          string.ci = "95% CI", show.intercept = F, digits = 3)

modplot<-plot_models(mod5, vline.color = "black",legend.title = "",
            rm.terms = c("yeardx", "age_dx_cat50-64", "age_dx_cat65 or older","insurance_statusMedicare",
                         "insurance_statusPublic/Government",
                         "insurance_statusOther", "insurance_statusNot insured", "facility_type1_cat_1Community",
                         "facility_type1_cat_1Specialty", "facility_type1_cat_1Teaching"),  show.legend = FALSE,
             dot.size = 5, line.size = 1)
              

jpeg("/Users/carmenrodriguez/Desktop/Research Projects/Conferences/ISBA 2024/Poster/modplot19.jpeg", width = 400, height = 400)
modplot
dev.off()

library(broom)

# Extract coefficients and confidence intervals
model_summary <- tidy(mod5, conf.int = TRUE, exp = TRUE)
model_summary1<- model_summary %>% filter(term %in% c("ClusterAssignmentrCluster 2", "ClusterAssignmentrCluster 3",
                                                      "ClusterAssignmentrCluster 4", "ClusterAssignmentrCluster 5",
                                                      "ClusterAssignmentrCluster 6", "ClusterAssignmentrCluster 7",
                                                      "ClusterAssignmentrCluster 8", "ClusterAssignmentrCluster 9"))

#Rename clusters
model_summary1$term<-factor(model_summary1$term, labels =  c(
   "Cluster 2", "Cluster 3", "Cluster 4",
  "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9"
))


# Plotting odds ratios
modplot1<-ggplot(model_summary1, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "#d62728", size = 1.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#8c564b") +
  coord_flip() +
  labs(y = "Adjusted Odds Ratio", x = "NSDoH profile") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30))

modplot2<-ggplot(model_summary1, aes(x = term, y = estimate)) +
  geom_point(color = "#d62728", size = 15) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color ="#d62728", size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#8c564b") +
  coord_flip() +
  labs(y = "Adjusted Odds Ratio", x = "NSDoH profile") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 30),
    axis.title = element_text(size = 35))

modplot2

jpeg("/Users/carmenrodriguez/Desktop/Research Projects/Conferences/ISBA 2024/Poster/modplot19_ggplot.jpeg", width = 800, height = 800)
modplot1
dev.off()

jpeg("/Users/carmenrodriguez/Desktop/Research Projects/Conferences/ISBA 2024/Poster/modplot219_ggplot.jpeg", width = 800, height = 800)
modplot2
dev.off()