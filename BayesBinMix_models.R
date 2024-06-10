
#=======================================================
#Using BayesBinMix package on ACS Data
#Author: Carmen Rodriguez
#Last Updated: 5/17/24
#=======================================================
##----Load packages----
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
source("Utils.R") #Load aux functions

##----Load census data and results from latest model----
censusdata_bin <- readRDS("./Data/censusdata_bin_raceeth_042524.rds")
censusdata<- load("./Data/censusdata_updated_newvars.RData")
agedist<-load("./Data/census_age_dist.RData")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")

###########################################################################
#     Output from heating vector tuning-- 4/25/24
###########################################################################

#Output from heating vector tuning-- 4/25/24-- had an issue with my binary data
res_all<-readRDS("./Model Outputs/tuningheatsvec_4.25.24.rds")
#It seems that heating vector with deltaT = 0.025 (delta3) (temperature gap between adjacent chains) worked well for all datasets-- swap acceptance rates ranged from 
#30.5 - 25.7%. We do not want these rates to be too high like > 60:

res_acs10<-res_all[[1]][["res_acs10_deltatemp3"]]
res_acs15<-res_all[[1]][["res_acs15_deltatemp3"]]
res_acs19<-res_all[[1]][["res_acs19_deltatemp3"]]


###########################################################################
#                           ACS 2006-2010
###########################################################################
#1. Look through the mixing weight estimates
mapK<-Mode(res_acs10$K.mcmc)[1]  #tells you mapK
#Class probabilities
stats<-cbind(summary(res_acs10$parameters.ecr.mcmc)$statistics[,c(1,2)], 
             summary(res_acs10$parameters.ecr.mcmc)$quantiles[,c(1,5)])
tail(round(stats, 4), mapK) %>% kable()
check<-as.matrix(res_acs10$parameters.ecr.mcmc)
wts<-check[,217:228]
colMeans(wts)
apply(wts, 2, sd) 
apply(wts, 2, quantile, probs = c(0.025, 0.975))

#Flagging mixture weights for clusters: 5,7

#2. Classification probabilities and cluster assignment based on ECR algorithm and my own classification
##Explore cluster assignment based on ECR algorithm + Distribution of census variables across clusters
classificationprobs_10<-res_acs10$classificationProbabilities.ecr
ClusterAssignment_ecr<-res_acs10$clusterMembershipPerMethod[,2]

# Add cluster classification based on maximum probability using my function
maxProb<-apply(classificationprobs_10, 1, max)
ClusterAssignment_temp<-apply(classificationprobs_10,1, highest_index)


classificationprobs_10$ClusterAssignment_ecr<-ClusterAssignment_ecr
classificationprobs_10$maxProb<-maxProb
classificationprobs_10$ClusterAssignment_temp<-ClusterAssignment_temp

#15 census tracts that are not classified based on highest prob
table(classificationprobs_10$ClusterAssignment_ecr,classificationprobs_10$ClusterAssignment_temp)

classificationprobs_10 %>% filter(ClusterAssignment_temp != ClusterAssignment_ecr)
##This is odd-- these algorithms assign observations to clusters probabilistically based on the highest posterior probabilities of cluster assignments given the observed data
##--> SOLVED: it seems like a rounding issue because it makes the correct assignment when none of the probabilities have similar/same leading number.

table(classificationprobs_10$ClusterAssignment_ecr)
table(classificationprobs_10$ClusterAssignment_temp)

classificationprobs_10$GEOID<-row.names(censusdata_bin$acs5_2010_bin)

#**Since the distribution is not much different, we can re-assign these 15 census tracts to the cluster they belong based on the highest probability-- all other classifications match perfectly, and so we will keep variable ClusterAssignment_temp.
#Also make a note of these "fuzzy" census tracts- to see if same across survey waves

fuzzygeoids_10<-classificationprobs_10 %>% filter(ClusterAssignment_ecr != ClusterAssignment_temp) %>% select(GEOID)

##Find census tracts for which the maximum probability of assignment is less than 50 for further exploration-- 60 census tracts -only
cts10lw50<-classificationprobs_10 %>% filter(maxProb < 0.5)
table(cts10lw50$ClusterAssignment_temp)

#EHR data only has 1260 unique census tracts for 2010 --check if these 60 are within cts10lw50
#cts10lw50$census_tract<-str_sub(cts10lw50$GEOID,start=6, end = 11) 
#sum(cts10lw50$census_tract %in% cts10) #only 41/60 are in the EHR data


#Prep Data
classificationprobs_10$ClusterAssignment_final<- factor(classificationprobs_10$ClusterAssignment_temp, levels = 1:mapK, labels = paste0("cluster", 1:mapK, sep= " "))

dat10_clusters<- classificationprobs_10 %>% select(GEOID,ClusterAssignment_final, maxProb)

###########################################################################################
#Distribution of assignment probabilities to the most probable cluster
#filter(maxProb > 0.5) %>%  
boxplot10<- dat10_clusters %>% ggplot(aes(x = ClusterAssignment_final, y = maxProb, fill = ClusterAssignment_final)) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment", 
       y = "Cluster Assignment Probability", 
       x= "") +
  theme(text = element_text(size = 12),
        axis.ticks = element_blank(),
        axis.text.x =element_blank()) 
boxplot10
#for < 50%
boxplot10_less<- dat10_clusters %>% filter(maxProb > 0.5) %>% ggplot(aes(x = ClusterAssignment_final, y = maxProb, fill = ClusterAssignment_final)) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment", 
       y = "Cluster Assignment Probability", 
       x= "") +
  theme(text = element_text(size = 12),
        axis.ticks = element_blank(),
        axis.text.x =element_blank()) 
boxplot10_less

dattable<- dat10_clusters %>% rename(`Assignment Probability` = maxProb,
                               `Cluster Assignment` = ClusterAssignment_final) 

table1::table1(~`Assignment Probability` |`Cluster Assignment`, data= dattable, overall = F)


##Exploration of the 60 census tracts with prob < 0.5

flags_boxplot<-cts10lw50 %>%  ggplot(aes(x = as.factor(ClusterAssignment_temp), y = maxProb, fill = as.factor(ClusterAssignment_temp))) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment", 
       y = "Assignment Probability (< 50%)", 
       x= "") +
  theme(text = element_text(size = 12),
        axis.ticks = element_blank(),
        axis.text.x =element_blank()) 


###########################################################################################
#Generate bar plots using estimated theta_kj and observed data
#Need to be in order
#names(censusdata_bin$acs5_2010_bin)
sesvars<-c("Median income", "Female household", "< HS", " >= HS", " >= Bacherlors", "Limited EN Proficiency", "Unemployment", "SNAP benefits", "Owner-occupied", "Renter-occupied", 
           "No vehicle", "Hispanic or Latino", "NH Black", "NH Asian", "Crowded housing", "Below 150% poverty", "Working class", "Lack complete plumbing")

# Other color options
viridis_palette <- viridis(18)
custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                   "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                   "#ff0000", "#00ff00", "#0000ff", "#ffff00")

group_cols<-c("#1f77b4","#2ca02c","salmon1", "#bcbd22", "#e377c2")


# Generate 14 pastel colors for ungrouped graph
pastel_palette1 <- generate_pastel_colors(18)


#fig_title = paste("Success Probabilities when mapK=", mapK, sep="")
fig_title = ""
dat_10<-preparedat_fig(res_acs10,mapK,sesvars)
p_10_indiv<-plot_thetakj_indiv(prob_est_long = dat_10, mapK,color_palette = pastel_palette1, fig_title)
p_10_group<-plot_thetakj_group(prob_est_long = dat_10,color_palette = group_cols, fig_title)
p_10_group2<-plot_thetakj_group(prob_est_long = dat_10,color_palette = group_cols, fig_title, numR = 7,numC = 2)
p_10_heatmap<- plot_thetakj_heatmap(prob_est_long = dat_10, fig_title)
p_10_heatmap2<- plot_thetakj_heatmap_r(prob_est_long = dat_10, fig_title)

p_10_group
p_10_group2
p_10_heatmap
p_10_heatmap2

pdf("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/barplot10_18vars_top.pdf", width = 10, height = 20)
p_10_group
dev.off()

png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/barplot10_18vars_top_mapK13.png", width = 950, height = 900)
p_10_group2
dev.off()


###########################################################################################
#Distribution of other demographic census variables across clusters to help with profiles
#merge age dist + race dist with cluster information-- these are proportions
merged10<- merge(dat10_clusters, agedist10_wide, by = "GEOID")

table1::table1(~  age_median + age_20_24 + age_25_34 + age_35_44 + age_45_54 + age_55_59 + age_60_64 + age_65_74 + age_75_84 + age_85_more |ClusterAssignment_final, data= merged10, overall = F)

#Distribution of original NSES variables (not binarized) by cluster
acs10<-acs5_2010_wide1 <- acs5_2010_wide  %>% select( !c( "Femalehousehold_2010_E","SNAP_2010_E","Two_or_more_rooms_2010_E", paste0("ms", 1:8), "ms10", "ms_to", paste0("wc", 1:14), "wc_to","pov_to", paste0("pov", 2:5), "white_collar_occupation_2010", "HouseIncBlowPovLineP_2010", "Lessthan2rooms_2010_E",  "Two_or_more_rooms_2010_P", "Total_occhousing_2010", "NH_some_other_race_2010", "NH_two_or_more_races_2010",  "NonHispanicWhite_2010" , "minoritystatus_2010"))
acs10$ClusterAssignment<-merged10$ClusterAssignment_final

acs10bin<-censusdata_bin$acs5_2010_bin
acs10bin$GEOID <- rownames(acs10bin)
acs10bin<- acs10bin %>% mutate_if(is.numeric, as.factor)
acs10bin$ClusterAssignment<-as.numeric(classificationprobs_10$ClusterAssignment_final)
#acs15bin$ClusterAssignment<- factor(acs15bin$ClusterAssignment, levels = 1:9, labels = paste0("cluster", 1:9, sep= " "))
table1(~ Lackplumbing_2010 + medianincome_2010 + BachelorHigherP_2010  +  Education9years_2010 +  Femalehousehold_2010_P + HighSchoolHigherP_2010 + No_vehicle_2010 + OwnerOccupiedUnitP_2010 + RenterOccupiedUnitP_2010 + UnemployementP_2010 + working_class_2010 + SNAP_2010_P +  Hispanic_or_Latino_2010 + NonHispanicBlack_2010 + NonHispanicAsian_2010 + pov_2010 + Crowding_housing_2010 + lang_home_EN_notwell_2010|ClusterAssignment, data= acs10bin, overall = F)

#Bar plot to look at distribution of the binary variables -- using acs10bin dataset
acs10bin$ClusterAssignment<- as.factor(acs10bin$ClusterAssignment)
datbinprop10<-propbin(acs10bin,mapK, sesvars)
p_10_groupbin<-plot_barBin(datbinprop10,color_palette = group_cols, fig_title = "", numR = 7, numC = 2)


#Plot figures side by side
names(dat_10)<- c("cluster","NSES_VARS", "prop","NSES_group")
dat_10$probtype<-1
datbinprop10$probtype<-2
alldat<-rbind(dat_10, datbinprop10)
alldat$probtype<-factor(alldat$probtype, levels = 1:2, labels = c("Model", "Observed"))


P1<-dat_10 %>% ggplot(aes(x = NSES_VARS, y = prop, fill = NSES_group)) +
  geom_col()  +
  facet_wrap(~cluster ,nrow = mapK, ncol = 1) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = "Model", x= "",
       y = "Probability",
       fill = "Neighborhood SES Variables") +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size=9, angle=90, vjust = 0.75, hjust = 0.88), 
        axis.title.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title.y = element_text(size = 8, color = "black", face = "bold"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

P2<-datbinprop10 %>% ggplot(aes(x = NSES_VARS, y = prop, fill = NSES_group)) +
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~cluster ,nrow = mapK, ncol = 1) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = "Observed", x= "",
       y = "Probability",
       fill = "Neighborhood SES Variables") +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size=9, angle=90, vjust = 0.75, hjust = 0.88), 
        axis.title.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title.y = element_text(size = 8, color = "black", face = "bold"),
        #axis.ticks = element_blank(),
        legend.title = element_text(size = 8, color = "black", face = "bold"),
        legend.text = element_text(size = 8, color = "black"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

library(gridExtra)
grid.arrange(P1,P2, ncol =2)

figcomb<-alldat %>% ggplot(aes(x = NSES_VARS, y = prop, fill = NSES_group)) +
  geom_col()   +
  facet_grid(cluster ~ probtype, scales = "free", space = "free") + 
  scale_fill_manual(values = group_cols) + 
  labs(title = "", x= "",
       y = "Probability",
       fill = "Neighborhood SES Variables") +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size=9, angle=45, vjust = 0.75, hjust = 0.88), 
        axis.title.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title.y = element_text(size = 8, color = "black", face = "bold"),
        #axis.ticks = element_blank(),
        legend.title = element_text(size = 8, color = "black", face = "bold"),
        legend.text = element_text(size = 8, color = "black"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/barplot10_combined.png", width = 850, height = 900)
figcomb
dev.off()


#added 5/15/24--to be removed
#mergedexport<- merge(classificationprobs_10, acs10bin, by= "GEOID")
#str(mergedexport)
#export this data to be used for further exploration
#saveRDS(mergedexport, file = "./acs10clust_5.15.24.rds")


###########################################################################
#                           ACS 2011-2015
###########################################################################

##--Cluster mixing weights estimates
mapK<-Mode(res_acs15$K.mcmc)[1]
#Class probabilities
stats<-cbind(summary(res_acs15$parameters.ecr.mcmc)$statistics[,c(1,2)], summary(res_acs15$parameters.ecr.mcmc)$quantiles[,c(1,5)])
tail(round(stats,4), mapK) %>% kable()

##--Explore cluster assignment based on ECR algorithm + Distribution of census variables across clusters
#For ACS 2015 and mapK = 12
classificationprobs_15<-res_acs15$classificationProbabilities.ecr
# Add cluster classification based on maximum probability 
maxProb<-apply(classificationprobs_15, 1, max)
ClusterAssignment_ecr_temp<-apply(classificationprobs_15,1, highest_index)
table(ClusterAssignment_ecr_temp)

classificationprobs_15$ClusterAssignment<-res_acs15$clusterMembershipPerMethod[,2]
classificationprobs_15$ClusterAssignment_ecr_temp<-ClusterAssignment_ecr_temp
classificationprobs_15$maxProb<-maxProb

#Prep Data 
table(classificationprobs_15$ClusterAssignment_ecr_temp, classificationprobs_15$ClusterAssignment)

classificationprobs_15%>% filter(ClusterAssignment != ClusterAssignment_ecr_temp) 
#only 4CTs
classificationprobs_15$GEOID<-row.names(censusdata_bin$acs5_2015_bin)
fuzzygeoids_15<-classificationprobs_15%>% filter(ClusterAssignment_ecr_temp != ClusterAssignment) %>% select(GEOID)


#Same as above we using clusterassignment_ecr_temp
classificationprobs_15$ClusterAssignment_final<- factor(classificationprobs_15$ClusterAssignment_ecr_temp, levels = 1:mapK, labels = paste0("cluster", 1:mapK, sep= " "))


dat15_clusters<- classificationprobs_15 %>% select(GEOID,ClusterAssignment_final, maxProb)
table(dat15_clusters$ClusterAssignment_final)


###########################################################################################
#Distribution of assignment probabilities to the most probable cluster
boxplot15<-dat15_clusters %>% ggplot(aes(x = ClusterAssignment_final, y = maxProb, fill = ClusterAssignment_final)) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment",
       y = "Assignment Probability after ECR algorithm",
       x= "")
boxplot15

dattable15<-dat15_clusters %>% rename(`Average Assignment Probability (ECR Algorithm)` = maxProb,
                                `Cluster Assignment` = ClusterAssignment_final) 

table1::table1(~`Average Assignment Probability (ECR Algorithm)` |`Cluster Assignment`, 
               data= dattable15, overall = F)

###########################################################################################
#Distribution of other demographic census variables across clusters to help with profiles

merged15<- merge(dat15_clusters, agedist15_wide, by = "GEOID")
table1::table1(~  age_median + age_20_24 + age_25_34 + age_35_44 + age_45_54 + age_55_59 + age_60_64 + age_65_74 + age_75_84 + age_85_more |ClusterAssignment_final, data= merged15, overall = F)

acs15<- acs5_2015_wide  %>% select( !c( "Femalehousehold_2015_E","SNAP_2015_E","Two_or_more_rooms_2015_E", paste0("ms", 1:8), "ms10", "ms_to", paste0("wc", 1:14), "wc_to",
                                        "pov_to", paste0("pov", 2:5), "white_collar_occupation_2015", "HouseIncBlowPovLineP_2015", "Lessthan2rooms_2015_E", "Two_or_more_rooms_2015_P", "Total_occhousing_2015",     "NH_some_other_race_2015","NH_two_or_more_races_2015",  "NonHispanicWhite_2015" ,  "minoritystatus_2015"))
acs15$ClusterAssignment<-merged15$ClusterAssignment_final

#Distrubution of SES variables by cluster
acs15bin<-censusdata_bin$acs5_2015_bin
acs15bin$GEOID <- rownames(acs15bin)
acs15bin<- acs15bin %>% mutate_if(is.numeric, as.factor)
acs15bin$ClusterAssignment<-classificationprobs_15$ClusterAssignment_final

table1(~ Lackplumbing_2015 + medianincome_2015 + BachelorHigherP_2015  +  Education9years_2015 +  Femalehousehold_2015_P + HighSchoolHigherP_2015 + No_vehicle_2015 + OwnerOccupiedUnitP_2015 + RenterOccupiedUnitP_2015 + UnemployementP_2015 + working_class_2015 + SNAP_2015_P +  Hispanic_or_Latino_2015 + NonHispanicBlack_2015 + NonHispanicAsian_2015 + pov_2015 + Crowding_housing_2015 + lang_home_EN_notwell_2015|ClusterAssignment, data= acs15, overall = F)


###########################################################################################
#Generate bar plots using estimated theta_kj and observed data

mapK<-Mode(res_acs15$K.mcmc)[1]  #tells you mapK

fig_title = ""

dat_15<-preparedat_fig(res_acs15,mapK,sesvars)
p_15_indiv<-plot_thetakj_indiv(prob_est_long = dat_15, mapK,color_palette = pastel_palette1, fig_title)
p_15_group<-plot_thetakj_group(prob_est_long = dat_15, mapK,color_palette = group_cols, fig_title)
p_15_group2<-plot_thetakj_group(prob_est_long = dat_15,color_palette = group_cols, fig_title, numR = 7,numC = 2)


pdf("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/barplot15_18vars.pdf", width = 10, height = 20)
p_15_group
dev.off()


png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/barplot15_18vars.png", width = 780, height = 900)
p_15_group2
dev.off()

#Bar plot to look at distribution of the binary variables -- using acs10bin dataset
acs15bin$ClusterAssignment<- as.factor(acs15bin$ClusterAssignment)
datbinprop15<-propbin(acs15bin,mapK, sesvars)
p_15_groupbin<-plot_barBin(datbinprop15,color_palette = group_cols, fig_title = "", numR = 7, numC = 2)


###########################################################################
#                           ACS 2015-2019
###########################################################################

##-Cluster mixing weights estimates
mapK<-Mode(res_acs19$K.mcmc)[1]
#Class probabilities
stats<-cbind(summary(res_acs19$parameters.ecr.mcmc)$statistics[,c(1,2)], summary(res_acs19$parameters.ecr.mcmc)$quantiles[,c(1,5)])

tail(stats, mapK) %>% kable()

##--Explore cluster assignment based on ECR algorithm + Distribution of census variables across clusters
#For ACS 2019 and mapK = 13
classificationprobs_19<-res_acs19$classificationProbabilities.ecr
# Add cluster classification based on maximum and 2nd highest probability 
maxProb<-apply(classificationprobs_19, 1, max)
ClusterAssignment_ecr_temp<-apply(classificationprobs_19,1, highest_index)

classificationprobs_19$maxProb<-maxProb
classificationprobs_19$ClusterAssignment<-res_acs19$clusterMembershipPerMethod[,2]
classificationprobs_19$ClusterAssignment_ecr_temp<-ClusterAssignment_ecr_temp


table(classificationprobs_19$ClusterAssignment_ecr_temp, classificationprobs_19$ClusterAssignment)

#9 cts HT1, 15 HT2
classificationprobs_19%>% filter(ClusterAssignment_ecr_temp != ClusterAssignment) 
classificationprobs_19$GEOID<-row.names(censusdata_bin$acs5_2019_bin)
fuzzygeoids_19<-classificationprobs_19%>% filter(ClusterAssignment_ecr_temp != ClusterAssignment) %>% select(GEOID)

classificationprobs_19$ClusterAssignment_final<- 
  factor(classificationprobs_19$ClusterAssignment, levels = 1:13, labels = paste0("cluster", 1:13, sep= " "))

dat19_clusters<- classificationprobs_19 %>% select(GEOID,ClusterAssignment_final, maxProb)

###########################################################################################
#Distribution of assignment probabilities to the most probable cluster
boxplot19<-dat19_clusters%>%  ggplot(aes(x = ClusterAssignment_final, y = maxProb, fill = ClusterAssignment_final)) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment", 
       y = "Assignment Probability after ECR algorithm", 
       x= "") +
  theme_classic() +
  theme(legend.position = "none") 

boxplot19

dattable19<-dat19_clusters %>% rename(`Average Assignment Probability (ECR Algorithm)` = maxProb,
                                `Cluster Assignment` = ClusterAssignment_final) 

table1::table1(~`Average Assignment Probability (ECR Algorithm)` |`Cluster Assignment`, 
               data= dattable19, overall = F)


###########################################################################################
#Distribution of other demographic census variables across clusters to help with profiles

#merge age dist + race dist with cluster information-- these are proportions
merged19<- merge(dat19_clusters, agedist19_wide, by = "GEOID")

table1::table1(~  age_median + age_20_24 + age_25_34 + age_35_44 + age_45_54 + age_55_59 + age_60_64 + age_65_74 + age_75_84 + age_85_more|ClusterAssignment_final, data= merged19, overall = F)

acs19 <- acs5_2019_wide  %>% select( !c( "Femalehousehold_2019_E","SNAP_2019_E","Two_or_more_rooms_2019_E",  paste0("ms", 1:8), "ms10", "ms_to", paste0("wc", 1:14), "wc_to",
                                         "pov_to", paste0("pov", 2:5), "white_collar_occupation_2019", "HouseIncBlowPovLineP_2019", "Lessthan2rooms_2019_E", "Two_or_more_rooms_2019_P", "Total_housing_2019",    "NH_some_other_race_2019", "NH_two_or_more_races_2019",  "NonHispanicWhite_2019" ,  "minoritystatus_2019" ))
acs19$ClusterAssignment<-merged19$ClusterAssignment_final


#Distrubution of SES variables by cluster
acs19bin<-censusdata_bin$acs5_2019_bin
acs19bin$GEOID <- rownames(acs19bin)
acs19bin<- acs19bin %>% mutate_if(is.numeric, as.factor)
acs19bin$ClusterAssignment<-merged19$ClusterAssignment


table1(~ Lackplumbing_2019 + medianincome_2019 + BachelorHigherP_2019  +  Education9years_2019 +  Femalehousehold_2019_P + HighSchoolHigherP_2019 + No_vehicle_2019 + OwnerOccupiedUnitP_2019 + RenterOccupiedUnitP_2019 + UnemployementP_2019 + working_class_2019 + SNAP_2019_P +  Hispanic_or_Latino_2019 + NonHispanicBlack_2019 + NonHispanicAsian_2019 + pov_2019 + Crowding_housing_2019 + lang_home_EN_notwell_2019 |ClusterAssignment, data= acs19, overall = F)


###########################################################################################
#Generate bar plots using estimated theta_kj and observed data
mapK<-Mode(res_acs19$K.mcmc)[1]  #tells you mapK

fig_title = ""

dat_19<-preparedat_fig(res_acs19,mapK,sesvars)
p_19_indiv<-plot_thetakj_indiv(prob_est_long = dat_19, mapK,color_palette = pastel_palette1, fig_title)
p_19_group<-plot_thetakj_group(prob_est_long = dat_19,color_palette = group_cols, fig_title)
p_19_group2<-plot_thetakj_group(prob_est_long = dat_19,color_palette = group_cols, fig_title, numR = 7,numC = 2)

p_19_heatmap<-plot_thetakj_heatmap(prob_est_long = dat_19, fig_title)
p_19_heatmap1<-plot_thetakj_heatmap_r(prob_est_long = dat_19, fig_title)

p_19_group
p_19_group2
p_19_heatmap
p_19_heatmap1

pdf("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/barplot19_18vars.pdf", width = 10, height = 20)
p_19_group
dev.off()


png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/barplot19_18vars.png", width = 780, height = 900)
p_19_group
dev.off()

#Filter by clusters n> 100
#3,8,11,12

p_19_group2<-plot_thetakj_group(prob_est_long = dat_19 %>% filter(cluster %in% c(3,8,11,12)),color_palette = group_cols, fig_title= "", numR = 4, numC= 1)

png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/barplot19_18vars_mapK13_4clust.png", width = 800, height = 600)
p_19_group2
dev.off()
pdf("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/barplot19_18vars_mapK13_4clust.pdf", width = 10, height = 8)
p_19_group2
dev.off()

###########################################################################
#     Output from models on ----
#Here we will focus on ACS 2015-2019
###########################################################################













