#=======================================================
#Using BayesBinMix package on ACS Data
#Latest models for manuscript--also used these results at ISBA
#We will focus on latest ACS wave 2015-2019, 
#but will also look at 2006-2010 and explore changes
#Author: Carmen Rodriguez
#Last Updated: 8/9/24
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
`%!in%` <- Negate(`%in%`)
source("Utils.R") #Load aux functions

censusdata_bin <- readRDS("./Data/censusdata_bin_raceeth_042524.rds")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
censusdata_bin_noraceeth<-readRDS("./Data/censusdata_bin_noraceeth_061024.rds")
names(censusdata_bin_noraceeth) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")

###########################################################################

#Includes ethnic minorities variables
res_all2<-readRDS("./Model Outputs/acs19_raceeth_priors.rds")
res19_poi<-res_all2[[1]][["res_acs19_deltatemp1_poisson"]]
print(res19_poi) 
#Small clusters that need more exploring: 2,4,5,7-- i will probably reclassify
mapK_poi<-Mode(res19_poi$K.mcmc)[1]

mix_wts_poi<-cbind(summary(res19_poi$parameters.ecr.mcmc)$statistics[,c(1,2)], summary(res19_poi$parameters.ecr.mcmc)$quantiles[,c(1,5)])
tail(mix_wts_poi, mapK_poi) %>% kable()


# Posterior probability for all clusters given Kmap
classificationprobs_poi<-res19_poi$classificationProbabilities.ecr

# Add cluster classification based on maximum posterior probability for cluster assignment
maxProb<-apply(classificationprobs_poi, 1, max) #value of the max prob
ClusterAssignment_highest<-apply(classificationprobs_poi,1, highest_index) #index/cluster #
classificationprobs_poi$maxProb<-maxProb

classificationprobs_poi$ClusterAssignment_ecr<-res19_poi$clusterMembershipPerMethod[,2] #from model

classificationprobs_poi$ClusterAssignment_highest<-ClusterAssignment_highest

#Prep data
classificationprobs_poi$GEOID<-row.names(censusdata_bin$acs5_2019_bin)
classificationprobs_poi$ClusterAssignment_final<- factor(classificationprobs_poi$ClusterAssignment_highest,
                                                         levels = 1:9, labels = paste0("Cluster",sep= " ", 1:9))

#dat19_clusters_poi<- classificationprobs_poi %>% select(GEOID,ClusterAssignment_final, maxProb)



###########################################################################################
#Reclassify clusters with small n based on their second highest assignment probability, 
#but first look at this distribution

smaxProb<-apply(classificationprobs_poi[,1:9], 1, second_highest)
ClusterAssignment_secondhighest<-apply(classificationprobs_poi[,1:9],1, second_highest_index)
classificationprobs_poi$smaxProb<-smaxProb
#classificationprobs_poi$ClusterAssignment_ecr<-res19_poi$clusterMembershipPerMethod[,2]
classificationprobs_poi$ClusterAssignment_secondhighest<-ClusterAssignment_secondhighest


classificationprobs_poi %>% filter(ClusterAssignment_highest == 5) %>% 
  select(maxProb, ClusterAssignment_highest,smaxProb, ClusterAssignment_secondhighest)%>% 
  mutate(diff = maxProb - smaxProb)
#only re-assign cluster 5 as it is the smallest (n=26)--although a lot of the maxProb are high 
#for some and < 0.8 but the mixing weight is the smallest: 0.02 with credible interval of (0.01,0.04)


classificationprobs_poi <- classificationprobs_poi %>% mutate(cluster_reassingment = if_else(
  ClusterAssignment_highest == 5,ClusterAssignment_secondhighest, ClusterAssignment_highest
))

table(classificationprobs_poi$cluster_reassingment)

#Recode by number
classificationprobs_poi <- classificationprobs_poi %>% 
  mutate(cluster_reassingment_recode = case_when( 
    cluster_reassingment %in% c(1,2,3,4) ~ cluster_reassingment,
    cluster_reassingment ==6 ~ 5,
       cluster_reassingment ==7 ~ 6,
      cluster_reassingment ==8 ~ 7,
       cluster_reassingment ==9 ~ 8,))
    
                                        
#Prep data
classificationprobs_poi$ClusterAssignment_recoded_num<- factor(classificationprobs_poi$cluster_reassingment_recode,
                                                         levels = 1:8, labels = paste0("Cluster",sep= " ", 1:8))

#Update maxprob values
classificationprobs_poi$maxProb_updated <- ifelse(
  classificationprobs_poi$ClusterAssignment_highest==5, smaxProb, maxProb)

head(classificationprobs_poi %>% filter(ClusterAssignment_highest==5) %>% select(cluster_reassingment_recode, maxProb, smaxProb,maxProb_updated),20)



dat19_clusters_poi<- classificationprobs_poi %>% select(GEOID,ClusterAssignment_recoded_num, maxProb_updated)

#Reorganize clusters by size

# Calculate the frequency of each category
freq_table <- table(dat19_clusters_poi$ClusterAssignment_recoded_num)

# Recode the factor based on the frequency of each category
dat19_clusters_poi$ClusterAssignment_recoded_size<- factor(dat19_clusters_poi$ClusterAssignment_recoded_num, levels = names(sort(freq_table, decreasing = TRUE)))
dat19_clusters_poi$nsdop_profiles<-dat19_clusters_poi$ClusterAssignment_recoded_size

#Box plots
#Distribution of assignment probabilities to the most probable cluster
#rename from cluster to profile:
levels(dat19_clusters_poi$nsdop_profiles)<- paste0("Profile",sep= " ", 1:8)



boxplot19<-dat19_clusters_poi %>%  ggplot(aes(x = nsdop_profiles, y = maxProb_updated, fill = nsdop_profiles)) +
  geom_boxplot() +
  labs(fill = "Profile Assignment", 
       y = "Profile Assignment Probability", 
       x= "") +
  theme_classic() +
  theme(legend.position = "none",
       text = element_text(size = 16),
      #axis.title.x = element_text(size = 18, color = "black", face = "bold"),
      axis.title.y = element_text(size = 16, color = "black"),
      axis.text.y = element_text(size=12), 
      axis.text.x = element_text(size=12))

boxplot19
png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/boxplot19_profile_size.png", width = 651, height = 604)
boxplot19
dev.off()

dattable19<-dat19_clusters_poi %>% rename(`Average Assignment Probability (ECR Algorithm)` = maxProb_updated,
                                      `Cluster Assignment` = ClusterAssignment_recoded) 

table1::table1(~`Average Assignment Probability (ECR Algorithm)` |`Cluster Assignment`, 
               data= dattable19, overall = F)


#Generate bar plots using estimated theta_kj and observed data
#Need to be in order
sesvars<-c("Median income", "Female household", "< HS", " >= HS", " >= Bacherlors", "Limited EN Proficiency", "Unemployment", "SNAP benefits", "Owner-occupied housing", "Renter-occupied housing", 
           "No vehicle", "Hispanic/Latino", "NH Black", "NH Asian", "Crowded housing", "Below 150% poverty", "Working class", "Lack complete plumbing")

group_cols<-c("#1f77b4","#2ca02c","salmon1", "#e377c2")


# Generate 18 pastel colors for ungrouped graph
pastel_palette1 <- generate_pastel_colors(18)

#fig_title = paste("Success Probabilities when mapK=", mapK, sep="")
fig_title = ""
dat_19<-preparedat_fig(res19_poi,mapK_poi,sesvars)
#Remove cluster 5 and relabel like we did above
dat_19_8clust<- dat_19 %>%  filter( cluster != 5)
dat_19_8clust$cluster<- rep(1:8, each = 18)

# dat_19_8clust$cluster<- factor(dat_19_8clust$cluster, levels = 1:8, labels = c(
#   "Cluster 1: 401", "Cluster 2: 76", "Cluster 3: 378", "Cluster 4: 100",
#   "Cluster 5: 109", "Cluster 6: 87", "Cluster 7: 142", "Cluster 8: 185"))
# 
dat_19_8clust<- dat_19_8clust %>% mutate(cluster_size = case_when(
  cluster== 1 ~ 1,
  cluster== 2 ~ 8 ,
  cluster== 3 ~ 2,
  cluster== 4 ~ 6,
  cluster== 5 ~ 5,
  cluster== 6 ~ 7,
  cluster== 7 ~ 4,
  cluster== 8 ~ 3,
))



dat_19_8clust$cluster_size<- factor(dat_19_8clust$cluster_size, levels = 1:8, labels = c( 
  "Profile 1 (n = 401)", "Profile 2 (n = 378)", "Profile 3 (n = 185)", "Profile 4 (n=142)",
  "Profile 5 (n =109)", "Profile 6 (n =100)", "Profile 7 (n =87)", "Profile 8 (n = 76)"))


# Generate plot
barplot<-dat_19_8clust  %>% ggplot(aes(x = NSES_VARS, y = theta_kj, fill = NSES_group)) +
  geom_col()  +
  facet_wrap(~cluster_size, nrow = 8, ncol = 1) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = fig_title, x= "",
       y = "Probability",
       fill = "Neighborhood SDoH Variables") +
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

#with cols
barplot1<-dat_19_8clust %>% ggplot(aes(x = NSES_VARS, y = theta_kj, fill = NSES_group)) +
  geom_col()  +
  facet_wrap(~cluster_size, nrow = 5, ncol = 2) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = fig_title, x= "",
       y = "Probability",
       fill = "Neighborhood SDoH Variables") +
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


png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/barplot19_2cols_labeled_size.png", width = 1100, height = 1000)
barplot1
dev.off()


#No coloring by domain

barplot2<-dat_19_8clust %>% ggplot(aes(x = NSES_VARS, y = theta_kj )) +
  geom_col(fill = "#1f77b4")  +
  facet_wrap(~cluster, nrow = 5, ncol = 2) + 
  #scale_fill_manual(values = group_cols) + 
  labs(title = fig_title, x= "",
       y = "Probability",
       fill = "Neighborhood SDoH Variables") +
  theme(strip.text = element_text(size = 14),
        text = element_text(size = 16),
        axis.text.x = element_text(size=16, angle=90, vjust = 0.88, hjust = 0.88), 
        #axis.title.x = element_text(size = 18, color = "black", face = "bold"),
        axis.title.y = element_text(size = 16, color = "black", face = "bold"),
        axis.text.y = element_text(size=12), 
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5))

png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/barplot19_2cols_labeled_1col.png", width = 1100, height = 1000)
barplot2
dev.off()


##Saving cluster data for models with ethnic minorities
saveRDS(dat19_clusters_poi, "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Data/mbmm_clusters_19eth_sized.rds")
write.csv(dat_19_8clust, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Table1/thetakj.csv")

##Table of ACS bin variables by clusters###
acs19dat<-censusdata_bin$acs5_2019_bin
acs19dat <- data.frame(lapply(acs19dat, as.factor))
acs19dat$GEOID<-row.names(censusdata_bin$acs5_2019_bin)
#join with dat19cluster
acsdat19clust<- merge(acs19dat, dat19_clusters_poi, by = "GEOID")

tab<-table1::table1( ~  medianincome_2019 + Femalehousehold_2019_P + HighSchoolHigherP_2019 + Education9years_2019 +
                       HighSchoolHigherP_2019 + BachelorHigherP_2019 + lang_home_EN_notwell_2019 + UnemployementP_2019 +
                       SNAP_2019_P + OwnerOccupiedUnitP_2019 + RenterOccupiedUnitP_2019 + No_vehicle_2019 + Hispanic_or_Latino_2019 +
                       NonHispanicBlack_2019 + NonHispanicAsian_2019 +Crowding_housing_2019 + pov_2019 + working_class_2019 + Lackplumbing_2019
                      |nsdop_profiles, data = acsdat19clust, overall = F)
tab

##Table of ACS prop variables by clusters###
load("./Data/censusdata_updated_newvars.RData")
acs19datprop<-acs5_2019_wide
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")






##############################################################################################################################
##############################################################################################################################
##############################################################################################################################


# 
# ### Map showing the distribution 
# temp<-get_acs(state = "MA", geography = "tract", 
#               variables = "B03002_001", year = 2019, geometry = TRUE, cache_table = TRUE)
# 
# temp1<- temp %>% select(GEOID, NAME, geometry, estimate)
# 
# dat19map<-dat19_clusters_poi %>% select(GEOID,ClusterAssignment_recoded)
# 
# #join datasets
# map19<- merge(temp1, dat19map, by = "GEOID")
# map19a<-map19 %>%  select(GEOID, NAME, ClusterAssignment_recoded,geometry) 
# map19a<- map19a %>% mutate(`NSDoH profile` = ClusterAssignment_recoded)
# #map19a$Cluster<-as.factor(map19a$Cluster)
# 
# #colors to match the bar plot
# custom_colors <- custom_colors <- c("#d62728","salmon1", "#2ca02c",  
#                                     "#9467bd",
#                                     "#e377c2",  "#17becf",
#                                     "#1f77b4", "#ffff00")
# map19_clust<-tm_shape(st_make_valid(map19a)) +
#   tm_fill("NSDoH profile",style="cat",palette=custom_colors,legend.hist=TRUE,alpha=1)  +
#   #tm_borders(col = "white") +
#   tm_layout(legend.outside = TRUE, legend.outside.position = "right",scale =1.2) 
# 
# tmap_save(tm = map19_clust, filename = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/map19.png")
# 
# #census tracts in cluster 1-- what counties do they belong to?
# checks<-map19a %>% filter(ClusterAssignment_recoded == "Cluster 1") %>% select("GEOID", "NAME")







###########################################################################
###########################################################################
#Does not include ethnic minorities variables                                                                
res_all3<-readRDS("./Model Outputs/acs19_noraceeth_priors.rds")
res19_poi_ne<-res_all3[[1]][["res_acs19_deltatemp1_poisson"]]
print(res19_poi_ne) #Kmap = 10
mapK_poi_ne<-Mode(res19_poi_ne$K.mcmc)[1]
 
mix_wts_poi_ne<-cbind(summary(res19_poi_ne$parameters.ecr.mcmc)$statistics[,c(1,2)], summary(res19_poi_ne$parameters.ecr.mcmc)$quantiles[,c(1,5)])
tail(mix_wts_poi_ne, mapK_poi_ne) %>% kable()                                                               
#Small clusters 6 and 7                                                              
  
classificationprobs_poi_ne<-res19_poi_ne$classificationProbabilities.ecr
# Add cluster classification based on maximum probability 
maxProb<-apply(classificationprobs_poi_ne, 1, max)
ClusterAssignment_highest<-apply(classificationprobs_poi_ne,1, highest_index)
classificationprobs_poi_ne$maxProb<-maxProb
classificationprobs_poi_ne$ClusterAssignment_ecr<-res19_poi_ne$clusterMembershipPerMethod[,2]
classificationprobs_poi_ne$ClusterAssignment_highest<-ClusterAssignment_highest 

classificationprobs_poi_ne %>% filter(ClusterAssignment_ecr !=ClusterAssignment_highest)
#5 missclasified

#Prep data
classificationprobs_poi_ne$GEOID<-row.names(censusdata_bin$acs5_2019_bin)
classificationprobs_poi_ne$ClusterAssignment_final<- factor(classificationprobs_poi_ne$ClusterAssignment_highest,
                                                         levels = 1:10, labels = paste0("Cluster",sep= " ", 1:10))

#Reclassify clusters with small n based on their second highest assignment probability, 
#but first look at this distribution

smaxProb<-apply(classificationprobs_poi_ne[,1:10], 1, second_highest)
ClusterAssignment_secondhighest<-apply(classificationprobs_poi_ne[,1:10],1, second_highest_index)
classificationprobs_poi_ne$smaxProb<-smaxProb
classificationprobs_poi_ne$ClusterAssignment_ecr<-res19_poi_ne$clusterMembershipPerMethod[,2]
classificationprobs_poi_ne$ClusterAssignment_secondhighest<-ClusterAssignment_secondhighest

#Second highest probability, max is 0.483

classificationprobs_poi_ne %>% filter(ClusterAssignment_highest %in% c(6,7)) %>% 
  select(maxProb, ClusterAssignment_highest,smaxProb, ClusterAssignment_secondhighest)
#only re-assign cluster 7 as it is the smallest

classificationprobs_poi_ne <- classificationprobs_poi_ne %>% mutate(cluster_reassingment = if_else(
  ClusterAssignment_highest == 7,ClusterAssignment_secondhighest, ClusterAssignment_highest
))

table(classificationprobs_poi_ne$cluster_reassingment)

#Recode
classificationprobs_poi_ne <- classificationprobs_poi_ne %>% 
  mutate(cluster_reassingment_recode = case_when( 
    cluster_reassingment %in% c(1,2,3,4,5,6) ~ cluster_reassingment,
    cluster_reassingment ==8 ~ 7,
    cluster_reassingment ==9 ~ 8,
    cluster_reassingment ==10 ~ 9,))


#Prep data
classificationprobs_poi_ne$GEOID<-row.names(censusdata_bin$acs5_2019_bin)
classificationprobs_poi_ne$ClusterAssignment_recoded<- factor(classificationprobs_poi_ne$cluster_reassingment_recode,
                                                           levels = 1:9, labels = paste0("Cluster",sep= " ", 1:9))

classificationprobs_poi_ne$maxProb_updated <- ifelse(
  classificationprobs_poi_ne$ClusterAssignment_highest == 7, smaxProb, maxProb)



dat19_clusters_poi_ne<- classificationprobs_poi_ne %>% select(GEOID,ClusterAssignment_recoded, maxProb_updated)


#Box plots
#Distribution of assignment probabilities to the most probable cluster
boxplot19_ne<-dat19_clusters_poi_ne %>%  ggplot(aes(x = ClusterAssignment_recoded, y = maxProb_updated, fill = ClusterAssignment_recoded)) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment", 
       y = "Assignment Probability after ECR algorithm", 
       x= "") +
  theme_classic() +
  theme(legend.position = "none") 

boxplot19_ne

dattable19_ne<-dat19_clusters_poi_ne %>% rename(`Average Assignment Probability (ECR Algorithm)` = maxProb_updated,
                                          `Cluster Assignment` = ClusterAssignment_recoded) 

table1::table1(~`Average Assignment Probability (ECR Algorithm)` |`Cluster Assignment`, 
               data= dattable19_ne, overall = F)

#Generate bar plots using estimated theta_kj and observed data
#Need to be in order
sesvars_ne<-c("Median income", "Female household", "< HS", " >= HS", " >= Bacherlors", "Limited EN Proficiency", "Unemployment", "SNAP benefits", "Owner-occupied", "Renter-occupied", 
           "No vehicle","Crowded housing", "Below 150% poverty", "Working class", "Lack complete plumbing")

group_cols<-c("#1f77b4","#2ca02c","salmon1", "#bcbd22", "#e377c2")


# Generate 14 pastel colors for ungrouped graph
#pastel_palette1 <- generate_pastel_colors(18)

#fig_title = paste("Success Probabilities when mapK=", mapK, sep="")
fig_title = ""
dat_19_ne<-preparedat_fig(res19_poi_ne,mapK_poi_ne,sesvars_ne)

#Remove cluster 7 and relabel
dat_19_9clust<- dat_19_ne %>%  filter(cluster != 7)
dat_19_9clust$cluster<- rep(1:9, each = 15)
dat_19_9clust$cluster<- factor(dat_19_9clust$cluster, levels = 1:9, labels = c(
  "Cluster 1: 75", "Cluster 2: 101", "Cluster 3: 183", "Cluster 4: 84",
  "Cluster 5: 378", "Cluster 6: 57", "Cluster 7: 95", "Cluster 8: 398","Cluster 9: 107"))

# Generate plot
barplot_ne<-dat_19_9clust %>% ggplot(aes(x = NSES_VARS, y = theta_kj, fill = NSES_group)) +
  geom_col()  +
  facet_wrap(~cluster, nrow = 9, ncol = 1) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = fig_title, x= "",
       y = "Probability",
       fill = "Neighborhood SDoH Variables") +
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

#with cols
barplot1_ne<-dat_19_9clust %>% ggplot(aes(x = NSES_VARS, y = theta_kj, fill = NSES_group)) +
  geom_col()  +
  facet_wrap(~cluster, nrow = 5, ncol = 2) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = fig_title, x= "",
       y = "Probability",
       fill = "Neighborhood SDoH Variables") +
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


png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/barplot19_2cols_noethnicity.png", width = 1100, height = 1000)
barplot1_ne
dev.off()

### Map showing the distribution 
# temp<-get_acs(state = "MA", geography = "tract", 
#               variables = "B03002_001", year = 2019, geometry = TRUE, cache_table = TRUE)
# temp1<- temp %>% select(GEOID, NAME, geometry, estimate)
 
dat19map_ne<-dat19_clusters_poi_ne %>% select(GEOID,ClusterAssignment_recoded)

#join datasets
map19_ne<- merge(temp1, dat19map_ne, by = "GEOID")
map19a_ne<-map19_ne %>%  select(GEOID, NAME, ClusterAssignment_recoded,geometry) 
map19a_ne<- map19a_ne %>% mutate(`NSDoH profile` = ClusterAssignment_recoded)


#colors to match the bar plot
custom_colors <- custom_colors <- c("#8c564b","salmon1", "#2ca02c",  
                                    "#9467bd","#d62728",
                                    "#e377c2",  "#17becf",
                                    "#1f77b4", "#ffff00")
map19_clust_ne<-tm_shape(st_make_valid(map19a_ne)) +
  tm_fill("NSDoH profile",style="cat",palette=custom_colors,legend.hist=TRUE,alpha=1)  +
  #tm_borders(col = "white") +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right",scale =1.2) 

tmap_save(tm = map19_clust_ne, filename = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Figures/map19_noethnicity.png")


# ##Saving cluster data for models with ethnic minorities
# dat19_final <- map19a %>% select(GEOID, NAME,ClusterAssignment_recoded)
# saveRDS(dat19_final, "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Data/mbmm_clusters_19eth.rds")

