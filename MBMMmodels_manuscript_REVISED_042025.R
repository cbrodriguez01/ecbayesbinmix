
##----Load packages----
library(BayesBinMix)
library(foreach)
library(label.switching)
library(doParallel)
library(coda)
library(tidyverse)
library(DescTools)
library(table1)
source("Utils.R") #Load aux functions

# Model output
#Determine the best model settings
#Important link:http://bamm-project.org/mc3.html

#Determine the optimal \Delta T value for a specific data set. I extracted the 
#percent acceptance of the chain swap proposals from all models 

#Our preliminary tests show that four chains produces good MCMC mixing
#The \Delta T value should be set such that the probability of accepting a chain swap proposal is between 20% and 60%.



swaps_summ<- read.csv(file = "Model_run_updates/ACS19model_summary_results14vars_2025.csv")
str(swaps_summ)

model_outputs<-readRDS("Model_run_updates/ACS19_14vars_full_model_outputs2025.rds")


#Add estimated K
# Initialize a vector to store the MAP K for each model
mapK <- numeric(length(model_outputs))
modnames<- c() #keep record

# Loop through each model output and extract the mode of K.mcmc
for (i in seq_along(model_outputs)) {
  temp1<-model_outputs[[i]][1]
  k_chain <- temp1[[1]]$K.mcmc
  modnames[i]<-names(model_outputs[i])
  mapK[i] <- Mode(k_chain)[1]
}
#cbind(modnames, mapK)

swaps_summ$mapK<-mapK
ggplot(swaps_summ, aes(x = deltatemp, y = swap_rate, color = Kprior, shape = gamma_label)) +
  geom_line(aes(group = interaction(Kprior, gamma_label))) +
  geom_point(size = 3) +
  labs(
    title = "Swap Rate vs Delta Temperature",
    x = "Delta Temperature",
    y = "Swap Rate"
  ) +
  theme_minimal()


ggplot(swaps_summ, aes(x = swap_rate, y = mapK, color = Kprior, shape = gamma_label)) +
  geom_point(size = 3) +
  labs(
    title = "Model Selection Landscape",
    x = "Swap Rate (Mixing Efficiency)",
    y = "MAP Estimate of K (Model Complexity)"
  ) +
  theme_minimal()


#Quick checks of DirichApprox -- very large and small clusters
print(model_outputs$poisson_DirichPAprox_deltatemp0.010)
print(model_outputs$uniform_DirichPAprox_deltatemp0.010) #some clusters have 1 obs
print(model_outputs$poisson_DirichSym_deltatemp0.010)
print(model_outputs$poisson_DirichSym_deltatemp0.025) #one smaller cluster of 76 


#PLAN: stick with deltatemp = 0.01 or 0.025 for good swap performance. 
#We will explore the models more closely through trace plots and other visuals

#---Models Summary----

mod1<-model_outputs$poisson_DirichSym_deltatemp0.010$res_acs19_deltatemp_0.010_poisson
mod2<-model_outputs$poisson_DirichSym_deltatemp0.025$res_acs19_deltatemp_0.025_poisson

###----Mixing weights distributions---- 

stats_mod1<-cbind(summary(mod1$parameters.ecr.mcmc)$statistics[,c(1,2)], 
             summary(mod1$parameters.ecr.mcmc)$quantiles[,c(1,5)])

tail(round(stats_mod1, 4), 5) #this looks promising!


stats_mod2<-cbind(summary(mod2$parameters.ecr.mcmc)$statistics[,c(1,2)], 
                  summary(mod2$parameters.ecr.mcmc)$quantiles[,c(1,5)])

tail(round(stats_mod2, 4), 7) #clusters with small mixing weights ranging from 0.07-0.09


#---Visualize MCMC samples using coda package----
#Trace plots and histograms
plot_param_group <- function(model, param_names, title_prefix = "Params", group_size = 4) {
  mcmcAll <- model$parameters.ecr.mcmc
  groups <- split(param_names, ceiling(seq_along(param_names) / group_size))
  
  for (i in seq_along(groups)) {
    param_subset <- groups[[i]]
    mcmcSubset <- mcmcAll[, param_subset, drop = FALSE]
    # Layout: 2 columns per parameter (trace + histogram)
    # layout(matrix(1:(2 * length(param_subset)), ncol = 2, byrow = TRUE))
    # par(mar = c(3, 3, 2, 1))  # Adjust margins
    # 
    plot(mcmcSubset, auto.layout = FALSE, ask = FALSE, col = "gray40")
    cat(sprintf("press [Enter] to continue ", 
                i, length(groups)))
    readline()
  }
}

## MCMC samples Model 1-----
params <- mod1$parameters.ecr.mcmc
# Get theta and p columns
theta_names <- grep("^theta", colnames(params), value = TRUE)
p_names <- grep("^p\\.", colnames(params), value = TRUE)

# Plot theta parameters
plot_param_group(mod1, theta_names, title_prefix = "Theta", group_size = 4)

# Plot mixing weights 
plot_param_group(mod1, p_names, title_prefix = "Mixing Proportions", group_size = 1)

## MCMC samples Model 2-----
params <- mod2$parameters.ecr.mcmc
# Get theta and p columns
theta_names <- grep("^theta", colnames(params), value = TRUE)
p_names <- grep("^p\\.", colnames(params), value = TRUE)

# Plot mixing weights 
plot_param_group(mod2, p_names, title_prefix = "Mixing Proportions", group_size = 1) #bimodal distributions



##Plots like Figure 4 in the bayesbinmix paper----


#(1) Illustrate the sampled values of K per chain according to the Poisson and uniform prior distribution using data from K.allChains

poiKallchains1<-read.table(file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/BayesBinMix_FASRC/Updates/acs19_2025-04-18_deltatemp_0.010_poisson_DirichSym/K.allChains.txt", header = T)
poiKallchains2<-read.table(file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/BayesBinMix_FASRC/Updates/acs19_2025-04-18_deltatemp_0.025_poisson_DirichSym/K.allChains.txt", header = T)


poiKallchains1$m <-as.numeric(row.names(poiKallchains1))
poiKallchains1a<-poiKallchains1 %>% pivot_longer(cols = 1:4, names_to ="Chain", values_to = "K")

poiKallchains2$m <-as.numeric(row.names(poiKallchains2))
poiKallchains2a<-poiKallchains2 %>% pivot_longer(cols = 1:4, names_to ="Chain", values_to = "K")



#this plot shows the generated values of K per heated chain-- note chain 1 is the original (^1)
poiKallchains1a %>% ggplot(aes(x = m, y = K, col = as.factor(Chain))) + 
  geom_line() + 
  ggtitle("MC^3 Sampler Poi") + 
  xlab("mcmc cycle")
poiKallchains2a %>% ggplot(aes(x = m, y = K, col = as.factor(Chain))) +
  geom_line() + 
  ggtitle("MC^3 Sampler Poi- Jeff") +
  xlab("mcmc cycle")


#(2) Raw output of p1,...,pK  and theta_kj conditional on K=10 & K=8 (NOT REORDERED!)

poiraw1<-read.table(file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/BayesBinMix_FASRC/Updates/acs19_2025-04-18_deltatemp_0.010_poisson_DirichSym/rawMCMC.mapK.5.txt", header = T)
poiraw2<-read.table(file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/BayesBinMix_FASRC/Updates/acs19_2025-04-18_deltatemp_0.025_poisson_DirichSym/rawMCMC.mapK.7.txt", header = T)

poiraw1a<-poiraw1[,paste0("p.", seq(1,5), "")]
colnames(poiraw1a)<-paste("Component", seq(1,5), "")
poiraw1a$iteration<-as.numeric(row.names(poiraw1a))
poiraw1a<-poiraw1a%>% pivot_longer(cols = 1:5, names_to = "Component", values_to = "raw.mix.prop")
poiraw1a %>% ggplot(aes(x=iteration, y = raw.mix.prop, col = as.factor(Component))) + geom_point()



poiraw2a<-poiraw2[,paste0("p.", seq(1,7), "")]
colnames(poiraw2a)<-paste("Component", seq(1,7), "")
poiraw2a$iteration<-as.numeric(row.names(poiraw2a))
poiraw2a<-poiraw2a%>% pivot_longer(cols = 1:7, names_to = "Component", values_to = "raw.mix.prop")
poiraw2a %>% ggplot(aes(x=iteration, y = raw.mix.prop, col = as.factor(Component))) + geom_point()


#(3) Reordered by ECR algorithm
poireordered1<-read.table(file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/BayesBinMix_FASRC/Updates/acs19_2025-04-18_deltatemp_0.010_poisson_DirichSym/reorderedMCMC-ECR.mapK.5.txt", header = T)
poireordered2<-read.table(file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/BayesBinMix_FASRC/Updates/acs19_2025-04-18_deltatemp_0.025_poisson_DirichSym/reorderedMCMC-ECR.mapK.7.txt", header = T)
  
  
poireordered1p<-poireordered1[,paste0("p.", seq(1,5), "")]
colnames(poireordered1p)<-paste("Component", seq(1,5), "")
poireordered1p$iteration<-as.numeric(row.names(poireordered1p))
poireordered1pa<-poireordered1p %>% pivot_longer(cols = 1:5, names_to = "Component", values_to = "reordered.mix.prop")

poireordered1pa%>% ggplot(aes(x=iteration, y = reordered.mix.prop, col = as.factor(Component))) + 
  geom_point() +
  ggtitle("Reordered sample (mixing weights) according to ECR Algorith") +
  xlab("Iteration")+
  ylab("reordered weight") + theme(legend.title=element_blank()) 


#(4) Reordered by STEPHENS algorithm
poireordered1_st<-read.table(file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/BayesBinMix_FASRC/Updates/acs19_2025-04-18_deltatemp_0.010_poisson_DirichSym/reorderedMCMC-STEPHENS.mapK.5.txt", header = T)

poireordered1p_st<-poireordered1_st[,paste0("p.", seq(1,5), "")]
colnames(poireordered1p_st)<-paste("Component", seq(1,5), "")
poireordered1p_st$iteration<-as.numeric(row.names(poireordered1p_st))
poireordered1p_st<-poireordered1p_st %>% pivot_longer(cols = 1:5, names_to = "Component", values_to = "reordered.mix.prop")

poireordered1p_st%>% ggplot(aes(x=iteration, y = reordered.mix.prop, col = as.factor(Component))) + 
  geom_point() +
  ggtitle("Reordered sample according to STEPHENS Algorith") +
  xlab("Iteration")+
  ylab("reordered weight") + theme(legend.title=element_blank()) 

#SMALL DIFF in comp1 vs comp3

###########################################################################
#---Figures and Tables with Model Summary----

mapK_mod<-Mode(mod1$K.mcmc)[1]  #tells you mapK
print(mod1)
tail(round(stats_mod1, 4), 5)
#Classification probabilities and cluster assignment based on ECR algorithm and my own classification
##Explore cluster assignment based on ECR algorithm + Distribution of census variables across clusters

cluster_assignment<-cbind(mod1$classificationProbabilities.ecr, assignment_ecr =mod1$clusterMembershipPerMethod[,2])

# Add cluster classification based based on the maximum a posteriori (MAP) probability at each point, 
#i.e., whichever cluster has the highest membership probability
maxProb<-apply(mod1$classificationProbabilities.ecr, 1, max)
ca_temp<-apply(mod1$classificationProbabilities.ecr,1, highest_index) 
cluster_assignment<-cbind(cluster_assignment, maxProb,ca_temp)
table(cluster_assignment$assignment_ecr, ca_temp)
#table(mod1$clusterMembershipPerMethod[,1],ca_temp) #CHECKING STEPHENS
cluster_assignment %>% filter(assignment_ecr != ca_temp) 
#it seems to be an issue with rounding-- maybe it only looks at the first number after decimal and rounds


#We assign classification based on ECR--see notion BayesBinMix Model/Model Results By Dates (4/20/25- Model Revisions for new submission to JRSS-A)
prob_matrix <- as.matrix(mod1$classificationProbabilities.ecr)
# Extract the probability that matches assignment_ecr for each row
prob_ecr <- prob_matrix[cbind(1:nrow(cluster_assignment), cluster_assignment$assignment_ecr)]
cluster_assignment$assignment_prob_ecr<- prob_ecr

#Add GEOIDs and subset.
acs19_bin<-readRDS("Model_run_updates/censusdata_bin.rds")
cluster_assignment1<- cbind(acs19_bin %>% select(GEOID, NAME), cluster_assignment %>% select(assignment_ecr,assignment_prob_ecr))
cluster_assignment1$assignment_ecr<- factor(cluster_assignment1$assignment_ecr, levels = 1:mapK_mod, labels = paste0("profile", 1:mapK_mod, sep= " "))
table(cluster_assignment1$assignment_ecr)

###########################################################################################

#Distribution of assignment probabilities to the most probable cluster
summary(cluster_assignment1$assignment_prob_ecr)
boxplot1<- cluster_assignment1 %>% ggplot(aes(x = assignment_ecr, y = assignment_prob_ecr, fill = assignment_ecr)) +
  geom_boxplot() +
  labs(fill = "NSDoH Profile Assignment", 
       y = "NSDoH Profile Assignment Probability", 
       x= "") +
  theme(text = element_text(size = 12),
        axis.ticks = element_blank(),
        axis.text.x =element_blank()) 
boxplot1

#for < 50%
probless50<-cluster_assignment1 %>% filter(assignment_prob_ecr < 0.5) #46 census tracts
summary(probless50)
boxplot_less<- probless50 %>% filter(assignment_prob_ecr < 0.5) %>% ggplot(aes(x = assignment_ecr, y = assignment_prob_ecr, fill =assignment_ecr)) +
  geom_boxplot() +
  labs(fill = "NSDoH Profile Assignment", 
       y = "NSDoH Profile Assignment Probability", 
       x= "") +
  theme(text = element_text(size = 12),
        axis.ticks = element_blank(),
        axis.text.x =element_blank()) 
boxplot_less

png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Model_run_updates/boxplot19_profilesprobs.png", width = 651, height = 604)
boxplot1
dev.off()



###########################################################################################
#Generate bar plots using estimated theta_kj and observed data
names(acs19_bin)
#Need to be in order
sesvars<-c("Median income", "Female household", "No HS Diploma", "Crowded housing", "Working class","Unemployment","Renter-occupied housing",
           "No vehicle", "NH Black", "NH Asian","Hispanic/Latino",  "Limited EN Proficiency", "SNAP benefits","Lack complete plumbing")
          

group_cols<-c("#1f77b4","#2ca02c","salmon1", "#e377c2")


# Generate 18 pastel colors for ungrouped graph
pastel_palette1 <- generate_pastel_colors(14)
fig_title = ""
dat_19<-preparedat_fig_updated(mod1,mapK_mod,sesvars)

# Generate plot
p_19_group<-plot_thetakj_group(prob_est_long = dat_19,color_palette = group_cols, fig_title, numR = mapK_mod)
p_19_group2<-plot_thetakj_group(prob_est_long = dat_19,color_palette = group_cols, fig_title, numR = 5,numC = 2)



dat_19$cluster_label<- factor(dat_19$cluster, levels = 1:5, labels = c( 
  "Profile 1 (n = 136)", "Profile 2 (n = 263)", "Profile 3 (n = 468)", "Profile 4 (n=375)",
  "Profile 5 (n = 236)"))



barplot<-dat_19  %>% ggplot(aes(x = NSDOH_VARS, y = theta_kj, fill = NSDOH_group)) +
  geom_col()  +
  facet_wrap(~cluster_label, nrow = 5, ncol = 1) + 
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
barplot1<- dat_19 %>% ggplot(aes(x = NSDOH_VARS, y = theta_kj, fill = NSDOH_group)) +
  geom_col()  +
  facet_wrap(~cluster_label, nrow = 3, ncol = 2) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = fig_title, x= "",
       y = "Probability",
       fill = "Neighborhood SDoH Domains") +
  theme(strip.text = element_text(size = 14),
        text = element_text(size = 18),
        axis.text.x = element_text(size=16, angle=90, vjust = 0.88, hjust = 0.88), 
        #axis.title.x = element_text(size = 18, color = "black", face = "bold"),
        axis.title.y = element_text(size = 14, color = "black", face = "bold"),
        axis.text.y = element_text(size=12), 
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5))

png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Model_run_updates/barplot19_2cols_14vars.png", width = 1200, height = 1000)
barplot1
dev.off()


# Order clusters by size
# Calculate the frequency of each category
freq_table <- table(cluster_assignment1$assignment_ecr)

# Recode the factor based on the frequency of each category
cluster_assignment1$assignment_ecr_size<- factor(cluster_assignment1$assignment_ecr, levels = names(sort(freq_table, decreasing = TRUE)))
cluster_assignment1$nsdoh_profiles<- cluster_assignment1$assignment_ecr_size
levels(cluster_assignment1$nsdoh_profiles)<- c("Profile 1", "Profile 2", "Profile 3",
                                               "Profile 4", "Profile 5")



boxplot2<- cluster_assignment1 %>% ggplot(aes(x = nsdoh_profiles, y = assignment_prob_ecr, fill = nsdoh_profiles)) +
  geom_boxplot() +
  labs(fill = "NSDoH Profile Assignment", 
       y = "NSDoH Profile Assignment Probability", 
       x= "") + theme_classic() +
  theme(text = element_text(size = 14),
        axis.ticks = element_blank(),
        axis.text.x =element_blank()) 
boxplot2

png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Model_run_updates/boxplot19_profilesprobs_size_wt.png", width = 651, height = 604)
boxplot2 
dev.off()

#Table for supplementary materials
cluster_assignment$GEOID<- acs19_bin$GEOID

result <- apply(cluster_assignment %>% select(paste0("cluster", sep= " ", 1:5)) , 2, function(x) {
  c(median = median(x), IQR = IQR(x))
})


median(cluster_assignment1$assignment_prob_ecr)
IQR(cluster_assignment1$assignment_prob_ecr)

cluster_assignment1 %>% group_by(nsdoh_profiles) %>% summarise( med = median(assignment_prob_ecr), iqr= IQR(assignment_prob_ecr))





dat_19<- dat_19 %>% mutate(cluster_size = case_when(
  cluster== 1 ~ 5,
  cluster== 2 ~ 3 ,
  cluster== 3 ~ 1,
  cluster== 4 ~ 2,
  cluster== 5 ~ 4,
))


dat_19$cluster_size<- factor(dat_19$cluster_size, levels = 1:5, labels = c( 
  "Profile 1 (n = 468)", "Profile 2 (n = 375)", "Profile 3 (n = 263)", "Profile 4 (n=236)",
  "Profile 5 (n = 136)"))


barplot2<- dat_19 %>% ggplot(aes(x = NSDOH_VARS, y = theta_kj, fill = NSDOH_group)) +
  geom_col()  +
  facet_wrap(~cluster_size, nrow = 3, ncol = 2) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = fig_title, x= "",
       y = "Probability",
       fill = "Neighborhood SDoH Domains") +
  theme(strip.text = element_text(size = 14),
        text = element_text(size = 18),
        axis.text.x = element_text(size=16, angle=90, vjust = 0.88, hjust = 0.88), 
        #axis.title.x = element_text(size = 18, color = "black", face = "bold"),
        axis.title.y = element_text(size = 14, color = "black", face = "bold"),
        axis.text.y = element_text(size=12), 
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5))



png("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Model_run_updates/barplot19_2cols_14vars_size.png", width = 1200, height = 1000)
barplot2
dev.off()

#cluster_assignment1 %>% filter(nsdoh_profiles== "profile5 ") %>% select(NAME) 


#Save Datasets
str(cluster_assignment1)
str(dat_19)
cluster_assignment1$census_tract<-str_sub(cluster_assignment1$GEOID,start=6, end = 11) 
cluster_assignment1$countycode<-str_sub(cluster_assignment1$GEOID,start=3, end = 5)



saveRDS(cluster_assignment1,"/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/Model_run_updates/mbmm_clustersassign_042025.rds")
saveRDS(cluster_assignment1 %>% select(GEOID, countycode, NAME, nsdoh_profiles),"/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/nsdoh_profiles_app_edited/nsdoh_data_042025.rds")
saveRDS(dat_19,"/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/nsdoh_profiles_app_edited/mbmm_results_042025.rds")






##Quick look over the  EJP data
EJP_BG<-st_read("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/nsdoh_profiles_app_edited/ej2020/EJ_POLY.shp") 
EJP_BG1<-EJP_BG %>% filter(EJ_CRITE_1 %in% c(2,3))
str(EJP_BG1)
EJP_BG1$census_tract<-str_sub(EJP_BG1$GEOID,start=6, end = 11) 
EJP_BG1$countycode<-str_sub(EJP_BG1$GEOID,start=3, end = 5)
EJP_BG1$bgcode<-str_sub(EJP_BG1$GEOID,start=12, end = 12)
EJP_BG2<-EJP_BG1 %>% select(GEOGRAPHIC,EJ_CRITERI,EJ_CRITE_1, census_tract, countycode, bgcode)

#merge EJ data
cluser_dat<- cluster_assignment1 %>% select(GEOID, NAME,nsdoh_profiles, assignment_ecr,census_tract, countycode)

merged_dat<-inner_join(cluser_dat, EJP_BG2, by=  c("census_tract", "countycode"))

table(cluster_assignment1$assignment_ecr)
table(merged_dat$nsdoh_profiles, merged_dat$EJ_CRITERI)
table(merged_dat$assignment_ecr, merged_dat$EJ_CRITERI)

