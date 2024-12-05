###########################################################################################
###########################################################################################
###########################################################################################
##---CROSS-TABS
#We will use 2006-2010 survey as the reference, that is we will compare to this survey
table(acs10 = merged10$ClusterAssignment_final, acs15 = merged15$ClusterAssignment_final)

#Relabel clusters in acs15 based on qualitative descriptions
merged15$clustersnum<- as.numeric(merged15$ClusterAssignment_new)
merged15<-merged15 %>% mutate(ClusterAssignment = case_when(
  clustersnum == 1 ~ 7,
  clustersnum == 2 ~ 4,
  clustersnum == 3 ~ 2,
  clustersnum == 4 ~ 5,
  clustersnum == 5 ~ 6,
  clustersnum == 6 ~ 1,
  clustersnum == 7 ~ 8,
  clustersnum == 8 ~ 3,
))

table(merged15$clustersnum, merged15$ClusterAssignment)
merged15$ClusterAssignment<- 
  factor(merged15$ClusterAssignment, levels = 1:8, labels = paste0("cluster", 1:8, sep= " "))

table(acs10 = merged10$ClusterAssignment, acs15 = merged15$ClusterAssignment)


#Similarly for 2015-2019 we use 2011-2015 as refence
table(acs15 = merged15$ClusterAssignment, acs19 = merged19$ClusterAssignment_new)
merged19$clustersnum<- as.numeric(merged19$ClusterAssignment_new)
merged19<-merged19 %>% mutate(ClusterAssignment = case_when(
  clustersnum == 1 ~ 2,
  clustersnum == 2 ~ 3,
  clustersnum == 3 ~ 6,
  clustersnum == 4 ~ 5,
  clustersnum == 5 ~ 8,
  clustersnum == 6 ~ 1,
))

table(merged19$clustersnum, merged19$ClusterAssignment)

merged19$ClusterAssignment<- 
  factor(merged19$ClusterAssignment, levels = c(1,2,3,5,6,8), labels = paste0("cluster", c(1,2,3,5,6,8), sep= " "))

table(merged15$ClusterAssignment,merged19$ClusterAssignment)


##---MAPS
#https://geodacenter.github.io/workbook/3a_mapping/lab3a.html#mapping-categorical-variables
#https://spatialanalysis.github.io/handsonspatialdata/basic-mapping.html#introduction-4
#https://stackoverflow.com/questions/73990261/unable-to-plot-the-census-tracts-using-tmap
#https://ggplot2.tidyverse.org/reference/ggsf.html
#https://r-spatial.org/r/2017/03/19/invalid.html#empty-geometries



#Brew colors
colorswant<-RColorBrewer::brewer.pal(9, "Set1")

options(tigris_use_cache = TRUE)
temp<-get_acs(state = "MA", geography = "tract", 
              variables = "B03002_001", year = 2010, geometry = TRUE, cache_table = TRUE)

temp1<- temp %>% select(GEOID, NAME, geometry, estimate)
dat10map<-merged10 %>% select(GEOID, ClusterAssignment_final, maxProb) 

#join datasets
map10<- merge(temp1, dat10map, by = "GEOID")
map10<-map10 %>%  select(GEOID,NAME, ClusterAssignment_final, maxProb, geometry) 

# tm_shape(st_make_valid(map10)) + 
#   tm_polygons(col = "maxProb",style = "quantile",
#           n = 9, palette= "Set1",
#           title = "2006-2010 MA Census") + 
#   tm_layout(title = "",
#             frame = FALSE,
#             legend.outside = TRUE)

map10<- map10 %>% mutate(`Probabilistic Cluster Assingment` = as.numeric(ClusterAssignment_final))
map10$`Probabilistic Cluster Assingment`<-as.factor(map10$`Probabilistic Cluster Assingment`)

map10_clust<-tm_shape(st_make_valid(map10)) +
  tm_fill("Probabilistic Cluster Assingment",style="cat",palette=colorswant,)  +
  tm_borders() +
  tm_layout(title = "2006-2010 ACS Census Tracts", title.position = c("left","top"), scale = 1.2)

temp<-get_acs(state = "MA", geography = "tract", 
              variables = "DP05_0065", year = 2015, geometry = TRUE, cache_table = TRUE)

temp1<- temp %>% select(GEOID, NAME, geometry, estimate)
dat15map<-merged15 %>% select(GEOID,NAME, ClusterAssignment, probassign) 

#join datasets
map15<- merge(temp1, dat15map, by = "GEOID")
map15<-map15 %>%  select(GEOID, NAME.x, ClusterAssignment, probassign, geometry) %>% rename(NAME = NAME.x)

# tm_shape(st_make_valid(map15)) + 
#   tm_polygons(col = "probassign",style = "quantile",
#           n = 9, palette= "Set1",
#           title = "2011-2015 MA Census") + 
#   tm_layout(title = "",
#             frame = FALSE,
#             legend.outside = TRUE)

map15<- map15 %>% mutate(`Probabilistic Cluster Assingment` = as.numeric(ClusterAssignment))
map15$`Probabilistic Cluster Assingment`<-as.factor(map15$`Probabilistic Cluster Assingment`)


map15_clust<-tm_shape(st_make_valid(map15)) +
  tm_fill("Probabilistic Cluster Assingment",style="cat",palette=colorswant,)  +
  tm_borders() +
  tm_layout(title = "2011-2015 ACS Census Tracts", title.position = c("left","top"), scale = 1.2)

temp<-get_acs(state = "MA", geography = "tract", 
              variables = "B03002_001", year = 2019, geometry = TRUE, cache_table = TRUE)

temp1<- temp %>% select(GEOID, NAME, geometry, estimate)
dat19map<-merged19 %>% select(GEOID,NAME, ClusterAssignment, probassign) 

#join datasets
map19<- merge(temp1, dat19map, by = "GEOID")
map19<-map19 %>%  select(GEOID, NAME.x, ClusterAssignment, probassign, geometry) %>% rename(NAME = NAME.x)

# tm_shape(st_make_valid(map19)) + 
#   tm_polygons(col = "probassign",style = "quantile",
#           n = 7, palette= "Set1",
#           title = "2015-2019 MA Census") + 
#   tm_layout(title = "",
#             frame = FALSE,
#             legend.outside = TRUE)

map19<- map19 %>% mutate(`Probabilistic Cluster Assingment` = ClusterAssignment)
map19$`Probabilistic Cluster Assingment`<-as.factor(map19$`Probabilistic Cluster Assingment`)

#colors to match other maps
colmatch<-c("#E41A1C", "#377EB8","#4DAF4A","#FF7F00", "#FFFF33","#F781BF")
map19_clust<-tm_shape(st_make_valid(map19)) +
  tm_fill("Probabilistic Cluster Assingment",style="cat",palette=colmatch,)  +
  tm_borders() +
  tm_layout(title = "2015-2019 ACS Census Tracts", title.position = c("left","top"), scale = 1.2)


#3 datasets with cluster assingment, prob of assingment
map10_1<- as.data.frame(map10)
dat10<-map10_1 %>% select(GEOID, NAME, ClusterAssignment, maxProb)
dat10$cluster_color<-as.numeric(dat10$ClusterAssignment)
dat10$cluster_color<- factor(dat10$cluster_color,levels = 1:9, labels= c("red", "blue",
                                                                         "green","purple","orange","yellow", "brown","pink","grey"))

table(dat10$cluster_color)
table(dat10$ClusterAssignment)



dat15<-as.data.frame(map15) %>% select(GEOID, NAME, ClusterAssignment, probassign)
dat15$cluster_color<-as.numeric(dat15$ClusterAssignment)
dat15$cluster_color<- factor(dat15$cluster_color,levels = 1:8, labels= c("red", "blue",
                                                                         "green","purple","orange","yellow", "brown","pink"))

table(dat15$cluster_color)
table(dat15$ClusterAssignment)


dat19<-as.data.frame(map19) %>% select(GEOID, NAME, ClusterAssignment, probassign)
dat19$cluster_color<-as.numeric(dat19$ClusterAssignment)
dat19$cluster_color<- factor(dat19$cluster_color,levels = c(1,2,3,5,6,8), labels= c("red", "blue","green","orange","yellow", "pink"))

table(dat19$cluster_color)
table(dat19$ClusterAssignment)



########### EXPORT ##############
#save data
save(dat10, dat15, dat19, file = "/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/bayesbinmix_clustered_tracts.RData")


#Export 2010 only
dat10_export<- merged10 %>% select("GEOID","ClusterAssignment_final")
saveRDS(dat10_export, file = "dat10_temp.rds")




nChains <- 4
heats <- seq(1, 0.4, length = nChains)

X10<- as.matrix(censusdata_bin$acs5_2010_bin)

acs5_2010_bin<- as.matrix(censusdata_bin$acs5_2010_bin)

res<-coupledMetropolis(Kmax=10, nChains = nChains, 
                       heats = heats,
                       binaryData = X10, 
                       outPrefix = "acs10_res_poiunif_demo"
                       m = 500)



## (3) Extract mean estimates
stats<-summary(res$parameters.ecr.mcmc)#reordered according to ECR algorithm
res_quant<-stats$quantiles[,c(1,5)]
res_summarized<-cbind(stats$statistics[,1:3],res_quant)

out<- as.data.frame(res_summarized[1:126,1]) #need to make this more reproducible
ids<-rownames(out)
sesvars<-c("Household:Two People Per Room","Household: Lack of complete Plumbing", "Household:Median Income", ">= Bacherlor's Degree ", "Below Poverty Line", "< High School",
           "Female Household", ">= High School","Household:No Vehicle","Household: Owner", "Household:Renter", "Unemployment","White Collar Occupation", "SNAP Benefits")


get_probs<-function(mapK, out){
  val<-c()
  indices <- seq(1, nrow(out) * 1, by = mapK)
  for (k in indices){
    v<- out[k:((k+mapK)-1),]
    val<-cbind(val, v)
  }
  probs<-val
  return(probs)
}
prob_est<-get_probs(9, out)
prob_est<-cbind(1:mapK,prob_est)
colnames(prob_est)<-c("cluster",sesvars)

#Clsuter probabilities
Clusterprobs<- as.data.frame(res_summarized[127:135,1])
colnames(Clusterprobs)<-"pi_mapK"
Clusterprobs


library(wesanderson)
library(viridis)
#create plot
prob_est1<-as.data.frame(prob_est)
prob_est1$cluster
prob_est_long<- prob_est1 %>% pivot_longer(!cluster, names_to = "NSES_VARS",values_to = "theta_kj")

viridis_palette <- viridis(14)
custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                   "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                   "#ff0000", "#00ff00", "#0000ff", "#ffff00")

generate_pastel_colors <- function(n) {
  hues <- seq(0, 1, length.out = n + 1)[-1]  # Equally spaced hues
  s <- 0.4  # Saturation for pastel colors
  v <- 0.9  # Brightness for pastel colors
  hsv(h = hues, s = s, v = v)
}

# Generate 14 pastel colors
pastel_palette <- generate_pastel_colors(14)


p1<-prob_est_long %>% ggplot(aes(x = NSES_VARS, y = theta_kj)) +
  geom_col(aes(fill = NSES_VARS)) +
  facet_wrap(~cluster) + scale_fill_manual(values = viridis_palette)+ labs(x= "Neighborhood SDOH variables",  
                                                                           y = "Probability",
                                                                           fill = "") +
  theme(text = element_text(size = 10),
        axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title.y = element_text(size = 8, color = "black", face = "bold"),
        legend.title = element_text(size = 8, color = "black", face = "bold"),
        legend.text = element_text(size = 6, color = "black"),
        legend.position = "bottom")




p2<-prob_est_long %>% ggplot(aes(x = NSES_VARS, y = theta_kj)) +
  geom_col(aes(fill = NSES_VARS)) +
  facet_wrap(~cluster) + scale_fill_manual(values = pastel_palette)+ labs(x= "Neighborhood SDOH variables",  
                                                                          y = "Probability",
                                                                          fill = "") +
  theme(text = element_text(size = 10),
        axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title.y = element_text(size = 8, color = "black", face = "bold"),
        legend.title = element_text(size = 8, color = "black", face = "bold"),
        legend.text = element_text(size = 6, color = "black"),
        legend.position = "bottom")



p1
p2

ggsave("/homes6/carmen/Other projects/ecbayesbinmix/Figures/fig_temp.pdf", plot = p2, width = 8, height = 8)

fig_title = paste("Success Probabilities of Neighborhood SES variables when mapK=", mapK, sep="")

# Other color options
viridis_palette <- viridis(14)
custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                   "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                   "#ff0000", "#00ff00", "#0000ff", "#ffff00")

# Generate 14 pastel colors
pastel_palette <- generate_pastel_colors(14)

plot_thetakj(reslst = res, mapK = 9, sesvars= sesvars, color_palette = pastel_palette)


mapK <- Mode(res_5$K.mcmc)[1]
n_ses<-length(sesvars)
dim<-n_ses*mapK
stats<-summary(res_5$parameters.ecr.mcmc)$statistics[,1] #get the mean, can also get SE and quantiles 

temp<-as.data.frame(stats[1:dim])

probs<-c()
indices <- seq(1, nrow(temp) * 1, by = mapK)
for (k in indices){
  v<- temp[k:((k+mapK)-1),]
  probs<-cbind(probs, v)
}

prob_est<-cbind(1:mapK,probs)
colnames(prob_est)<-c("cluster",sesvars)

#Long format for ggplot
prob_est_long<- prob_est %>% as.data.frame() %>% 
  pivot_longer(!cluster, names_to = "NSES_VARS",values_to = "theta_kj")

# Group variables
prob_est_long<-prob_est_long %>% mutate(NSES_group = case_when(
  NSES_VARS %in% c("Household:Two People Per Room","Household: Lack of complete Plumbing", "Household:No Vehicle","Household: Owner", "Household:Renter", "Female Household") ~ "Household",
  NSES_VARS %in% c(">= Bacherlor's Degree", "< High School", ">= High School") ~ "Education",
  NSES_VARS %in% c("Unemployment","White Collar Occupation") ~ "Occupation",
  NSES_VARS %in% c("Median Household Income","Below Poverty Line","SNAP Benefits") ~ "Income",))

color_palette <-generate_pastel_colors(4)

# Figure title
fig_title = paste("Success Probabilities when mapK=", mapK, sep="")

# Generate plot
prob_est_long %>% ggplot(aes(x = NSES_VARS, y = theta_kj)) +
  geom_col(aes(fill = as.factor(NSES_group))) +
  facet_wrap(~cluster, nrow = mapK) + scale_fill_manual(values = color_palette) + 
  labs(title = fig_title, x= "",   y = "Probability",fill = "Neighborhood SES variables") +
  theme(text = element_text(size = 12),
        axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title.y = element_text(size = 8, color = "black", face = "bold"),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 8, color = "black", face = "bold"),
        legend.text = element_text(size = 8, color = "black"),
        legend.position = "right")




#to describe patterns
#dat_10 %>% group_by(cluster) %>% arrange(desc(theta_kj),.by_group = T)

# dat_10_1<-dat_10 %>%  mutate(hml= case_when(
#                              theta_kj >=0.70 ~ 1,
#                              theta_kj< 0.70 & theta_kj >=0.4 ~ 2,
#                              theta_kj < 0.4 ~ 3,))
# 
# dat_10_1$hml <- factor(dat_10_1$hml, levels = 1:3, labels = c("High", "Med", "Low"))
# 
# 
# out<-dat_10_1 %>%  group_by(cluster) %>% arrange(desc(theta_kj),.by_group = T) %>% select( cluster, NSES_VARS, theta_kj,hml)

# options(tigris_use_cache = TRUE)
temp<-get_acs(state = "MA", geography = "tract", 
              variables = "B03002_001", year = 2015, geometry = TRUE)

temp1<- temp %>% select(GEOID, NAME, geometry, estimate)
dat15map<-merged15 %>% select(GEOID,NAME, ClusterAssignment_new, probassign)

#join datasets
map15<- left_join(temp1, dat15map, by = c("GEOID", "NAME"))
#map15 %>% ggplot(aes(fill = probassign, group= ClusterAssignment_new)) + 
#  geom_sf(color = NA) + 
#  coord_sf(crs = 6491, datum = NA) +
#  scale_fill_viridis_c(option = "C",direction = 1) +
#  labs(title = "Hispanic or Latino: South American groups by Census Tracts in MA",
#       caption = "Data source: 2019 5-year ACS",
#       fill = "Percent") + theme_minimal()

map15<- map15 %>% mutate(clusters = as.numeric(ClusterAssignment_new))
#map15$clusters<-as.factor(map15$clusters)

plot(map15["ClusterAssignment_new"])
plot(map15["clusters"])


+
  #  scale_fill_viridis_c(option = "C",direction = 1) +
  #  labs(title = "Hispanic or Latino: South American groups by Census Tracts in MA",
  #       caption = "Data source: 2019 5-year ACS",
  #       fill = "Percent") + theme_minimal()



#Bar plot to look at distribution of the binary variables -- using acs10bin dataset
  binvars<- names(acs10bin)[1:18]

props<-matrix(NA, nrow = length(binvars), ncol = mapK + 1)
colnames(props)<-c("var", paste0("cluster", 1:mapK))
for (i in 1:length(binvars)) {
  binvar<-binvars[i]
  # Create a contingency table
  tab<- table(acs10bin[["ClusterAssignment"]], acs10bin[[binvar]], useNA = "always")
  
  # Calculate proportions
  p <- prop.table(tab, margin = 1)[,2]  # proportions for == 1
  
  props[i,1]<-binvar
  props[i,2:(mapK+1)]<-p[1:mapK]
}

props2<-as.data.frame(t(props))
colnames(props2)<-sesvars
props2<-props2[-1,]
props2$cluster<- row.names(props2)

props2_long<-props2 %>% pivot_longer(!cluster, names_to = "NSES_VARS", values_to = "prop")

# Group variables
prop2_long1<-props2_long %>% mutate(NSES_group = case_when(
  NSES_VARS %in% c("Crowded housing","Lack complete plumbing", "No vehicle","Owner-occupied","Renter-occupied", "Female household") ~ 1,
  NSES_VARS %in% c("< HS", " >= HS", " >= Bacherlors") ~ 2,
  NSES_VARS %in% c("Unemployment","Working class") ~ 4,
  NSES_VARS %in% c("Median income","Below 150% poverty","SNAP benefits") ~ 3,
  NSES_VARS %in% c("Hispanic or Latino", "NH Black", "NH Asian", "Limited EN Proficiency") ~ 5,))


prop2_long1$NSES_group<- factor(prop2_long1$NSES_group,levels = 1:5, labels = c("Household", "Education", "Income", "Occupation", "Ethnic Minorities and Language"))


prop2_long1$NSES_VARS<- factor(prop2_long1$NSES_VARS, levels = unique(prop2_long1$NSES_VARS[order(prop2_long1$NSES_group)]))
prop2_long1$prop<-round(as.numeric(prop2_long1$prop),3)

plot_group2<-prop2_long1 %>% ggplot(aes(x = NSES_VARS, y = prop, fill = NSES_group)) +
  geom_col()  +
  facet_wrap(~cluster, nrow = 5, ncol = 2) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = "", x= "",
       y = "Probability",
       fill = "Neighborhood SES Variables") +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(size=9, angle=90, vjust = 0.75, hjust = 0.88), 
        axis.title.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title.y = element_text(size = 8, color = "black", face = "bold"),
        #axis.ticks = element_blank(),
        legend.title = element_text(size = 8, color = "black", face = "bold"),
        legend.text = element_text(size = 8, color = "black"),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5))





#checks<-readRDS("./tuningheatsvec_4.25.24.rds")
#checks$sar #it seems that for these datasets a smaller deltaT works better

#colnames(checks$sar)<-c("2010", "2015", "2019")

# Check if mixing well-- for heats vector 1

# #Plots like Figure 4 in the paper
# 
# #(1) Illustrate the sampled values of K per chain according to the Poisson and uniform prior distribution using data from K.allChains
# Kallchains<-read.table(file = "/Users/carmenrodriguez/Desktop/temp/acs10_2024-04-11_deltatemp1/K.allChains.txt", header = T)
# 
# 
# Kallchains$m <-as.numeric(row.names(Kallchains))
# Kallchains1<-Kallchains %>% pivot_longer(cols = 1:4, names_to ="Chain", values_to = "K")
# 
# #Need more iterations-- this plot shows the generated values of K per heated chain-- note chain 1 is the original (^1)
# Kallchains1 %>% ggplot(aes(x = m, y = K, col = as.factor(Chain))) + geom_line()
# 
# 
# 
# 
# #(2) Raw output of p1,...,pK  and theta_kj conditional on K=2 (NOT REORDERED!)-- rawMCMC.mapK.3.txt
# datraw<-read.table(file = "/Users/carmenrodriguez/Desktop/temp/archive/acs10_2024-04-11_deltatemp1/rawMCMC.mapK.2.txt", header = T)
# 
# datraw<-datraw[,c("p.1","p.2")]
# colnames(datraw)<-c("Component 1", "Component 2")
# datraw$iteration<-as.numeric(row.names(datraw))
# datraw<-datraw %>% pivot_longer(cols = 1:2, names_to = "Component", values_to = "raw.mix.prop")
# datraw %>% ggplot(aes(x=iteration, y = raw.mix.prop, col = as.factor(Component))) + geom_point(size = 4)
# 
# 
# # #(3) Reordered by ECR algorithm
# reordered<-read.table(file = "/Users/carmenrodriguez/Desktop/temp/archive/acs10_2024-04-11_deltatemp1/reorderedMCMC-ECR.mapK.2.txt", header = T)
# # 
# reorderedp<-reordered[,c("p.1","p.2")]
# colnames(reorderedp)<-c("Component 1", "Component 2")
# reorderedp$iteration<-as.numeric(row.names(reorderedp))
# reorderedp<-reorderedp %>% pivot_longer(cols = 1:2, names_to = "Component", values_to = "reordered.mix.prop")
# reorderedp %>% ggplot(aes(x=iteration, y = reordered.mix.prop, col = as.factor(Component))) + geom_point(size =4 )
# 
# # 
# 

# wd<-"/Users/carmenrodriguez/Desktop/temp/"
# checking<-run_models(dataset = datasets[[acsid[1]]], Kmax = Kmax,gamma=gamma, 
#            nChains= nChains, m= 500, ClusterPrior, wd = wd, 
#            acsid = acsid[1], burnin= 100, heats = heatslist[[heatsid[1]]], heatsid = heatsid[1])


