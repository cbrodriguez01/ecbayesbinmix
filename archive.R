
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







