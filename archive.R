
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
