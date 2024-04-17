#Look at the traceplots, mixing weights and classificaiton probailities for both models
# Mod1: 18 variables including proportions of minority race/ethnicities
# Mod2: 16 varibales including a proportion of ethnic minorities-- this include more race/ethnic variables than just Hispanic, NHB and Asian.


library(BayesBinMix)
library(tidyverse)
library(table1)

#Find index of highest probability
highest_index<-function(vec){
  s<-length(vec)
  vec1<-order(vec)
  return(vec1[s])
}

#Find second highest probability
second_highest<-function(vec){
  vec1<-sort(vec, decreasing = T)
  return(vec1[2])
}
second_highest_index<-function(vec){
  s<-length(vec)
  vec1<-order(vec)
  return(vec1[s-1])
}


out_ht<-readRDS("./tuningheatsvec_4.14.24.rds")

# Firstly, the model with ethnic minority resulted in 8 clusters for each dataset, while the model with prop race-eth resulted in 9,8,8 cluster
res_acs10_ht1<-res_all[[1]][["res_acs10_deltatemp1"]]
res_acs15_ht1<-res_all[[1]][["res_acs15_deltatemp1"]]
res_acs19_ht1<-res_all[[1]][["res_acs19_deltatemp1"]]

res_acs10_ht2<-res_all[[1]][["res_acs10_deltatemp2"]]
res_acs15_ht2<-res_all[[1]][["res_acs15_deltatemp2"]]
res_acs19_ht2<-res_all[[1]][["res_acs19_deltatemp2"]]

#####-------- ACS 2006-2010----------------------#######
#Mixing weights distributions
#The credible interval range for cluster 8 is ^-63 that's why it shows up as zero
round(tail(cbind(summary(acs10_1$parameters.ecr.mcmc)$statistics[,c(1,2)], 
             summary(acs10_1$parameters.ecr.mcmc)$quantiles[,c(1,5)]),8), 4)
#----- 45 census tracts assigned to this cluster


##already flagging cluster 2, only has 2 census tracts--- mixing weights ^-62
round(tail(cbind(summary(acs10_2$parameters.ecr.mcmc)$statistics[,c(1,2)], 
      summary(acs10_2$parameters.ecr.mcmc)$quantiles[,c(1,5)]), 9),4)

#Trace plots and histogram of theta_kj, p_k for 2nd component of this model to explore, and the minority eth variable
summary(res_acs15_ht2$parameters.ecr.mcmc)
mat1 <- matrix(c(1:4), byrow = TRUE, ncol = 2)
layout(mat1, widths = rep(c(2, 1), 2), heights = rep(1, 4))
mcmcSubset <- res_acs15_ht2$parameters.ecr.mcmc[ , c("p.6", "p.1")]
plot(mcmcSubset, auto.layout = FALSE, ask = FALSE, col = "gray40")


#summary(acs10_1$parameters.ecr.mcmc)
mat1 <- matrix(c(1:10), byrow = TRUE, ncol = 2)
layout(mat1, widths = rep(c(2, 1), 2), heights = rep(1, 4))
mcmcSubset <-  acs10_1$parameters.ecr.mcmc[ , c("theta.2.2", "theta.2.3","theta.2.4", "theta.2.5", "p.8")]
plot(mcmcSubset, auto.layout = FALSE, ask = FALSE, col = "gray40")

#summary(acs10_2$parameters.ecr.mcmc)
mat1 <- matrix(c(1:10), byrow = TRUE, ncol = 2)
layout(mat1, widths = rep(c(2, 1), 2), heights = rep(1, 4))
mcmcSubset <-  acs10_2$parameters.ecr.mcmc[ , c("theta.2.2", "theta.2.3","theta.2.4", "theta.2.5", "p.2")]
plot(mcmcSubset, auto.layout = FALSE, ask = FALSE, col = "gray40")

mat1 <- matrix(c(1:8), byrow = TRUE, ncol = 2)
layout(mat1, widths = rep(c(2, 1), 2), heights = rep(1, 4))
mcmcSubset <-  acs10_2$parameters.ecr.mcmc[ , c("p.1", "p.3", "p.4", "p.5")]
plot(mcmcSubset, auto.layout = FALSE, ask = FALSE, col = "gray40")


#Assignment probabilities--->  With minority status
probs10_1<-acs10_1$classificationProbabilities.ecr
# Add cluster classification based on maximum probability 
maxProb<-apply(probs10_1, 1, max)
ClusterAssignment<-acs10_1$clusterMembershipPerMethod[,2]
ClusterAssignment_ecr_temp<-apply(probs10_1,1, highest_index)


probs10_1$ClusterAssignment<-ClusterAssignment
probs10_1$maxProb<-maxProb
probs10_1$ClusterAssignment_ecr_temp<-ClusterAssignment_ecr_temp


#I think it's a rounding problem - the differences are not higher than 0.01
chk<-probs10_1 %>% filter(ClusterAssignment_ecr_temp != ClusterAssignment)

#Assignment probabilities

probs10_1%>%  ggplot(aes(x = as.factor(ClusterAssignment), y = maxProb, fill = as.factor(ClusterAssignment))) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment", 
       y = "Assignment Probability after ECR algorithm", 
       x= "")

table1::table1(~ maxProb |as.factor(ClusterAssignment), data= probs10_1, overall = F)


#Assignment probabilities--->  race/ethnicity
probs10_2<-acs10_2$classificationProbabilities.ecr
# Add cluster classification based on maximum probability 
maxProb<-apply(probs10_2, 1, max)
ClusterAssignment<-acs10_2$clusterMembershipPerMethod[,2]
ClusterAssignment_ecr_temp<-apply(probs10_2,1, highest_index)


probs10_2$ClusterAssignment<-ClusterAssignment
probs10_2$maxProb<-maxProb
probs10_2$ClusterAssignment_ecr_temp<-ClusterAssignment_ecr_temp


#I think it's a rounding problem - the differences are not higher than 0.01
chk<-probs10_2 %>% filter(ClusterAssignment_ecr_temp != ClusterAssignment)

#Assignment probabilities
probs10_2%>%  ggplot(aes(x = as.factor(ClusterAssignment), y = maxProb, fill = as.factor(ClusterAssignment))) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment", 
       y = "Assignment Probability after ECR algorithm", 
       x= "")

table1::table1(~ maxProb |as.factor(ClusterAssignment), data= probs10_2, overall = F)


## I think we should use the model with the proportion of minority race/eth (Hispanic, Black and Asian), we can say 


#####-------- ACS 2011-2015----------------------#######

#Mixing weights distributions
round(tail(cbind(summary(acs15_1$parameters.ecr.mcmc)$statistics[,c(1,2)], 
                 summary(acs15_1$parameters.ecr.mcmc)$quantiles[,c(1,5)]),8), 4)



##already flagging cluster 4, only has 2 census tracts--- mixing weights ^-47-- only 7 census tracts per ECR algorithm
round(tail(cbind(summary(acs15_2$parameters.ecr.mcmc)$statistics[,c(1,2)], 
                 summary(acs15_2$parameters.ecr.mcmc)$quantiles[,c(1,5)]), 8),4)



#Trace plots and histogram of theta_kj, p_k for 2nd component of this model to explore, and the minority eth variable
#summary(acs10_2$parameters.ecr.mcmc)
mat1 <- matrix(c(1:4), byrow = TRUE, ncol = 2)
layout(mat1, widths = rep(c(2, 1), 2), heights = rep(1, 4))
mcmcSubset <- acs15_2$parameters.ecr.mcmc[ , c("p.1", "p.4")]
plot(mcmcSubset, auto.layout = FALSE, ask = FALSE, col = "gray40")


summary(acs15_2$parameters.ecr.mcmc)
mat1 <- matrix(c(1:10), byrow = TRUE, ncol = 2)
layout(mat1, widths = rep(c(2, 1), 2), heights = rep(1, 4))
mcmcSubset <-  acs15_2$parameters.ecr.mcmc[ , c("theta.4.13", "theta.4.14","theta.4.15", "theta.4.16", "p.4")]
plot(mcmcSubset, auto.layout = FALSE, ask = FALSE, col = "gray40")


#Assignment probabilities--->  With minority status
probs15_1<-acs15_1$classificationProbabilities.ecr
# Add cluster classification based on maximum probability 
maxProb<-apply(probs15_1, 1, max)
ClusterAssignment<-acs15_1$clusterMembershipPerMethod[,2]
ClusterAssignment_ecr_temp<-apply(probs15_1,1, highest_index)


probs15_1$ClusterAssignment<-ClusterAssignment
probs15_1$maxProb<-maxProb
probs15_1$ClusterAssignment_ecr_temp<-ClusterAssignment_ecr_temp


#I think it's a rounding problem - the differences are not higher than 0.01--only 5
chk<-probs15_1 %>% filter(ClusterAssignment_ecr_temp != ClusterAssignment)

#Assignment probabilities

probs15_1%>%  ggplot(aes(x = as.factor(ClusterAssignment), y = maxProb, fill = as.factor(ClusterAssignment))) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment", 
       y = "Assignment Probability after ECR algorithm", 
       x= "")

table1::table1(~ maxProb |as.factor(ClusterAssignment), data= probs15_1, overall = F)


#Assignment probabilities--->  race/ethnicity
probs15_2<-acs15_2$classificationProbabilities.ecr
# Add cluster classification based on maximum probability 
maxProb<-apply(probs15_2, 1, max)
ClusterAssignment<-acs15_2$clusterMembershipPerMethod[,2]
ClusterAssignment_ecr_temp<-apply(probs15_2,1, highest_index)


probs15_2$ClusterAssignment<-ClusterAssignment
probs15_2$maxProb<-maxProb
probs15_2$ClusterAssignment_ecr_temp<-ClusterAssignment_ecr_temp


#I think it's a rounding problem - the differences are not higher than 0.01--8 obs
chk<-probs15_2 %>% filter(ClusterAssignment_ecr_temp != ClusterAssignment)

#Assignment probabilities
probs15_2%>%  ggplot(aes(x = as.factor(ClusterAssignment), y = maxProb, fill = as.factor(ClusterAssignment))) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment", 
       y = "Assignment Probability after ECR algorithm", 
       x= "")

table1::table1(~ maxProb |as.factor(ClusterAssignment), data= probs15_2, overall = F)



#----- ACS 2015-2019----------
#Mixing weights distributions
#cluster 5 (^-63 lb of credible interval)
round(tail(cbind(summary(acs19_1$parameters.ecr.mcmc)$statistics[,c(1,2)], 
                 summary(acs19_1$parameters.ecr.mcmc)$quantiles[,c(1,5)]),8), 4)



##already flagging cluster 1, only has 2 census tracts--- mixing weights ^-77-- no obs were classified as 1 by any of the 3 algorithms
round(tail(cbind(summary(acs19_2$parameters.ecr.mcmc)$statistics[,c(1,2)], 
                 summary(acs19_2$parameters.ecr.mcmc)$quantiles[,c(1,5)]), 8),4)



#Trace plots and histogram of theta_kj, p_k for 2nd component of this model to explore, and the minority eth variable
#summary(acs10_2$parameters.ecr.mcmc)
mat1 <- matrix(c(1:4), byrow = TRUE, ncol = 2)
layout(mat1, widths = rep(c(2, 1), 2), heights = rep(1, 4))
mcmcSubset <- acs19_2$parameters.ecr.mcmc[ , c("p.1", "p.7")]
plot(mcmcSubset, auto.layout = FALSE, ask = FALSE, col = "gray40")


#We are going to grey out this cluster
summary(acs19_2$parameters.ecr.mcmc)
mat1 <- matrix(c(1:10), byrow = TRUE, ncol = 2)
layout(mat1, widths = rep(c(2, 1), 2), heights = rep(1, 4))
mcmcSubset <-  acs19_2$parameters.ecr.mcmc[ , c("theta.1.13", "theta.1.14","theta.1.15", "theta.1.16", "p.1")]
plot(mcmcSubset, auto.layout = FALSE, ask = FALSE, col = "gray40")
mat1 <- matrix(c(1:10), byrow = TRUE, ncol = 2)
layout(mat1, widths = rep(c(2, 1), 2), heights = rep(1, 4))
mcmcSubset <-  acs19_2$parameters.ecr.mcmc[ , c("theta.1.1", "theta.1.2","theta.1.3", "theta.1.4", "theta.1.5")]
plot(mcmcSubset, auto.layout = FALSE, ask = FALSE, col = "gray40")




#Assignment probabilities--->  With minority status
probs19_1<-acs19_1$classificationProbabilities.ecr
# Add cluster classification based on maximum probability 
maxProb<-apply(probs19_1, 1, max)
ClusterAssignment<-acs19_1$clusterMembershipPerMethod[,2]
ClusterAssignment_ecr_temp<-apply(probs19_1,1, highest_index)


probs19_1$ClusterAssignment<-ClusterAssignment
probs19_1$maxProb<-maxProb
probs19_1$ClusterAssignment_ecr_temp<-ClusterAssignment_ecr_temp

#I think it's a rounding problem - the differences are not higher than 0.01--only 8
chk<-probs19_1 %>% filter(ClusterAssignment_ecr_temp != ClusterAssignment)

#Assignment probabilities

probs19_1%>%  ggplot(aes(x = as.factor(ClusterAssignment), y = maxProb, fill = as.factor(ClusterAssignment))) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment", 
       y = "Assignment Probability after ECR algorithm", 
       x= "")

table1::table1(~ maxProb |as.factor(ClusterAssignment), data= probs19_1, overall = F)
#5 


#Assignment probabilities--->  race/ethnicity
probs19_2<-acs19_2$classificationProbabilities.ecr
# Add cluster classification based on maximum probability 
maxProb<-apply(probs19_2, 1, max)
ClusterAssignment<-acs19_2$clusterMembershipPerMethod[,2]
ClusterAssignment_ecr_temp<-apply(probs19_2,1, highest_index)


probs19_2$ClusterAssignment<-ClusterAssignment
probs19_2$maxProb<-maxProb
probs19_2$ClusterAssignment_ecr_temp<-ClusterAssignment_ecr_temp


#I think it's a rounding problem - the differences are not higher than 0.01--7 obs
chk<-probs19_2 %>% filter(ClusterAssignment_ecr_temp != ClusterAssignment)
#no obs classified to cluster 1 so why output an empty cluster!!



#Assignment probabilities
probs19_2%>%  ggplot(aes(x = as.factor(ClusterAssignment), y = maxProb, fill = as.factor(ClusterAssignment))) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment", 
       y = "Assignment Probability after ECR algorithm", 
       x= "")

table1::table1(~ maxProb |as.factor(ClusterAssignment), data= probs19_2, overall = F)



### SUMMARY
# ACS 10
##1
#The credible interval range for cluster 8 is ^-63 that's why it shows up as zero 
#The average assignment probability for cluster 8 was 0.52 (45 cts)
#Looked at the traceplots this cluster-- not stable
##2 
#CLuster 2 only has 2 observations and the avg classificaiton prob is 0.357
#Looked at the traceplots and all other weights seemed to be mixing well
# ACS 15
##1
#All have decent n's

##2
#CLuster 4 has 7 obs and avg ap is 0.368

#ACS 19
##1
#cluster 5

##2
#cluster 1 is empty



# BASED ON THIS EXPLORATION, I AM GOING TO USE THE MODEL WITH THE RACE/ETHNICITY INDIVIDUAL VARIABLES --  WE WILL "GRAY" OUT CLUSTERs THAT ARE NOT VERY POPULATED.
#KEEP CLUSTERS WITH AN AVERAGE CLASSIFICAITON PROBABILITY OF AT LEAST 60%










