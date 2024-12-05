#CARMEN
##Created 6/10/24
###1. Running model without race/ethnicity variables (d= 15)
# Load libraries
library(BayesBinMix)
library(foreach)
library(label.switching)
library(doParallel)
library(coda)

run_models<-function(dataset,Kmax,gamma, nChains, m, ClusterPrior, wd, acsid, burnin, heats, heatsid){
  out.path<-paste0(wd, acsid, sep= "_",Sys.Date() ,sep = "_",heatsid,sep = "_", ClusterPrior)
  
  # This produces a list and a folder in out.path
  res<- coupledMetropolis(Kmax=Kmax, nChains = nChains, 
                          heats = heats,
                          binaryData = dataset, 
                          outPrefix = out.path,
                          m = m,
                          gamma = gamma, burn = burnin)
  
  out.list<-list(res)
  names(out.list)<-c(paste0("res_", acsid,sep = "_",heatsid,sep = "_", ClusterPrior))
  return(out.list)
}



#Tuning heats for achieving reasonable acceptance rates
censusdata_bin <- readRDS("/n/home03/crodriguezcabrera/ecbayesbinmix/Data/censusdata_bin_noraceeth_061024.rds")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
datasets_noraceeth<- list(acs10=as.matrix(censusdata_bin$acs5_2010_bin),acs15=as.matrix(censusdata_bin$acs5_2015_bin), acs19=as.matrix(censusdata_bin$acs5_2019_bin))

##Function to generate heating vectors
generateHeats<-function(deltatempvec, npchains){
  heatslist<-list()
  for (j in 1:length(deltatempvec)){
    heats<-c()
    for (i in 1:npchains){
      heats[i]<- 1/(1 +  (i-1) * deltatempvec[j])
    }
    #print(heats)
    #Save heats vector in list
    heatslist[[j]]<-heats
  }
  return(heatslist)
  
}

#nChains<- 4

#heatsid is the temperature ID to see which heated chains to use

#It seems deltaT OF 0.025 work better for these datasets BUT i'll keep all
# temps1<-c(0.005,0.01,0.025)
# heatslist<-generateHeats(temps1, nChains)
# 
# #Same set up
# Kmax<-50
# gamma<-rep((1/Kmax),Kmax)
# m<-1500
# burnin<-500
# wd<-"/n/home03/crodriguezcabrera/BayesBinMix_Project/"
# acsid<- c("acs10","acs15", "acs19")
# heatsid<-paste0("deltatemp", 1:length(heatslist))
# names(heatslist) <-heatsid
# 
# res_all<-list()
# swap_rate<-matrix(1, nrow = 3 , ncol= 3)
# row.names(swap_rate)<-heatsid
# colnames(swap_rate)<-acsid
# for (h in 1:length(heatslist)){
#   for (i in 1:length(datasets)){
#     model.res<-run_models(dataset = datasets[[acsid[i]]], Kmax = Kmax,gamma=gamma, 
#                           nChains= nChains, m= m, ClusterPrior = "poisson", wd = wd, 
#                           acsid = acsid[i], burnin= burnin, heats = heatslist[[heatsid[h]]], heatsid = heatsid[h])
#     #Keep swap acceptance rate
#     swap_rate[h, i]<-model.res[[1]]$chainInfo[2]
#     
#     res_all<-append(res_all, model.res)
#   }
# }
# 
# output<- list(res_all,  "sar2"=swap_rate)
# 
# 
# 
# #SAVE OUTPUT
# saveRDS(output, "/n/home03/crodriguezcabrera/ecbayesbinmix/binmod_noraceeth_6.10.24.rds")
# 


## 2. Explore the effect of prior settings-- only ACS 2015-2019 with race/ethnicity
censusdata_bin2 <- readRDS("/n/home03/crodriguezcabrera/ecbayesbinmix/Data/censusdata_bin_raceeth_042524.rds")
names(censusdata_bin2) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
#acs19_raceth<- as.matrix(censusdata_bin2$acs5_2019_bin)

#Model set up-- we let pi~Dirichlet(1,…,1) instead of inducing sparsity
# We are already imposing a prior on the number of components
##ClusterPrior
#--> truncated poisson at Kmax = 50
#--> Uniform (1,Kmax)
nChains<- 4
Kmax<-50
m<-1500
burnin<-500
wd<-"/n/home03/crodriguezcabrera/BayesBinMix_Project/"
gamma1<-rep(1, Kmax)
temps2<-c(0.025, 0.05,0.1)
heatslist1<-generateHeats(temps2, nChains)
heatsid<-paste0("deltatemp", 1:length(heatslist1))
names(heatslist1) <-heatsid
#Kprior<-c("poisson", "uniform")


#Run and save output with race/ethnicity
# res_acs19_raceeth<-list()
# swap_rate1<-matrix(1, nrow = 3 , ncol= 2)
# row.names(swap_rate1)<-heatsid
# colnames(swap_rate1)<-c("acs19_Kpoi", "acs19_Kunif")
# for (h in 1:length(heatslist1)){
#   #change prior settings
#   for (i in 1:length(Kprior)){
#     model.res<-run_models(dataset = acs19_raceth, Kmax = Kmax,gamma=gamma1, 
#                           nChains= nChains, m= m, ClusterPrior= Kprior[i], wd = wd, 
#                           acsid = "acs19", burnin= burnin, heats = heatslist1[[heatsid[h]]], heatsid = heatsid[h])
#     #Keep swap acceptance rate
#     swap_rate1[h, i]<-model.res[[1]]$chainInfo[2]
#     #Name models to keep track
#     #name_mod<- paste0("deltaT",h,sep="_", "K", Kprior[i])
#     
#     res_acs19_raceeth<-append(res_acs19_raceeth, model.res)
#   }
# }
# 
# output_raceeth<- list(res_acs19_raceeth,  "sar_priors"=swap_rate1)

#Run and save output without race/ethnicity
# acs19_noraceeth<- as.matrix(censusdata_bin$acs5_2019_bin)
# res_acs19_noraceeth<-list()
# swap_rate2<-matrix(1, nrow = 3 , ncol= 2)
# row.names(swap_rate2)<-heatsid
# colnames(swap_rate2)<-c("acs19_Kpoi", "acs19_Kunif")
# for (h in 1:length(heatslist1)){
#   #change prior settings
#   for (i in 1:length(Kprior)){
#     model.res<-run_models(dataset = acs19_noraceeth, Kmax = Kmax,gamma=gamma1, 
#                           nChains= nChains, m= m, ClusterPrior= Kprior[i], wd = wd, 
#                           acsid = "acs19", burnin= burnin, heats = heatslist1[[heatsid[h]]], heatsid = heatsid[h])
#     #Keep swap acceptance rate
#     swap_rate2[h, i]<-model.res[[1]]$chainInfo[2]
#     res_acs19_noraceeth<-append(res_acs19_noraceeth, model.res)
#   }
# }
# 
# output_noraceeth<- list(res_acs19_noraceeth,  "sar_priors"=swap_rate2)
# 


#SAVE OUTPUT
#saveRDS(output_noraceeth, "/n/home03/crodriguezcabrera/ecbayesbinmix/acs19_noraceeth_priors.rds")
#saveRDS(output_raceeth, "/n/home03/crodriguezcabrera/ecbayesbinmix/acs19_raceeth_priors.rds")


## 2.1. Explore the effect of prior settings-- for the other ACS 2006-2010 and 2011-2015 with race/ethnicity
datasets_raceeth<- list(acs10=as.matrix(censusdata_bin2$acs5_2010_bin),acs15=as.matrix(censusdata_bin2$acs5_2015_bin))
acsid<- c("acs10","acs15")

#Run and save output with race/ethnicity
res_raceeth<-list()
swap_rate1<-matrix(1, nrow = 3 , ncol= 2)
row.names(swap_rate1)<-heatsid
colnames(swap_rate1)<-acsid
for (h in 1:length(heatslist1)){
  #change prior settings
  for (i in 1:2){
    model.res<-run_models(dataset = datasets_raceeth[[acsid[i]]], Kmax = Kmax,gamma=gamma1, 
                          nChains= nChains, m= m, ClusterPrior= "poisson", wd = wd, 
                          acsid = acsid[i], burnin= burnin, heats = heatslist1[[heatsid[h]]], heatsid = heatsid[h])
    #Keep swap acceptance rate
    swap_rate1[h, i]<-model.res[[1]]$chainInfo[2]
    res_raceeth<-append(res_raceeth, model.res)
  }
}

output_raceeth<- list(res_raceeth,  "sar_priors"=swap_rate1)

#Run and save output without race/ethnicity
res_noraceeth<-list()
swap_rate2<-matrix(1, nrow = 3 , ncol= 2)
row.names(swap_rate2)<-heatsid
colnames(swap_rate2)<-acsid
for (h in 1:length(heatslist1)){
  #change prior settings
  for (i in 1:2){
    model.res<-run_models(dataset = datasets_noraceeth[[acsid[i]]], Kmax = Kmax,gamma=gamma1, 
                          nChains= nChains, m= m, ClusterPrior= "poisson", wd = wd, 
                          acsid = acsid[i], burnin= burnin, heats = heatslist1[[heatsid[h]]], heatsid = heatsid[h])
    #Keep swap acceptance rate
    swap_rate2[h, i]<-model.res[[1]]$chainInfo[2]
    res_noraceeth<-append(res_noraceeth, model.res)
  }
}

output_noraceeth<- list(res_noraceeth,  "sar_priors"=swap_rate2)


#SAVE OUTPUT
saveRDS(output_noraceeth, "/n/home03/crodriguezcabrera/ecbayesbinmix/Model Outputs/acs2waves_noraceeth_priors.rds")
saveRDS(output_raceeth, "/n/home03/crodriguezcabrera/ecbayesbinmix/Model Outputs/acs2waves_raceeth_priors.rds")


