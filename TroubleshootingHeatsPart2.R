
# Load libraries
library(BayesBinMix)
library(foreach)
library(label.switching)
library(doParallel)
library(coda)

run_models<-function(dataset,Kmax,gamma, nChains, m, ClusterPrior, wd, acsid, burnin, heats, heatsid){
  out.path<-paste0(wd, acsid, sep= "_",Sys.Date() ,sep = "_",heatsid)
  
  # This produces a list and a folder in out.path
  res<- coupledMetropolis(Kmax=Kmax, nChains = nChains, 
                          heats = heats,
                          binaryData = dataset, 
                          outPrefix = out.path,
                          m = m,
                          gamma = gamma, burn = burnin)
  
  out.list<-list(res)
  names(out.list)<-c(paste0("res_", acsid,sep = "_",heatsid))
  return(out.list)
}

#Tuning heats for achieving reasonable acceptance rates
censusdata_bin <- readRDS("./censusdata_bin_raceeth_042524.rds")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
datasets<- list(acs10=as.matrix(censusdata_bin$acs5_2010_bin),acs15=as.matrix(censusdata_bin$acs5_2015_bin), acs19=as.matrix(censusdata_bin$acs5_2019_bin))


# We are going to use the approach in Altekar 2004 
# We are going to get deltaT = {0.1,0.01, 0.001, 0.0001} and nchains = 4
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

nChains<- 4
#heatsid is the temperature ID to see which heated chains to use
#temps<- c(0.05, 0.1, 0.15, 0.2)
#It seems smaller deltaT work better for these datasets
temps1<-c(0.005,0.01,0.025)
heatslist<-generateHeats(temps1, nChains)

Kmax<-50
gamma<-rep((1/Kmax),Kmax)
m<-1500
burnin<-500
wd<-"/n/home03/crodriguezcabrera/BayesBinMix_Project/"
acsid<- c("acs10","acs15", "acs19")
heatsid<-paste0("deltatemp", 1:length(heatslist))
names(heatslist) <-heatsid

res_all<-list()
swap_rate<-matrix(1, nrow = 3 , ncol= 3)
row.names(swap_rate)<-heatsid
colnames(swap_rate)<-acsid
for (h in 1:length(heatslist)){
  for (i in 1:length(datasets)){
    model.res<-run_models(dataset = datasets[[acsid[i]]], Kmax = Kmax,gamma=gamma, 
                        nChains= nChains, m= m, ClusterPrior, wd = wd, 
                        acsid = acsid[i], burnin= burnin, heats = heatslist[[heatsid[h]]], heatsid = heatsid[h])
  
  #print(model.res[[i]]$chainInfo)
  #Keep swap acceptance rate
    swap_rate[h, i]<-model.res[[1]]$chainInfo[2]
  
    res_all<-append(res_all, model.res)
  }
}

output<- list(res_all,  "sar2"=swap_rate)


#SAVE OUTPUT
saveRDS(output, "/n/home03/crodriguezcabrera/ecbayesbinmix/tuningheatsvec_4.25.24.rds")

