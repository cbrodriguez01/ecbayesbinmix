# Load libraries
library(BayesBinMix)
library(foreach)
library(label.switching)
library(doParallel)
library(coda)

#' @param dataset The observed binary data (array). Missing values are allowed as long as the corresponding entries are denoted as NA.

#' @param Kmax 	 Maximum number of clusters (integer, at least equal to two).
#' @param gamma  Kmax-dimensional vector (positive) corresponding to the parameters of the Dirichlet prior of the mixture weights. 
#'   Default value: rep(1,Kmax).

#' @param nChains. Number of parallel (heated) chains. Ideally, it should be equal to the number of available threads.

#' @param  m The number of MCMC cycles. At the end of each cycle a swap between a pair of heated chains is attempted. Each cycle consists of 10 iterations.

#' @param ClusterPrior Character string specifying the prior distribution of the number of clusters.It defaults to the truncated Poisson distribution.
#' 
#' @param wd  string directory where we want to store the outPrefix folder that contains more output
#' @param acsid which dataset, character; options: acs10, acs15, acs19
#' 
#' @returns  a named list containing model results 

run_models<-function(dataset,Kmax,gamma, nChains, m, ClusterPrior, wd, acsid, burnin, heats, heatsid){
  
  #Part of function->outPrefix:The name of the produced outut folder. An error is thrown if the directory exists.Add date to fix this issue
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
censusdata_bin <- readRDS("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/censusdata_bin_raceeth_033024.rds")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
datasets<- list(acs10=as.matrix(censusdata_bin$acs5_2010_bin),acs15=as.matrix(censusdata_bin$acs5_2015_bin), acs19=as.matrix(censusdata_bin$acs5_2019_bin))
nChains<- 5
heatslist<- list(seq(1, 0.1, length = nChains), seq(1, 0.2, length = nChains),
                 seq(1, 0.3, length = nChains), seq(1, 0.4, length = nChains))
Kmax<-50
gamma<-rep((1/Kmax),Kmax)
m<-1500
burnin<-500
wd<-"/n/home03/crodriguezcabrera/BayesBinMix_Project/"
acsid<- c("acs10","acs15", "acs19")
heatsid<-paste0("ht", 1:length(heatslist))
names(heatslist) <-heatsid

res_all<-list()
swap_rate<-matrix(1, nrow = 4 , ncol= 3)
for (h in 1:length(heatslist)){
for (i in 1:length(datasets)){
  model.res<-run_models(dataset = datasets[[acsid[3]]], Kmax = Kmax,gamma=gamma, 
                        nChains= nChains, m= m, ClusterPrior, wd = wd, 
                        acsid = acsid[3], burnin= burnin, heats = heatslist[[heatsid[h]]], heatsid = heatsid[h])
  
  print(model.res[[i]]$chainInfo)
  #Keep swap acceptance rate
  swap_rate[h, i]<-model.res[[i]]$chainInfo[2]
  
  res_all<-append(res_all, model.res)
}
}

output<- list(res_all,  "sar"=swap_rate)



#SAVE OUTPUT
saveRDS(output, "/n/home03/crodriguezcabrera/ecbayesbinmix/tuningheatsvec_4.7.24.rds")








