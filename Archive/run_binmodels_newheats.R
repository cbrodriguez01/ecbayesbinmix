#Running latest models
#Packages required
library(BayesBinMix)
library(tidyverse)
library(foreach)
library(label.switching)
library(doParallel)
library(coda)
#Last edited: 4/30/24

#Run models-- edited to change the parameter of the Dirichlet distribution to 1/Kmax (2/10/24)

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
#' 
#' 
#' 

run_models<-function(dataset,Kmax,gamma, nChains, m, ClusterPrior, wd, acsid, burnin, heats){
  
  #Part of function->outPrefix:The name of the produced outut folder. An error is thrown if the directory exists.Add date to fix this issue
  out.path<-paste0(wd, acsid, sep= "_",Sys.Date() ,sep = "_",Kmax)
  #out.path<-paste0(wd, acsid, sep= "_","3.13.24" ,sep = "_",Kmax)
  
  # This produces a list and a folder in out.path
  res<- coupledMetropolis(Kmax=Kmax, nChains = nChains, 
                          heats = heats,
                          binaryData = dataset, 
                          outPrefix = out.path,
                          m = m,
                          gamma = gamma, burn = burnin)
  
  out.list<-list(res)
  names(out.list)<-c(paste0("res_", acsid,sep = "_",Kmax))
  return(out.list)
}

#ADDED ON 4/30/24-- running model without race/eth--
censusdata_bin <- readRDS("./censusdata_bin_raceeth_042524.rds")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
#Exclude race vars
acs10<-censusdata_bin$acs5_2010_bin %>% select(-c("Hispanic_or_Latino_2010","NonHispanicBlack_2010","NonHispanicAsian_2010"))
acs15<-censusdata_bin$acs5_2015_bin %>% select(-c("Hispanic_or_Latino_2015","NonHispanicBlack_2015","NonHispanicAsian_2015"))
acs19<-censusdata_bin$acs5_2019_bin %>% select(-c("Hispanic_or_Latino_2019","NonHispanicBlack_2019","NonHispanicAsian_2019"))


datasets<- list(acs10=as.matrix(acs10),acs15=as.matrix(acs15), acs19=as.matrix(acs19))

Kmax<-50
gamma<-rep((1/Kmax),Kmax)
m<-1500
burnin<-500

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
#Already tuned deltaT-see TroubleshootingHeatsPart2.R
heatsvec<-generateHeats(0.01, nChains)[[1]]


wd<-"/n/home03/crodriguezcabrera/BayesBinMix_Project/"
acsid<- c("acs10","acs15", "acs19")

res_all<-list()
for (i in 1:length(datasets)){
    model.res<-run_models(dataset = datasets[[acsid[i]]], Kmax = Kmax,gamma=gamma, 
                          nChains= nChains, m= m, ClusterPrior, wd = wd, 
                          acsid = acsid[i], burnin= burnin, heats = heatsvec)
    
    res_all<-append(res_all, model.res)
  }



#Output will include 3 lists of length 7
saveRDS(res_all, "/n/home03/crodriguezcabrera/ecbayesbinmix/ACS_noraceeth_4.30.24.rds")






