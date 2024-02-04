#Packages required
library(BayesBinMix)
library(foreach)
library(label.switching)
library(doParallel)
library(coda)

#Run models

#' @param dataset The observed binary data (array). Missing values are allowed as long as the corresponding entries are denoted as NA.

#' @param Kmax 	 Maximum number of clusters (integer, at least equal to two).

#' @param nChains. Number of parallel (heated) chains. Ideally, it should be equal to the number of available threads.

#' @param  m The number of MCMC cycles. At the end of each cycle a swap between a pair of heated chains is attempted. Each cycle consists of 10 iterations.

#' @param ClusterPrior Character string specifying the prior distribution of the number of clusters.It defaults to the truncated Poisson distribution.
#' 
#' @param wd  string directory where we want to store the outPrefix folder that contains more output
#' @param acsid which dataset, character; options: acs10, acs15, acs19
#' 
#' @returns  list containing model results and time it took for it to run

run_models<-function(dataset,Kmax, nChains, m, ClusterPrior, wd, acsid){
  start.time <- Sys.time()
  heats <- seq(1, 0.4, length = nChains)
  
  #Part of function->outPrefix:The name of the produced outut folder. An error is thrown if the directory exists.
  out.path<-paste0(wd, acsid, sep = "_",Kmax)
  #print(out.path)
  
  # This produces a list and a folder in out.path
  res<- coupledMetropolis(Kmax=Kmax, nChains = nChains, 
                          heats = heats,
                          binaryData = dataset, 
                          outPrefix = out.path,
                          m = m)
  
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time,2)
  return(list(res, time.taken))
}

#---DFCI server
#censusdata_bin <- readRDS("/homes6/carmen/Other projects/ecbayesbinmix/censusdata_bin.rds")
#---My mac
#censusdata_bin <- readRDS("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/censusdata_bin.rds")

#---FASRC
censusdata_bin <- readRDS("/n/home03/crodriguezcabrera/ecbayesbinmix/censusdata_bin.rds")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
datasets<- list(acs15=as.matrix(censusdata_bin$acs5_2015_bin), acs19=as.matrix(censusdata_bin$acs5_2019_bin))

Kmaxes<-c(5,10,15)
m<-500
nChains<-4
#wd<-"/homes6/carmen/Other projects/"
wd<-"/n/home03/crodriguezcabrera/BayesBinMix_Project/"
acsid<- c("acs15", "acs19")



res_all<-list()
for (i in 1:2){
  for (k in Kmaxes){
    model.res<-run_models(dataset = datasets[acsid[i]], Kmax = k , nChains= nChains, m= m, ClusterPrior, wd = wd, acsid = acsid)
    
    res_all<-append(res_all, as.name(acsid[i])=model.res)
  }
}


saveRDS(res_all, "/n/home03/crodriguezcabrera/ecbayesbinmix/ACS15_19_results.rds")

