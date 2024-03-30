#Running latest models
#Packages required
library(BayesBinMix)
library(foreach)
library(label.switching)
library(doParallel)
library(coda)
#Last edited: 3/10/2024

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

run_models<-function(dataset,Kmax,gamma, nChains, m, ClusterPrior, wd, acsid, burnin){
  #start.time <- Sys.time()
  heats <- seq(1, 0.4, length = nChains)
  
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

#---DFCI server
#censusdata_bin <- readRDS("/homes6/carmen/Other projects/ecbayesbinmix/censusdata_bin.rds")
#---My mac
#censusdata_bin <- readRDS("/Users/carmenrodriguez/Desktop/Research Projects/BayesBinMix/ecbayesbinmix/censusdata_bin.rds")

#---FASRC
#ADDED ON 3/29/24-- 16 VARIABLES NOW
censusdata_bin <- readRDS("./censusdata_bin_rownames_032924.rds")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
datasets<- list(acs10=as.matrix(censusdata_bin$acs5_2010_bin),acs15=as.matrix(censusdata_bin$acs5_2015_bin), acs19=as.matrix(censusdata_bin$acs5_2019_bin))

#INCREASE ITERATIONS
Kmax<-50
gamma<-rep((1/Kmax),Kmax) #to induce sparsity
#We want 15,000 iterations and 5000 burn-in
#INCREASE ITERATIONS AND BURN-IN-- HAVE NOT RUN YET AS OF 3/12/24
#m<-2000 
#burnin<-m/2
m<-1500
burnin<-500
nChains<-4

#3/10/24 added a burn in of 500 (technically 5000) and we changed gamma = 1/kmax, and set Kmax = 50
#wd<-"/homes6/carmen/Other projects/"

wd<-"/n/home03/crodriguezcabrera/BayesBinMix_Project/"
acsid<- c("acs10","acs15", "acs19")

res_all<-list()
for (i in 1:length(datasets)){
    model.res<-run_models(dataset = datasets[[acsid[i]]], Kmax = Kmax,gamma=gamma, 
                          nChains= nChains, m= m, ClusterPrior, wd = wd, 
                          acsid = acsid[i], burnin= burnin)
    
    res_all<-append(res_all, model.res)
  }



#Output will include 3 lists of length 7
saveRDS(res_all, "/n/home03/crodriguezcabrera/ecbayesbinmix/ACSall_3.29.24.rds")


