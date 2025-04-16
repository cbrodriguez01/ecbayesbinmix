# Created April 15, 2025
#Updated models for ECBAYESBINMIX now only including 14 variables and incorporating suggestions from reviewers in JRSS

# Load libraries
library(BayesBinMix)
library(foreach)
library(label.switching)
library(doParallel)
library(coda)
library(tidyverse)
library(DescTools)

setwd("/n/home03/crodriguezcabrera/tests/")
#PLAN
# 1. Only include ACS 2015-2019
# 2. Tuning heats for achieving reasonable acceptance rates
# 3. `pi` prior ~ Dirichlet (1,...,1)
# 4. `pi` prior ~ Dirichlet (1/Kmax)
# 5. Kmax = 50
#

#' @param dataset The observed binary data (array). Missing values are allowed as long as the corresponding entries are denoted as NA.

#' @param Kmax 	 Maximum number of clusters (integer, at least equal to two).
#' @param gamma  Kmax-dimensional vector (positive) corresponding to the parameters of the Dirichlet prior of the mixture weights. 
#'   Default value: rep(1,Kmax).

#' @param nChains. Number of parallel (heated) chains. Ideally, it should be equal to the number of available threads.

#' @param  m The number of MCMC cycles. At the end of each cycle a swap between a pair of heated chains is attempted. Each cycle consists of 10 iterations.

#' @param ClusterPrior Character string specifying the prior distribution of the number of clusters.It defaults to the truncated Poisson distribution.
#' 
#' @param wd  string directory where we want to store the outPrefix folder that contains more output
#' @param acsid which dataset, character; options: acs10, acs15, acs19 --
#' 
#' @returns  a named list containing model results 
#' 
#' 
run_models<-function(dataset,Kmax,gamma, nChains, m, ClusterPrior, wd, acsid, burnin, heats, heatsid, gammaid){
  out.path<-paste0(wd, acsid, sep= "_",Sys.Date() ,sep = "_",heatsid,sep = "_", ClusterPrior, sep = "_", gammaid)
  
  # This produces a list and a folder in out.path
  res<- coupledMetropolis(Kmax=Kmax, nChains = nChains, 
                          heats = heats,
                          binaryData = dataset, 
                          outPrefix = out.path, 
                          ClusterPrior = ClusterPrior,
                          m = m,
                          gamma = gamma, 
                          burn = burnin)
  
  out.list<-list(res)
  names(out.list)<-c(paste0("res_", acsid,sep = "_",heatsid,sep = "_", ClusterPrior))
  return(out.list)
}


##Function to generate heating vectors-- using method from Altekar 2004
#' @param deltatempvec vector containing multiple deltaT for the heats vector
#' #in the other models smaller delta (not too small) worked best-- we used 0.025
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


#Dataset
acs19_bin<-readRDS("censusdata_bin.rds")
str(acs19_bin)
acs19_bin_rowGEOID<- as.matrix(acs19_bin %>% select(-c(GEOID, NAME)))
acs19_bin_rowGEOID <-apply(acs19_bin_rowGEOID,2, as.numeric)
row.names(acs19_bin_rowGEOID)<-acs19_bin$GEOID


#Model parameters
set.seed(2004)
wd<-"/n/home03/crodriguezcabrera/BayesBinMix_Project/Updates/"
nChains<- 4
Kmax<-50
m<-1500
burnin<-500
gammalist<-list(rep(1, Kmax),rep((1/Kmax),Kmax))
gamma_labels <-c("DirichSym", "DirichPAprox")
deltatemps<-c(0.005,0.01,0.025, 0.05,0.1)
heatslist<-generateHeats(deltatemps, nChains)
heatsid<-paste0("deltatemp_", formatC(deltatemps, format = "f", digits = 3))
names(heatslist) <-heatsid
Kprior<-c("poisson", "uniform")


# Initialize summary and full model list
results_summary <-c()
model_outputs <- list()  # This will hold full outputs

for (kp in Kprior) {
  for (i in seq_along(gammalist)) {
    gamma <- gammalist[[i]]
    gamma_label <- gamma_labels[i]
    
    for (j in seq_along(heatslist)) {
      deltatemp <- deltatemps[j]
      heatvec <- heatslist[[j]]
      
      # Run model
      model_output <- run_models(dataset = acs19_bin_rowGEOID, Kmax = Kmax,gamma=gamma, 
                                 nChains= nChains, m= m, ClusterPrior= kp, wd = wd, 
                                 acsid = "acs19", burnin= burnin, heats = heatvec, heatsid = heatsid[j], gammaid = gamma_label)
      
      # Extract  swap rate and Kmap
      swap_rate <- model_output[[1]]$chainInfo[2]
      mapK <- Mode(model_output[[1]]$K.mcmc)[1]
      
      # Save summary
      # Save summary
      res<-c(kp, gamma_label, deltatemp, swap_rate, mapK)
      results_summary <- rbind(results_summary,res)
      
      # Create a unique name for this model run
      model_name <- paste0(kp, "_", gamma_label, "_deltatemp", formatC(deltatemp, format = "f", digits = 3))
      
      # Save full model output
      model_outputs[[model_name]] <- model_output
    }
  }
}

colnames(results_summary)<-c("Kprior", "gamma_label", "deltatemp", "swap_rate", "mapK")
write.csv(results_summary, "/n/home03/crodriguezcabrera/ecbayesbinmix/Model Outputs/ACS19model_summary_results14vars_2025.csv", row.names = FALSE)
saveRDS(model_outputs, "/n/home03/crodriguezcabrera/ecbayesbinmix/Model Outputs/ACS19_14vars_full_model_outputs2025.rds")



########################################################################################################################
#TESTS BEFORE CLUSTER SUBMISSION
########################################################################################################################

# set.seed(2005)
# wd<-"/n/home03/crodriguezcabrera/tests/model_outputs/"
# nChains<- 4
# Kmax<-50
# m<-100
# burnin<-20
# gammalist<-list(rep(1, Kmax),rep((1/Kmax),Kmax))
# gamma_labels <-c("DirichSym", "DirichPAprox")
# deltatemps<-c(0.05,0.1)
# heatslist<-generateHeats(deltatemps, nChains)
# heatsid<-paste0("deltatemp_", formatC(deltatemps, format = "f", digits = 3))
# names(heatslist) <-heatsid
# Kprior<-c("poisson", "uniform")
# 
# #test run models
# heatsid = heatsid[1]; gammaid = gamma_labels[1];acsid = "acs19";ClusterPrior = Kprior[1]
# out.path<-paste0(wd, "testa")
# 
# testa<-coupledMetropolis(Kmax=Kmax, nChains = nChains, 
#                          heats = heatslist[[1]],
#                          binaryData = acs19_bin_rowGEOID, 
#                          outPrefix = out.path,
#                          ClusterPrior = Kprior[1],
#                          m = m,
#                          gamma = gammalist[[1]], burn = burnin)
# closeAllConnections()
# str(testa)
# 
#   
# test1<-run_models(dataset = acs19_bin_rowGEOID, Kmax = Kmax,gamma=gammalist[[1]], 
#            nChains= nChains, m= m, ClusterPrior= Kprior[2], wd = wd, 
#            acsid = "acs19", burnin= burnin, heats = heatslist[[1]], heatsid = heatsid[1], gammaid = gamma_labels[1])
# 
# 
# 
# 

#Now test the full forloop
# wd<-"/n/home03/crodriguezcabrera/tests/model_outputs/"
# nChains<- 4
# Kmax<-50
# m<-50
# burnin<-5
# gammalist<-list(rep(1, Kmax),rep((1/Kmax),Kmax))
# gamma_labels <-c("DirichSym", "DirichPAprox")
# deltatemps<-c(0.05,0.1)
# heatslist<-generateHeats(deltatemps, nChains)
# heatsid<-paste0("deltatemp_", formatC(deltatemps, format = "f", digits = 3))
# names(heatslist) <-heatsid
# Kprior<-c("poisson", "uniform")
# 


# Initialize summary and full model list
# results_summary <-c()
# colnames(results_summary)<-c("Kprior", "gamma_label", "deltatemp", "swap_rate", "mapK")
# 
# model_outputs <- list()  # This will hold full outputs
# 
# for (kp in Kprior) {
#   for (i in seq_along(gammalist)) {
#     gamma <- gammalist[[i]]
#     gamma_label <- gamma_labels[i]
#     
#     for (j in seq_along(heatslist)) {
#       deltatemp <- deltatemps[j]
#       heatvec <- heatslist[[j]]
#       
#       # Run model
#       model_output <- run_models(dataset = acs19_bin_rowGEOID, Kmax = Kmax,gamma=gamma, 
#                                  nChains= nChains, m= m, ClusterPrior= kp, wd = wd, 
#                                  acsid = "acs19", burnin= burnin, heats = heatvec, heatsid = heatsid[j], gammaid = gamma_label)
#       
#       # Extract  swap rate and Kmap
#       swap_rate <- model_output[[1]]$chainInfo[2]
#       
#       print(paste0("Swap rate final", ":", swap_rate))
#       mapK <- Mode(model_output[[1]]$K.mcmc)[1]
#       print(paste0("Most probable K", ":", mapK))
#       
#       # Save summary
#       res<-c(kp, gamma_label, deltatemp, swap_rate, mapK)
#       results_summary <- rbind(results_summary,res)
#         
#         
#       # Create a unique name for this model run
#       model_name <- paste0(kp, "_", gamma_label, "_deltatemp", formatC(deltatemp, format = "f", digits = 3))
#       
#       # Save full model output
#       model_outputs[[model_name]] <- model_output
#     }
#   }
# }





