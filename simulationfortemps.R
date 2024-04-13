
library(LaplacesDemon)
library(BayesBinMix)
library(coda)
library(tidyverse)

#Generate Multivariate Bernoulli mixture model
#generate dataset from a mixture of 2 ten-dimensional Bernoulli distributions.
set.seed(1)
d <- 18 # number of columns
n <- 200 # number of rows (sample size)
K <- 8 	 # true number of clusters
p.true <- as.vector(rdirichlet(1, alpha = rep(10, K)))
z.true <- numeric(n) # true cluster membership
z.true <- sample(K,n,replace=TRUE,prob = p.true)
theta.true <- array(data = NA, dim = c(K,d)) 	#true probability of positive responses per cluster-- assume Beta prior
for(j in 1:d){
  theta.true[,j] <- rbeta(K, shape1 = 1, shape2 = 1)
}
x <- array(data=NA,dim = c(n,d)) # data: n X d array
for(k in 1:K){
  myIndex <- which(z.true == k) #indices for the rows/obs
  for (j in 1:d){
    x[myIndex,j] <- rbinom(n = length(myIndex), size = 1, prob = theta.true[k,j])   
  }
}



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
temps<- c(0.05, 0.1, 0.15, 0.2)
heatslist<-generateHeats(temps, nChains)

Kmax<-50
gamma<-rep((1/Kmax),Kmax)
m<-1500
burnin<-500
wd<- "/Users/carmenrodriguez/Desktop/temp/"
heatsid<-paste0("deltatemp", 1:length(heatslist))
names(heatslist) <-heatsid


run_models<-function(dataset,Kmax,gamma, nChains, m, ClusterPrior, wd,burnin, heats, heatsid,z.true){
  out.path<-paste0(wd, "sim", sep= "_",Sys.Date() ,sep = "_",heatsid)
  
  # This produces a list and a folder in out.path
  res<- coupledMetropolis(Kmax=Kmax, nChains = nChains, 
                          heats = heats,
                          binaryData = dataset, 
                          outPrefix = out.path,
                          m = m,
                          gamma = gamma, burn = burnin, z.true = z.true)
  
  out.list<-list(res)
  names(out.list)<-c(paste0("res_", "sim",sep = "_",heatsid))
  return(out.list)
}

res2<-list()
swap_rate_sim2<-matrix(1, nrow = 4 , ncol= 1)
for (h in 1:length(heatslist)){
  model.res<-run_models(dataset = x, Kmax = Kmax,gamma=gamma, 
                        nChains= nChains, m= m, ClusterPrior, wd = wd, 
                         burnin= burnin, heats = heatslist[[heatsid[h]]], heatsid = heatsid[h], z.true = z.true)
  swap_rate_sim2[h, 1]<-model.res[[1]]$chainInfo[2]
  
  res2<-append(res2, model.res)

}


hsar<-res2$res_sim_deltatemp1
stats<-cbind(summary(hsar$parameters.ecr.mcmc)$statistics[,c(1,2)], 
             summary(hsar$parameters.ecr.mcmc)$quantiles[,c(1,5)])

tail(round(stats, 2), 7)


