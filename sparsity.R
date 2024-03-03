#Checking the sparsity of the Dirichlet prior on the mixing weights
#https://stats.stackexchange.com/questions/239216/what-is-meant-by-non-convex-prior-and-sparsity-inducing-prior


#alpha<- c(1, 1/(seq(20,60, by = 15)))

# pd<-list()
# for (i in 1:length(alpha)){
#    alphavec<-rep(alpha[i], K)
#    pdir<- list(BayesBinMix::myDirichlet(alphavec))
#   pd<-append(pd, pdir)
# }

#Note that as alpha decreases so does the number of vector elements with much appreciable mass, thus inducing sparsity

library(BayesBinMix)
#library(DescTools)
library(tidyverse)
#We are going to explore changes in the value of K for the ACS 2010
censusdata_bin <- readRDS("./censusdata_bin.rds")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
acs10<-as.matrix(censusdata_bin$acs5_2010_bin)
Kmax<-c(10,20)
alpha<- c(1, 1/(seq(20,60, by = 15)), 0.01)
m<-1000
burnin<-500
nChains<-4
heats <- seq(1, 0.4, length = nChains)
wd<-"/n/home03/crodriguezcabrera/BayesBinMix_Project/"

Gammas<-c()
Kmap<-c()
for (k in Kmax){
  for (g in alpha){
  out.path<-paste0(wd, "acs10", sep= "_",k ,sep = "_",g)
    gammavec<-rep(g, k)
    res<- coupledMetropolis(Kmax=k, nChains = nChains, 
                            heats = heats,
                            binaryData = acs10, 
                            outPrefix = out.path,
                            m = m,
                            gamma = gammavec, burn = burnin)
    
    #Collect Kmap and g
    Gammas<-c(Gammas, g)
    #Kmap<-c(Kmap,Mode(res$K.mcmc)[1])
    #Another way to extracting Kmap
    Kmap<- c(Kmap,ncol(res$classificationProbabilities.ecr))
  }
}

Mat<-matrix(nrow = 10, ncol = 3)
colnames(Mat)<-c("Kmax", "Gamma", "Kmap")
Mat[,1]<-c(rep(10,5), rep(20,5))
Mat[,2]<-Gammas
Mat[,3]<-Kmap


plot<-Mat %>% as.data.frame()%>% ggplot(aes(x= Gamma, y = Kmap, color= as.factor(Kmax))) +
 geom_line()


save(Mat, plot, file = "test.RData")


## Added on 3/3/24
library(latex2exp)
load("test.RData")
 plot

 dat<-Mat %>% as.data.frame() %>% filter(Gamma != 1)

 dat%>% ggplot(aes(x= Gamma, y = Kmap, color= as.factor(Kmax))) +
   geom_line() + 
   geom_point() + 
   labs(color = "Kmax", y = "Most Probable K (Kmap)", 
        x = TeX("Gamma parameter of Dirichlet prior on \\pi")) +
   scale_discrete_manual("Kmax")
 
 
  
 



