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


#save(Mat, plot, file = "test.RData")


## Added on 3/3/24
library(latex2exp)
load("test.RData")
 #plot

dat<-Mat %>% as.data.frame() %>% filter(Gamma != 1)

 dat%>% ggplot(aes(x= Gamma, y = Kmap, color= as.factor(Kmax))) +
 geom_line() +
 geom_point() +
 labs(color = "Kmax", y = "Most Probable K (Kmap)",
    x = TeX("Gamma parameter of Dirichlet prior on \\pi"), title = "ACS 2006-2010") +
 scale_discrete_manual("Kmax")

 
#Run without changing Gamma, just doing 5 iterations of the same model with Kmax = c(10,20,50) 
 #For each Kmax run 5 iterations of the same model ==> 15 models total

 library(BayesBinMix)
 #library(DescTools)
 library(tidyverse)
 #We are going to explore changes in the value of K for the ACS 2010
 censusdata_bin <- readRDS("./censusdata_bin.rds")
 names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
 acs10<-as.matrix(censusdata_bin$acs5_2010_bin)
 
Kmax<-c(10,20,50)
 alpha<- 1/50
 m<-1000
 burnin<-500
 nChains<-4
 heats <- seq(1, 0.4, length = nChains)
 wd<-"/n/home03/crodriguezcabrera/BayesBinMix_Project/"
 #wd_temp<-"/Users/carmenrodriguez/Desktop/temp/"
 
 Kmap<-c()
 for (k in Kmax){
   for (i in 1:5){
     out.path<-paste0(wd, "acs10", sep= "_",k ,sep = "_","iter", i)
     gammavec<-rep(alpha, k)
     res<- coupledMetropolis(Kmax=k, nChains = nChains, 
                             heats = heats,
                             binaryData = acs10, 
                             outPrefix = out.path,
                             m = m,
                             gamma = gammavec, burn = burnin)
     
     #Collect Kmap 
     #Kmap<-c(Kmap,Mode(res$K.mcmc)[1])
     #Another way to extracting Kmap
     Kmap<- c(Kmap,ncol(res$classificationProbabilities.ecr))
   }
 }
 
 #had to do manually because job timeout
 Mat<-matrix(nrow = length(Kmax)*4, ncol = 3)
 colnames(Mat)<-c("Kmax", "Iter", "Kmap")
Mat[,1]<-c(rep(10,4), rep(20,4), rep(50,4))
 Mat[,2]<-c(rep(1:4,3))
Mat[,3]<-c(9,9,10,9,rep(9,4), 9,9,10,9)
 
 
#save(Kmap, file = "test2.RData")





# What does ejection alpha do?
#Probability of ejecting an empty component. Defaults to 0.2.
acs15<-as.matrix(censusdata_bin$acs5_2015_bin)
alpha_eject<- c(0.2,0.3,0.4,1)
temp<-"/Users/carmenrodriguez/Desktop/temp/"
alpha<- 1/50
m<-100
burnin<-50
nChains<-4
heats1 <- seq(1, 0.4, length = nChains)
Kmax<-10
gamma<-rep(1/50,Kmax)


res_all_aj<-list()
for (i in alpha_eject){
out.path<-paste0(temp, "acs15_ejalpha", sep= "_",Sys.Date() ,sep = "_",i)

res<- coupledMetropolis(Kmax=Kmax, nChains = nChains, 
                        heats = heats1,
                        binaryData = acs15, 
                        outPrefix = out.path,
                        m = m,
                        gamma = gamma, 
                        burn = burnin, 
                        ejectionAlpha = i)

res_all_aj<-append(res_all_aj, res)
}

#Look through the cluster assignment for all of these
#Extract tables for all runs
res_split<-split(res_all_aj, rep(1:ceiling(length(res_all_aj)/7), each=7, length.out=length(res_all_aj)))
alpha_eject<- c(0.2,0.3,0.4,1)
for (l in length(res_split)){
  tab<-res_split[[l]]$clusterMembershipPerMethod
  print(paste0("For ejection alpha=", sep= "", alpha_eject[l]))
}  
 


tab<-res_split[[4]]$clusterMembershipPerMethod

 # Get column names
  column_names <- names(tab)
  #print(paste0("For ejection alpha=", sep= "", alpha_eject[1]))
  # Create tables for each column
  print(paste0("For ejection alpha=", sep= "", alpha_eject[4]))
  for (col_name in column_names) {
  
    cat("Table for column:", col_name, "\n")
    print(table(tab[[col_name]]))
    cat("\n")
  }






#What about the heats?
# 
# out.path<-paste0(temp, "acs15_ejalpha", sep= "_",Sys.Date() ,sep = "_",0.3)
# res<- coupledMetropolis(Kmax=Kmax, nChains = nChains, 
#                         heats = heats1,
#                         binaryData = acs15, 
#                         outPrefix = out.path,
#                         m = m,
#                         gamma = gamma, 
#                         burn = burnin, 
#                         ejectionAlpha = 0.3)
