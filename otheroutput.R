#Explore other output

#Trace plots-selecting a cluster at random for examination- Example when Kmax = 5

set.seed(2020) #year-month-day
#-------Explore output---------
mapK<-Mode(res_5$K.mcmc)[1]  #tells you mapK
n_ses<-ncol(censusdata_bin$acs5_2010_bin)

## (1) Trace plots for convergence exploration -- select a cluster at random
clusterran<-sample(1:mapK, size = 1)
clustersel<-c(paste0("theta", sep=".", clusterran, sep=".",1:n_ses), paste("p", clusterran, sep = "."))
params_est<-res_5$parameters.ecr.mcmc

#Filename is for most probable K,not the cluster being plotted
MCMCtrace(params_est[,clustersel], filename = paste("MCMCtrace_mapK", mapK, sep = "_"), wd = "./Figures")



#Illustrate the sampled values of K per chain according to the Poisson and uniform prior distribution using data from K.allChains— 
#This we can do last if necessary, otherwise we don't run it

poiKallchains<-read.table(file = "/homes6/carmen/Other projects/acs10_res_poiunif_demo/K.allChains.txt",header = T)

poiKallchains$m <-as.numeric(row.names(poiKallchains))
poiKallchains1<-poiKallchains %>% pivot_longer(cols = 1:4, names_to ="Chain", values_to = "K")
#this plot shows the generated values of K per heated chain-- note chain 1 is the original (^1)
poiKallchains1 %>% ggplot(aes(x = m, y = K, col = as.factor(Chain))) + 
  geom_line() + 
  ggtitle("MC^3 Sampler Poi-Unif") + 
  xlab("mcmc cycle")