#---Trying other clustering methods--
library(tidyverse)
library(mclust)
library(NbClust)
library(factoextra)
library(mixtools)
library(flexmix)

censusdata_bin <- readRDS("./censusdata_bin_raceeth_042524.rds")
censusdata<- load("./censusdata_updated_newvars.RData")


###########################################################################
#                           ACS 2006-2010
###########################################################################
acs10bin<-censusdata_bin[[1]]
#Have to exclude rows with missing values
acs10bin_nomiss <- na.omit(acs10bin)

#-----Hierarchical clustering of rows------
#The complete linkage method finds similar clusters
set.seed(1)
row_dist <- dist(acs10bin_nomiss, method = "euclidean")  # compute the distance matrix
hclust10a <- hclust(row_dist, method = "ward.D2")  

# Plot the dendrogram
plot(hclust10a, sub = "", xlab = "", cex = 0.01)
rect.hclust(hclust10a,k=2, border="red")
# Cutting the dendrogram to form 6 clusters
clusters <- cutree(hclust10a , k = 2)
table(clusters)

fviz_cluster(list(data = acs10bin_nomiss, cluster = clusters))


#-----Hierarchical clustering using the package NbClust
#https://rpubs.com/ranvirkumarsah/Intro2Clustering
#--number of cluster is dependent on method we choose: 
hclust10<-NbClust(acs10bin_nomiss, distance = "euclidean",min.nc = 2, max.nc = 20, 
                       method = "ward.D2", index = "all")
#table(hclust10$Best.partition)
#not sure about the index
hclust10$Best.nc


#----- Gaussian Mixture Model----- using proportions
#https://journal.r-project.org/articles/RJ-2023-043/
#Probit transformation  in order to use GMM
acs10<- acs5_2010_wide  %>% select( !c( "Femalehousehold_2010_E","SNAP_2010_E","Two_or_more_rooms_2010_E", paste0("ms", 1:8), 
                                                          "ms10", "ms_to", paste0("wc", 1:14), "wc_to","pov_to", paste0("pov", 2:5), "white_collar_occupation_2010",
                                                          "HouseIncBlowPovLineP_2010", "Lessthan2rooms_2010_E",  "Two_or_more_rooms_2010_P", "Total_occhousing_2010", 
                                                          "NH_some_other_race_2010", "NH_two_or_more_races_2010",  "NonHispanicWhite_2010" , "minoritystatus_2010"))

acs10_nomiss <- na.omit(acs10)
acs10_nomiss$medianincome_2010_2<- acs10_nomiss$medianincome_2010/max(acs10_nomiss$medianincome_2010, na.rm = T)
acs10a<-acs10_nomiss
acs10a[,3:19] <- acs10a[,3:19] / 100
#Check for Edge Cases
#Probit transformation is undefined for probabilities of 0 or 1, so make sure your data does not include these values.
dat10<-acs10a[,3:20]
dat10[dat10==0] <-0.0001
dat10[dat10==1] <-0.9999

probit_dat10<- apply(dat10,2, qnorm)
summary(probit_dat10)

####MCLUST#######
gmm.mclust10 <- Mclust(probit_dat10)
summary(gmm.mclust10, parameters = TRUE) #8 components!

#plot(gmm.mclust1, what = "classification")


####MIXTOOLS#######
gmm.mixtools10<-mvnormalmixEM(probit_dat10, arbmean = FALSE,
              sigma = NULL, lambda = NULL,
              epsilon = 1e-02)
gmm.mixtools10[c("lambda", "mu", "sigma")]


###########################################################################
#                           ACS 2011-2015
###########################################################################

acs15bin<-censusdata_bin[[2]]
#Have to exclude rows with missing values
acs15bin_nomiss <- na.omit(acs15bin)

#-----Hierarchical clustering of rows------
#The complete linkage method finds similar clusters
set.seed(1)
row_dist <- dist(acs15bin_nomiss, method = "euclidean")  # compute the distance matrix
hclust15a <- hclust(row_dist, method = "ward.D2")  

# Plot the dendrogram
plot(hclust15a, sub = "", xlab = "", cex = 0.01)
rect.hclust(hclust15a,k=2, border="red")
# Cutting the dendrogram to form 6 clusters
#clusters <- cutree(hclust15a , k = 2)
#table(clusters)

#fviz_cluster(list(data = acs10bin_nomiss, cluster = clusters))


#-----Hierarchical clustering using the package NbClust
#https://rpubs.com/ranvirkumarsah/Intro2Clustering
#--number of cluster is dependent on method we choose: 
hclust15<-NbClust(acs15bin_nomiss, distance = "euclidean",min.nc = 2, max.nc = 20, 
                  method = "ward.D2", index = "all")
#table(hclust10$Best.partition)
#not sure about the index
hclust15$Best.nc



#----- Gaussian Mixture Model----- using proportions
#https://journal.r-project.org/articles/RJ-2023-043/
#Probit transformation  in order to use GMM
acs15<- acs5_2015_wide  %>% select( !c( "Femalehousehold_2015_E","SNAP_2015_E","Two_or_more_rooms_2015_E", paste0("ms", 1:8), 
                                        "ms10", "ms_to", paste0("wc", 1:14), "wc_to","pov_to", paste0("pov", 2:5), "white_collar_occupation_2015",
                                        "HouseIncBlowPovLineP_2015", "Lessthan2rooms_2015_E",  "Two_or_more_rooms_2015_P", "Total_occhousing_2015", 
                                        "NH_some_other_race_2015", "NH_two_or_more_races_2015",  "NonHispanicWhite_2015" , "minoritystatus_2015"))

acs15_nomiss <- na.omit(acs15)
acs15_nomiss$medianincome_2015_2<- acs15_nomiss$medianincome_2015/max(acs15_nomiss$medianincome_2015, na.rm = T)
acs15a<-acs15_nomiss
acs15a[,3:19] <- acs15a[,3:19] / 100
#Check for Edge Cases
#Probit transformation is undefined for probabilities of 0 or 1, so make sure your data does not include these values.
dat15<-acs15a[,3:20]
dat15[dat15==0] <-0.0001
dat15[dat15==1] <-0.9999

probit_dat15<- apply(dat15,2, qnorm)
summary(probit_dat15)

####MCLUST#######
gmm.mclust15 <- Mclust(probit_dat15)
summary(gmm.mclust15, parameters = TRUE) #9 components!

#plot(gmm.mclust1, what = "classification")


###########################################################################
#                           ACS 2015-2019
###########################################################################

acs19bin<-censusdata_bin[[3]]
#Have to exclude rows with missing values
acs19bin_nomiss <- na.omit(acs19bin)

#-----Hierarchical clustering of rows------
#The complete linkage method finds similar clusters
set.seed(1)
row_dist <- dist(acs19bin_nomiss, method = "euclidean")  # compute the distance matrix
hclust19a <- hclust(row_dist, method = "ward.D2")  

# Plot the dendrogram
plot(hclust19a, sub = "", xlab = "", cex = 0.01)
rect.hclust(hclust19a,k=2, border="red")
# Cutting the dendrogram to form 6 clusters
#clusters <- cutree(hclust19a , k = 2)
#table(clusters)


#-----Hierarchical clustering using the package NbClust
#https://rpubs.com/ranvirkumarsah/Intro2Clustering
#--number of cluster is dependent on method we choose: 
hclust19<-NbClust(acs19bin_nomiss, distance = "euclidean",min.nc = 2, max.nc = 20, 
                  method = "ward.D2", index = "friedman")
#table(hclust10$Best.partition)
#not sure about the index
hclust19$Best.nc



#----- Gaussian Mixture Model----- using proportions
#https://journal.r-project.org/articles/RJ-2023-043/
#Probit transformation  in order to use GMM
acs19<- acs5_2019_wide  %>% select( !c( "Femalehousehold_2019_E","SNAP_2019_E","Two_or_more_rooms_2019_E", paste0("ms", 1:8), 
                                        "ms10", "ms_to", paste0("wc", 1:14), "wc_to","pov_to", paste0("pov", 2:5), "white_collar_occupation_2019",
                                        "HouseIncBlowPovLineP_2019", "Lessthan2rooms_2019_E",  "Two_or_more_rooms_2019_P","Total_housing_2019" ,
                                        "NH_some_other_race_2019", "NH_two_or_more_races_2019",  "NonHispanicWhite_2019" , "minoritystatus_2019"))

acs19_nomiss <- na.omit(acs19)
acs19_nomiss$medianincome_2019_2<- acs19_nomiss$medianincome_2019/max(acs19_nomiss$medianincome_2019, na.rm = T)
acs19a<-acs19_nomiss
acs19a[,3:19] <- acs19a[,3:19] / 100
#Check for Edge Cases
#Probit transformation is undefined for probabilities of 0 or 1, so make sure your data does not include these values.
dat19<-acs19a[,3:20]
dat19[dat19==0] <-0.0001
dat19[dat19==1] <-0.9999

probit_dat19<- apply(dat19,2, qnorm)
summary(probit_dat19)

####MCLUST#######
gmm.mclust19 <- Mclust(probit_dat19)
summary(gmm.mclust19, parameters = TRUE) #8 components!

#plot(gmm.mclust1, what = "classification")

