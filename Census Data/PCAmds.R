#for each census tract you have a vector of number between 0 and 1 (if I understand it right).
# I think we discussed two things: one was to do a heatmap of the resulting matrix, 
# with census tracts clustered but one of their default methods, and variable grouped by topic. 
# the other was to feed the matrix into something like PCA or MDS and plot the first two or three dimensions. 

library(MASS) 
library(factoextra)
library(ggplot2)
library(tidyverse)
library(cluster)
censusdata<- load("/Users/carmenrodriguez/Desktop/Research Projects/Census Data/censusdata_updated_newvars.RData")
 #Keep variables we need

acs5_2010_wide1 <- acs5_2010_wide  %>% select(!c("Femalehousehold_2010_E","SNAP_2010_E","Two_or_more_rooms_2010_E",  
                                                   paste0("ms", 1:8), "ms10", "ms_to", paste0("wc", 1:14), "wc_to",
                                                   "pov_to", paste0("pov", 2:5), "white_collar_occupation_2010", "HouseIncBlowPovLineP_2010", "Lessthan2rooms_2010_E", 
                                                   "Two_or_more_rooms_2010_P", "Total_occhousing_2010", "NH_some_other_race_2010",
                                                   "NH_two_or_more_races_2010",  "NonHispanicWhite_2010" , "minoritystatus_2010"))
names(acs5_2010_wide1)
acs5_2010_wide2<- as.data.frame(apply(acs5_2010_wide1[,3:19], 2, function(x) as.numeric(x/100)))
acs10<- as.data.frame(cbind(acs5_2010_wide1[,1:2],acs5_2010_wide2))
acs10_nomiss<- na.omit(acs10)


acs5_2015_wide1 <- acs5_2015_wide  %>% select(!c("Femalehousehold_2015_E","SNAP_2015_E","Two_or_more_rooms_2015_E",  
                                                 paste0("ms", 1:8), "ms10", "ms_to", paste0("wc", 1:14), "wc_to",
                                                 "pov_to", paste0("pov", 2:5), "white_collar_occupation_2015", "HouseIncBlowPovLineP_2015", "Lessthan2rooms_2015_E", 
                                                 "Two_or_more_rooms_2015_P", "Total_occhousing_2015", "NH_some_other_race_2015",
                                                 "NH_two_or_more_races_2015",  "NonHispanicWhite_2015" , "minoritystatus_2015"))
names(acs5_2015_wide1)
acs5_2015_wide2<- as.data.frame(apply(acs5_2015_wide1[,3:19], 2, function(x) as.numeric(x/100)))
acs15<- as.data.frame(cbind(acs5_2015_wide1[,1:2],acs5_2015_wide2))
acs15_nomiss<- na.omit(acs15)






#Run PCA
#Delete Cases with Missingness  
acs10_nomiss<- na.omit(acs10)
pca10 <- prcomp(acs10_nomiss[,2:19], scale = TRUE)

#Summary of Analysis 
summary(pca10)


#Elements of PCA object 
names(pca10)
#Eigenvectors-- 
pca10$rotation


fviz_eig(pca10, 
         addlabels = TRUE,
         ylim = c(0, 70))

#Biplot with Default Settings
fviz_pca_biplot(pca10)

#Biplot with Labeled Variables--
fviz_pca_biplot(pca10,
                label="var")


#Biplot with Customized Colored Groups and Variables
fviz_pca_biplot(pca10,
                label="var",
                col.var = "black") +
  scale_color_manual(values=c("orange", "purple"))




### MDS -- multi dimensional scaling
#https://www.statmethods.net/advstats/mds.html
#https://www.stat.pitt.edu/sungkyu/course/2221Fall13/lec8_mds_combined.pdf
#https://stackoverflow.com/questions/57854012/r-is-there-a-way-to-generate-a-heatmap-of-the-dissimilarity-matrix-only-and-sor


#2010

#Randomly select 200 census tracts
set.seed(1)
samp<-sample(acs10_nomiss$GEOID,200)
dat<- acs10_nomiss %>% filter(GEOID %in% samp)
dat1<-as.matrix(dat[,2:19])

#Dissimilarity Matrix
dissm<-daisy(dat1, metric = "euclidean", stand = TRUE)
summary(dissm)

#Kruskal's Non-metric Multidimensional Scaling
d <- dist(dat1) # euclidean distances between the rows
fit <- MASS::isoMDS(d, k=2) # k is the number of dim
#fit # view results

# plot solution
x <- as.numeric(fit$points[,1])
y <- as.numeric(fit$points[,2])
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = dat$GEOID , cex=.5)

#2015

dat2<- acs15_nomiss %>% filter(GEOID %in% samp) #n =198
dat3<-as.matrix(dat2[,2:19])

#Dissimilarity Matrix
dissm1<-daisy(dat3, metric = "euclidean", stand = TRUE)
summary(dissm1)

#Kruskal's Non-metric Multidimensional Scaling
d1 <- dist(dat3) # euclidean distances between the rows
fit1 <- MASS::isoMDS(d1, k=2) # k is the number of dim
#fit # view results

# plot solution
x1 <- as.numeric(fit1$points[,1])
y1 <- as.numeric(fit1$points[,2])
plot(x1, y1, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = dat2$GEOID , cex=.5)



