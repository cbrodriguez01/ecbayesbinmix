### binary data matrix
library(tidyverse)
library(ggplot2)
library(BayesBinMix)
library(coda)
library(DescTools)
library(reshape2)

#Load census data and results from latest model
acs10clust<- readRDS("./acs10clust_5.15.24.rds")

#Output from heating vector tuning-- 4/25/24-- had an issue with my binary data
res_all<-readRDS("./tuningheatsvec_4.25.24.rds")
#It seems that heating vector with deltaT = 0.025 (delta3) (temperature gap between adjacent chains) worked well for all datasets-- swap acceptance rates ranged from 
#30.5 - 25.7%. We do not want these rates to be too high like > 60:






# Keep only binary data
str(acs10clust)
#acs10clust$census_tract<-str_sub(acs10clust$GEOID,start=6, end = 11) 
BinMat1<-as.matrix(acs10clust[,c(1,19:37)])
BinMat2<-apply(BinMat1, 2, as.numeric)
#rownames(BinMat2)<-acs10clust$GEOID
colnames(BinMat2)<-c("GEOID","Median income", "Female household", "< HS", " >= HS", " >= Bacherlors", "Limited EN Proficiency", "Unemployment", "SNAP benefits", "Owner-occupied", "Renter-occupied", 
                     "No vehicle", "Hispanic or Latino", "NH Black", "NH Asian", "Crowded housing", "Below 150% poverty", "Working class", "Lack complete plumbing", "clusters")
BinMat3<-as.data.frame(BinMat2)
#clusters<-acs10clust$ClusterAssignment
#Order rows based on cluster assignment
ordered_BinMat <- BinMat3[order(BinMat3$clusters), ]
#ordered_BinMat1<- ordered_BinMat %>% select(!c("clusters"))

BinMat_OL<-melt(ordered_BinMat, id.vars = c("GEOID", "clusters"))
colnames(BinMat_OL) <- c("GEOID","cluster", "SDoH", "Value")

# Create a factor for rows to maintain the order in the plot
BinMat_OL$GEOID <- factor(BinMat_OL$GEOID, levels = unique(BinMat_OL$GEOID))


#Plot # 1- just the binary data

ggplot(BinMat_OL, aes(x = SDoH, y = GEOID, fill = factor(Value))) +
  geom_tile(color = "white") +  # Use white lines to separate the tiles
  scale_fill_manual(values = c("0" = "#bcbd22", "1" = "salmon1")) +  # Color scale
  labs(fill = "Value", x = "SDoH variables", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")

# Plot with clusters
ggplot(BinMat_OL, aes(x = SDoH, y = GEOID, fill = factor(Value))) +
  geom_tile(color = "white") +
  #facet_grid(cluster ~ ., scales = "free_y", space = "free") +  # Facet by cluster
  scale_fill_manual(values = c("0" = "#bcbd22", "1" = "salmon1")) +
  labs(fill = "Value", x = "SDoH variables", y = "Census-tracts") +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())  # Rotate facet labels if necessary



#Plot without re-ordering based on clusters

BinMat_OL2<-melt(BinMat3, id.vars = c("GEOID", "clusters"))
colnames(BinMat_OL2) <- c("GEOID","cluster", "SDoH", "Value")

# Create a factor for rows to maintain the order in the plot
BinMat_OL2$GEOID <- factor(BinMat_OL2$GEOID, levels = unique(BinMat_OL2$GEOID))


#Plot # 1- just the binary data

ggplot(BinMat_OL2, aes(x = SDoH, y = GEOID, fill = factor(Value))) +
  geom_tile(color = "white") +  # Use white lines to separate the tiles
  scale_fill_manual(values = c("0" = "#bcbd22", "1" = "salmon1")) +  # Color scale
  labs(fill = "Value", x = "SDoH variables", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")


