#ARCHIVE

#Generate bar plots using estimated theta_kj and observed data
#Need to be in order
#names(censusdata_bin$acs5_2019_bin)
sesvars<-c("Median income", "Female household", "< HS", " >= HS", " >= Bacherlors", "Limited EN Proficiency", "Unemployment", "SNAP benefits", "Owner-occupied", "Renter-occupied", 
           "No vehicle", "Hispanic or Latino", "NH Black", "NH Asian", "Crowded housing", "Below 150% poverty", "Working class", "Lack complete plumbing")

group_cols<-c("#1f77b4","#2ca02c","salmon1", "#bcbd22", "#e377c2")


# Generate 14 pastel colors for ungrouped graph
pastel_palette1 <- generate_pastel_colors(18)


#fig_title = paste("Success Probabilities when mapK=", mapK, sep="")
fig_title = ""
dat_19<-preparedat_fig(res19_poi,mapK_poi,sesvars)
dat_19$cluster <- factor(dat_19$cluster, levels = 1:9, labels = c(
  "Cluster 1: 401", "Cluster 2: 74", "Cluster 3: 378", "Cluster 4: 94",
  "Cluster 5: 26", "Cluster 6: 109", "Cluster 7: 86", "Cluster 8: 143", "Cluster 9: 167"
))

# Generate plot
barplot<-dat_19 %>% ggplot(aes(x = NSES_VARS, y = theta_kj, fill = NSES_group)) +
  geom_col()  +
  facet_wrap(~cluster, nrow = 9, ncol = 1) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = fig_title, x= "",
       y = "Probability",
       fill = "Neighborhood SES Variables") +
  theme(strip.text = element_text(size = 25),
        text = element_text(size = 25),
        axis.text.x = element_text(size=25, angle=45, vjust = 0.88, hjust = 0.88), 
        #axis.title.x = element_text(size = 18, color = "black", face = "bold"),
        axis.title.y = element_text(size = 28, color = "black", face = "bold"),
        axis.text.y = element_text(size=22), 
        legend.title = element_text(size = 28, color = "black", face = "bold"),
        legend.text = element_text(size = 25, color = "black"),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5))

barplot1<-dat_19 %>% ggplot(aes(x = NSES_VARS, y = theta_kj, fill = NSES_group)) +
  geom_col()  +
  facet_wrap(~cluster, nrow = 5, ncol = 2) + 
  scale_fill_manual(values = group_cols) + 
  labs(title = fig_title, x= "",
       y = "Probability",
       fill = "Neighborhood SES Variables") +
  theme(strip.text = element_text(size = 14),
        text = element_text(size = 18),
        axis.text.x = element_text(size=16, angle=90, vjust = 0.88, hjust = 0.88), 
        #axis.title.x = element_text(size = 18, color = "black", face = "bold"),
        axis.title.y = element_text(size = 18, color = "black", face = "bold"),
        axis.text.y = element_text(size=12), 
        legend.title = element_text(size = 18, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5))


jpeg("/Users/carmenrodriguez/Desktop/Research Projects/Conferences/ISBA 2024/Poster/barplot19.jpeg", width = 1500, height = 1800)
barplot
dev.off()
jpeg("/Users/carmenrodriguez/Desktop/Research Projects/Conferences/ISBA 2024/Poster/barplot19_2cols.jpeg", width = 1100, height = 1000)
barplot1
dev.off()
png("/Users/carmenrodriguez/Desktop/Research Projects/Conferences/ISBA 2024/Poster/barplot19_2cols.png", width = 1100, height = 1000)
barplot1
dev.off()
