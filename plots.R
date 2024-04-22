#' 2/1/24
#'  @author Carmen

#Generate pastel colors for plot
generate_pastel_colors <- function(n_ses){
  hues <- seq(0, 1, length.out = n_ses + 1)[-1]  # Equally spaced hues
  s <- 0.4  # Saturation for pastel colors
  v <- 0.9  # Brightness for pastel colors
  hsv(h = hues, s = s, v = v)
}


#' @description
#' Create the long format dataset that has the estimated mean success probabilities and labels for the different SES variables and groups
#' 
#' @param reslst list containing output from bayesbinmix function
#' @param  mapK most probable K  from the model
#' @param sesvars vector with variable names 
#' @return a long-format dataset to be the input for profile figures

preparedat_fig<-function(reslst,mapK, sesvars){
  n_ses<-length(sesvars)
  dim<-n_ses*mapK
  stats<-summary(reslst$parameters.ecr.mcmc)$statistics[,1] #get the mean, can also get SE and quantiles 
  
  temp<-as.data.frame(stats[1:dim])
  
  probs<-c()
  indices <- seq(1, nrow(temp) * 1, by = mapK)
  for (k in indices){
    v<- temp[k:((k+mapK)-1),]
    probs<-cbind(probs, v)
  }
  
  prob_est<-cbind(1:mapK,probs)
  colnames(prob_est)<-c("cluster",sesvars)
  
  #Long format for ggplot
  prob_est_long<- prob_est %>% as.data.frame() %>% 
    pivot_longer(!cluster, names_to = "NSES_VARS",values_to = "theta_kj")
  
  # Group variables
  prob_est_long<-prob_est_long %>% mutate(NSES_group = case_when(
    NSES_VARS %in% c("Crowded housing","Lack complete plumbing", "No vehicle","Owner-occupied","Renter-occupied", "Female household") ~ 1,
    NSES_VARS %in% c("< HS", " >= HS", " >= Bacherlors") ~ 2,
    NSES_VARS %in% c("Unemployment","Working class") ~ 4,
    NSES_VARS %in% c("Median income","Below 150% poverty","SNAP benefits") ~ 3,
    NSES_VARS %in% c("Hispanic or Latino", "NH Black", "NH Asian", "Limited EN Proficiency") ~ 5,))
  
  
  prob_est_long$NSES_group<- factor(prob_est_long$NSES_group,levels = 1:5, labels = c("Household", "Education", "Income", "Occupation", "Ethnic Minorities and Language"))


  prob_est_long$NSES_VARS<- factor(prob_est_long$NSES_VARS, levels = unique(prob_est_long$NSES_VARS[order(prob_est_long$NSES_group)]))
    
  return(prob_est_long)
}



#' @description
#' Summary plot for success probabilities/cluster profiles
#' @param prob_est_long long-format dataset containing the estimated mean success probabilities and labels for the different SES variables and groups
#' @param  mapK most probable K  from the model
#' @param fig_title  string specifying the title of the figure
#' @param color_palette colors for the plot, vector needs to be the same length as the variable NSES_VARS in prob_long_est.
#' 
#' #' @return
#' Returns a `ggplot2` object displaying a probabilities of each ses var and cluster.
#' 

plot_thetakj_indiv<-function(prob_est_long,mapK, color_palette, fig_title){
  # Generate plot
  prob_est_long %>% ggplot(aes(x = NSES_VARS, y = theta_kj)) +
    geom_col(aes(fill = NSES_VARS)) +
    facet_wrap(~cluster, nrow = mapK) + scale_fill_manual(values = color_palette) + 
    labs(title = fig_title, x= "Neighborhood SES variables",   y = "Probability",fill = "") +
    theme(text = element_text(size = 10),
          axis.text.x = element_blank(), 
          axis.title.x = element_text(size = 8, color = "black", face = "bold"),
          axis.title.y = element_text(size = 8, color = "black", face = "bold"),
          legend.title = element_text(size = 8, color = "black", face = "bold"),
          legend.text = element_text(size = 6, color = "black"),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  
}


#' @description
#' Summary plot for success probabilities/cluster profiles - grouped-
#' @param prob_est_long long-format dataset containing the estimated mean success probabilities and labels for the different SES variables and groups
#' @param  mapK most probable K  from the model
#' @param fig_title  string specifying the title of the figure
#' @param color_palette colors for the plot, vector needs to be the same length as the variable NSES_groups in prob_long_est.
#' 
#' #' @return
#' Returns a `ggplot2` object displaying a probabilities of each ses var and cluster by NSES assigned group
#' 
#aes(fill = as.factor(NSES_group))


plot_thetakj_group<-function(prob_est_long,mapK, color_palette, fig_title, numR = mapK, numC= 1){

  # Generate plot
  prob_est_long %>% ggplot(aes(x = NSES_VARS, y = theta_kj, fill = NSES_group)) +
    geom_col()  +
    facet_wrap(~cluster, nrow = numR, ncol = numC) + 
    scale_fill_manual(values = color_palette) + 
    labs(title = fig_title, x= "",
         y = "Probability",
         fill = "Neighborhood SES Variables") +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(size=9, angle=90, vjust = 0.75, hjust = 0.88), 
          axis.title.x = element_text(size = 8, color = "black", face = "bold"),
          axis.title.y = element_text(size = 8, color = "black", face = "bold"),
          #axis.ticks = element_blank(),
          legend.title = element_text(size = 8, color = "black", face = "bold"),
          legend.text = element_text(size = 8, color = "black"),
          legend.position = "right",
          plot.title = element_text(hjust = 0.5))
  
  
}
#6.8

plot_thetakj_group_flipped<-function(prob_est_long,mapK, color_palette, fig_title){
  
  # Generate plot
  prob_est_long %>% ggplot(aes(x = NSES_VARS, y = theta_kj, fill = NSES_group)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~cluster, nrow = mapK) + 
    scale_fill_manual(values = color_palette) + 
    labs(title = fig_title, x= "",  
         y = "Probability",
         fill = "Neighborhood SES Variables") +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(size=8), 
          axis.title.x = element_text(size = 5, color = "black", face = "bold"),
          axis.title.y = element_text(size = 8, color = "black", face = "bold"),
          #axis.ticks = element_blank(),
          legend.title = element_text(size = 8, color = "black", face = "bold"),
          legend.text = element_text(size = 8, color = "black"),
          legend.position = "right",
          plot.title = element_text(hjust = 0.5))
  
  
}



#Make plot as a heatmap

plot_thetakj_heatmap<-function(prob_est_long, fig_title){

  prob_est_long %>% ggplot(aes(x = as.factor(cluster),y = NSES_VARS,fill = theta_kj)) +
  geom_tile() + 
  geom_text(aes(label = round(theta_kj,2)), color = "black", size = 4) +
  xlab(label = "Cluster") + ggtitle(fig_title) +
  scale_fill_viridis(name = "Probability") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) # Remove y-axis title

}

#Reordered
plot_thetakj_heatmap_r<-function(prob_est_long, fig_title){
  
  prob_est_long %>% ggplot(aes(x = as.factor(cluster),y = reorder(NSES_VARS, theta_kj),fill = theta_kj)) +
    geom_tile() + 
    geom_text(aes(label = round(theta_kj,2)), color = "black", size = 4) +
    xlab(label = "Cluster") + ggtitle(fig_title) +
    scale_fill_viridis(name = "Probability") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank()) # Remove y-axis title
  
}




#Bar graph of distribution of binary variables across clusters
plot_barBin<-function(data, mapK, color_palette, fig_title){
  
  
  
  
  
  
  
}





