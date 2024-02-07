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
    NSES_VARS %in% c("Two People Per Room","Lack of complete Plumbing", "No Vehicle","Owner", "Renter", "Female Household") ~ "Household",
    NSES_VARS %in% c(">= Bacherlor's Degree", "< High School", ">= High School") ~ "Education",
    NSES_VARS %in% c("Unemployment","White Collar Occupation") ~ "Occupation",
    NSES_VARS %in% c("Median Household Income","Below Poverty Line","SNAP Benefits") ~ "Income",))
  
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
          legend.position = "bottom")
  
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

plot_thetakj_group<-function(prob_est_long,mapK, color_palette, fig_title){
  # Generate plot
  prob_est_long %>% ggplot(aes(x = NSES_VARS, y = theta_kj)) +
    geom_col(aes(fill = as.factor(NSES_group))) +
    facet_wrap(~cluster, nrow = mapK) + scale_fill_manual(values = color_palette) + 
    labs(title = fig_title, x= "",   y = "Probability",fill = "Neighborhood SES variables") +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(size=6.8, angle =45, vjust = 0.5), 
          axis.title.x = element_text(size = 8, color = "black", face = "bold"),
          axis.title.y = element_text(size = 8, color = "black", face = "bold"),
          #axis.ticks = element_blank(),
          legend.title = element_text(size = 8, color = "black", face = "bold"),
          legend.text = element_text(size = 8, color = "black"),
          legend.position = "right")
  
  
}


