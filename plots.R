#' 2/1/24
#'  @author Carmen

#Generate pastel colors for plot
generate_pastel_colors <- function(n_ses){
  hues <- seq(0, 1, length.out = n_ses + 1)[-1]  # Equally spaced hues
  s <- 0.4  # Saturation for pastel colors
  v <- 0.9  # Brightness for pastel colors
  hsv(h = hues, s = s, v = v)
}


#pastel_palette <- generate_pastel_colors(n_ses)

#' @description
#' Summary plot for success probabilities/cluster profiles
#' @param reslst list containing output from bayesbinmix function
#' @param  mapK most probable K 
#' @param sesvars vector with variable names
#' @color_palette colors for the plot, vector needs to be the same lenght as sesvars
#' 
#' #' @return
#' Returns a `ggplot2` object displaying a probabilities of each ses var and cluster.
#' 

plot_thetakj<-function(reslst,mapK, sesvars, color_palette){
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
  
  
  prob_est_long<- prob_est1 %>% pivot_longer(!cluster, names_to = "NSES_VARS",values_to = "theta_kj")
  
  
  # Figure title
  fig_title = paste("Success Probabilities of Neighborhood SES variables when mapK=", mapK, sep="")
  
  # Generate plot
  prob_est_long %>% ggplot(aes(x = NSES_VARS, y = theta_kj)) +
    geom_col(aes(fill = NSES_VARS)) +
    facet_wrap(~cluster) + scale_fill_manual(values = color_palette) + 
    labs(title = fig_title, x= "Neighborhood SDOH variables",   y = "Probability",fill = "") +
    theme(text = element_text(size = 10),
          axis.text.x = element_blank(), 
          axis.title.x = element_text(size = 8, color = "black", face = "bold"),
          axis.title.y = element_text(size = 8, color = "black", face = "bold"),
          legend.title = element_text(size = 8, color = "black", face = "bold"),
          legend.text = element_text(size = 6, color = "black"),
          legend.position = "bottom")
  
}