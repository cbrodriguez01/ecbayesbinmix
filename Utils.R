
#=======================================================
#Combined File with helper functions for generating plots, calculating posterior probabilities and cluster assignments
#Author: Carmen Rodriguez
#Last Updated: 6/8/24
#=======================================================

#Find index of highest probability
highest_index<-function(vec){
  s<-length(vec)
  vec1<-order(vec)
  return(vec1[s])
}

#Find second highest probability and index
second_highest<-function(vec){
  vec1<-sort(vec, decreasing = T)
  return(vec1[2])
}
second_highest_index<-function(vec){
  s<-length(vec)
  vec1<-order(vec)
  return(vec1[s-1])
}


############### Plots for model output ################
#02/1/24

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

# preparedat_fig<-function(reslst,mapK, sesvars){
#   n_ses<-length(sesvars)
#   dim<-n_ses*mapK
#   stats<-summary(reslst$parameters.ecr.mcmc)$statistics[,1] #get the mean, can also get SE and quantiles 
#   
#   temp<-as.data.frame(stats[1:dim])
#   
#   probs<-c()
#   indices <- seq(1, nrow(temp) * 1, by = mapK)
#   for (k in indices){
#     v<- temp[k:((k+mapK)-1),]
#     probs<-cbind(probs, v)
#   }
#   
#   prob_est<-cbind(1:mapK,probs)
#   colnames(prob_est)<-c("cluster",sesvars)
#   
#   #Long format for ggplot
#   prob_est_long<- prob_est %>% as.data.frame() %>% 
#     pivot_longer(!cluster, names_to = "NSES_VARS",values_to = "theta_kj")
#   
#   # Group variables
#   prob_est_long<-prob_est_long %>% mutate(NSES_group = case_when(
#     NSES_VARS %in% c("Crowded housing","Lack complete plumbing", "No vehicle","Owner-occupied","Renter-occupied", "Female household") ~ 1,
#     NSES_VARS %in% c("< HS", " >= HS", " >= Bacherlors") ~ 2,
#     NSES_VARS %in% c("Unemployment","Working class") ~ 4,
#     NSES_VARS %in% c("Median income","Below 150% poverty","SNAP benefits") ~ 3,
#     NSES_VARS %in% c("Hispanic or Latino", "NH Black", "NH Asian", "Limited EN Proficiency") ~ 5,))
#   
#   
#   prob_est_long$NSES_group<- factor(prob_est_long$NSES_group,levels = 1:5, labels = c("Household", "Education", "Income", "Occupation", "Ethnic Minorities and Language"))
#   
#   
#   prob_est_long$NSES_VARS<- factor(prob_est_long$NSES_VARS, levels = unique(prob_est_long$NSES_VARS[order(prob_est_long$NSES_group)]))
#   
#   return(prob_est_long)
# }

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
    NSES_VARS %in% c("Owner-occupied housing","Renter-occupied housing", "Crowded housing","Lack complete plumbing", "No vehicle") ~ 1,
    NSES_VARS %in% c("< HS", " >= HS", " >= Bacherlors") ~ 3,
    NSES_VARS %in% c("Median income","Below 150% poverty","SNAP benefits", "Unemployment","Working class","Female household") ~ 2,
    NSES_VARS %in% c("Limited EN Proficiency", "Hispanic/Latino", "NH Black", "NH Asian") ~ 4,))
  
  
  prob_est_long$NSES_group<- factor(prob_est_long$NSES_group,levels = 1:4, labels = c("Housing conditions and resources", "Economic security", "Educational attainment", "Sociocultural diversity"))
  
  
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


plot_thetakj_group<-function(prob_est_long, color_palette, fig_title, numR = mapK, numC= 1){
  
  # Generate plot
  prob_est_long %>% ggplot(aes(x = NSES_VARS, y = theta_kj, fill = NSES_group)) +
    geom_col()  +
    facet_wrap(~cluster, nrow = numR, ncol = numC) + 
    scale_fill_manual(values = color_palette) + 
    labs(title = fig_title, x= "",
         y = "Probability",
         fill = "Neighborhood SES Variables") +
    theme(text = element_text(size = 12),
          axis.text.x = element_text(size=10, angle=90, vjust = 0.75, hjust = 0.88), 
          axis.title.x = element_text(size = 8, color = "black", face = "bold"),
          axis.title.y = element_text(size = 9, color = "black", face = "bold"),
          axis.text.y = element_text(size=5), 
          legend.title = element_text(size = 8, color = "black", face = "bold"),
          legend.text = element_text(size = 8, color = "black"),
          legend.position = "top",
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
#1) Get proportions for level "1"
propbin<-function(dataset,  mapK, sesvars){
  binvars<- names(dataset)[1:18]
  props<-matrix(NA, nrow = length(binvars), ncol = mapK + 1)
  colnames(props)<-c("var", paste0("cluster", 1:mapK))
  for (i in 1:length(binvars)) {
    binvar<-binvars[i]
    # Create a contingency table
    tab<- table(dataset[["ClusterAssignment"]], dataset[[binvar]], useNA = "always")
    
    # Calculate proportions
    p <- prop.table(tab, margin = 1)[,2]  # proportions for == 1
    
    
    props[i,1]<-binvar
    props[i,2:(mapK+1)]<-p[1:mapK]
  }
  
  props2<-as.data.frame(t(props))
  colnames(props2)<-sesvars
  props2<-props2[-1,]
  #print(props2)
  props2$cluster<- as.factor(1:mapK)
  #props2$cluster<- as.factor(as.numeric( props2$cluster))
  #print(class(props2$cluster))
  
  props2_long<-props2 %>% pivot_longer(!cluster, names_to = "NSES_VARS", values_to = "prop")
  
  # Group variables
  prop2_long1<-props2_long %>% mutate(NSES_group = case_when(
    NSES_VARS %in% c("Crowded housing","Lack complete plumbing", "No vehicle","Owner-occupied","Renter-occupied", "Female household") ~ 1,
    NSES_VARS %in% c("< HS", " >= HS", " >= Bacherlors") ~ 2,
    NSES_VARS %in% c("Unemployment","Working class") ~ 4,
    NSES_VARS %in% c("Median income","Below 150% poverty","SNAP benefits") ~ 3,
    NSES_VARS %in% c("Hispanic or Latino", "NH Black", "NH Asian", "Limited EN Proficiency") ~ 5,))
  
  
  prop2_long1$NSES_group<- factor(prop2_long1$NSES_group,levels = 1:5, labels = c("Household", "Education", "Income", "Occupation", "Ethnic Minorities and Language"))
  
  prop2_long1$NSES_VARS<- factor(prop2_long1$NSES_VARS, levels = unique(prop2_long1$NSES_VARS[order(prop2_long1$NSES_group)]))
  prop2_long1$prop<-round(as.numeric(prop2_long1$prop),3)
  
  return(prop2_long1)
}

plot_barBin<-function(data,color_palette, fig_title,numR = mapK, numC= 1){
  
  data %>% ggplot(aes(x = NSES_VARS, y = prop, fill = NSES_group)) +
    geom_col()  +
    facet_wrap(~cluster,nrow = numR, ncol = numC) + 
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


############ Posterior Probabilitites ###############
#' Extract params
#' @param mapK infered # of cluster from model
#' @param reslst list containing output from bayesbinmix function
#'    From this we can get theta  mapK x d matrix containing theta_kj: Posterior mean of probability of success per feature and cluster (ECR algorithm):
#'    and pi- mixing weights:  Posterior means

extract_params<-function(mapK, reslst, sesvars){
  dim<-length(sesvars)*mapK
  stats<-summary(reslst$parameters.ecr.mcmc)$statistics[,1] #get the mean, can also get SE and quantiles 
  thetavec<- stats[1:dim]
  pivec<-tail(stats, mapK)
  thetamat<-t(matrix(thetavec, ncol=mapK, byrow = T))
  colnames(thetamat)<-sesvars
  
  return(list(thetamat, pivec))
}

#'Multivariate Bernoulli- Single observation
#'@param xi d-dimensional vector, (xi1, xi2,...,xid)--> row of X
#'@param theta_k parameters for mixture component/cluster k (single), row of theta matrix
#'@return Value of the Multivariate Bernoulli PDF for a single row 
mvb.pdf.i<- function(xi, theta_k){
  d<-length(theta_k)
  prod((theta_k^xi) *(1-theta_k)^(1-xi))
}

#' Multivariate Bernoulli PDF- n observations
#' @param X  Input data. n x d matrix
#' @param theta mapK x d matrix Posterior mean of probability of success per feature and cluster (ECR algorithm)
#' @return n x k matrix containing the values of the Multivariate Bernoullu PDF for all N obs and for each component 
mvb.pdf<-function(X, theta,mapK){
  n<-nrow(X)
  p_ij<-matrix(NA, nrow = n, ncol = mapK)
  for (i in 1:n){
    p_ij[i,]<-sapply(1:mapK, function(j) mvb.pdf.i(X[i,], theta[j,]))
  }
  return(p_ij)
}

#'Multivariate Bernoulli Mixture PDF for each component j
#'@param xi D-dimensional vector, (xi1, xi2,...,xid)--> row of X
#'@param theta mapK x d matrix Posterior mean of probability of success per feature and cluster (ECR algorithm)
#'@param pk vector of mixing proportions/probabilities 
#'@return Value of the Multivariate Bernoulli Mixture PDF for each component k, for data point xi
mbmm.pdf.k<-function(xi, theta, pk, mapK){
  sapply(1:mapK, function(j) pk[j]*mvb.pdf.i(xi, theta[j,]))
}

#'Multivariate Bernoulli Mixture model PDF - sum of all components
#'@param Xi D-dimensional vector, (xi1, xi2,...,xid)--> row of X
#'@param theta mapK x d matrix Posterior mean of probability of success per feature and cluster (ECR algorithm)
#'@param pk vector of mixing proportions/probabilities 
#'@return Value of the Multivariate Bernoulli Mixture PDF for a data point Xi

mbmm.pdf<-function(xi, theta, pk,mapK){
  sum(mbmm.pdf.k(xi, theta, pk, mapK))
}

#' Posterior probability to assign xi to one of the cluster/components k
#' @param mapK infered # of cluster from model
#' @param reslst list containing output from bayesbinmix function
#'    From this we can get theta  mapK x d matrix containing theta_kj: Posterior mean of probability of success per feature and cluster (ECR algorithm):
#'    and pi- mixing weights:  Posterior means

posteriorprob<-function(X,theta, pk, mapK){
  n<-nrow(X)
  d<-ncol(X)
  pZij<-matrix(NA, nrow = n, ncol = mapK)
  for (i in 1:n){
    pZij[i,]<- (mbmm.pdf.k(X[i,],theta, pk, mapK))/sum(mbmm.pdf.k(X[i,],theta, pk, mapK))
  }
  return(as.data.frame(pZij))
}


