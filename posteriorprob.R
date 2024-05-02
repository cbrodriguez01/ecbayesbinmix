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
  

#X<-acs10bin[1:5,1:18]
#g<-extract_params(12, res_acs10, sesvars)
#posteriorprob(X, theta= g[[1]], pk=g[[2]],12)



#Check for the 60 cts
flagcts<-cts10lw50$GEOID
X1<-acs10bin
X1$GEOID<- rownames(acs10bin)
X<-X1 %>% filter( GEOID %in% flagcts) %>% select(-c(GEOID))
g<-extract_params(12, res_acs10, sesvars)
flagposterior<-posteriorprob(X, theta= g[[1]], pk=g[[2]],12)
head(flagposterior)
head(cts10lw50[,1:12], 3)

maxProb_flagcts<-apply(flagposterior, 1, max)
clustertemp<-apply(flagposterior,1, highest_index)

flagposterior$maxProb_flagcts<-maxProb_flagcts
flagposterior$clustertemp<-clustertemp
table(flagposterior$clustertemp)
table(cts10lw50$ClusterAssignment_temp)
table(cts10lw50$ClusterAssignment_ecr)

table(cts10lw50$ClusterAssignment_temp,flagposterior$clustertemp)

flags_boxplot2<-flagposterior %>%  ggplot(aes(x = as.factor(clustertemp), y = maxProb_flagcts, fill = as.factor(clustertemp))) +
  geom_boxplot() +
  labs(fill = "Cluster Assignment", 
       y = "Assignment Probability (< 50%)", 
       x= "") +
  theme(text = element_text(size = 12),
        axis.ticks = element_blank(),
        axis.text.x =element_blank()) 

flags_boxplot

table1::table1(~maxProb_flagcts |as.factor(clustertemp), data= flagposterior, overall = T)
