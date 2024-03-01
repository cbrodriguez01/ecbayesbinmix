#Checking the sparsity of the Dirichlet prior on the mixing weights
#https://stats.stackexchange.com/questions/239216/what-is-meant-by-non-convex-prior-and-sparsity-inducing-prior

K<-10
alpha<- c(1,0.5, 1/(seq(10,60, by = 5)))

pd<-list()
for (i in 1:length(alpha)){
   alphavec<-rep(alpha[i], K)
   pdir<- list(BayesBinMix::myDirichlet(alphavec))
  pd<-append(pd, pdir)
}

#Note that as alpha decreases so does the number of vector elements with much appreciable mass, thus inducing sparsity
