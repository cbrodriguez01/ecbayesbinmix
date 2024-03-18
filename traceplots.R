
library(BayesBinMix)

out<-readRDS("./ACSall_3.10.24.rds")
acs15<-out$res_acs15_50

#Visualize the output using coda package
#Trace plots and histogram of theta_kj, p_k for 1st component
summary(acs15$parameters.ecr.mcmc)
mat1 <- matrix(c(1:4), byrow = TRUE, ncol = 2)
layout(mat1, widths = rep(c(2, 1), 2), heights = rep(1, 4))
mcmcSubset <- acs15$parameters.ecr.mcmc[ , c("theta.2.1", "p.2")]
plot(mcmcSubset, auto.layout = FALSE, ask = FALSE, col = "gray40")


summary(out$parameters.ecr.mcmc)
mat1 <- matrix(c(1:8), byrow = TRUE, ncol = 2)
layout(mat1, widths = rep(c(2, 1), 2), heights = rep(1, 4))
mcmcSubset <-  acs15$parameters.ecr.mcmc[ , c("theta.2.2", "theta.2.3","theta.2.4", "theta.2.5")]
plot(mcmcSubset, auto.layout = FALSE, ask = FALSE, col = "gray40")



