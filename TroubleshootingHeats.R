library(BayesBinMix)
library(foreach)
library(label.switching)
library(doParallel)
library(coda)

censusdata_bin <- readRDS("./censusdata_bin_raceeth_033024.rds")
names(censusdata_bin) <- c("acs5_2010_bin","acs5_2015_bin","acs5_2019_bin")
dataset<-as.matrix(censusdata_bin$acs5_2019_bin)



Kmax<-50
gamma<-rep((1/Kmax),Kmax)
m<-500
burnin<-100
nChains<-6
wd<-"/Users/carmenrodriguez/Desktop/temp/"
out.path<-paste0(wd, "acs19", sep= "_",Sys.Date() ,sep = "_",Kmax)
heats <- seq(1, 0.2, length = nChains)


# This produces a list and a folder in out.path
res<- coupledMetropolis(Kmax=Kmax, nChains = nChains, 
                        heats = heats,
                        binaryData = dataset, 
                        outPrefix = out.path,
                        m = m,
                        gamma = gamma, burn = burnin)













# Load required libraries
library(ggplot2)

# Function to compute truncated Poisson PMF
truncated_poisson <- function(lambda, k, lower, upper) {
  pmf <- dpois(k, lambda)
  pmf[k < lower | k > upper] <- 0  # Truncate PMF outside the range [lower, upper]
  return(pmf)
}

# Parameters
lambda <- 1  # Poisson parameter
lower <- 1   # Lower truncation limit
upper <- 50  # Upper truncation limit

# Generate values for k
k_values <- seq(lower, upper)

# Compute PMF for truncated Poisson
pmf_values <- truncated_poisson(lambda, k_values, lower, upper)

# Create a data frame for plotting
data <- data.frame(k = k_values, pmf = pmf_values)

# Plot
ggplot(data, aes(x = k, y = pmf)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Truncated Poisson Distribution",
       x = "k",
       y = "Probability") +
  theme_minimal()

