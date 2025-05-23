## Census data set prep for BayesBinMix
#Last updated: 04/10/25

library(tidycensus)
library(tidyverse)
library(Hmisc)
library(reshape2)
library(ggplot2)
library(psych)
library(table1)
library(Hmisc)
getwd()

load("/Users/carmenrodriguez/Desktop/Research Projects/Census Data/Census Data Revisions/censusdata_updated_acs2019.RData")


###-------DATASET SET UP ----------###
#----------------Make all variable binary dataset (ACS 2019)
##2015-2019
names(acs2019)
acs2019a<- as.data.frame(apply(acs2019[,3:16], 2, function(x) as.numeric(x)))
#Add GEOID and NAME back
acs2019a<-cbind(acs2019[,1:2], acs2019a)
str(acs2019a)

#Check variable two or more rooms
median(acs2019a$Crowding_housing_2019, na.rm = T) #1.28%
#check lack of plumbing
summary(acs2019a$Lackplumbing_2019) #-zeroes mean they have complete plumbing--we classify this as 0, and 1 as lack of
table(acs2019a$Lackplumbing_2019)
lcp<-ifelse(is.na(acs2019a$Lackplumbing_2019), NA, ifelse(acs2019a$Lackplumbing_2019 == 0, 0, 1))

#Exclude Lack of plumbing  
acs2019b<- acs2019a %>% select(!c("Lackplumbing_2019"))
acs_2019_medians<- cbind(names(acs2019b[3:length(acs2019b)]),sapply(3:length(acs2019b), function(i) median(acs2019b[,i], na.rm = TRUE)))

#Create the binary variables, 1 if above or equal to median for MA and 0 if below (vars 3:17)  and then we append lack of p.
acs_2019_bin<- as.data.frame(sapply(3:length(acs2019b), function(i) {ifelse(is.na(acs2019b[,i]), NA, ifelse(as.numeric(acs2019b[,i]) >= as.numeric(acs_2019_medians[i-2,2]),1,0))}))


colnames(acs_2019_bin)<-names(acs2019b[3:length(acs2019b)])
#Add lack of plumbing and GEOID and NAME
acs_2019_bin1<-cbind(acs2019b[,1:2],acs_2019_bin)
acs_2019_bin1$Lackplumbing_2019<-lcp

#Reverse code income
acs_2019_bin1$medianincome_2019<- ifelse(acs_2019_bin1$medianincome_2019 == 1, 0,1)
table(acs_2019_bin1$medianincome_2019)



rows_with_all_missing19 <- which(rowSums(is.na(acs_2019_bin1)) == ncol(acs_2019_bin1)-2)
#[1] "25001990000" "25005990000" "25007990000" "25009990100" "25017980000" "25019990000" "25023990003" "25025980700" "25025981000" "25025981201" "25025981501" "25025981502" "25025981600" "25025981700"
#14 census tracts missing data for all variables








###################################################################################################################
colnames(acs_2019_bin1)<- c("GEOID", "NAME", "medianincome", "Femalehousehold" ,"NoHSDiploma", "crowded_housing","working_class", 
                        "Unemployement","RenterOccupiedUnit", "No_vehicle","NHB", "NHA", "Hispanic","ENProficiency", "SNAP", "lcplumbing")


###################################################################################################################
###################################################################################################################
## Save binary dataset
saveRDS(acs_2019_bin1, "/Users/carmenrodriguez/Desktop/Research Projects/Census Data/Census Data Revisions/censusdata_bin.rds")

#Export medians as a table
write.table(acs_2019_medians, file = "ACS_medians2.txt",sep = ",", quote = FALSE, row.names = F)
###################################################################################################################
###################################################################################################################
###################################################################################################################





