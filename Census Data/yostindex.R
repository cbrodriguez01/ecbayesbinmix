#Yost Index for ACS 2006-2010, 2011-2015 and 2015-2019 for Massachussets Census Tracts
#03/28/2024
#Following code included in the paper "Estimating uncertainty    
# in a socioeconomic index derived from the American Community Survey"  
# by Boscoe et al. 2022

library(tidycensus)
library(tidyverse)
library(data.table)
library(psych)
library(ggplot2)
library(ggExtra)
library(leaflet)
library(maps)
library(tigris)
library(scales)
`%!in%` <- Negate(`%in%`)
#Census API
readRenviron("~/.Renviron") #run this to use  API key
census_api_key("39161c3ce4709a7b70320416e2762a2ace0e66d0")
#Check API key
Sys.getenv("CENSUS_API_KEY")

#Example of variable search using API through tidycensus
v10_acsprofile<-load_variables(2010, "acs5/profile", cache = TRUE)
v10_acstable<-load_variables(2010, "acs5", cache = TRUE)


##------ Yost Index: 2006-2010 ACS 5-year estimates-- ---

##Extract data using tidycensus package and Census API
yvars10<-c( "B25077_001", "B25064_001",
            "B19013_001", "C17002_001", "C17002_002", "C17002_003","C17002_004","C17002_005", "B15002_001", paste0("B15002_00", sep="", 3:9), "B15002_010",
           paste0("B15002_02", sep="", 0:7), "B15002_011", "B15002_028",
           paste0("B15002_01", sep="", 2:8), paste0("B15002_02", sep="", 8:9),paste0("B15002_03", sep="", 0:5), "C24010_020" , "C24010_024" , "C24010_025","C24010_026",
 "C24010_027" , "C24010_030","C24010_034",  "C24010_056", "C24010_060","C24010_061" , "C24010_062" , "C24010_063" , "C24010_066" , "C24010_070", "C24010_001","DP03_0005P")

acs5_2010_yost<-get_acs(geography = "tract", state="MA", variables = yvars10, year = 2010) 
acs5_2010_wide_yost<-acs5_2010_yost %>% select("GEOID","NAME","variable","estimate") %>% 
  pivot_wider(id_cols = "GEOID",names_from = variable,values_from = estimate)
str(acs5_2010_wide_yost)

#Construct the variables needed for the index- following SEER <https://seer.cancer.gov/seerstat/variables/countyattribs/time-dependent.html>
#median rent, median income,median home value,unemployment rate,poverty rate (150% poverty rate), education index,working class employment

acs5_2010_wide_yost<-acs5_2010_wide_yost %>% rename(inc = B19013_001,
                                                    unemp= DP03_0005P,
                                                    rent = B25064_001,
                                                    houseval =B25077_001)
                                                    

acs5_2010_wide_yost<-acs5_2010_wide_yost %>% mutate(pov = ((C17002_002 + C17002_003 + C17002_004 + C17002_005) / C17002_001) * 100,
                                                    lhs= ((B15002_003 + B15002_004 + B15002_005 + B15002_006 + B15002_007 + B15002_008 + B15002_009
                                                          + B15002_010 +  B15002_020+B15002_021+B15002_022+B15002_023+B15002_024+B15002_025+B15002_026+B15002_027)/B15002_001)*100,
                                                    hs= ((B15002_011+B15002_028)/B15002_001) * 100,
                                                    hhs=  ((B15002_012+B15002_013+B15002_014+B15002_015+B15002_016+B15002_017+B15002_018 +  
                                                              B15002_028+B15002_029 + B15002_030+B15002_031+B15002_032+B15002_033+B15002_034+B15002_035)/B15002_001) * 100,
                                                    edind= (lhs * 9) + (hs*12) + (hhs* 16),
                                                    wc= ((C24010_020 + C24010_024 + C24010_025 + C24010_026 + C24010_027 + C24010_030 + C24010_034 + C24010_056 +
                                                            C24010_060 + C24010_061 + C24010_062 + C24010_063 + C24010_066 + C24010_070) / C24010_001) * 100)
                                                    
yostdat10<-acs5_2010_wide_yost %>% select(GEOID, inc, unemp, rent, houseval, pov, edind,wc)
summary(yostdat10)



                                           
completes <- yostdat10 %>% group_by(GEOID) %>%
summarize(ntract=n())

#Assign ranks to each measure, distinguishing between variables where higher number = more affluent         
# and where higher number = less affluent  

#Make into long format for this step as it is easier
yostdat10_long<-yostdat10 %>% pivot_longer(cols = 2:8, names_to = "var", values_to = "est")
completes <- yostdat10_long %>% group_by(GEOID) %>%
  summarize(ntract=n())

#Make sure we have complete data for all Cts
completes <- filter(completes,ntract==7)

complete <- nrow(completes)

yost02 <- left_join(completes,yostdat10_long,by="GEOID")
yost02 <- select(yost02,-ntract)

yost03a <- filter(yost02,var %in% c('pov','unemp','wc'))
yost03b <- filter(yost02,var %in% c('edind','rent',"houseval",'inc'))


# assigns the ranks -- for these variables higher values mean less affluent-- so the higher the rank the less affluent
mutate_a <- function(data_a, new_name, name1){
  data_a %>% 
    group_by(var) %>%
    mutate(UQ(rlang::sym(new_name)) :=  rank(UQ(rlang::sym(name1)))) 
}
#Higher values implies more affluent
mutate_b <- function(data_b, new_name, name1){
  data_b %>% 
    group_by(var) %>%
    mutate(UQ(rlang::sym(new_name)) :=  complete-rank(UQ(rlang::sym(name1))))
}
rankvar <- "rank"
estvar <- "est"
tempa <- mutate_a(yost03a,eval(rankvar), eval(estvar))
tempb <- mutate_b(yost03b,eval(rankvar), eval(estvar))
tempa <- ungroup(tempa)
tempb <- ungroup(tempb)
tempa <- select(tempa,eval(rankvar))
tempb <- select(tempb,eval(rankvar))
yost03a <- cbind(yost03a,tempa)
yost03b <- cbind(yost03b,tempb)

yost04 <- rbind(yost03a,yost03b)
yost04 <- select(yost04,GEOID,var,rank)
yost04 <- arrange(yost04,GEOID,var)


#Transpose the data to wide format again
yost05 <- pivot_wider(yost04, id_cols=GEOID, names_from=var, values_from=rank)
yost05 <- arrange(yost05,GEOID)
GEOID <- yost05$GEOID

# Perform factor analysis-
#fm="ml" will do a maximum likelihood factor analysis

#Scale data before conducting FA-- gives same result if raw data used
#yost05a<-scale(yost05 %>% select(-GEOID))

fa_yost <- fa(yost05 %>% select(-GEOID),fm="ml")
#Get factor scores and group them into quintiles
yost06 <- mutate(arrange(as.data.frame(cbind(GEOID, fa_yost$scores)), ML1), row=row_number())
yost06$pctile <- ceiling((yost06$row-0.0001)/(nrow(yost06)/100))
yost06a <- arrange(yost06,GEOID)


#From SEER:The first quintile (the group with the lowest SES) is the 20th centile or less, and the fifth quintile
#(the group with the highest SES) corresponds to the 80th centile or higher. The way we made the score following Boscoe et al.,
#we have the opposite, where 1=most affluent and 100=most deprived.
quintiles <- quantile(yost06a$pctile, probs = 0:5/5)
#yostquintilesb<-cut(yost06a$pctile, breaks =quintiles, include.lowest = F, labels = F)--missing 14 obs

yost06b<-yost06a %>% mutate(yostquintiles10 = case_when(
  pctile <= 21 ~ 1,
  pctile > 21 & pctile <= 40.8 ~ 2,
  pctile > 40.8 & pctile <= 61 ~ 3,
  pctile > 61 & pctile < 80.6 ~ 4,
  pctile >= 80.6 ~ 5,
))
yost06b$yostquintiles10<-factor(yost06b$yostquintiles,levels =1:5, labels = c("Highest","High-middle","Middle","Lower-middle", "Lowest"))
table(yost06b$yostquintiles10)
yostindex10<-copy(yost06b)


###We follow same steps as above for
## 2015-2019  ACS 2019 5-year
## 2011-2015, ACS 2015 5-year
#We could write a function later
yvars15<-c("B25077_001","B25064_001",
           "B19013_001", "C17002_001", "C17002_002", 
           "C17002_003","C17002_004","C17002_005", "B15002_001", paste0("B15002_00", sep="", 3:9), "B15002_010",
           paste0("B15002_02", sep="", 0:7), "B15002_011", "B15002_028",
           paste0("B15002_01", sep="", 2:8), paste0("B15002_02", sep="", 8:9), 
           paste0("B15002_03", sep="", 0:5), "C24010_020" , "C24010_024" , "C24010_025" ,
           "C24010_026" , "C24010_027" , "C24010_030" , "C24010_034" , "C24010_056" , "C24010_060" , 
           "C24010_061" , "C24010_062" , "C24010_063" , "C24010_066" , "C24010_070", "C24010_001",
           "DP03_0005P")

yvars19<-c("B25077_001", "B25064_001",
          "B19013_001", "C17002_001", "C17002_002", 
           "C17002_003","C17002_004","C17002_005", "B15002_001", paste0("B15002_00", sep="", 3:9), "B15002_010",
           paste0("B15002_02", sep="", 0:7), "B15002_011", "B15002_028",
           paste0("B15002_01", sep="", 2:8), paste0("B15002_02", sep="", 8:9), 
           paste0("B15002_03", sep="", 0:5), "C24010_020" , "C24010_024" , "C24010_025" ,
           "C24010_026" , "C24010_027" , "C24010_030" , "C24010_034" , "C24010_056" , "C24010_060" , 
           "C24010_061" , "C24010_062" , "C24010_063" , "C24010_066" , "C24010_070", "C24010_001",
           "DP03_0005P")

#Download data
acs5_2015_yost<-get_acs(geography = "tract", state="MA", variables = yvars15, 
                        year = 2015) 
acs5_2015_wide_yost<-acs5_2015_yost %>% select("GEOID","variable","estimate") %>% 
  pivot_wider(names_from = variable,values_from = estimate)

acs5_2019_yost<-get_acs(geography = "tract", state="MA", variables = yvars19, 
                        year=2019) 
acs5_2019_wide_yost<-acs5_2019_yost %>% select("GEOID","variable","estimate") %>% 
  pivot_wider(names_from = variable,values_from = estimate)

## 2011-2015  ACS 2019 5-year
  #Define new variables
acs5_2015_wide_yost<-acs5_2015_wide_yost%>% rename(inc = B19013_001,unemp= DP03_0005P,
                                                rent = B25064_001,
                                              houseval =B25077_001)
  
  
acs5_2015_wide_yost<-acs5_2015_wide_yost %>% mutate(pov = ((C17002_002 + C17002_003 + C17002_004 + C17002_005) / C17002_001) * 100,
                                                      lhs= ((B15002_003 + B15002_004 + B15002_005 + B15002_006 + B15002_007 + B15002_008 + B15002_009
                                                             + B15002_010 +  B15002_020+B15002_021+B15002_022+B15002_023+B15002_024+B15002_025+B15002_026+B15002_027)/B15002_001)*100,
                                                      hs= ((B15002_011+B15002_028)/B15002_001) * 100,
                                                      hhs=  ((B15002_012+B15002_013+B15002_014+B15002_015+B15002_016+B15002_017+B15002_018 +  
                                                                B15002_028+B15002_029 + B15002_030+B15002_031+B15002_032+B15002_033+B15002_034+B15002_035)/B15002_001) * 100,
                                                      edind= (lhs * 9) + (hs*12) + (hhs* 16),
                                                      wc= ((C24010_020 + C24010_024 + C24010_025 + C24010_026 + C24010_027 + C24010_030 + C24010_034 + C24010_056 +
                                                              C24010_060 + C24010_061 + C24010_062 + C24010_063 + C24010_066 + C24010_070) / C24010_001) * 100)
  
  yostdat15<-acs5_2015_wide_yost %>% select(GEOID, inc, unemp, rent, houseval, pov, edind,wc)
   
  ##----RANKING---- 
  #Make into long format for this step as it is easier
  yostdat15_long<-yostdat15 %>% pivot_longer(cols = 2:8, names_to = "var", values_to = "est")
  completes <- yostdat15_long %>% group_by(GEOID) %>%
    summarize(ntract=n())
  
  #Make sure we have complete data for all Cts
  completes <- filter(completes,ntract==7)
  complete <- nrow(completes)
  
  yost01 <- left_join(completes,yostdat15_long,by="GEOID")
  yost01 <- select(yost01,-ntract)
  
  yost02a <- filter(yost01,var %in% c('pov','unemp','wc'))
  yost02b <- filter(yost01,var %in% c('edind','rent',"houseval",'inc'))
  
  rankvar <- "rank"
  estvar <- "est"
  tempa <- mutate_a(yost02a,eval(rankvar), eval(estvar))
  tempb <- mutate_b(yost02b,eval(rankvar), eval(estvar))
  tempa <- ungroup(tempa)
  tempb <- ungroup(tempb)
  tempa <- select(tempa,eval(rankvar))
  tempb <- select(tempb,eval(rankvar))
  yost02a <- cbind(yost02a,tempa)
  yost02b <- cbind(yost02b,tempb)
  
  yost03 <- rbind(yost02a,yost02b)
  yost03 <- select(yost03,GEOID,var,rank)
  yost03 <- arrange(yost03,GEOID,var)
  
  
  #Transpose the data to wide format again
  yost04 <- pivot_wider(yost03, id_cols=GEOID, names_from=var, values_from=rank)
  yost04 <- arrange(yost04,GEOID)
  GEOID <- yost04$GEOID
  
  # Perform factor analysis
  fa_yost15 <- fa(yost04 %>% select(-GEOID),fm="ml")
  #Get factor scores and group them into percentiles, where 1=most affluent and 100=most deprived.
  yost05 <- mutate(arrange(as.data.frame(cbind(GEOID, fa_yost15$scores)), ML1), row=row_number())
  yost05$pctile <- ceiling((yost05$row-0.0001)/(nrow(yost05)/100))
  yost05a <- arrange(yost05,GEOID)
  
  #From SEER:The first quintile (the group with the lowest SES) is the 20th centile or less, and the fifth quintile
  #(the group with the highest SES) corresponds to the 80th centile or higher. The way we made the score following Boscoe et al.,
  #we have the opposite, where 1=most affluent and 100=most deprived.
  #quintiles15 <- quantile(yost05a$pctile, probs = 0:5/5)
  yost05b<-yost05a %>% mutate(yostquintiles15 = case_when(
    pctile <= 21 ~ 1,
    pctile > 21 & pctile <= 40.8 ~ 2,
    pctile > 40.8 & pctile <= 61 ~ 3,
    pctile > 61 & pctile < 80.6 ~ 4,
    pctile >= 80.6 ~ 5,
  ))
  yost05b$yostquintiles15<-factor(yost05b$yostquintiles15,levels =1:5, labels = c("Highest","High-middle","Middle","Lower-middle", "Lowest"))
yostindex15<-copy(yost05b)




## 2015-2019, ACS 2015 5-year
#Define new variables
acs5_2019_wide_yost<-acs5_2019_wide_yost%>% rename(inc = B19013_001,unemp= DP03_0005P,
                                                   rent = B25064_001,
                                                   houseval =B25077_001)


acs5_2019_wide_yost<-acs5_2019_wide_yost %>% mutate(pov = ((C17002_002 + C17002_003 + C17002_004 + C17002_005) / C17002_001) * 100,
                                                    lhs= ((B15002_003 + B15002_004 + B15002_005 + B15002_006 + B15002_007 + B15002_008 + B15002_009
                                                           + B15002_010 +  B15002_020+B15002_021+B15002_022+B15002_023+B15002_024+B15002_025+B15002_026+B15002_027)/B15002_001)*100,
                                                    hs= ((B15002_011+B15002_028)/B15002_001) * 100,
                                                    hhs=  ((B15002_012+B15002_013+B15002_014+B15002_015+B15002_016+B15002_017+B15002_018 +  
                                                              B15002_028+B15002_029 + B15002_030+B15002_031+B15002_032+B15002_033+B15002_034+B15002_035)/B15002_001) * 100,
                                                    edind= (lhs * 9) + (hs*12) + (hhs* 16),
                                                    wc= ((C24010_020 + C24010_024 + C24010_025 + C24010_026 + C24010_027 + C24010_030 + C24010_034 + C24010_056 +
                                                            C24010_060 + C24010_061 + C24010_062 + C24010_063 + C24010_066 + C24010_070) / C24010_001) * 100)

yostdat19<-acs5_2019_wide_yost %>% select(GEOID, inc, unemp, rent, houseval, pov, edind,wc)

##----RANKING---- 
#Make into long format for this step as it is easier
yostdat19_long<-yostdat19 %>% pivot_longer(cols = 2:8, names_to = "var", values_to = "est")
completes <- yostdat19_long %>% group_by(GEOID) %>%
  summarize(ntract=n())

#Make sure we have complete data for all Cts
completes <- filter(completes,ntract==7)
complete <- nrow(completes)

yost01 <- left_join(completes,yostdat19_long,by="GEOID")
yost01 <- select(yost01,-ntract)

yost02a <- filter(yost01,var %in% c('pov','unemp','wc'))
yost02b <- filter(yost01,var %in% c('edind','rent',"houseval",'inc'))

rankvar <- "rank"
estvar <- "est"
tempa <- mutate_a(yost02a,eval(rankvar), eval(estvar))
tempb <- mutate_b(yost02b,eval(rankvar), eval(estvar))
tempa <- ungroup(tempa)
tempb <- ungroup(tempb)
tempa <- select(tempa,eval(rankvar))
tempb <- select(tempb,eval(rankvar))
yost02a <- cbind(yost02a,tempa)
yost02b <- cbind(yost02b,tempb)

yost03 <- rbind(yost02a,yost02b)
yost03 <- select(yost03,GEOID,var,rank)
yost03 <- arrange(yost03,GEOID,var)


#Transpose the data to wide format again
yost04 <- pivot_wider(yost03, id_cols=GEOID, names_from=var, values_from=rank)
yost04 <- arrange(yost04,GEOID)
GEOID <- yost04$GEOID

# Perform factor analysis
fa_yost19 <- fa(yost04 %>% select(-GEOID),fm="ml")
#Get factor scores and group them into percentiles, where 1=most affluent and 100=most deprived.
yost05 <- mutate(arrange(as.data.frame(cbind(GEOID, fa_yost19$scores)), ML1), row=row_number())
yost05$pctile <- ceiling((yost05$row-0.0001)/(nrow(yost05)/100))
yost05a <- arrange(yost05,GEOID)

#From SEER:The first quintile (the group with the lowest SES) is the 20th centile or less, and the fifth quintile
#(the group with the highest SES) corresponds to the 80th centile or higher. The way we made the score following Boscoe et al.,
#we have the opposite, where 1=most affluent and 100=most deprived.
#quintiles15 <- quantile(yost05a$pctile, probs = 0:5/5)
yost05b<-yost05a %>% mutate(yostquintiles19 = case_when(
  pctile <= 21 ~ 1,
  pctile > 21 & pctile <= 40.8 ~ 2,
  pctile > 40.8 & pctile <= 61 ~ 3,
  pctile > 61 & pctile < 80.6 ~ 4,
  pctile >= 80.6 ~ 5,
))
yost05b$yostquintiles19<-factor(yost05b$yostquintiles19,levels =1:5, labels = c("Highest","High-middle","Middle","Lower-middle", "Lowest"))
yostindex19<-copy(yost05b)








table(yostindex10$yostquintiles10)
table(yostindex15$yostquintiles15)
table(yostindex19$yostquintiles19)

table(asc10= yostindex10$yostquintiles10, asc15= yostindex15$yostquintiles15)
table(asc10= yostindex10$yostquintiles10, asc19= yostindex19$yostquintiles19)
table(asc15= yostindex15$yostquintiles15, asc19= yostindex19$yostquintiles19)


#SAVE
save(yostindex10, yostindex15, yostindex19, file = "YostIndexDat.RData")
