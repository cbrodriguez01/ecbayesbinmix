library(tidycensus)
library(tidyverse)
library(Hmisc)
library(reshape2)
library(ggplot2)
library(psych)
library(table1)
library(Hmisc)
library(flextable)
getwd()

load("/Users/carmenrodriguez/Desktop/Research Projects/Census Data/Census Data Revisions/censusdata_updated_acs2019.RData")
acs19_bin<-readRDS("/Users/carmenrodriguez/Desktop/Research Projects/Census Data/Census Data Revisions/censusdata_bin.rds")
str(acs2019)
str(acs19_bin)

#Some data exploration
histdat<-acs2019 %>% select(-c(GEOID, NAME)) %>% pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
# Compute medians for each variable
medians <- histdat %>% filter(variable!= "Lackplumbing_2019") %>% group_by(variable) %>% summarise(median_val = median(value, na.rm=T))


#Group variables by domain
domain_map <- tribble(
  ~variable,                  ~domain,
  "less_than_hs_p",          "Educational attainment",
  "UnemployementP_2019",     "Economic security",
  "medianincome_2019",       "Economic security",
  "Femalehousehold_2019_P","Economic security",
  "SNAP_2019_P","Economic security",
  "working_class_2019","Economic security",
  "RenterOccupiedUnitP_2019", "Housing conditions and resources",
  "Lackplumbing_2019","Housing conditions and resources",
  "Crowding_housing_2019","Housing conditions and resources",
  "No_vehicle_2019","Housing conditions and resources",
  "lang_home_EN_notwell_2019", "Social and community context",
 "Hispanic_or_Latino_2019","Social and community context",
  "NonHispanicBlack_2019","Social and community context",
  "NonHispanicAsian_2019","Social and community context"
)
histdat1 <- histdat %>%
  left_join(domain_map, by = "variable")


# Reorder variables by domain
histdat1$variable <- factor(histdat1$variable,
                            levels = domain_map %>%
                              arrange(domain, variable) %>%
                              pull(variable))

# Add names for display
varlabels <- c(
  less_than_hs_p = "No HS Diploma",
  RenterOccupiedUnitP_2019 = "Renter-occupied housing",
  UnemployementP_2019 = "Unemployment",
  Lackplumbing_2019 = "Lack complete plumbing in household",
  Femalehousehold_2019_P = "Female-head household",
  No_vehicle_2019 = "No vehicle in household",
  Crowding_housing_2019 = "Crowding in household",
  working_class_2019 = "Working class",
  SNAP_2019_P = "SNAP benefits",
  medianincome_2019 = "Median income",
  lang_home_EN_notwell_2019 = "Limited EN proficiency",
  Hispanic_or_Latino_2019 = "Hispanic",
  NonHispanicBlack_2019 = "Non-Hispanic Black",
  NonHispanicAsian_2019 = "Non-Hispanic Asian"
)

# Now relabel the factor levels directly using varlabels
levels(histdat1$variable) <- varlabels[levels(histdat1$variable)]
  
# Also reorder medians$variable to match
medians$variable <- factor(medians$variable,
                           levels = names(varlabels),
                           labels = varlabels[names(varlabels)])



hist_plot<-ggplot(histdat1, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(data = medians, aes(xintercept = median_val), color = "red", linetype = "dashed", linewidth = 0.7) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() + theme(
    plot.title = element_text(size = 17),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 11)) +
  labs(title = "Distribution of Selected SDoH Variables from ACS 2015-2019", 
       subtitle =  "5-year estimates for Massachusetts",
       x = "Proportions", y = "Count")
hist_plot


#SAVE
png("/Users/carmenrodriguez/Desktop/Research Projects/Census Data/Census Data Revisions/histograms_4.19.25.png", width = 1155, height = 647)
hist_plot
dev.off()



##  Correlations  between potential SDoH variables
acs2019_corr<-acs2019 %>% select(RenterOccupiedUnitP_2019,UnemployementP_2019, medianincome_2019,
                                           Lackplumbing_2019,Femalehousehold_2019_P,
                                           No_vehicle_2019,Crowding_housing_2019,working_class_2019, lang_home_EN_notwell_2019,
                                           SNAP_2019_P, Hispanic_or_Latino_2019, NonHispanicBlack_2019, NonHispanicAsian_2019, less_than_hs_p) %>% 
                                 rename(`No HS Diploma` =  less_than_hs_p, 
                                             `Renter-occupied housing` = RenterOccupiedUnitP_2019,
                                             `Unemployment`= UnemployementP_2019, 
                                             `Lack complete plumbing` = Lackplumbing_2019,
                                             `Female-head household` = Femalehousehold_2019_P,
                                             `No vehicle in household` = No_vehicle_2019,
                                             `Crowded housing`= Crowding_housing_2019,
                                             `Working class` = working_class_2019,
                                             `SNAP benefits`=SNAP_2019_P,
                                             `Median income`= medianincome_2019,
                                             `Limited EN proficiency` = lang_home_EN_notwell_2019,
                                             `Hispanic` = Hispanic_or_Latino_2019,
                                             `NH Black`= NonHispanicBlack_2019,
                                             `NH Asian` = NonHispanicAsian_2019)


corrmatrix_2019<-round(rcorr(as.matrix(acs2019_corr))$r,2)



## New correlation plots-- for new submission 4/15/25
#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
library(corrplot)
library(RColorBrewer)
#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram


jpeg("/Users/carmenrodriguez/Desktop/Research Projects/Census Data/Census Data Revisions/corr19_4.15.2025.jpg", width = 850, height = 850)
corrplot(corrmatrix_2019, method = "circle", type = "lower",
         col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, tl.cex = 1.3, cl.ratio = 0.08,  mar=c(0,0,1,0), cl.pos ="b")
dev.off()


## Descriptive statistics-- rename variables to be same across and add survey identifier
names(acs2019)

colnames(acs2019)<- c("GEOID", "NAME", "medianincome", "Femalehousehold" ,"NoHSDiploma", "crowded_housing","working_class", 
                              "Unemployement","RenterOccupiedUnit", "No_vehicle", "lcplumbing","NHB", "NHA", "Hispanic","ENProficiency", "SNAP")


tab1<-table1::table1( ~ RenterOccupiedUnit + No_vehicle + crowded_housing + lcplumbing + medianincome + Femalehousehold + SNAP +
                        Unemployement + working_class + NoHSDiploma + ENProficiency + Hispanic + NHB+ NHA, data = acs2019)
tab1


# BIG NOTE: DO NOT PANICK IF INCOME IS DIFFERENT WHEN OUTPUT TAB-- SOMETHING IS WEIRD WITH THE TABLE BUT IF TOU DO EACH DATASET INDIVIDUALLY YOU 
#GET THE RIGHT VALUES-- ALL OTHER VARIBALES ARE OKAY- I did checks!
t1flex(tab1) %>% 
  save_as_docx(path="/Users/carmenrodriguez/Desktop/Research Projects/Census Data/Census Data Revisions/censusvarsdist_4.15.25.docx")


### Get distribution of binarized data
#Load census data-- make sure this has the row names so that we can get CTs
names(acs19_bin)
# colnames(acs19_bin)<- c("GEOID", "NAME", "medianincome", "Femalehousehold" ,"NoHSDiploma", "crowded_housing","working_class", 
#                     "Unemployement","RenterOccupiedUnit", "No_vehicle","NHB", "NHA", "Hispanic","ENProficiency", "SNAP", "lcplumbing")

acs19_bin<-acs19_bin %>% mutate_if(is.numeric, as.factor)
str(acs19_bin)

tab2<-table1::table1( ~ RenterOccupiedUnit + No_vehicle + crowded_housing + lcplumbing + medianincome + Femalehousehold + SNAP +
                       Unemployement + working_class + NoHSDiploma + ENProficiency + Hispanic + NHB+ NHA, 
                     data = acs19_bin)
tab2
library(flextable)
t1flex(tab2) %>% 
  save_as_docx(path="/Users/carmenrodriguez/Desktop/Research Projects/Census Data/Census Data Revisions/censusbindist_04.2025.docx")





# 1/26/26; as per reviwer recommendations... add IQRs to descriptive tables

IQR_func<-function(x){
  IQR(x, na.rm=TRUE)
}

iqrest<-as.matrix(acs2019[,3:16] %>% apply(2, IQR_func))

IQR(acs2019$RenterOccupiedUnit, na.rm=TRUE) # 21.2
