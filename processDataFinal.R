# This code takes SG patterns data as a 
# data.frame (or, even better, a data.table)
# and vertically explodes the `visitor_home_cbgs`
# column into many rows, or the `visits_by_day` column.
# This results in one row for safegraph_place_id
# one for visitor_count
# and one for visitor_home_cbg/day

# if you don't have the SafeGraphR package:
#install.packages('remotes')
#remotes::install_github('SafeGraphInc/SafeGraphR')

library(SafeGraphR)
library(tidyverse)
library(stringr)
library(tidycensus)
library(zipcodeR)
library(cowplot)

remove(list = ls())

cuberoot <- function(x)sign(x)*abs(x)^(1/3)

setwd("~/Projects/VulnerabilityMobility/Data/DataPrueba/")

# zipsTravis<-c("78739","78730","78733","78703","78736","78726","78746","78749","78756","78732","78759",
#               "78734","78738","78731","78727","78701","78751","78704","78669","78750","78757","78645",
#               "78722","78748","78652","78660","78735","78745","78705","78728","78747","78758","78653",
#               "78723","78725","78754","78741","78753","78702","78752","78617","78719","78721","78744",
#               "78724","78742")

load("~/Projects/SVITexas/zipsAndCountiesUsed.RData")
zipsAndCountiesUsed

# setwd("~/Projects/VulnerabilityMobility/Data/DataPrueba/")

#####

#Files used here come from TACC. They have been pre-processed to extract
#statistics to estimate visitors/visits from each cbg/zip to places in zip codes
blindEstos<-c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#000000", "#D55E00", "#CC79A7")

devices<-Sys.glob("Archivos/devicesResiding_*")
rawVisits<-Sys.glob("Archivos/rawValuesForNormalization*")
placeHome<-Sys.glob("Archivos/placeAndVisitorHome*")
load("Archivos/dictionaryPlacekeyCategory1.RData")
load("Archivos/dictionaryPlacekeyCategory.RData")
dictionaryPlacekeyCategory
dictionaryPlacekeyCategory1
cbgToZip<-read_csv("ZIP_TRACT_CBG_04042022.csv")
load("../../Data/MovementZipToZip/forMapsWithSVI.RData")
load("~/Projects/SVITexas/svi_Travis.RData")

#dictionaryWholeTexas<-read_csv("Archivos/dictionaryNaicsZips4CountiesTexas.csv")
dictionaryWholeTexas<-read_csv("Archivos/dictionaryNaicsZipsAllCountiesTexas.csv")
dictionaryWholeTexas

population_cbg_Texas <- get_acs(
  geography = "cbg",
  variables = "B01003_001",
  year = 2019,
  state="TX"
)

population_cbg_Texas<-population_cbg_Texas %>% select(home_cbgs=GEOID,pop_home_cbg=estimate)

naicsHierarchy<-read_csv("2017NAICShierarchy.csv")
naicsLevel1<-naicsHierarchy %>% filter(str_length(Code)==2) %>%
  select(Code,Title1=Title) #%>% mutate(Title1=Title1 %>% str_sub(end=-2))
naicsLevel2<-naicsHierarchy %>% filter(str_length(Code)==3) %>%
  select(Code,Title2=Title) #%>% mutate(Title2=Title2 %>% str_sub(end=-2))

#Filtering of cbg and zip codes for unique identification 
cbgToZip_Texas<-cbgToZip %>% filter(State=="TX") %>%
  select(ZipCode,cbg) %>% unique()

#Unique label for naics and top_categories. I arbitrarely asign to
#all repeated naics_code the same code (the first one). 
naicsSixDigits<-dictionaryPlacekeyCategory1 %>% select(top_category,naics_code) %>%
  unique() %>% filter(top_category!="") %>% arrange(top_category) %>%
  mutate(naics_code1=naics_code %>% str_pad(width = 6,pad = "0",side = "right"))
naicsUnique<-dictionaryPlacekeyCategory1 %>% select(top_category,naics_code) %>%
  unique() %>% filter(top_category!="") %>% arrange(top_category) %>%
  mutate(naics_Unique=naics_code %>% str_pad(width = 6,pad = "0",side = "right")) %>%
  arrange(naics_Unique) %>% group_by(top_category) %>% slice(1)
topCatsAndNaicsUnique<-naicsSixDigits %>% merge(naicsUnique, by="top_category") %>%
  select(top_category,naics_Unique) %>% unique()

#These are for all Texas
naicsSixDigitsTexas<-dictionaryWholeTexas %>% select(top_category,naics_code) %>%
  unique() %>% filter(top_category!="") %>% arrange(top_category) %>%
  mutate(naics_code1=naics_code %>% str_pad(width = 6,pad = "0",side = "right"))
naicsUniqueTexas<-dictionaryWholeTexas %>% select(top_category,naics_code) %>%
  unique() %>% filter(top_category!="") %>% arrange(top_category) %>%
  mutate(naics_Unique=naics_code %>% str_pad(width = 6,pad = "0",side = "right")) %>%
  arrange(naics_Unique) %>% group_by(top_category) %>% slice(1)
topCatsAndNaicsUniqueTexas<-naicsSixDigitsTexas %>% merge(naicsUniqueTexas, by="top_category") %>%
  select(top_category,naics_Unique) %>% unique() %>% as_tibble()

#This dictionary has unique naics and identification of placekeys and their
#corresponding postal_code
dictionaryPlacekeyCategory2<-dictionaryPlacekeyCategory1 %>% filter(top_category!="") %>%
  merge(topCatsAndNaicsUnique,by="top_category") %>% as_tibble()
#For whole Texas
dictionaryPlacekeyCategory2<-dictionaryWholeTexas %>% filter(top_category!="") %>%
  merge(topCatsAndNaicsUniqueTexas,by="top_category") %>% as_tibble()

cbgsInMultipleZip<-cbgToZip_Texas %>% select(cbg) %>% group_by(cbg) %>% count() %>% 
  arrange(desc(n)) %>% filter(n>1)
repetidos<-cbgToZip_Texas %>% filter(cbg %in% cbgsInMultipleZip$cbg) %>% 
  arrange(cbg) %>% group_by(cbg) %>% slice(1) %>% arrange(ZipCode)
zipCbgNewLabels<-cbgToZip_Texas %>% 
  mutate(ZipCode1=ifelse(cbg %in% repetidos$cbg,repetidos$ZipCode,ZipCode)) %>%
  arrange(ZipCode)
cbgToZipTexas_new<-cbgToZip_Texas %>% merge(zipCbgNewLabels,by="cbg",all=T) %>% arrange() %>%
  select(home_cbgs=cbg,zip_home=ZipCode1) %>% unique() %>% as_tibble()

#cbgToZipTexas_new %>% filter(zip_home=="75040")

naicsLevel1 %>% print(n=24)
naicsLevel2
#this loop takes all the data above and computes total
#visits to zip codes by place of visitation
i=1
completeZipToZipNaics3Digs<-NULL
for (i in 1:length(rawVisits)){
  #Merging data about cbgs, raw visits, devices and population to
  #calculate estimated visits to places in zip codes
  dataForEstimationOfVisitsToPlaces<-read_csv(rawVisits[i]) %>%
    select(placekey,raw_visit_counts,raw_visitor_counts,poi_cbg,Week) %>%
    merge(read_csv(placeHome[i]) %>% select(home_cbgs,placekey,visitor_home_cbgs,Week),
          by=c("placekey","Week")) %>%
    merge(read_csv(devices[i]) %>% select(home_cbgs=census_block_group,number_devices_residing,Week),
          by=c("home_cbgs","Week")) %>% as_tibble() %>%
    left_join(population_cbg_Texas) %>% drop_na() %>% mutate(poi_cbg=as.factor(poi_cbg))
  #Odd cbgs, where pop is smaller than number of devices
  #I need to check those cbgs that show more devices than population.
  estos<-dataForEstimationOfVisitsToPlaces %>% filter(number_devices_residing>pop_home_cbg) %>%
    pull(home_cbgs) %>% unique()
  #removing cbgs where devices are more than population
  dataForEstimationOfVisitsToPlaces<-dataForEstimationOfVisitsToPlaces %>% filter(!home_cbgs %in% estos)
  
  zipToZipNaics<-dataForEstimationOfVisitsToPlaces %>%
    mutate(totalVisits=visitor_home_cbgs*(pop_home_cbg/number_devices_residing)*(raw_visit_counts/raw_visitor_counts)) %>%
    merge(dictionaryPlacekeyCategory2 %>% select(placekey,naics_Unique,postal_code),by="placekey") %>%
    select(-contains("raw"),-visitor_home_cbgs,-number_devices_residing,-pop_home_cbg) %>%
    merge(cbgToZipTexas_new,by="home_cbgs",all=T) %>% drop_na() %>% mutate(totalVisits=round(totalVisits)) %>%
    as_tibble() %>% select(Week,postal_home=zip_home,postal_dest=postal_code,naics_Unique,totalVisits) %>%
    mutate(postal_dest=as.character(postal_dest)) %>% #pull(postal_home) %>% unique()
    group_by(Week,postal_home,postal_dest,naics_Unique) %>% summarise_each(sum) %>%
    #filter(postal_home %in% zipsTravis,postal_dest %in% zipsTravis) %>%
    mutate(Level1=substr(naics_Unique,1,3)) %>%
    merge(naicsLevel2,by.x = "Level1",by.y="Code") %>% mutate(postal_dest=as.factor(postal_dest)) #%>%
  #    merge(svi_Travis %>% as_tibble() %>% select(Zip,svi_home=SVI),
  #          by.x="postal_home",by.y="Zip") %>%
  #    merge(svi_Travis %>% as_tibble() %>% select(Zip,svi_dest=SVI),
  #          by.x="postal_dest",by.y="Zip") %>% as_tibble()
  
  completeZipToZipNaics3Digs<-rbind(completeZipToZipNaics3Digs,zipToZipNaics)
}

completeZipToZipNaics3Digs %>% as_tibble()
