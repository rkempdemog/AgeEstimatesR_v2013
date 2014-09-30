######################################################################
#                 Age Estimates Input Data Clean - Vintage 2013
#                 Age Est Script 2: Last Edit July 30, 2014
######################################################################
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(car)
source("C:/Users/rkemp/Desktop/GenericRCode/savexlsx.r")
############ Read in Data ############
base10=read.csv(file="InputData/Base2010.csv")
countycodes=read.csv(file="InputData/countycodes.csv")
countymaster=read.csv(file="InputData/County_Estimates_Master_v2013.csv")
birth11m=read.csv("InputData/birth11final.csv")
birth12m=read.csv("InputData/birth12final.csv")
birth13m=read.csv("InputData/birth13final.csv")
death11m=read.csv("InputData/death11final.csv")
death12m=read.csv("InputData/death12final.csv")
death13m=read.csv("InputData/death13final.csv")
netmigage=read.csv("InputData/netmigage0010.csv")
netmigages=read.csv("InputData/netmigrateacs20105yr_splined.csv")
############ Create Input Data ############
######Creates the Base  July 1 Data and Calculates and Age Distribution in Rates######
base10=base10%>%
  gather(name,population, Adams:Yuma)%>%
  merge(countycodes, by="name", all=T)%>%
  group_by(county, Age) %>% 
  summarise(BasePop=sum(population))
####Creates Births Table#####
births=rbind(birth11m, birth12m, birth13m)
births=births%>%
  mutate(fy=ifelse(hyear==1, year-1, year),
         age=0)%>%
  group_by(county,fy)%>%
  summarise(births=n())%>%
  filter(fy>2010)
#adds in an age so I can merge them more easily and spreads them out to be tidy
births=births%>%
  mutate(fy=paste("births", fy, sep=""))%>%
  spread(fy,births)%>%
  mutate(births2011=ifelse(is.na(births2011)==T, 0, births2011),
         births2012=ifelse(is.na(births2012)==T, 0, births2012),
         births2013=ifelse(is.na(births2013)==T, 0, births2013),
         age=0)
####Creates Deaths Table####
deaths=rbind(death11m, death12m, death13m)
deaths=deaths%>%
  mutate(fy=ifelse(hyear==1, year-1, year))%>%
  group_by(county,age, fy)%>%
  summarise(deaths=n())%>%
  filter(fy>2010)
#makes deaths Tidy by putting variables into columns and obervations in rows
deaths=deaths%>%
  mutate(fy=paste("deaths", fy, sep=""))%>%
  spread(fy,deaths)%>%
  mutate(deaths2011=ifelse(is.na(deaths2011)==T, 0, deaths2011),
         deaths2012=ifelse(is.na(deaths2012)==T, 0, deaths2012),
         deaths2013=ifelse(is.na(deaths2013)==T, 0, deaths2013),
         age=ifelse(age>90, 90, age))%>%
  group_by(county, age)%>%
  summarise(deaths2011=sum(deaths2011),
            deaths2012=sum(deaths2012),
            deaths2013=sum(deaths2013))
####creates Migration Table####
#Creates the Migration Rates to USe
netmig=netmigage%>%
  group_by(county)%>%
  mutate(migrate=total/sum(total))
#Merges in the Adjusted Rates for Specific Counties
netmig=merge(netmig, netmigages, by=c("county", "age"), all=T)
netmig=mutate(netmig,
  netmigdist=ifelse(is.na(netmig$netmigsp)==TRUE, netmig$migrate, netmig$netmigsp))
#Generates the actual age specific counts
netmig=merge(netmig,subset(countymaster, CountyNumber>0, select=c("CountyNumber", "NetMig1011", "NetMig1112", "NetMig1213")), by.x="county", by.y="CountyNumber", all=T)%>%
  mutate(netmig11=NetMig1011*netmigdist,
         netmig12=NetMig1112*netmigdist,
         netmig13=NetMig1213*netmigdist)
#save.xlsx("cleaninputdata_v2013_archive.xlsx", base10, births, deaths, netmig)
