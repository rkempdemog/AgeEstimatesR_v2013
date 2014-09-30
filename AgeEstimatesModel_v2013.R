######################################################################
#                 Age Estimates Model - Vintage 2013                #
#                 Age Est Script 3: Last Edit July 30, 2014         #
######################################################################
#library(plyr)
library(reshape2)
library(dplyr)
library(tidyr)
library(car)
library(ggvis)
#Must have run the Age Estimates INput Data Code Previously
ageest=base10%>%
  select(county, Age, BasePop)%>%
  mutate(age=Age,
         tp10=BasePop)%>%
  select(county,age, tp10)
#Generating the initial 2011 Estimates
ageest11a=ageest%>%
  merge(deaths, by=c("county", "age"))%>%
  select(county, age, tp10, deaths2011)%>%
  mutate(tp10a=tp10-deaths2011,
         age=ifelse(age<90,age+1, 90))%>%
  select(county, age, tp10a)%>%
    group_by(county,age)%>%
    summarise(tp10a=sum(tp10a))
ageest=ageest%>%
  merge(ageest11a, by=c("county", "age"), all=T)%>%
  mutate(tp10a=ifelse(is.na(tp10a)==T, tp10,tp10a ))%>%
  merge(births, by=c("county", "age"), all=T)%>%
  mutate(tp11=ifelse(age==0, births2011, tp10a))%>%
  merge(netmig, by=c("county", "age"))%>%
  mutate(tp11=tp11+netmig11)%>%
  select(county, age, tp10,tp11)
#Generatng the Initital 2012 Estimates
ageest12a=ageest%>%
  merge(deaths, by=c("county", "age"))%>%
  select(county, age, tp11, deaths2012)%>%
  mutate(tp11a=tp11-deaths2012,
         age=ifelse(age<90,age+1, 90))%>%
  select(county, age, tp11a)%>%
  group_by(county,age)%>%
  summarise(tp11a=sum(tp11a))
ageest=ageest%>%
  merge(ageest12a, by=c("county", "age"), all=T)%>%
  mutate(tp11a=ifelse(is.na(tp11a)==T, tp11,tp11a ))%>%
  merge(births, by=c("county", "age"), all=T)%>%
  mutate(tp12=ifelse(age==0, births2012, tp11a))%>%
  merge(netmig, by=c("county", "age"))%>%
  mutate(tp12=tp11+netmig12)%>%
  select(county, age, tp10, tp11, tp12)
#Generatng the Initital 2013 Estimates
ageest13a=ageest%>%
  merge(deaths, by=c("county", "age"))%>%
  select(county, age, tp12, deaths2013)%>%
  mutate(tp12a=tp12-deaths2013,
         age=ifelse(age<90,age+1, 90))%>%
  select(county, age, tp12a)%>%
  group_by(county,age)%>%
  summarise(tp12a=sum(tp12a))
ageest=ageest%>%
  merge(ageest13a, by=c("county", "age"), all=T)%>%
  mutate(tp12a=ifelse(is.na(tp12a)==T, tp12,tp12a ))%>%
  merge(births, by=c("county", "age"), all=T)%>%
  mutate(tp13=ifelse(age==0, births2013, tp12a))%>%
  merge(netmig, by=c("county", "age"))%>%
  mutate(tp13=tp12+netmig13)%>%
  select(county, age, tp10, tp11, tp12, tp13)
##### Raking Procedures #####
## Standard Raking Factors
agerake1=ageest%>%
  group_by(county)%>%
  summarise(agetotal10=sum(tp10),
            agetotal11=sum(tp11),
            agetotal12=sum(tp12),
            agetotal13=sum(tp13))%>%
  merge(subset(countymaster,CountyNumber>0, select=c("CountyNumber", "Tp10", "Tp11", "Tp12", "Tp13")), by.x="county", by.y="CountyNumber", all=TRUE) %>%
  mutate(rake10=Tp10/agetotal10,
         rake11=Tp11/agetotal11,
         rake12=Tp12/agetotal12,
         rake13=Tp13/agetotal13)%>%
  select(county, rake10, rake11, rake12, rake13)
## Age Specific Factors to hold portions of age dist constant
#Generates proportions for the 5 age groups
raketotals=base10%>%
  mutate(age2=recode(Age, "0:17=1;18:24=2;25:34=3;35:64=4; 65:90=5"))%>%
  group_by(county,age2)%>%
  summarise(agetotal=sum(BasePop))%>%
  mutate(ageper=agetotal/sum(agetotal))%>%
  merge(subset(countymaster,CountyNumber>0, select=c("CountyNumber", "Tp10", "Tp11", "Tp12", "Tp13")), by.x="county", by.y="CountyNumber", all=TRUE) %>%
  mutate(raketotal10=ageper*Tp10,
         raketotal11=ageper*Tp11,
         raketotal12=ageper*Tp12,
         raketotal13=ageper*Tp13)
agerake2=ageest%>%
  mutate(age2=recode(age, "0:17=1;18:24=2;25:34=3;35:64=4; 65:90=5"))%>%
  group_by(county, age2)%>%
  summarise(age2total10=sum(tp10),
            age2total11=sum(tp11),
            age2total12=sum(tp12),
            age2total13=sum(tp13))%>%
  merge(raketotals,by=c("county", "age2"))%>%
  mutate(rake10r=raketotal10/age2total10,
         rake11r=raketotal11/age2total11,
         rake12r=raketotal12/age2total12,
         rake13r=raketotal12/age2total13)%>%
  select(county, age2, rake10r, rake11r, rake12r, rake13r)
## Raking Procedures
#ageest_final=ageest%>%
#  mutate(age2=recode(age, "0:17=1;18:24=2;25:34=3;35:64=4; 65:90=5"))%>%
#  merge(agerake1, by="county")%>%
#  merge(agerake2, by=c("county", "age2"))%>%
#  mutate(tp10=ifelse(county==13|county==31|county==41|county==67|county==69|county==77, tp10*rake10r, tp10*rake10),
#         tp11=ifelse(county==13|county==31|county==41|county==67|county==69|county==77, tp11*rake11r, tp11*rake11),
#         tp12=ifelse(county==13|county==31|county==41|county==67|county==69|county==77, tp12*rake12r, tp12*rake12),
#         tp13=ifelse(county==13|county==31|county==41|county==67|county==69|county==77, tp13*rake13r, tp13*rake13))
ageest_final=ageest%>%
  mutate(age2=recode(age, "0:17=1;18:24=2;25:34=3;35:64=4; 65:90=5"))%>%
  merge(agerake1, by="county")%>%
  merge(agerake2, by=c("county", "age2"))%>%
  mutate(tp10=tp10*rake10,
         tp11=tp11*rake11,
         tp12=tp12*rake12,
         tp13=tp13*rake13)
##### Verification and Testing ####
cindy_data=read.csv("InputData/DraftFinalForecastV2013_Cindy.csv")%>%
  filter(Year>=2010 & Year<2014)%>%
  mutate(Year=paste("Cindy", Year, sep=""),
         county=CountyFIPS,
         age=Age)%>%
  select(county, age,Sex, Year, population)%>%
  group_by(county, Year, age)%>%
  summarise(total=sum(population))%>%
  spread(Year, total)
cindy_deaths=read.csv("InputData/DeathsByAgeV2013.csv")%>%
  filter(year>=2010 & year<2014)%>%
  mutate(year=paste("Cdeaths", year, sep=""))%>%
  select(county, age, sex,year, deaths)%>%
  group_by(county,age, year)%>%
  summarise(total=sum(deaths))%>%
  spread(year, total)
cindy_mig=read.csv("J:/Estimates/Age Estimates/v2013/AgeEstimatesR_v2013/InputData/DANetMigUpdate.csv")%>%
  select(Year:Yuma)%>%
  group_by(Year,Gender,Age)%>%
  gather(name,netmig,Colorado:Yuma)%>%
  merge(countycodes, by="name", all=T)%>%
  filter(Gender=="Total")%>%
  mutate(year="Cnetmig2011",
         age=Age)%>%
  select(-(Year:Age))%>%
  spread(year, netmig)
ageest_verify=ageest_final%>%
  select(county, age, tp10, tp11, tp12, tp13)%>%
  merge(cindy_data, by=c("county", "age"))%>%
  mutate(diff10=tp10-Cindy2010,
         diff11=tp11-Cindy2011,
         diff12=tp12-Cindy2012,
         diff13=tp13-Cindy2013)

agedeath_verify=ageest_verify%>%
  merge(deaths, by=c("county", "age"), all=T)%>%
  mutate(deaths2011=ifelse(is.na(deaths2011)==T, 0, deaths2011),
         deaths2012=ifelse(is.na(deaths2012)==T, 0, deaths2012),
         deaths2013=ifelse(is.na(deaths2013)==T, 0, deaths2013))%>%
  merge(cindy_deaths, by=c("county", "age"))
ageall_verify=agedeath_verify%>%
  merge(netmig, by=c("county", "age"), all=T)%>%
  mutate(netmig11=ifelse(is.na(netmig11)==T, 0, netmig11),
         netmig12=ifelse(is.na(netmig12)==T, 0, netmig12),
         netmig13=ifelse(is.na(netmig13)==T, 0, netmig13))%>%
  merge(cindy_mig, by=c("county", "age"), all=T)%>%
  select(-(CountyFIPS))%>%
  filter(county!=0)
ct=ageest_verify%>%
  group_by(county)%>%
  summarise(t1=sum(Cindy2010),
           t2=sum(tp10))%>%
  mutate(diff=t2-t1)
agecatlabel=c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39","40 to 44","45 to 49","50 to 54","55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79","80 to 84", "85 and Over" )
x=c(1:18)
graphlabels=cbind(x,agecatlabel)
colnames(graphlabels)=c("age5", "age_label")
age5=ageall_verify%>%
  mutate(age5=cut(age, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,100), right=F))%>%
  mutate(age5=recode(age5,"'[0,5)'=1;'[5,10)'=2;'[10,15)'=3;'[15,20)'=4;'[20,25)'=5;'[25,30)'=6;'[30,35)'=7;'[35,40)'=8;'[40,45)'=9;'[45,50)'=10;'[50,55)'=11;'[55,60)'=12;'[60,65)'=13;'[65,70)'=14;'[70,75)'=15;'[75,80)'=16;'[80,85)'=17;'[85,100)'=18"))%>%
  group_by(county,name, age5)%>%
  summarise(tp10=sum(tp10),
            tp11=sum(tp11),
            tp12=sum(tp12),
            tp13=sum(tp13),
            Cindy2010=sum(Cindy2010),
            Cindy2011=sum(Cindy2011),
            Cindy2012=sum(Cindy2012),
            Cindy2013=sum(Cindy2013), 
            deaths2011=sum(deaths2011),
            deaths2012=sum(deaths2012),
            deaths2013=sum(deaths2013),
            Cdeaths2011=sum(Cdeaths2011),
            Cdeaths2012=sum(Cdeaths2012),
            Cdeaths2013=sum(Cdeaths2013),
            netmig11=sum(netmig11),
            netmig12=sum(netmig12),
            netmig13=sum(netmig13),
            Cnetmig2011=sum(Cnetmig2011))%>%
  merge(graphlabels, by=("age5"))

save.xlsx("5yrgroupcompare_Cindy.xlsx", age5)
write.csv(age5, "age5.csv")

write.csv(ageest_verify,"ageest_verify.csv")
###Graphical testing
#save.xlsx("ageraketest.xlsx", ageest_final)
t=ggplot(subset(age5, county==1), aes(x=age5, group=1))+geom_line(aes(y=tp11, group=1))+geom_line(aes(y=Cindy2011, group=1))
ageest_final%>%
  ggvis(x=~age)%>%
    filter(county==13)%>%
    layer_lines(y=~tp10, stroke:="red")%>%
    layer_lines(y=~tp11, stroke:="green")%>%
    layer_lines(y=~tp12, stroke:="blue")%>%
    layer_lines(y=~tp13, stroke:="orange")
