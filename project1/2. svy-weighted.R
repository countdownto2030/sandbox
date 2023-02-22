# last edited 29 Nov 2022
# last run 29 Nov 2022
# Objective: get data files for survey-weighted and stratified results

rm(list=ls())
Start.time <- Sys.time()
########################################################### Load Libraries
library(ggplot2)
library(survey) #allows for design based analysis
library(RColorBrewer)
library(forcats)
library(tidyverse)

'%!in%' <- function(x,y)!('%in%'(x,y))

date = substr(date(),5,10)
# mics_date <- date
# dhs_date <- date
mics_date <- "Nov 29"
dhs_date <- "Nov 29"

location <- "/Users/EWilson/Desktop/DAC/Delivery"
setwd(location)

########################################################### GET DATA FILES
countrydata_dhs <- read.csv(paste0(location,"/Results/delivery_indicators.dhs_",dhs_date,".csv"),colClasses=c("v023"="character"))
countrydata_mics <- read.csv(paste0(location,"/Results/delivery_indicators.mics_",mics_date,".csv"))

names(countrydata_mics)[names(countrydata_mics)=="wmweight"] <- "v005"
names(countrydata_mics)[names(countrydata_mics)=="wb4"] <- "v012"
names(countrydata_mics)[names(countrydata_mics)=="hh7"] <- "v023"
countrydata_mics$v001 <- NA
countrydata_mics$midx <- NA

setdiff(colnames(countrydata_dhs), colnames(countrydata_mics))
setdiff(colnames(countrydata_mics), colnames(countrydata_dhs))
colnames(countrydata_mics)

countrydata <- rbind(countrydata_dhs,countrydata_mics)
data_master <- countrydata
sort(unique(data_master$country))
head(countrydata)

# check covariates
table(data_master$wealth, exclude=NULL)

data_master$age[data_master$age==0]  <- "20-49"
data_master$age[data_master$age==2] <- 1
data_master$age[data_master$age==1] <- "15-19"
table(data_master$age, exclude=NULL)

data_master$urban.rural[data_master$urban.rural==0]  <- "rural"
data_master$urban.rural[data_master$urban.rural==1]  <- "urban"
table(data_master$urban.rural, exclude=NULL)

data_master$education[data_master$education %in% c(0)] <- "none"
data_master$education[data_master$education %in% c(1)] <- "primary"
data_master$education[data_master$education %in% c(2)] <- "secondary+"
table(data_master$education, exclude=NULL)

data_master$marital.status[data_master$marital.status==0] <- "never married"
data_master$marital.status[data_master$marital.status==1] <- "married/partnered"
data_master$marital.status[data_master$marital.status==2] <- "widowed/divorced/separated"
table(data_master$marital.status, exclude=NULL)

data_master$first.time.mother[data_master$first.time.mother==0] <- "no"
data_master$first.time.mother[data_master$first.time.mother==1] <- "yes"
table(data_master$first.time.mother, exclude=NULL)

########################################################### INDIVIDUAL LEVEL
# parse facility level and score into variables for each level
data_master$faclevel0 <- 0
data_master$faclevel0[data_master$faclevel==0]<-1
data_master$faclevel1 <- 0
data_master$faclevel1[data_master$faclevel==1]<-1
data_master$faclevel2 <- 0
data_master$faclevel2[data_master$faclevel==2]<-1

data_master$score0 <- 0
data_master$score0[data_master$score==0]<-1
data_master$score1 <- 0
data_master$score1[data_master$score==1]<-1
data_master$score2 <- 0
data_master$score2[data_master$score==2]<-1
data_master$score3 <- 0
data_master$score3[data_master$score==3]<-1
data_master$score4 <- 0
data_master$score4[data_master$score==4]<-1
data_master$score5 <- 0
data_master$score5[data_master$score==5]<-1

dim(data_master)
data_master <- data_master %>% filter(country!='SouthAfrica' | v023 != 17) # Only 1 PSU in this cluster which is problem for svyby
data_master <- data_master %>% filter(country!='India' | v023 %!in% c(241,32221,3261,63221,8731,89821)) # Only 1 PSU in this cluster which is problem for svyby
dim(data_master)

write.csv(data_master,paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/",date,"_data.ind.csv"))


########################################################### SURVEY-WEIGHTED
indicators <- c('sba','ideliv','sbaORideliv','ideliv24hr','pncwm','faclevel','faclevel0','faclevel1','faclevel2','score','score0','score1','score2','score3','score4','score5')
indiclist = list()
datalist = list()

sort(unique(data_master$country))

##
#datbkup <- data_master
# data_master <- subset(datbkup,country=="India")
# dim(data_master)
##

for(k in 1:length(sort(unique(data_master$country)))){
  data <- subset(data_master, country %in% sort(unique(data_master$country))[k])
  data$sw <- data$v005/(10^6)
  data$v001 <- 1
  # table(data$v023, exclude=NULL)

# WEIGHTED ESTIMATES
svydata1 <- svydesign(id=~caseid,strata=~v023, data=data, weights=~sw, nest=TRUE)
for(i in 1:length(indicators)){
  denom <- sum(table(data[[indicators[i]]]))
  indic <-svyby(make.formula(indicators[i]), ~v001 ,svydata1, svymean, na.rm=TRUE) 
  indic$n <- denom
  indic$CIL <- indic[,2] - 1.96*indic[,3]
  indic$CIU <- indic[,2] + 1.96*indic[,3]
  
  indic$indicator <- indicators[i]
  indic$country <- sort(unique(data_master$country))[k]
  names(indic)[names(indic) == indicators[i]] <- 'value'
  indic <- indic[,c("country","indicator","n","value","se","CIL","CIU")]
  
  print(sort(unique(data_master$country))[k])
  print(indic)
  indiclist[[i]] <- indic
}

datalist[[k]] <- do.call(rbind,indiclist)

}

svy_data = do.call(rbind, datalist)
svy_data
sort(unique(svy_data$country))
write.csv(svy_data,paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/",date,"_data.svy.csv"), row.names = FALSE)







########################################################### SURVEY-WEIGHTED & STRATIFIED
indicators <- c('score')
# covariates <- c('wealth','urban.rural','age','education','marital.status','first.time.mother','region')
covariates <- c('wealth')

indiclist = list()
covlist = list()
datalist = list()

sort(unique(data_master$country))

for(k in 1:length(sort(unique(data_master$country)))){
  data <- subset(data_master, country %in% sort(unique(data_master$country))[k])
  data$sw <- data$v005/(10^6)
  data$v001 <- 1

for(j in 1:length(covariates)){
  
  for(i in 1:length(indicators)){
    data$sw <- data$v005/(10^6)
    data$v001 <- 1
    svydata1 <- svydesign(id=~caseid,strata=~v023, data=data, weights=~sw) # including strata affects standard error only, not estimate
    denom <- sum(table(data[[indicators[i]]]))
    indic <-svyby(make.formula(indicators[i]), make.formula(covariates[j]), svydata1, svymean, na.rm=TRUE)
    indic$n <- denom
    indic$CIL <- indic[,2] - 1.96*indic[,3]
    indic$CIU <- indic[,2] + 1.96*indic[,3]
    
    indic$indicator <- indicators[i]
    indic$covariate <- covariates[j]
    indic$country <- sort(unique(data_master$country))[k]
    names(indic)[names(indic) == indicators[i]] <- 'value'
    names(indic)[names(indic) == covariates[j]] <- 'level'
    indic <- indic[,c("country","indicator","covariate","level","n","value","se","CIL","CIU")]
    
    print(indic)
    indiclist[[i]] <- indic
  }
  
  covlist[[j]] <- do.call(rbind,indiclist)
}

datalist[[k]] <- do.call(rbind,covlist)
}

strat_svy_data = do.call(rbind, datalist)
strat_svy_data
data1 <- strat_svy_data # Senegal to Zimbabwe
unique(strat_svy_data$country)

write.csv(strat_svy_data,paste0("/Users/EWilson/Desktop/DAC/Delivery/Results/",date,"_data.strat.svy.csv"), row.names = FALSE)

End.time <- Sys.time()
Run.time <- Start.time - End.time

Run.time














