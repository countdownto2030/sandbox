#******************************************************************#
#   Creating Delivery Indicators from MICS 6/7
#         COUNTDOWN TO 2030 
#         VER. 29 Nov 2022
#******************************************************************#

rm(list=ls())
Start.time <- Sys.time()
date = substr(date(),5,10)

########################################################### Load Libraries
library(foreign) #reads in data from STATA file format
library(readstata13)
library(survey) #allows for design based analysis
library(tidyverse)
library(haven)
library(reshape2)

'%!in%' <- function(x,y)!('%in%'(x,y))
########################################################### Set Locations 
location <- "/Users/EWilson/Desktop/DAC/Delivery"
setwd(location)

data_location  = "/Users/EWilson/Desktop/Countdown/Data" # folder with survey data sets
export_location = "/Users/EWilson/Desktop/DAC/Delivery/Results/"
CDdata = "ReadyforAnalysis_2022-10-26.csv" # list of country data sets to be analyzed

########################################################### GET DATA FILES
CDdata <- read_csv(CDdata)

mics_full <- CDdata %>% filter(source=="MICS")
# mics <- mics_full %>% filter(analyzed==0) #select only countries not yet analyzed

complete_countries <- sort(c("Algeria","Bangladesh", "Belarus","Belize","CentralAfricanRepublic","Congo",
                             "CostaRica","CotedIvoire","Cuba","DominicanRepublic","DRC","ElSalvador","Eswatini",
                             "Georgia","Ghana","GuineaBissau","Guyana","Iraq","Kazakhstan","Kiribati","Kyrgyzstan",
                             "Laos","Lesotho","Madagascar","Mexico","Mongolia","Montenegro","Nepal","NorthMacedonia",
                             "Palestine","Panama","Paraguay","Sudan","Suriname","Thailand","Togo",
                             "Tonga","Tunisia","Turkmenistan","Vietnam","Zimbabwe"))

mics_full <- CDdata %>% filter(country %in% complete_countries)
mics <- mics_full 
# mics <-mics[which(mics$country=="DominicanRepublic"),] # temporary - select first MICS survey (=Algeria) -> Bangladesh
# index =1 # temp, not running loop

# exceptions <- c("Belize","Congo","CotedIvoire","ElSalvador","DominicanRepublic","Eswatini","Kazakhstan","Mauritania","Mexico",
  #              "Panama","Paraguay","Sudan","Vietnam") # MICS5

indicators= c("sba","faclevel","ideliv","sbaORideliv","ideliv24hr","pncwm","score")
covariates <- c('wealth','urban.rural','age','education','marital.status','first.time.mother','region')


#****************************************************************************#
#**************** MICS SURVEYS INDICATOR CODING ******************************

##****************************************************************************
##*                     IMPORT MICS DATA SETS
##**************************************************************************##
for(index in 1:nrow(mics) )  { # start MICS loop

  if (mics$contract_phase[index] == 6) {v="MICS6"}
  if (mics$contract_phase[index] == 5) {v="MICS5"}
  year <- mics$year[index]
  country <- mics$country[index]
  folder <- paste(country,year,sep="/")
  file <- "wm.dta" 
  path <- paste(data_location,folder,sep="/")
  file.name<-paste(path,file,sep="/")
  dat <-read.dta13(file.name, convert.factors=FALSE)
  dat_labels <- read.dta13(file.name,convert.factors=TRUE, generate.factors = TRUE)
  
  # check that this is a summary birth history, not full
  # dat_numeric$caseid <-  paste(dat_numeric$hh1," ",dat_numeric$hh2," ",dat_numeric$ln)
  # dat_labels$caseid <-  paste(dat_labels$hh1," ",dat_labels$hh2," ",dat_labels$ln) 
  # table(duplicated(dat_numeric$caseid)) 
  # table(duplicated(dat_labels$caseid)) 
  
  # merge numeric data and labels
  # dat <- merge(dat_numeric, dat_labels, by = "caseid")
  # colnames(dat)[c(1:ncol(dat_numeric))] <- colnames(dat_numeric) #take the .x off numeric dataset labels

  # standardize col names to lower case, create case id and check to make sure every case # is unique
  colnames(dat) <- tolower(names(dat))
  colnames(dat_labels) <- tolower(names(dat_labels))
  dat$caseid <-paste(dat$hh1," ",dat$hh2," ",dat$ln)
  dat_labels$caseid <-paste(dat_labels$hh1," ",dat_labels$hh2," ",dat_labels$ln)
  table(duplicated(dat$caseid)) 
  
  if (v=="MICS6") { # MICS6 recode
    dat <- dat %>% filter(wm17==1) # completed interview
    dat <- dat %>% filter(wm9==1) # consent given
    # should we also filter based on this being their first interview and consent being given!?
    # dat <- dat %>% filter(wm8==2) # not previously interviewed - check if should include - seems like its ok
  
    } else { # MICS5 recode
    dat <- dat %>% filter(wm7==1) # completed interview
    dat$wm17 <- dat$wm7
  }

  ##****************************************************************************
  ##*                       DATA SET-UP
  ##**************************************************************************##

  # last child's dob (CMC): wdoblc
  # interview date (CMC): wdoi
  if (mics$country[index] == "CotedIvoire") { # missing wdoblc variable
    # cm12m - month of last birth; cm12y - year of last birth
    
    dat$wdoblc <- 12*((dat$cm12y+1982)-1900)+dat$cm12m
    table(dat$wdoblc)
  }
  
  dat$agemo <- (dat$wdoi/12 + 1900) - (dat$wdoblc/12 +1900) # child's age
  dat$agemo[dat$cm12m==15 | dat$cm12y==35 | dat$cm12y==36 ] <-NA # none recorded, 18 births mi for year, 697 mi for month
  
# standardize to just be months
  
  # WB4 = age of woman
  # birthage = age of the woman at birth
  if(mics$country[index] %in% c("Laos","Togo")){
    dat$wb4 <- dat$wb4 + 14 # these are read in as ordered integers from 1 to 35, instead of 15 to 49
  }

  
  if ( v=="MICS6" ) { # mics6
    
  dat$birthage <- dat$wb4 - dat$agemo
  
  } else { dat$birthage <- dat$wb2 - dat$agemo } # mics5

  # only look at most recent births in the <2 years
  dat <- dat %>% filter(agemo >= 0 & agemo < 2)
  
  # keep only data labels with case ids matching dat file
  dat_labels <- dat_labels[which(dat_labels$caseid %in% dat$caseid),]

  ##****************************************************************************
  ##*                  DELIVERY INDICATOR CODING
  ##**************************************************************************##
  
  #1. SBA: Proportion of live births delivered by a skilled provider (qualifications are country-specific)

  if (!(is.na(mics$sba_extra[index])) & mics$sba_extra[index]==1) { print(mics$skilled_prov[index]) } # include as skilled
  
  dat$sba <- 0
  
  if (mics$country[index] == "Algeria") { 
    dat$sba[dat$mn19a==2 | dat$mn19b==2 | dat$mn19d==2 | dat$mn19e==2] <- 1 } # nurses; general doctors; gynecologists; midwives
  if (mics$country[index] == "Bangladesh") {
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2 | dat$mn19d== 2 | dat$mn19e== 2)] <- 1 } # doctors, nurses/midwives, additional skilled
  if (mics$country[index] == "Belarus") {
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2)] <- 1 }# doctors, nurses/midwives, Feldsher  } # no response 
  if (mics$country[index] == "Belize") { 
    dat$sba[(dat$mn17a== 2 | dat$mn17b== 2)] <- 1 } # doctors, nurses/midwives
  if (mics$country[index] == "CentralAfricanRepublic") {
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2 | dat$mn19d== 2)] <- 1 } # doctors, midwives, nurses, auxiliary midwives
  #if (mics$country[index] == "Chad") {
  # dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2)] <- 1 
  # dat$sba[dat$mn19nr == (unique(dat$mn19nr)[1]) ] <- NA # no response } # doctors, nurses/midwives, other qual
  if (mics$country[index] == "Congo") { 
    dat$sba[(dat$mn17a== 2 | dat$mn17b== 2 | dat$mn17c== 2) | (dat$mn17d== 2)] <- 1 } #doctor;
  if (mics$country[index] == "CostaRica") {
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19i== 2)] <- 1 } # doctors (general or obstetrician), general nurse, obstetric nurse
  if (mics$country[index] == "CotedIvoire") {
    dat$sba[(dat$mn17a== 2 | dat$mn17b== 2 | dat$mn17c== 2)] <- 1 } #doctor; nurse/midwife; aide-soignante
  if (mics$country[index] == "Cuba") {
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2)] <- 1 } # doctors, obstetric nurses, other nurses  
  if (mics$country[index] == "DominicanRepublic") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2 | dat$mn19d== 2)] <- 1 } #general doctor; nurse or midwife; gynecologist or obstetrician; doctor of another specialty
  if (mics$country[index] == "DRC") { 
    dat$sba[dat$mn19a==2 | dat$mn19c==2 | dat$mn19d==2] <- 1 } # d is midwives (mn19c is nurses only)
  if (mics$country[index] == "ElSalvador") { 
    dat$sba[(dat$mn17a== 2 | dat$mn17b== 2| dat$mn17c== 2)] <- 1 } # doctors, nurses, auxiliary nurses (c - check)
  if (mics$country[index] == "Eswatini") { 
    dat$sba[(dat$mn17a== 2 | dat$mn17b== 2)] <-1 }
  if (mics$country[index] == "Guyana") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2 | dat$mn19e== 2)] <- 1 } # doctors, nurses/midwives, Medex, single midwife
  if (mics$country[index] == "Georgia") { 
    dat$sba <- NA }# no variable exists
  if (mics$country[index] == "Ghana") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2)] <- 1 # doctors, nurses/midwives
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "GuineaBissau") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2)] <- 1 # doctors, nurses/midwives
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Iraq") { 
    dat$mn19a <- trimws(dat$mn19a)
    dat$mn19b <- trimws(dat$mn19b)
    dat$mn19c <- trimws(dat$mn19c)
    dat$sba[(dat$mn19a== "A" | dat$mn19b== "B" | dat$mn19c== "C")] <- 1 # doctors, nurses/midwives
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Kazakhstan") { # mn17, NOT mn19
    dat$sba[(dat$mn17a== 2 | dat$mn17b== 2| dat$mn17d== 2)] <- 1 } # doctors, nurses, feldshers
  if (mics$country[index] == "Kiribati") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2)] <- 1 # doctors, nurses/midwives, "other qualified"
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Kyrgyzstan") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2)] <- 1 # doctors, nurses/midwives, "other qualified"
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Laos") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2)] <- 1 # doctors, nurses/midwives, auxiliary nurse
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Lesotho") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2)] <- 1 # doctors, nurses general/midwives/clinicians
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Madagascar") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2)] <- 1 # doctors, nurses/midwives, "other qualified"
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Mexico") {
    dat$sba[(dat$mn17a== 2 | dat$mn17b== 2)] <- 1 # doctors, nurses (not midwives - check on this for techncial, "C")
    dat$sba[dat$mn17q== 2] <- NA }
  if (mics$country[index] == "Mongolia") { 
    dat$sba[(dat$mn19d== 2 | dat$mn19e== 2 | dat$mn19i== 2 | dat$mn19j== 2 | dat$mn19c== 2 | dat$mn19k== 2)] <- 1 # gynaecologist, physician, family/soum doctors, nurses/midwives, auxiliary miwife
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Montenegro") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2)] <- 1 # doctors, nurses/midwives, outreach nurses
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Nepal") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2)] <- 1 # doctors, nurses, ANMs
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "NorthMacedonia") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2)] <- 1 # doctors, nurses/midwives
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Palestine") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2)] <- 1 # doctors, nurses/midwives
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Panama") {
    dat$sba[(dat$mn17a== 2 | dat$mn17b== 2 | dat$mn17c== 2)] <- 1 } # doctors, nurses, auxiliary nurses
  if (mics$country[index] == "Paraguay") {
    dat$sba[(dat$mn17a== 2 | dat$mn17b== 2 | dat$mn17c== 2)] <- 1  # doctors, nurses, obsetric professional
    dat$sba[dat$mn17q== 2] <- NA }
  # if (mics$country[index] == "SaoTomeandPrincipe") { # data not re-coded
  #   dat$sba[(dat$mn19a== "A" | dat$mn19b== "B")] <- 1 # doctors, nurses/midwives
  #   dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Serbia") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2)] <- 1 # doctors, nurses/midwives
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Sudan") { # data not re-coded 
    dat$sba[(dat$mn17a== "A" | dat$mn17b== "B" | dat$mn17d== "C" | dat$mn17g== "G")] <- 1 } # doctors, nurses/midwives, trained midwives, CHWs
  if (mics$country[index] == "Suriname") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19d== 2 | dat$mn19e== 2 | dat$mn19g== 2)] <- 1 # doctors, nurses, other qualified, CHWs
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Thailand") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2 | dat$mn19g== 2)] <- 1 # doctors, nurses/midwives, practical nurses
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Togo") {  # data not re-coded
    dat$mn19a <- trimws(dat$mn19a)
    dat$mn19b <- trimws(dat$mn19b)
    dat$mn19c <- trimws(dat$mn19c)
    dat$mn19d <- trimws(dat$mn19d)
    dat$sba[(dat$mn19a== "A" | dat$mn19b== "B" | dat$mn19c== "C" | dat$mn19d== "D")] <- 1 # doctors, nurses/midwives, auxiliary midwives
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Tonga") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2 | dat$mn19d== 2)] <- 1 # doctors, nurses/midwives, health officers, community nurses
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Tunisia") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2)] <- 1 # doctors, nurses/midwives
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Turkmenistan") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2 | dat$mn19c== 2)] <- 1 # doctors, nurses/midwives, feldshers
    dat$sba[dat$mn19nr== 2] <- NA }
  if (mics$country[index] == "Vietnam") { # data not re-coded
    dat$sba[(dat$mn3a== "A" | dat$mn3b== "B")] <- 1 } # doctors, nurses/midwives
  if (mics$country[index] == "Zimbabwe") { 
    dat$sba[(dat$mn19a== 2 | dat$mn19b== 2)] <- 1 # doctors, nurses/midwives
    dat$sba[dat$mn19nr== 2] <- NA }
  
  table(dat$sba, useNA = "ifany")
  
  ###########################################################################
# Code facilities for births (country-specific coding)
  ## for coding key, see Factilities_category_exploration_updated.xls

  # table(dat$mn20,dat_labels$mn20)
  
  if (mics$country[index] %in% c("Algeria")) {
    home <- c(1,2,11) # respondent's home, other home, other
    basic  <- c(7,10) # govt. health post, other public, other private medical
    primary <- c(5,6,8,9) # govt. clinic/heatlh centre; private clinic; private maternity ward; polyclinic/maternity
    secondary <- c(3,4) # CHU, Hospital (EHS/EH/EPH)
    NR <- c(12)
  }
  
  if (mics$country[index] %in% c("Bangladesh")) {
    
    home <- c(1,2)# respondent's or other
    basic  <- c(5,6) # community clinic; other public
    primary <- c(4,8,9,10) # govt. clinic/heatlh centre; private clinic; private maternity home, NGO Clinic/Hospital (10)
    secondary <- c(3,7) # govt. hospital, private hospital
    NR <- c(14)
  }
  
  if (mics$country[index] %in% c("Belarus","Cuba")) {
    home <- c(1,2,7,9) # respondent's home, other home, other
    basic  <- c(5,8) # govt. health post, other public, other private; # Cuba: Consultation with doctor or nurse
    primary <- c(4) # govt. clinic/heatlh centre; private clinic; private maternity ward; polyclinic/maternity; Cuba: Polyclinic
    secondary <- c(3) # CHU, Hospital (EHS/EH/EPH); Cuba: "Hospital"
    NR <- c()
  }
  
  if (mics$country[index] %in% c("Belize","CentralAfricanRepublic","Congo")) {
    home <- c(1,2,11)# respondent's or other
    basic  <- c(5,6,10) # govt. health post, other public, other private
    primary <- c(4,8,9) # govt. clinic/heatlh centre; private clinic; private maternity ward
    secondary <- c(3,7) # govt. hospital, private hospital
    NR <- c(12)
  }
  

  if (mics$country[index] %in% c("Chad","DRC","Guyana")) {
    home <- c(1,2,12)# respondent's home, other home, or other
    basic  <- c(5,6,10) # govt. health post, other public, other private
    primary <- c(4,8,9) # govt. clinic/heatlh centre; private clinic; private maternity ward
    secondary <- c(3,7) # govt. hospital, private hospital
    NR <- c(11,13)
  }

  if (mics$country[index] %in% c("CotedIvoire")) {
    home <- c(1,2,11)
    basic  <- c(6,10) 
    primary <- c(5,8,9) 
    secondary <- c(3,4,7) 
  }
  
  if (mics$country[index] %in% c("CostaRica")) {
    home <- c(1,2,11)
    basic  <- c(6) # other public
    primary <- c(4,8) # CCSS clinic, Private clinic
    secondary <- c(3,5,7) # CCSS Hospital, Sede de Ebais, Private hospital
    NR <- c(12)
  }
  
  if (mics$country[index] %in% c("DominicanRepublic")) { # mn18
    home <- c(1,2,11,12)
    basic  <- c(5,6,10) # other public; other private or ONG
    primary <- c(7,8,9) # Private clinic or center; Profamilia Clinic
    secondary <- c(3,4) # Hospital de la red publica del MSP; Hospital Militar o del Seguro
  }
  
  if (mics$country[index] %in% c("ElSalvador")) { # mn18
    home <- c(1,2,13)
    basic  <- c(8,12) # other public or private
    primary <- c(4,10) # Govt. (MINSAL) or private clinic
    secondary <- c(3,5,9) # Govt. (MINSAL) or private hospital
    NR <- c(14)
  }
  
  if (mics$country[index] %in% c("Eswatini")) { 
    home <- c(1,2,11,12) # home, other, "on the way"
    basic  <- c(8) # other public
    primary <- c(4,5,10) # govt health centre, govt clinic/PHU, private clinic
    secondary <- c(3,9) # Hospital (govt, private)
  }
  
  if (mics$country[index] %in% c("Georgia")) { 
    home <- c(1,2)
    basic  <- c(5,6) # other public or private
    primary <- c(3) # maternity home - UNCERTAIN LEVEL
    secondary <- c(4) # Hospital/Clinic/He - UNCERTAIN LEVEL
  }
  
  if (mics$country[index] %in% c("Ghana")) {
    home <- c(1,2,11)
    basic  <- c(5,6,10) 
    primary <- c(4,8,9)
    secondary <- c(3,7)
  }
  
  if (mics$country[index] %in% c("GuineaBissau")) {
    home <- c(1,2,11)
    basic  <- c(5,9,10) 
    primary <- c(4,7,8)
    secondary <- c(3,6)
  }
  
  if (mics$country[index] %in% c("Iraq")) {
    home <- c(1:4,11)
    basic  <- c(7,10) 
    primary <- c(6,9)
    secondary <- c(5,8)
  }
  
  if (mics$country[index] %in% c("Kazakhstan")) {
    home <- c(1)
    basic  <- c(6,8) 
    primary <- c(4,7,10,11)
    secondary <- c(3)
    NR <- c(14)
  }
  
  if (mics$country[index] %in% c("Kiribati")) {
    home <- c(1,2,10)
    basic  <- c(5,6,8) 
    primary <- c(4)
    secondary <- c(3)
  }
  
  if (mics$country[index] %in% c("Kyrgyzstan")) {
    home <- c(1,12)
    basic  <- c(5,6) 
    primary <- c(4,8,9)
    secondary <- c(3,7)
    NR <- c(13)
  }
  
  if (mics$country[index] %in% c("Laos")) {
    home <- c(1,2,10)
    basic  <- c(5,8) 
    primary <- c(4)
    secondary <- c(3,6)
  }
  
  if (mics$country[index] %in% c("Lesotho")) {
    home <- c(1,2,15)
    basic  <- c(5,6,10,13) 
    primary <- c(4,8,9,12)
    secondary <- c(3,7,11)
  }
  
  if (mics$country[index] %in% c("Madagascar","Nepal")) {
    home <- c(1,2,12)
    basic  <- c(5,6,10,11) 
    primary <- c(4,8,9)
    secondary <- c(3,7)
    NR <- c(13)
  }
  
  if (mics$country[index] %in% c("Mexico")) {
    home <- c(1,2,3,10)
    basic  <- c(6,9) 
    primary <- c(5,8)
    secondary <- c(4,7)
    NR <- c(11)
  }
  
  if (mics$country[index] %in% c("Mongolia")) {
    home <- c(1,10)
    basic  <- c() 
    primary <- c()
    secondary <- c(3:8)
  }
  
  if (mics$country[index] %in% c("Montenegro")) {
    home <- c(9)
    basic  <- c(5) 
    primary <- c(4)
    secondary <- c(3,6)
    NR <- c(11)
  }
  
  if (mics$country[index] %in% c("NorthMacedonia")) {
    home <- c(11)
    basic  <- c(5) 
    primary <- c(4,7,8)
    secondary <- c(3,6)
  }
  
  if (mics$country[index] %in% c("Palestine")) {
    home <- c(1,2,11)
    basic  <- c(10) 
    primary <- c(4,6)
    secondary <- c(3,5,7,8,9)
  }

  if (mics$country[index] %in% c("Panama")) {
    home <- c(1,2,11)
    basic  <- c(5,6) 
    primary <- c(4,8)
    secondary <- c(3,7)
    NR <- c(12)
  }
  
  if (mics$country[index] %in% c("Paraguay")) {
    home <- c(1,2,12)
    basic  <- c(7,11) 
    primary <- c(3,4,5,9)
    secondary <- c(6,8,10)
  }
  
  if (mics$country[index] %in% c("SaoTomeandPrincipe")) {
    home <- c(1,2,10)
    basic  <- c(5,6) 
    primary <- c(4)
    secondary <- c(3)
  }
  
  if (mics$country[index] %in% c("Serbia")) {
    home <- c()
    basic  <- c() 
    primary <- c(4)
    secondary <- c(3,7)
  }
  
  if (mics$country[index] %in% c("Sudan")) {
    home <- c(11,12,96)
    basic  <- c(23,26,36) 
    primary <- c(22,32)
    secondary <- c(21,31)
    NR <- c(99)
  }
  
  if (mics$country[index] %in% c("Suriname")) {
    home <- c(1,2,11)
    basic  <- c(5,9,10) 
    primary <- c(4,7,8)
    secondary <- c(3,6)
    NR <- c(12)
  }
  
  if (mics$country[index] %in% c("Thailand")) {
    home <- c(1,10)
    basic  <- c(5,9) 
    primary <- c(4,7)
    secondary <- c(3,6)
    NR <- c(11)
  }
  
  if (mics$country[index] %in% c("Togo")) {
    home <- c(1,2,14)
    basic  <- c(5,6,7,12,13) 
    primary <- c(4,9,10,11)
    secondary <- c(3,8)
  }
  
  if (mics$country[index] %in% c("Tonga","Tunisia","Turkmenistan")) {
    home <- c(1,2,10)
    basic  <- c(5) 
    primary <- c(4,7)
    secondary <- c(3,6)
  }
  
  if (mics$country[index] %in% c("Vietnam")) {
    home <- c(11,12,96)
    basic  <- c(36,76) 
    primary <- c(22,23)
    secondary <- c(21,24,31)
  }
  
  if (mics$country[index] %in% c("Zimbabwe")) {
    home <- c(1,2,15)
    basic  <- c(5,6,10,14) 
    primary <- c(4,8,9,12)
    secondary <- c(3,7,11)
  }

  ########################## Institutional Delivery w/ facility levels (ordinal) ##############################
  
  #2A. Facility level - Proportion of women who gave birth at home and at each facility level: 
  # Facility level coding: 0 home, 1 basic, 2 primary, 3 secondary
  # include other and DK as home births
  
  if (v=="MICS6") { # MICS6 recode
    dat$faclevel <- NA # No response
    dat$faclevel[(dat$mn20 %in% home)] <- 0 
    dat$faclevel[(dat$mn20 %in% c(basic,primary))] <- 1 
    dat$faclevel[(dat$mn20 %in% secondary)] <- 2
  
  } else{ # MICS5 recode
    
    dat$faclevel <- NA # No response
    dat$faclevel[(dat$mn18 %in% home)] <- 0 
    dat$faclevel[(dat$mn18 %in% c(basic,primary))] <- 1 
    dat$faclevel[(dat$mn18 %in% secondary)] <- 2
    }
  
  #2B. IDELIV: Proportion of women who gave birth in a health facility (binary)
  dat$ideliv <- NA
  dat$ideliv[dat$faclevel==0] <- 0 # home birth
  dat$ideliv[dat$faclevel>0] <- 1 # institutional birth
  
  #3. SBA or IDELIV: Proportion of women who gave birth at a facility OR with a skilled provider at home
  # Purpose - avoid double-counting women with institutional births/excluding women with home births 
  dat$sbaORideliv <- NA
  dat$sbaORideliv[!is.na(dat$sba) | !is.na(dat$ideliv)] <- 0
  dat$sbaORideliv[dat$sba==1 | dat$ideliv==1] <- 1
  table(dat$ideliv, dat$sba, exclude=NULL)
  table(dat$sbaORideliv, dat$sba, exclude=NULL)
  table(dat$sbaORideliv, dat$ideliv, exclude=NULL)
  
  #############################################################################################
  #4. ideliv24hr: Proportion of women who spent at least 24 hours in the hospital after delivery
  # assume 0 for DK responses and "special" units
  
  dat$ideliv24hr <- NA
  if (mics$country[index]=="Georgia") { # missing health stay variables
    dat$pn3u <-NA
    dat$pn3n <-NA
    }
  
  if (v=="MICS5") { dat$ideliv24hr[( !(is.na(dat$pn2u)) & !(is.na(dat$pn2n)))] <- 0 # mics5 recode
  } else { dat$ideliv24hr[( !(is.na(dat$pn3u)) & !(is.na(dat$pn3n)))] <- 0 # mics6 recode
    }

  if (mics$country[index]=="Guyana") { dat$ideliv24hr[
    (dat$pn3u == 1 & dat$pn3n >16) | # 24 hours or more
    (dat$pn3u == 2 & dat$pn3n >1) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >1) ] <-1 } # 1 week or more

  if (mics$country[index] %in% c("Algeria")) { dat$ideliv24hr[ # 25=NSP/DK
    (dat$pn3u == 2 & dat$pn3n >1 & dat$pn3n <25) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >1  & dat$pn3n <25) ] <- 1 } # 1 week or more
  
  if (mics$country[index] %in% c("Bangladesh")) { dat$ideliv24hr[ # 25=DK/DR
    (dat$pn3u == 2 & dat$pn3n >1 & dat$pn3n <25) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >1  & dat$pn3n <25) ] <- 1 } # 1 week or more
  
  if (mics$country[index] %in% c("Belarus")) { dat$ideliv24hr[ # 25=DK/DR
    (dat$pn3u == 2 & dat$pn3n >=1) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=1)] <- 1 } # 1 week or more
  
  if (mics$country[index] %in% c("Belize")) { dat$ideliv24hr[
    (dat$pn2u == 2 & dat$pn2n >1) | # 1 day or more
    (dat$pn2u == 3 & dat$pn2n >1)] <- 1 } # 1 week or more
  
  if (mics$country[index] %in% c("CentralAfricanRepublic")) { dat$ideliv24hr[# 25=NSP
    (dat$pn3u == 1 & (dat$pn3n ==17 | dat$pn3n ==18)) | # 30 or 33 hours
    (dat$pn3u == 2 & dat$pn3n >=2 & dat$pn3n <25) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=2  & dat$pn3n <25)] <- 1 }# 1 week or more
  
  if (mics$country[index] %in% c("Congo")) { dat$ideliv24hr[
    (dat$pn2u == 1 & dat$pn2n==23) | # 36 hours
    (dat$pn2u == 2 & dat$pn2n >1) | # 1 day or more
    (dat$pn2u == 3 & dat$pn2n >1)] <- 1 } # 1 week or more
  
  if (mics$country[index] %in% c("CostaRica")) { dat$ideliv24hr[ # 21=NR; no qualifying hours unit births
    (dat$pn3u == 2 & dat$pn3n >=1 & dat$pn3n <21) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=1  & dat$pn3n <21)] <- 1 }# 1 week or more
  
  if (mics$country[index] %in% c("CotedIvoire")) { dat$ideliv24hr[ # 24=NSP; 25=NR
    (dat$pn2u == 1 & dat$pn2n==23) | # 24 hours
    (dat$pn2u == 2 & dat$pn2n >1 & dat$pn2n <24) | # 1 day or more
    (dat$pn2u == 3 & dat$pn2n >1 & dat$pn2n <24)] <- 1 } # 1 week or more
  
  if (mics$country[index] %in% c("Cuba")) { dat$ideliv24hr[
    (dat$pn3u == 1 & dat$pn3n ==13) | # 24 hours or more
    (dat$pn3u == 2 & dat$pn3n >=1) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=1) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="DominicanRepublic") { dat$ideliv24hr[ # 22=DK; 23=NR
  #  (dat$pn2u == 1 & dat$pn2n ==25) | # 24 hours or more
    (dat$pn3u == 2 & (dat$pn3n >1 & dat$pn3n <22) ) | # 1 day or more
    (dat$pn3u == 3 & (dat$pn3n >1 & dat$pn3n <22)) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="DRC") { dat$ideliv24hr[ # 24=DK/DR
    (dat$pn3u == 1 & dat$pn3n ==17) | # 24 hours or more
    (dat$pn3u == 2 & dat$pn3n >=2 & dat$pn3n <24) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=2 & dat$pn3n <24 ) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="ElSalvador") { dat$ideliv24hr[ # 34=DK; 35=NR
    (dat$pn2u == 1 & dat$pn2n >=24 & dat$pn2n<34) | # 24 hours or more
    (dat$pn2u == 2 & (dat$pn2n >1 & dat$pn2n <34)) | # 1 or more days
    (dat$pn2u == 3 & (dat$pn2n >1 & dat$pn2n <34)) ] <- 1 }# 1 week or longer
  
  if (mics$country[index] =="Eswatini") { dat$ideliv24hr[
    (dat$pn2u == 2 & dat$pn2n >= 1) | # 1 day or more
    (dat$pn2u == 3 & dat$pn2n >= 1) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Ghana") { dat$ideliv24hr[ # 26=DK/DR
    (dat$pn3u == 1 & dat$pn3n ==23) | # 24 hours or more
    (dat$pn3u == 2 & dat$pn3n >=2 & dat$pn3n <26) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=2 & dat$pn3n <26) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="GuineaBissau") { dat$ideliv24hr[ # weeks only unit given in responses
      (dat$pn3u == 1 & dat$pn3n >1) ] <- 1 } # 1 week or more (1 is weeks!)
  
  if (mics$country[index] =="Iraq") { dat$ideliv24hr[ # 27=NR
    (dat$pn3u == 1 & dat$pn3n ==19) | # 24 hours or more (=30 hours)
    (dat$pn3u == 2 & dat$pn3n >=2 & dat$pn3n <27) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=2 & dat$pn3n <27) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Kazakhstan") { dat$ideliv24hr[ # 10=DK
    (dat$pn2u == 2 & (dat$pn2n >=1 & dat$pn2n <10)) | # 1 or more days
    (dat$pn2u == 3 & (dat$pn2n >=1 & dat$pn2n <10)) ] <- 1 } # 1 week or longer
  
  if (mics$country[index] =="Kiribati") { dat$ideliv24hr[ # 21=NR
    (dat$pn3u == 2 & dat$pn3n >=2 & dat$pn3n <21) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=2 & dat$pn3n <21) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Kyrgyzstan") { dat$ideliv24hr[
    (dat$pn3u == 2 & dat$pn3n >=2) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=2) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Laos") { dat$ideliv24hr[ # 22=DK/DR
    (dat$pn3u == 2 & dat$pn3n >=2 & dat$pn3n <22) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=2 & dat$pn3n <22) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Lesotho") { dat$ideliv24hr[ # 25=DK/DR
    (dat$pn3u == 2 & dat$pn3n >=2 & dat$pn3n <25) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=2 & dat$pn3n <25) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Madagascar") { dat$ideliv24hr[ 
    (dat$pn3u == 2 & dat$pn3n >=2) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=2) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Mexico") { dat$ideliv24hr[ # 33=DK
    (dat$pn2u == 1 & (dat$pn2n >=24 & dat$pn2n <33)) | # 24 or more hours
    (dat$pn2u == 2 & (dat$pn2n >=2 & dat$pn2n <33)) | # 1 or more days
    (dat$pn2u == 3 & (dat$pn2n >=2 & dat$pn2n <33)) ] <- 1 } # 1 week or longer
  
  if (mics$country[index] =="Mongolia") { dat$ideliv24hr[ # 22=DK/DR, 23=NR
    (dat$pn3u == 1 & (dat$pn3n ==15 | dat$pn3n ==18) ) | # 1 day or more
    (dat$pn3u == 2 & dat$pn3n >=2 & dat$pn3n <22) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=2 & dat$pn3n <22) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Montenegro") { dat$ideliv24hr[
    (dat$pn3u == 2 & dat$pn3n >=1) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=1) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Nepal") { dat$ideliv24hr[ # 27=DK/DR
    (dat$pn3u == 1 & dat$pn3n >=24 & dat$pn3n <27) | # 24 hours or more
    (dat$pn3u == 2 & dat$pn3n >=2 & dat$pn3n <27) | # 1 day or more
    (dat$pn3u == 3 & dat$pn3n >=2 & dat$pn3n <27) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="NorthMacedonia") { dat$ideliv24hr[ # 8=DK/DR
    (dat$pn3u ==2 & dat$pn3n >=1 & dat$pn3n <8) | # 1 day or more
    (dat$pn3u ==3 & dat$pn3n >=1 & dat$pn3n <8) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Palestine") { dat$ideliv24hr[ # 25=DK/DR; 26=NR
    (dat$pn3u ==2 & dat$pn3n >=2 & dat$pn3n <25) | # 1 day or more
    (dat$pn3u ==3 & dat$pn3n >=2 & dat$pn3n <25) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Panama") { dat$ideliv24hr[ # 18=DK
    (dat$pn2u == 2 & (dat$pn2n >=1 & dat$pn2n <18)) | # 1 or more days
    (dat$pn2u == 3 & (dat$pn2n >=1 & dat$pn2n <18)) ] <- 1 } # 1 week or longer
  
  if (mics$country[index] =="Paraguay") { dat$ideliv24hr[ # 18=DK
    (dat$pn2u == 2 & (dat$pn2n >=2 & dat$pn2n <18)) | # 1 or more days
    (dat$pn2u == 3 & (dat$pn2n >=2 & dat$pn2n <18)) ] <- 1 } # 1 week or longer
  
  if (mics$country[index] =="SaoTomeandPrincipe") { dat$ideliv24hr[ 
    (dat$pn3u ==2 & dat$pn3n >=2) | # 1 day or more
    (dat$pn3u ==3 & dat$pn3n >=2) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Serbia") { dat$ideliv24hr[ 
    dat$ideliv24hr] <- NA } # 1 no variable exists
  
  if (mics$country[index] =="Sudan") { dat$ideliv24hr[ # 98=DK/DR; 99=Mi
    (dat$pn2u ==1 & dat$pn2n ==24) | # 24 hours
    (dat$pn2u ==2 & dat$pn2n >=1  & dat$pn2n <98) | # 1 day or more
    (dat$pn2u ==3 & dat$pn2n >=1  & dat$pn2n <98) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Suriname") { dat$ideliv24hr[ # 20=DK/DR; 21=NR
    (dat$pn3u ==2 & dat$pn3n >=2  & dat$pn3n <20) | # 1 day or more
    (dat$pn3u ==3 & dat$pn3n >=2  & dat$pn3n <20) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Togo") { dat$ideliv24hr[ # 26=DK/DR; 27=NR
    (dat$pn3u ==2 & dat$pn3n >=2  & dat$pn3n <26) | # 1 day or more
    (dat$pn3u ==3 & dat$pn3n >=2  & dat$pn3n <26) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Tonga") { dat$ideliv24hr[ # 16=DK/DR
    (dat$pn3u ==1 & dat$pn3n ==15) |
    (dat$pn3u ==2 & dat$pn3n >=2  & dat$pn3n <16) | # 1 day or more
    (dat$pn3u ==3 & dat$pn3n >=2  & dat$pn3n <16) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Tunisia") { dat$ideliv24hr[
    (dat$pn3u ==2 & dat$pn3n >=2) | # 1 day or more
    (dat$pn3u ==3 & dat$pn3n >=2) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Vietnam") { dat$ideliv24hr[ # 26=DK/DR; 27=NR
    (dat$pn3u ==2 & dat$pn3n >=1) | # 1 day or more
    (dat$pn3u ==3 & dat$pn3n >=1) ] <- 1 } # 1 week or more
  
  if (mics$country[index] =="Zimbabwe") { dat$ideliv24hr[
    (dat$pn3u ==2 & dat$pn3n >=2) | # 1 day or more
    (dat$pn3u ==3 & dat$pn3n >=2) ] <- 1 } # 1 week or more
  
  table(dat$ideliv24hr,useNA="ifany")
  
  ##########################################################################################
  #4. PNCWM Proportion of women who received a postnatal check within 2 days of giving birth
  
  # includes all women (home and facility births)
  # if units are weeks or special (DK/DR), assume first check not within 2 days
  # Time variable (PN22) provided for PN17, PN19 and PN20
  # No time variable provided for PN9 or PN5 but assume check was within two days of delivery
  
  dat$pncwm=NA
  
  # Georgia has PN17 and PN17C as time var but no way to distinguish within 48 hrs
  if ( v=="MICS6" & mics$country[index]!="Georgia") { # MICS6 recode files use PN17, PN19, PN20, PN5, PN9

  dat$pncwm[( !(is.na(dat$pn17)) | !(is.na(dat$pn19)) | !(is.na(dat$pn20)) |
                !(is.na(dat$pn5)) | !(is.na(dat$pn9)))] <- 0
  dat$pncwm[(dat$pn5==1 | dat$pn9==1)] <-1 # health checks, assume within 2 days of delivery
  
  }
  
  if (v=="MICS5") { # MICS5 recode files use PN18, PN19, PN16, PN4, PN8
    dat$pncwm[( !(is.na(dat$pn18)) | !(is.na(dat$pn19)) | !(is.na(dat$pn16))  |
                  !(is.na(dat$pn4)) | !(is.na(dat$pn8)) )] <- 0
    dat$pncwm[(dat$pn4==1 | dat$pn8==1)] <-1
  }
  
  ## Country specific coding - WITHIN 2 days (by day 2, on or before day 1)
  
  if (mics$country[index] == "Algeria") { dat$pncwm[
      (dat$pn22u==1 & dat$pn22n<25) | 
      (dat$pn22u==2 & dat$pn22n<=2) ] <-1 } 

  if (mics$country[index] == "Bangladesh") { dat$pncwm[ #28=DK
      (dat$pn22u==1 & dat$pn22n<28) | 
      (dat$pn22u==2 & dat$pn22n<=2) ] <-1 } 
  
  if (mics$country[index] == "Belarus") { dat$pncwm[ # 28=DK/DR; no hours units coded
      (dat$pn22u==2 & dat$pn22n<=1) ] <-1 } 
  
  if (mics$country[index] == "Belize") { dat$pncwm[ # 12=DK
      (dat$pn21u==1 & dat$pn21n<12) | 
      (dat$pn21u==2 & dat$pn21n<=2) ] <-1 } 
  
  if (mics$country[index] %in% c("CentralAfricanRepublic")) { dat$pncwm[ # 20=NSP; 21=NR
      (dat$pn22u==1 & dat$pn22n<20) | 
      (dat$pn22u==2 & dat$pn22n<=2)] <-1 } 

  if (mics$country[index] == "Congo") { dat$pncwm[ # 20=NSP
      (dat$pn21u==1 & dat$pn21n<20) | 
      (dat$pn21u==2 & dat$pn21n<=2) ] <-1 } 
  
  if (mics$country[index] %in% c("CostaRica")) { dat$pncwm[ # 13=DK/DR; 14=NR
      (dat$pn22u==1 & dat$pn22n<13) | 
      (dat$pn22u==2 & dat$pn22n<=2) ] <-1 } 
  
  if (mics$country[index] == "CotedIvoire") { dat$pncwm[ # 22=NSP; 23=NR
      (dat$pn21u==1 & dat$pn21n<19) | 
      (dat$pn21u==2 & dat$pn21n<=2) ] <-1 } 
  
  if (mics$country[index] %in% c("Cuba")) { dat$pncwm[ # 15=DK/DR
      (dat$pn22u==1 & dat$pn22n<=13) | 
      (dat$pn22u==2 & dat$pn22n<=1) ] <-1 } 

  if (mics$country[index] %in% c("DRC")) { dat$pncwm[ # 19 - DK/DR
      (dat$pn22u==1 & dat$pn22n<=18) | 
      (dat$pn22u==2 & dat$pn22n<=1) ] <-1 } 

  if (mics$country[index] %in% c("DominicanRepublic")) { dat$pncwm[ # 21=DK; 22=NR
      (dat$pn22u==1 & dat$pn21n<=20) | 
      (dat$pn22u==2 & dat$pn21n<=1) ] <-1 } 
  
  if (mics$country[index] %in% c("ElSalvador")) { dat$pncwm[ # 41=DK
      (dat$pn21u==1 & dat$pn21n<40) | 
      (dat$pn21u==2 & dat$pn21n<=2) ] <-1 } 

  if (mics$country[index] %in% c("Eswatini")) { dat$pncwm[ # 20=Inconsistent; 21=DK; 22=Mi
      (dat$pn21u==1 & dat$pn21n<20) | 
      (dat$pn21u==2 & dat$pn21n<=2) ] <-1 } 
  
  if (mics$country[index] %in% c("Guyana")) { dat$pncwm[
    (dat$pn22u==1 & dat$pn22n<17) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 } 
  
  if (mics$country[index] %in% c("Ghana")) { dat$pncwm[
    (dat$pn22u==1 & (dat$pn22n<13 | dat$pn22n>13)) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("GuineaBissau")) { dat$pncwm[
    (dat$pn22u==1 & dat$pn22n<18) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("Iraq")) { dat$pncwm[
    (dat$pn22u==1 & dat$pn22n<=19) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("Kazakhstan")) { dat$pncwm[ # 17=Mi; 16=DK
    (dat$pn21u==1 & dat$pn21n<15) | (dat$pn21u==2 & dat$pn21n<=1) ] <-1 }
  
  if (mics$country[index] %in% c("Kiribati")) { dat$pncwm[ # 13=DK/DR; 14=NR
    (dat$pn22u==1 & dat$pn22n<=12) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("Kyrgyzstan")) { dat$pncwm[ # 11=DK/DR
    (dat$pn22u==1 & dat$pn22n<=10) | (dat$pn22u==2 & dat$pn22n==1) ] <-1 }
  
  if (mics$country[index] %in% c("Laos")) { dat$pncwm[ # 13=DK/DR; 14=NR
    (dat$pn22u==1 & dat$pn22n<=12) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("Lesotho")) { dat$pncwm[ # 12=DK/DR
    (dat$pn22u==1 & dat$pn22n<12) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("Madagascar")) { dat$pncwm[ # 19=DK/DR; 20=NR
    (dat$pn22u==1 & dat$pn22n<19) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }

  if (mics$country[index] %in% c("Mexico")) { dat$pncwm[ # 19=NR
    (dat$pn21u==1 & dat$pn21n<17) | (dat$pn21u==2 & dat$pn21n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("Mongolia")) { dat$pncwm[ # 27=DK/DR; 28=NR
    (dat$pn22u==1 & (dat$pn22n<21 | (dat$pn22n>21 & dat$pn22n<27 ) )) | (dat$pn22u==2 & dat$pn22n==1) ] <-1 }
  
  if (mics$country[index] %in% c("Montenegro")) { dat$pncwm[ # 13=DK/DR; 14=NR
    (dat$pn22u==1 & dat$pn22n<13) | (dat$pn22u==2 & dat$pn22n==1) ] <-1 }
  
  if (mics$country[index] %in% c("Nepal")) { dat$pncwm[ 
    (dat$pn22u==1 & dat$pn22n<=11) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("NorthMacedonia")) { dat$pncwm[ # 17=DK/DR
    (dat$pn22u==1 & dat$pn22n<=16) | (dat$pn22u==2 & dat$pn22n<=1) ] <-1 }
  
  if (mics$country[index] %in% c("Palestine")) { dat$pncwm[ # 13=DK/DR; 14=NR
    (dat$pn22u==1 & dat$pn22n<13) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("Panama")) { dat$pncwm[ # 18=DK
    (dat$pn21u==1 & dat$pn21n<=17) | (dat$pn21u==2 & dat$pn21n<=2) ] <-1 } 
  
  if (mics$country[index] %in% c("Paraguay")) { dat$pncwm[ # 18=DK
    (dat$pn21u==1 & dat$pn21n<=17) | (dat$pn21u==2 & dat$pn21n<=2) ] <-1 } 
  
  if (mics$country[index] %in% c("SaoTomeandPrincipe")) { dat$pncwm[ # 17=NS
    (dat$pn22u==1 & dat$pn22n<17) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("Suriname")) { dat$pncwm[ # 17=DK/DR; 18=NR
    (dat$pn22u==1 & dat$pn22n<17) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("Sudan")) { dat$pncwm[ # 98=DK; 99=Mi
    (dat$pn21u==1 & dat$pn21n<=20) | (dat$pn21u==2 & dat$pn21n<=1) ] <-1 }
  
  if (mics$country[index] %in% c("Togo")) { dat$pncwm[ # 18=NR
    (dat$pn22u==1 & dat$pn22n<17) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("Tonga")) { dat$pncwm[ # 24=DK/DR; 15=NR
    (dat$pn22u==1 & dat$pn22n<14) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("Tunisia")) { dat$pncwm[
    (dat$pn22u==1 & dat$pn22n<=18) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
  if (mics$country[index] %in% c("Vietnam")) { dat$pncwm[ # 98=DK
    (dat$pn22u==1 & dat$pn22n<98) | (dat$pn22u==2 & dat$pn22n<=1) ] <-1 } 
  
  if (mics$country[index] %in% c("Zimbabwe")) { dat$pncwm[ # 20=DK/DR
    (dat$pn22u==1 & dat$pn22n<=19) | (dat$pn22u==2 & dat$pn22n<=2) ] <-1 }
  
table(dat$pncwm,useNA="ifany")  

# NO SURVEY Q RESPONSES CURRENTLY AVAILABLE FOR INDICATORS:
    
    #5. RESPECT - Proportion of women who were treated with respect during delivery
    
    #6. PNC_CONTENT - Proportion of women with PNC visits who received all standard interventions
    # Std interventions: a) bp measurement, b) vaginal bleeding check, c) FP counseling

  
  ########  COMPOSITE INDICATORS  ###########################################################
  
  #9. Delivery Composite Score: 0-3 for facility level  + 1 for sba + 1 for 24hr stay + 1 for pncwma
    
  dat$score <- (rowSums(dat[ , c("faclevel","sba","ideliv24hr","pncwm")], na.rm=TRUE))

  ## Covariates ..............................................................
  
  #1. wealth quintile
  #table(dat$windex5)

# MICS6 Recode + Panama, Sudan and Vietnam
if (mics$country[index] %!in% c("Panama", "Sudan", "Vietnam")) { 
    dat$wealth <- dat$windex5
    dat$wealth[dat$windex5==6]<-5
    dat$wealth[dat$windex5==5]<-4
    dat$wealth[dat$windex5==4]<-3
    dat$wealth[dat$windex5==3]<-2
    dat$wealth[dat$windex5==2]<-1
    dat$wealth[dat$windex5==1]<-NA
    dat$wealth[dat$windex==0] <- NA
    } else {
  dat$wealth <- dat$windex5
}

  # } else { # MICS5 Recode
  #   dat$wealth <- dat$windex5
  #   dat$wealth[dat$windex5==6]<-5
  #   dat$wealth[dat$windex5==5]<-4
  #   dat$wealth[dat$windex5==4]<-3
  #   dat$wealth[dat$windex5==3]<-2
  #   dat$wealth[dat$windex5==2]<-1
  #   dat$wealth[dat$windex5==1]<-NA
  # }
  
  #table(dat$windex5,dat$wealth)
  
  #2. residence
  #table(dat$hh6)
# Palestine - rural, urban, camp
    if (mics$country[index] == "Laos") {
      #hh6a combines categories "rural with road" and "rural without road" from hh6
      dat$urban.rural<-dat$hh6a
      dat$urban.rural[dat$hh6a==2]<-0 # rural
    }else if (mics$country[index] == "Suriname") {
      # 3 cats for hh6: 1) urban, 2) rural coastal, 3) rural interior
      dat$urban.rural<-dat$hh6
      dat$urban.rural[dat$hh6==2 | dat$hh6==3]<-0 # rural
    }else if (mics$country[index] == "Palestine") {
      # 3 cats for hh6: 1) urban, 2) rural, 3) camp
      dat$urban.rural<-dat$hh6
      dat$urban.rural[dat$hh6==2]<-0 # rural
      dat$urban.rural[dat$hh6==3]<-1 # set camp to urban
    }else {
    dat$urban.rural<-dat$hh6
    dat$urban.rural[dat$hh6==2]<-0 # rural 
  }


  #3. age group
  if (v=="MICS6") {
  #table(dat$wb4)
  dat$age <- NA 
  dat$age[dat$wb4 >=20 & dat$wb4 <= 49 ] <- 0
  dat$age[dat$wb4 >= 15 & dat$wb4 <= 17] <- 1
  dat$age[dat$wb4 >=18  & dat$wb4 <= 19] <- 2
  table(dat$age)
  #  table(dat$age)
  
  } else {
    #table(dat$wb2)
    dat$age <- NA 
    dat$age[dat$wb2 >=20 & dat$wb2 <= 49 ] <- 0
    dat$age[dat$wb2 >= 15 & dat$wb2 <= 17] <- 1
    dat$age[dat$wb2 >=18  & dat$wb2 <= 19] <- 2
   # table(dat$age)
  }




  #4. education level : no education (or pre-primary only), primary, secondary or higher 
  table(dat$welevel)
  
  if (mics$country[index] %in% c("Guyana", "Chad","Bangladesh","Congo","CostaRica","DominicanRepublic","Belize",
                                 "Panama","SaoTomeandPrincipe","Sudan","Tunisia","Zimbabwe") ){
  dat$education <- dat$welevel
  dat$education[dat$welevel==3 | dat$welevel==4]<-2 # secondary or higher
  dat$education[dat$welevel==2]<-1 # primary/basic
  dat$education[dat$welevel==1]<-0 # pre-primary/none
  dat$education[dat$welevel==5 | dat$welevel==9] <- NA # missing/DK/Other
  }
  
  if(mics$country[index] %in% c("Belarus")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==4]<-2 # General Secondary
    dat$education[dat$welevel==3]<-1 # General basic
    dat$education[dat$welevel==1]<-0 # pre-primary or none
    dat$education[dat$welevel==5] <- 2 # vocational-technical/secondary specialized
    dat$education[dat$welevel==6] <- 2 # higher
    dat$education[dat$welevel==7] <- NA # DK/missing
  }
  
  if(mics$country[index] %in% c("Algeria","Suriname")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==4 | dat$welevel==5 ]<-2 # Secondary or Higher
    dat$education[dat$welevel==3]<-1 # Average ("moyen") / Lower Secondary
    dat$education[dat$welevel==2]<-1 # Primary
    dat$education[dat$welevel==1]<-0 # pre-primary or none / or ECE
    dat$education[dat$welevel==6] <- NA # DK/missing
  }
  
  if(mics$country[index] %in% c("CentralAfricanRepublic")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==4]<-2 # Secondary or Higher
    dat$education[dat$welevel==3]<-1 # Fundamental 2
    dat$education[dat$welevel==2]<-1 # Fundamental 1
    dat$education[dat$welevel==1]<-0 # preschool or none
    dat$education[dat$welevel==5] <- NA # DK/missing (NSP)
  }
  
  if(mics$country[index] %in% c("DRC","GuineaBissau","Iraq","Thailand")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==5]<-2 # Higher
    dat$education[dat$welevel==4]<-2 # Secondary 2; Medio/techncial profession; Upper secondary
    dat$education[dat$welevel==3]<-1 # Secondary 1; Lower secondary
    dat$education[dat$welevel==2]<-1 # Primary; Basic
    dat$education[dat$welevel==1] <-0 # Pre-primary/other; or none
    dat$education[dat$welevel==6] <-NA # Missing/DK (GB and Thailand)
  }
  
  if(mics$country[index] %in% c("Eswatini","Mexico","Georgia")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==3 | dat$welevel==4 | dat$welevel==5]<-2 # Secondary, Higher, Tertiary; Secondary, middle secondary, higher
    dat$education[dat$welevel==2]<-1 # Primary
    dat$education[dat$welevel==1] <-0 # None
  }
  
  if(mics$country[index] %in% c("Cuba")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==4]<-2 # Higher
    dat$education[dat$welevel==3]<-2 # Pre-university/tec med
    dat$education[dat$welevel==2]<-2 # Secondary or Esc. ofic
    dat$education[dat$welevel==1]<-0 # Primary/Preschool or none *** no primary separate category!
  }
  
  if(mics$country[index] %in% c("ElSalvador")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==4 | dat$welevel==5]<-2 # Higher: Bachelor's or University
    dat$education[dat$welevel==3]<-2 # Secondary
    dat$education[dat$welevel==2]<-1 # Primary
    dat$education[dat$welevel==1]<-0 # No school
  }
  
  if(mics$country[index] %in% c("Ghana")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==4 | dat$welevel==5]<-2 # SSS/SHS/Secondary; Higher
    dat$education[dat$welevel==3]<-1 # JSS/JHS/Middle
    dat$education[dat$welevel==2]<-1 # Primary
    dat$education[dat$welevel==1]<-0 # Pre-primary or none
    dat$education[dat$welevel==6] <- NA # DK/missing
  }
  
  if(mics$country[index] %in% c("Kazakhstan")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==4 | dat$welevel==5]<-2 # Technical and Professional; Higher
    dat$education[dat$welevel==3]<-1 # Upper Secondary
    dat$education[dat$welevel==2]<-1 # Lower Secondary
    dat$education[dat$welevel==1]<-0 # None/Primary
  }
  
  if(mics$country[index] %in% c("Kiribati")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==4]<-2 # Senior secondary 2nd level +
    dat$education[dat$welevel==3]<-1 # Junior secondary complete, below Sr.
    dat$education[dat$welevel==2]<-1 # Primary complete - Junior secondary
    dat$education[dat$welevel==1]<-0 # Pre-primary/none - primary incomplete
  }
  
  if(mics$country[index] %in% c("Kyrgyzstan")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==3 | dat$welevel==4 | dat$welevel==5]<-2 # Complete Secondary; Professional primary/middle; Higher
    dat$education[dat$welevel==2]<-1 # Basic secondary
    dat$education[dat$welevel==1]<-0 # Pre-school or none/Primary
  }
  
  if(mics$country[index] %in% c("Laos")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==4 | dat$welevel==5 | dat$welevel==6]<-2 # Upper secondary; Post secondary/non tertiary; Higher
    dat$education[dat$welevel==2 | dat$welevel==3]<-1 # Primary; Lower secondary
    dat$education[dat$welevel==1]<-0 # None or ECE
  }
  
  if(mics$country[index] %in% c("Lesotho","Montenegro","NorthMacedonia","Palestine")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==3]<-2 # Higher
    dat$education[dat$welevel==2]<-1 # Secondary
    dat$education[dat$welevel==1]<-0 # Primary or none/less
    dat$education[dat$welevel==4]<-NA # missing
  }
  
  if(mics$country[index] %in% c("Madagascar","Togo","CotedIvoire")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==3]<-2 # Secondary +
    dat$education[dat$welevel==2]<-1 # Primary
    dat$education[dat$welevel==1]<-0 # Preschool or none
  }
  
  if(mics$country[index] %in% c("Mongolia")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==4 | dat$welevel==5 | dat$welevel==6]<-2 # Upper secondary; Vocational; College/university
    dat$education[dat$welevel==2 | dat$welevel==3]<-1 # Primary; Lower secondary (basic)
    dat$education[dat$welevel==1]<-0 # Pre-primary or none
    dat$education[dat$welevel==5]<-NA # Missing/DK
  }
  
  if(mics$country[index] %in% c("Nepal")){
    dat$education <- dat$welevel1
    dat$education[dat$welevel1==5 | dat$welevel1==8]<-2 # Secondary (incl. lower and upper); Higher
    dat$education[dat$welevel1==2]<-1 # Basic (Gr 1-8)
    dat$education[dat$welevel1==1]<-0 # None
  }
  
  if(mics$country[index] %in% c("Paraguay")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==4 | dat$welevel==5]<-2 # Secondary or Higher
    dat$education[dat$welevel==2 | dat$welevel==3]<-1 # Basic (Gr 1-9)
    dat$education[dat$welevel==1]<-0 # None
    dat$education[dat$welevel==6]<-NA # Not reported/DK
  }
  
  if(mics$country[index] %in% c("Tonga")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==3]<-2 # Upper secondary
    dat$education[dat$welevel==2]<-1 # Lower secondary
    dat$education[dat$welevel==1]<-0 # Up to primary
  }

  if(mics$country[index] %in% c("Turkmenistan")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==4| dat$welevel==5]<-2 # secondary vocational; higher
    dat$education[dat$welevel==2 | dat$welevel==3]<-1 # primary or secondary; primary vocational
    dat$education[dat$welevel==1]<-0 # pre-primary or none
  }
  
  if(mics$country[index] %in% c("Vietnam")){
    dat$education <- dat$welevel
    dat$education[dat$welevel==3 | dat$welevel==4 | dat$welevel==5]<-2 # upper secondary; tertiary
    dat$education[dat$welevel==1 | dat$welevel==2]<-1 # primary; lower secondary
    dat$education[dat$welevel==0]<-0 # pre-primary or none
  }
  
  
  #5. marital status (married or in union)
  
    dat$marital.status[dat$mstatus==3] <- 0 # never married/in union
    dat$marital.status[dat$mstatus==1] <- 1 # currently married/in union
    dat$marital.status[dat$mstatus==2] <- 2 # formerly married/in union
    dat$marital.status[dat$mstatus==4] <- NA # missing
    #table(dat$mstatus, dat$marital.status)

  #6. first time mother
  
  if ( !(mics$country[index] %in% c("Chad","Belarus")) ) {
    
    # table(dat$cm1, dat$cm11)
  dat$first.time.mother <-NA
  dat$first.time.mother[dat$cm1==1]<-0 # NOT first time mother
  dat$first.time.mother[dat$cm1==2]<-1 # YES, first time mother, only 1 for Guyana!!
  }
    # Note - for MICS 5 surveys, CM11 is CM10 
    # in full DR data set, 9 misclassified
  
  if(mics$country[index] %in% c("Chad")){
    table(dat$cm1)
    dat$first.time.mother <-NA
    dat$first.time.mother[dat$cm1==1]<-0 # NOT first time mother
    dat$first.time.mother[dat$cm1==2]<-1 # YES, first time mother
    dat$first.time.mother[dat$cm1==2 & dat$cm11 > 0]<-NA
  }
  # For Chad, 8 women who report being first time mothers, also report # of children, excluding for now
  # query if correct decision
  # table(dat$cm1,dat$cm11) - why does this show 10?
  
  if(mics$country[index] %in% c("Belarus")){
    table(dat$cm1a)
    dat$first.time.mother <-NA
    dat$first.time.mother[dat$cm1a==1]<-0 # NOT first time mother
    dat$first.time.mother[dat$cm1a==2]<-1 # YES, first time mother
  }
  
  #7. region
  dat$region <- dat$hh7
  
  
  ############### get individual-level data, all in one place, for inferential analysis

  dat$country <- mics$country[index]
  dat$source <- mics$source[index]
  dat$recode_phase <- mics$contract_phase[index] # no recode phase for mics
  
  if (v=="MICS5") { dat$wb4 <- dat$wb2 } # Change variable for export
  
  columns <- c('source','recode_phase','caseid','wmweight','hh7','country','wb4','birthage','agemo',indicators,covariates) 

  # check
  columns %in% colnames(dat)
  columns[which(!(columns %in% colnames(dat)))]
  
  assign(paste0(mics$country[index]), as.data.frame(dat[,columns])) # this works to get subset of dataframe
  print(file.name)
  
} # end MICS survey loop

# remove Georgia, Sao Tome and Principe Thailand, Turkmenistan
master <- rbind(Algeria,Bangladesh,Belarus,Belize,CentralAfricanRepublic,Congo,CostaRica,CotedIvoire,Cuba,DominicanRepublic,DRC,ElSalvador,Eswatini,
                Ghana,Guyana,GuineaBissau,Iraq,Kazakhstan,Kiribati,Kyrgyzstan,Laos,Lesotho,Madagascar,
                Mexico, Mongolia, Montenegro,Nepal,NorthMacedonia,Palestine,Panama,Paraguay,
                Sudan,Suriname,Togo,Tonga,Tunisia,Vietnam,Zimbabwe)

export_fi <- paste0("delivery_indicators.mics_",date,".csv")

write_csv(master,paste0(export_location,export_fi))


End.time <- Sys.time()
Run.time <- Start.time - End.time

Run.time



