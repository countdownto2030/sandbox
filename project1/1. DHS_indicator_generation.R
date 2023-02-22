# THIS ONE IS ALREADY THERE

options(echo=F)

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

########################################################### Set Locations 
location <- "/Users/EWilson/Desktop/DAC/Delivery"
setwd(location)

data_location  = "/Users/EWilson/Desktop/Countdown/Data" # folder with survey data sets
export_location = "/Users/EWilson/Desktop/DAC/Delivery/Results/"
CDdata = "ReadyforAnalysis_2022-10-26.csv" # list of country data sets to be analyzed

########################################################### GET DATA FILES
CDdata <- read_csv(CDdata)

dhs_full <- CDdata %>% filter(source=="DHS")

complete_countries <- sort(c("Afghanistan","Albania","Angola","Armenia","Benin","Burundi","Cambodia","Cameroon",
                             "Colombia","Egypt","Ethiopia","Gambia","Guatemala","Guinea","Haiti","India","Indonesia","Jordan",
                             "Kenya","Liberia","Malawi","Maldives","Mali","Mauritania","Myanmar","Nigeria","Pakistan","Philippines",
                             "Rwanda","Senegal","SierraLeone","SouthAfrica","Tajikistan","Tanzania","TimorLeste","Uganda","Zambia"))

dhs_full <- CDdata %>% filter(country %in% complete_countries)
dhs <- dhs_full #temp
# dhs <-dhs_full[which(dhs_full$country %in% c("Rwanda")),] #temp (non-loop)
# index=1 # temp (non-loop)


indicators= c("sba","faclevel","ideliv","sbaORideliv","ideliv24hr","pncwm","score")
covariates <- c('wealth','urban.rural','age','education','marital.status','first.time.mother','region')


#****************************************************************************#
#**************** DHS SURVEYS INDICATOR CODING ******************************

##****************************************************************************
##*                     IMPORT DHS DATA SETS
##**************************************************************************##
for(index in 1:nrow(dhs) )  { # start DHS loop
  
  if (dhs$recode_phase[index] == 6) {v="dhs6"}
  if (dhs$recode_phase[index] == 7) {v="dhs7"}
  if (dhs$recode_phase[index] == 8) {v="dhs8"}
  year <- dhs$year[index]
  country <- dhs$country[index]
  folder <- paste(country,year,sep="/")
  file <- paste0(dhs$births[index],".DTA")
  path <- paste(data_location,folder,sep="/")
  file.name<-paste(path,file,sep="/")
  dat <-read.dta13(file.name, convert.factors=FALSE)
  # dat <-read.dta13(file.name, convert.factors=FALSE, select.cols=datcols)
  #dat_labels <- read.dta13(file.name,convert.factors=TRUE, generate.factors = TRUE)
  
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
  #colnames(dat_labels) <- tolower(names(dat_labels))
  #table(duplicated(dat$caseid)) 
  
  # if (dhs$country[index]=="Senegal") { dat$v015 <- 1 } # TEMP solution
  
  dat <- dat %>% filter(v015==1) # completed interview
  
  ##****************************************************************************
  ##*                       DATA SET-UP
  ##**************************************************************************##
  
  dat$agemo <- dat$v008 - dat$b3 # child's age
  dat <- dat %>% filter(midx==1) %>% filter(agemo < 24) # most recent births in the <2 years only
  dat$birthage <- dat$v008 - dat$agemo # age of the woman at time of giving birth
  
  # keep only data labels with case ids matching dat file
  #dat_labels <- dat_labels[which(dat_labels$caseid %in% dat$caseid),]
  
  ##****************************************************************************
  ##*                  DELIVERY INDICATOR CODING
  ##**************************************************************************##
  
  #1. SBA: Proportion of live births delivered by a skilled provider (qualifications are country-specific)
  
  if (!(is.na(dhs$sba_extra[index])) & dhs$sba_extra[index]==1) { print(dhs$skilled_prov[index]) } # include as skilled
  
  dat$sba <- 0
  
  if (dhs$country[index] == "Afghanistan") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; cs nurse/midwife; cs auxiliary midwife
  if (dhs$country[index] == "Albania") { 
    dat$sba[dat$m3a==1 | dat$m3b==1| dat$m3d==1] <- 1 } # family doctor; nurse/midwife; obstetrician/gynecologist
  if (dhs$country[index] == "Angola") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse; attendant
  if (dhs$country[index] == "Armenia") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; midwife; feldshare
  if (dhs$country[index] == "Benin") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1 | dat$m3d==1] <- 1 } # doctor; nurse; midwife; auxiliary nurse
  if (dhs$country[index] == "Burundi") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; midwife; nurse
  if (dhs$country[index] == "Cambodia") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; midwife; nurse
  if (dhs$country[index] == "Cameroon") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse/midwife; auxiliary midwife
  if (dhs$country[index] == "Colombia") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse; auxiliary nurse
  if (dhs$country[index] == "Egypt") { 
    dat$sba[dat$m3a==1 | dat$m3b==1] <- 1 } # doctor; nurse/midwife
  if (dhs$country[index] == "Ethiopia") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1 | dat$m3e==1 | dat$m3f==1] <- 1 } # doctor; nurse/midwife; CHO; CHEW
  if (dhs$country[index] == "Gambia") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse/midwife; auxiliary nurse/comm. nurse attendant
  if (dhs$country[index] == "Guatemala") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; ambulatory doctor; nurse
  if (dhs$country[index] == "Guinea") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse/midwife, technical health officer
  if (dhs$country[index] == "Haiti") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse; nurse/midwife
  if (dhs$country[index] == "India") { 
    dat$sba[dat$m3a==1 | dat$m3b==1] <- 1 } # doctor; nurse/amn/midwife/lady health visitor
  if (dhs$country[index] == "Indonesia") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1 | dat$m3d==1 | dat$m3e==1] <- 1 } # gen pracitioner, obstertrician, nurse, midwive, village midwife
  if (dhs$country[index] == "Jordan") { 
    dat$sba[dat$m3a==1 | dat$m3b==1] <- 1 } # doctor; nurse/midwife
  if (dhs$country[index] == "Kenya") { 
    dat$sba[dat$m3a==1 | dat$m3b==1] <- 1 } # doctor; nurse/midwife
  if (dhs$country[index] == "Liberia") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse/midwife; physician assistant
  if (dhs$country[index] == "Malawi") { 
    dat$sba[dat$m3a==1 | dat$m3b==1] <- 1 } # doctor/clinical officer/medical assistant; nurse/midwife
  if (dhs$country[index] == "Maldives") { 
    dat$sba[dat$m3a==1 | dat$m3b==1] <- 1 } # doctor/clinical officer/medical assistant; nurse/midwife
  if (dhs$country[index] == "Mali") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse/midwife; matron
  if (dhs$country[index] == "Mauritania") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse/midwife; auxiliary midwife
  if (dhs$country[index] == "Myanmar") { 
    dat$sba[dat$m3a==1 | dat$m3b==1] <- 1 } # doctor; nurse/midwife/ lady health visitor
  if (dhs$country[index] == "Nigeria") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse/midwife; auxiliary midwife
  
  if (dhs$country[index] == "Pakistan") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1 | dat$m3d==1 | dat$m3e==1] <- 1 } # doctor; nurse; midwife; lady health visitor; community midwife
  
  if (dhs$country[index] == "Philippines") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse; midwife
  
  if (dhs$country[index] == "Rwanda") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1 | dat$m3h==1 | dat$m3i==1] <- 1 } # doctors, nurses/midwives, auxiliary midwives, community health worker, community health mother and child
  
  if (dhs$country[index] == "Senegal") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse, midwife
  if (dhs$country[index] == "SierraLeone") { 
    dat$sba[dat$m3a==1 | dat$m3b==1] <- 1 } # doctor; nurse/midwife
  if (dhs$country[index] == "SouthAfrica") { 
    dat$sba[dat$m3a==1 | dat$m3b==1] <- 1 } # doctor/gynecologist; nurse/midwife
  if (dhs$country[index] == "Tajikistan") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1 | dat$m3d==1 | dat$m3e==1 | dat$m3f==1] <- 1 } # doctors/gynecologist; nurse;midwife
  if (dhs$country[index] == "Tanzania") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1 | dat$m3g==1 | dat$m3h==1 | dat$m3i==1] <- 1 } # doctor/amo; clinical officers; assistant COs; nurses/midwives; assistant nurse; mch aides
  if (dhs$country[index] == "TimorLeste") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse/midwife; assistant nurse
  if (dhs$country[index] == "Uganda") { 
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse/midwife; medical assistant/clinical officer
  if (dhs$country[index] == "Zambia") {  # different variable structure
    dat$sba[dat$m3a==1 | dat$m3b==1 | dat$m3c==1] <- 1 } # doctor; nurse/midwife; clinical officer
  
  table(dat$sba, useNA = "ifany")
  
  ###########################################################################
  # Code facilities for births (country-specific coding)
  ## for coding key, see Factilities_category_exploration_updated.xls
  
  # table(dat$m15,dat_labels$m15)
  home <- c(11,12,13,96) # same for all surveys
  
  if (dhs$country[index] %in% c("Afghanistan")) {
    basic  <- c(23:27,36,43,46)
    primary <- c(22,31,32)
    secondary <- c(21,41) }
  
  if (dhs$country[index] %in% c("Albania")) {
    basic  <- c(26,36)
    primary <- c(22,23)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Angola")) {
    basic  <- c(33,26,36)
    primary <- c(24,25,32)
    secondary <- c(21,22,23,31) }
  
  if (dhs$country[index] %in% c("Armenia")) {
    basic  <- c()
    primary <- c(22,25,32,41)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Benin")) {
    basic  <- c(24,26,32,36)
    primary <- c(22,23)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Burundi")) {
    basic  <- c(26,36,46)
    primary <- c(24,32,42)
    secondary <- c(21,22,23,31,41) }
  
  if (dhs$country[index] %in% c("Cambodia")) {
    basic  <- c(25,27,33)
    primary <- c(24,32)
    secondary <- c(21,22,23,31) }
  
  if (dhs$country[index] %in% c("Cameroon")) {
    basic  <- c(26,33,36)
    primary <- c(22,23,34)
    secondary <- c(21,31,32) }
  
  if (dhs$country[index] %in% c("Colombia")) {
    basic  <- c()
    primary <- c(32)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Egypt")) {
    basic  <- c(23,27,36,41,42,46)
    primary <- c(22,25,26,32)
    secondary <- c(21,24,31) }
  
  if (dhs$country[index] %in% c("Ethiopia")) {
    basic  <- c(23,26,41,46)
    primary <- c(22,32)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Gambia")) {
    basic  <- c(23)
    primary <- c(22)
    secondary <- c(21,31,32) }
  
  if (dhs$country[index] %in% c("Guatemala")) {
    basic  <- c(23,26,32,33,35)
    primary <- c(22)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Guinea")) {
    basic  <- c(25,26)
    primary <- c(24,32,33)
    secondary <- c(21,22,23,31) }
  
  if (dhs$country[index] %in% c("Haiti")) {
    basic  <- c()
    primary <- c(22,23,25,26,32,33)
    secondary <- c(21,24,31) }
  
  if (dhs$country[index] %in% c("India")) {
    basic  <- c(22,26,27,32)
    primary <- c(23,25)
    secondary <- c(21,24,31,33) }
  
  if (dhs$country[index] %in% c("Indonesia")) {
    basic  <- c(24:29,35:37)
    primary <- c(22:23,32:34)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Jordan")) {
    basic  <- c(26,36)
    primary <- c(22)
    secondary <- c(21,23,24,31,32) }
  
  if (dhs$country[index] %in% c("Kenya")) {
    basic  <- c(26,32,33,36)
    primary <- c(22,23)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Liberia")) {
    basic  <- c(23,26,36)
    primary <- c(22)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Malawi")) {
    basic  <- c(23,26,34)
    primary <- c(22,33)
    secondary <- c(21,31,32) }
  
  if (dhs$country[index] %in% c("Mali")) {
    basic  <- c(25,26,32,33,36)
    primary <- c(23,24,34)
    secondary <- c(21,22,31) }
  
  if (dhs$country[index] %in% c("Mauritania")) {
    basic  <- c(23,26,36)
    primary <- c(22)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Myanmar")) {
    basic  <- c(23:25,36,46)
    primary <- c(22,32,33)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Nigeria")) {
    basic  <- c(23,26,36)
    primary <- c(22)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Pakistan")) {
    basic  <- c(23,24,26,36)
    primary <- c(22)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Philippines")) {
    basic  <- c(23,24,26,32,36)
    primary <- c(22,33)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Rwanda")) {
    basic  <- c(24,26,33,36)
    primary <- c(23,31,32)
    secondary <- c(21,22) }
  
  if (dhs$country[index] %in% c("Senegal")) {
    basic  <- c(24:28,36)
    primary <- c(22,23,24,25,26,28,36)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("SierraLeone")) {
    basic  <- c(23,26)
    primary <- c(22)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("SouthAfrica")) {
    basic  <- c(23,26)
    primary <- c(22)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Tajikistan")) {
    basic  <- c(24:26,36)
    primary <- c(22,23)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Tanzania")) {
    basic  <- c(26,35,44,45)
    primary <- c(25,27,34,43)
    secondary <- c(21:24,31:33,41:42) }
  
  if (dhs$country[index] %in% c("TimorLeste")) {
    basic  <- c(24,25)
    primary <- c(23)
    secondary <- c(21,22,31) }
  
  if (dhs$country[index] %in% c("Uganda")) {
    basic  <- c(26,36)
    primary <- c(22)
    secondary <- c(21,31) }
  
  if (dhs$country[index] %in% c("Zambia")) {
    basic  <- c(23,26,36)
    primary <- c(22)
    secondary <- c(21,31,32) }
  
  ########################## Institutional Delivery w/ facility levels (ordinal) ##############################
  
  #2A. Facility level - Proportion of women who gave birth at home and at each facility level: 
  # Facility level coding: 0 home, 1 basic,primary, 2 secondary
  # include other and DK as home births
  
  dat$faclevel <- NA # No response
  dat$faclevel[(dat$m15 %in% home)] <- 0 
  dat$faclevel[(dat$m15 %in% c(basic,primary))] <- 1 
  dat$faclevel[(dat$m15 %in% secondary)] <- 2
  
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
  dat$ideliv24hr[dat$ideliv == 1] <- 0
  dat$ideliv24hr[dat$ideliv == 1 & dat$m61>=124 & dat$m61<998] <- 1
  
  table(dat$ideliv24hr,useNA="ifany")
  
  ##########################################################################################
  #4. PNCWM Proportion of women who received a postnatal check within 2 days of giving birth
  # m63 - how long before discharging respondent health check took place
  # m67 - how long after discharge/delivery at home respondent health check took place
  # includes all women (home and facility births)
  
  dat$pncwm=NA
  
  if ( v!="dhs6" & (dhs$country[index]!="Afghanistan")) {
    dat$pncwm[ !(is.na(dat$m63)) & !(is.na(dat$m67))] <- 0
    dat$pncwm[(dat$m63 <= 201 & dat$ideliv==1)] <- 1 # facility births checked at facility <2 days after delivery
    dat$pncwm[(dat$m67 <= 201 & dat$ideliv==0)] <- 1 # home births checked <2 days after delivery
    dat$pncwm[(dat$m67 <= 201 & dat$ideliv==1)] <- 1 # facility births checked at home <2 days after delivery
    
    # Set don't know responses to zero- don't drop from data set
    dat$pncwm[dat$m63 >= 998 & dat$ideliv==1] <- 0 # DK responses (facility births)
    dat$pncwm[(dat$m67 >=998 & dat$ideliv==0)] <- 0 # DK responses (home births)
  }
  
  # DHS6 recode files used different variables for pncwm
  # Afghanistan is recorded as a phase 7 recode file, but variable for pncwm matches that of dhs phase 6 recode
  if ( v=="dhs6" | (dhs$country[index]=="Afghanistan")) {
    # table(dat$m50)
    dat$pncwm[ !(is.na(dat$m50)) & !(is.na(dat$m51))] <- 0
    dat$pncwm[(dat$m50 == 1 & dat$ideliv<=201 )] <- 1 
    dat$pncwm[(dat$m50 ==1 & (dat$m51==199 | dat$m51==998))] <- 0 # hours missing and DK responses set to 0
  }
  
  table(dat$pncwm,useNA="ifany")  
  
  # NO SURVEY Q RESPONSES CURRENTLY AVAILABLE FOR INDICATORS:
  
  #5. RESPECT - Proportion of women who were treated with respect during delivery
  
  #6. PNC_CONTENT - Proportion of women with PNC visits who received all standard interventions
  # Std interventions: a) bp measurement, b) vaginal bleeding check, c) FP counseling
  
  
  ########  COMPOSITE INDICATORS  ###########################################################
  
  #9. Delivery Composite Score: 0-2 for facility level + 1 for 24hr stay + 1 for pncwm
  
  dat$score <- (rowSums(dat[ , c("faclevel","sba","ideliv24hr","pncwm")], na.rm=TRUE))
  
  ## Covariates ..............................................................
  #1. wealth quintile
  #dat$v190     
  dat$wealth <- dat$v190
  
  #2. residence
  #dat$v025
  dat$urban.rural<-dat$v025
  dat$urban.rural[dat$v025==2]<-0 # rural
  #  table(dat$v025,dat$urban.rural)
  
  #3. age group
  #dat$v012 
  dat$age <- NA 
  dat$age[dat$v012 >=20 & dat$v012 <= 49 ] <- 0
  dat$age[dat$v012 >= 15 & dat$v012 <= 17] <- 1
  dat$age[dat$v012 >=18  & dat$v012 <= 19] <- 2
  #  table(dat$age)
  
  #4. education level
  #dat$v106
  dat$education <- dat$v106
  dat$education[dat$v106==9 | dat$v106==6 ]<-NA # missing
  dat$education[dat$v106==3]<-2 # Higher
  dat$education[dat$v106==2]<-2 # Secondary
  dat$education[dat$v106==1]<-1 # Primary
  dat$education[dat$v106==0]<-0 # No education
  # dat$education <- as.numeric(dat$education)
  
  #5. marital status (married or in union)
  # unique(dat$v501)
  dat$marital.status[dat$v501==0] <- 0 # never married
  dat$marital.status[dat$v501==1 | dat$v501==2] <- 1 # married or living with partner
  dat$marital.status[dat$v501==3 | dat$v501==4 | dat$v501==5] <- 2 # widowed, divorced, or separated
  #table(dat$v501, dat$marital.status)
  
  #6. first time mother
  dat$first.time.mother <- ifelse(dat$v224>=2, 0, 1)
  #table(dat$midx, dat$first.time.mother, exclude=NULL)
  
  #7. region
  dat$region <- dat$v024
  
  
  ############### get individual-level data, all in one place, for inferential analysis
  
  dat$country <- dhs$country[index]
  dat$source <- dhs$source[index]
  dat$recode_phase <- dhs$recode_phase[index]
  
  columns <- c('source','recode_phase','caseid','v001','v005','v023','country','v012','midx','birthage','agemo',indicators,covariates) 
  
  # check
  columns %in% colnames(dat)
  columns[which(!(columns %in% colnames(dat)))]
  
  assign(paste0(dhs$country[index]), as.data.frame(dat[,columns]))
  print(file.name)
  
} # end DHS survey loop

####################################################################################
# remove Cambodia, Colombia, Ethiopia which do not have the time spent at place of delivery variable, m61
master <- rbind(Afghanistan,Albania,Angola,Armenia,Benin,Burundi,Cameroon,
                Egypt,Gambia,Guatemala,Guinea,Haiti,India,Indonesia,Jordan,
                Kenya,Liberia,Malawi,Maldives,Mali,Mauritania,Myanmar,Nigeria,Pakistan,Philippines,
                Rwanda,Senegal,SierraLeone,SouthAfrica,Tajikistan,Tanzania,TimorLeste,Uganda,Zambia)

export_fi <- paste0("delivery_indicators.dhs_",date,".csv")

write_csv(master,paste0(export_location,export_fi))

End.time <- Sys.time()
Run.time <- Start.time - End.time

Run.time
