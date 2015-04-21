rm(list = ls(all.names = TRUE))
library(foreign)
library(plyr)

setwd("S:/CCAN/CCANResEval/MIECHV/RedCap/Chomp/DataSnapshots/C1/Activity/2015-04-10")
ds<-read.csv("childrenfirstdata.csv")
setwd("C:/Users/APETERS4/Documents/GitHub/SurvivalExercises/SurvivalExercises/SurvivalExercises/Andrew")


b_first_name<-ds$Child_First_Name_4610
b_last_name<-ds$Child_Last_Name_6756

birth_date<-ds$CHild_DOB

birth_gender<-ifelse(ds$Infant_Gender_4634=="Female", 1, ifelse(ds$Infant_Gender_4634=="Male", 0, NA))
table(ds$Infant_Gender_4634)
table(birth_gender)

mailing_street<-ds$address1
#RES_ADDR2<-ds$address2
mailing_city<-ds$city
address_zip<-ds$zip

city_of_residence<-ds$city


mother_first_name<-ds$clientfirst
mother_last_name<-ds$clientlast

mother_dob<-ds$dob

mother_ssn<-ds$PCG_SSN

#Taken from https://stat.ethz.ch/pipermail/r-help/2006-March/101023.html
#interleave <- function(v1,v2)
#{
#  ord1 <- 2*(1:length(v1))-1
#  ord2 <- 2*(1:length(v2))
#  c(v1,v2)[order(c(ord1,ord2))]
#}

#insert_str <- function(target, insert, index) {
#  insert <- insert[order(index)]
#  index <- sort(index)
#  paste(interleave(split_str_by_index(target, index), insert), collapse="")
#}

#MSSN<-vector()

#for(i in 1:length(ds$PCG_SSN)){
#  if (ds$PCG_SSN[i]=="NA" | is.na(ds$PCG_SSN[i])==TRUE){
#    next
#  } 
#  MSSN[i]<-insert_str(ds$PCG_SSN[i], c("-","-"), c(4,6))
#}
#MSSN[1:10]
#PCG_SSN[1:10]

raw_mothers_hispanic_origin<-ifelse(ds$PCG_ethnic=="Non-Hispanic", 0, "Hispanic")
table(ds$PCG_ethnic)
table(raw_mothers_hispanic_origin)


raw_mothers_race<-ifelse(
  ds$PCG_racecat=="White", 1, ifelse(
    ds$PCG_racecat=="Black or African American", 2, ifelse(
      ds$PCG_racecat=="American Indian or Alaska Native", 3, ifelse(
        ds$PCG_racecat=="Unrecorded", 9, "Unclassified"))))
table(ds$PCG_racecat)
table(raw_mothers_race)

mothers_marital_status<-ifelse(
  ds$marital=="Married" | ds$marital=="Separated", 1, ifelse(
    ds$marital=="Widowed" | ds$marital=="Divorced" | ds$marital=="Never married", 2, "Unclassified"))
table(ds$marital)
table(mothers_marital_status)

dsOld<-cbind(b_first_name, b_last_name, birth_date, birth_gender, mailing_street, mailing_city, address_zip, city_of_residence, mother_first_name, mother_last_name, mother_dob, mother_ssn, raw_mothers_hispanic_origin, raw_mothers_race, mothers_marital_status)
