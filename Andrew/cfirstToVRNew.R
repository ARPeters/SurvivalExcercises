rm(list = ls(all.names = TRUE))
library(foreign)
library(plyr)

setwd("S:/CCAN/CCANResEval/MIECHV/RedCap/Chomp/DataSnapshots/C1/Activity/2015-04-10")
ds<-read.csv("childrenfirstdata.csv")
setwd("C:/Users/APETERS4/Documents/GitHub/SurvivalExercises/SurvivalExercises/SurvivalExercises/Andrew")



CHILD_GNAME<-ds$Child_First_Name_4610
CHILD_LNAME<-ds$Child_Last_Name_6756

#CHild_DOB > IDOB; MM/DD/YYYY
IDOB<-ds$CHild_DOB

#Infant_Gender_4634 > INFANT_SEX; MFU

INFANT_SEX<-ifelse(ds$Infant_Gender_4634=="Female", "F", ifelse(ds$Infant_Gender_4634=="Male", "M", "U"))
table(ds$Infant_Gender_4634)
table(INFANT_SEX)

#Clientfirst>MOTHER_GNAME
MOTHER_GNAME<-ds$clientfirst

#Clientlast>MOTHER_LNAME
MOTHER_LNAME<-ds$clientlast

#dob > MODB 
MDOB<-ds$dob

#MRACEBRT>...PCG_racecat and PCG_racec are apparently identical but it isn't clear how to turn cf format into VR format

colnames(ds)
table(ds$PCG_racecat)
table(ds$PCG_racec)

#marital>MARRIED_NOW; Y, N
#Married, Separated=Y
#Divorced, never married, widowed, = N
#"" = ""

MARRIED_NOW<-ifelse(ds$marital=="Married" | ds$marital=="Separated", "Y", ifelse(ds$marital=="Divorced" |ds$marital=="Never married" | ds$marital=="Widowed", "N", ""))

table(ds$marital)
table(MARRIED_NOW)

#PCG_SSN into MSSN; turn XXXXXXXXX into XXX-XX-XXXX
#http://stackoverflow.com/questions/13863599/insert-a-character-at-a-specific-location-in-a-string

split_str_by_index <- function(target, index) {
  index <- sort(index)
  substr(rep(target, length(index) + 1),
         start = c(1, index),
         stop = c(index -1, nchar(target)))
}

#Taken from https://stat.ethz.ch/pipermail/r-help/2006-March/101023.html
interleave <- function(v1,v2)
{
  ord1 <- 2*(1:length(v1))-1
  ord2 <- 2*(1:length(v2))
  c(v1,v2)[order(c(ord1,ord2))]
}

insert_str <- function(target, insert, index) {
  insert <- insert[order(index)]
  index <- sort(index)
  paste(interleave(split_str_by_index(target, index), insert), collapse="")
}

MSSN<-vector()

for(i in 1:length(ds$PCG_SSN)){
  if (ds$PCG_SSN[i]=="NA" | is.na(ds$PCG_SSN[i])==TRUE){
    next
  } 
  MSSN[i]<-insert_str(ds$PCG_SSN[i], c("-","-"), c(4,6))
}
MSSN[1:10]
PCG_SSN[1:10]


#address1>MAIL_ADDR1
#address2>MAIL_ADDR2
#city>MAIL_CITY
#zip>MAIL_ZIP

RES_ADDR1<-ds$address1
RES_ADDR2<-ds$address2
RES_CITY<-ds$city
RES_ZIP<-ds$zip

#address1>MAIL_ADDR1
#address2>MAIL_ADDR2
#city>MAIL_CITY
#zip>MAIL_ZIP

MAIL_ADDR1<-ds$address1
MAIL_ADDR2<-ds$address2
MAIL_CITY<-ds$city
MAIL_ZIP<-ds$zip


#PCG_ethnic>METHNIC_NO; N=Not Mexican/Chicana; H=Yes, U=unknown
#Formatting of VR data is ambiguous; Y=NO, Not spanish/hispanic/latina?
#BE SURE TO DOUBLE CHECK THIS ONE
table(ds$PCG_ethnic)
table(ds$ethnicity_c)

METHNIC_NO<-ifelse(ds$PCG_ethnic=="Hispanic", "N", "Y")
table(METHNIC_NO)

table(ds$racec)
table(ds$PCG_racecat)

#PCG_racecat>MRACE1; Y=White, N= not white
MRACE1<-ifelse(ds$PCG_racecat=="White", "Y", "N")
table(MRACE1)

#PCG_racecat>MRACE2; Y=Black/African American, N
MRACE2<-ifelse(ds$PCG_racecat=="Black or African American", "Y", "N")
table(MRACE2)

#PCG_racecat>MRACE3; Y=American Indian/Alaskan Native, N
MRACE3<-ifelse(ds$PCG_racecat=="American Indian or Alaska Native", "Y", "N")
table(MRACE3)

dsNew<-cbind(MOTHER_LNAME, MOTHER_GNAME, MSSN, MDOB, MRACE1, MRACE2, MRACE3, METHNIC_NO, RES_ADDR1, RES_ADDR2, RES_CITY, RES_ZIP, MARRIED_NOW, CHILD_LNAME, CHILD_GNAME, INFANT_SEX, IDOB)
