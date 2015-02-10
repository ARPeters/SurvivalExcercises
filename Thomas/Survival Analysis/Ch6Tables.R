library(survival)
library(foreign)

rm(list=ls(all=TRUE))
################################
#CHAPTER 6: Test
################################


#File Layout

#  Column 1 = Rx (1=chemotherapy, 2=chemotherapy and radiation)
#  Column 2 = status (0=censored, 1=died)
#  Column 3 = survival time (days)

#Read Anderson dataset
dsChemo <- read.table("http://statweb.stanford.edu/~olshen/hrp262spring01/spring01Assignments/chemo.txt", skip = 17)

names(dsChemo)<-c("Rx", "Status", "SurvTime")
#Test dsChemo

#KM plots for each treatment group
kmfit1 <- survfit(Surv(SurvTime, Status==1)~Rx, data=dsChemo)
summary(kmfit1)
plot(kmfit1)


#Test 2
ChemoPh<-coxph(Surv(SurvTime, Status==1)~Rx, data=dsChemo, ties="breslow")
summary(ChemoPh)


#Test 6 
#start at page 646 of text as a reference

#Chemo.cp=survSplit(dsChemo,cut=dsChemo$SurvTime[dsChemo$Status==1], end="SurvTime", event="Status", start="start", id="id")


Chemo.cp250 <- survSplit(dsChemo, cut=250, end="SurvTime", event="Status", start="start", id="id")                   

Chemo.cp250=Chemo.cp250[order(Chemo.cp250$id, Chemo.cp250$start,])]


Chemo.cp250$Time1 <- Chemo.cp250$Rx*(Chemo.cp250$start<250)
Chemo.cp250$Time2 <- Chemo.cp250$Rx*(Chemo.cp250$start>=250)


Y250=Surv(Chemo.cp250$start, Chemo.cp250$SurvTime, Chemo.cp250$Status)

ChemoPh2<-coxph(Y250~Time1 +  Time2, data=Chemo.cp250)

summary(ChemoPh2)



##Why doesn't this give the same answer as above?
ChemoPh3<-coxph(Surv(Chemo.cp250$start, Chemo.cp250$SurvTime, Chemo.cp250$Status)~Time1 +  Time2, data=Chemo.cp250)

summary(ChemoPh3)



#Test 3

Chemo.cp250500 <- survSplit(dsChemo, cut=c(250, 500), end="SurvTime", event="Status", start="start", id="id")                   


Chemo.cp250500$Time1 <- Chemo.cp250500$Rx*(Chemo.cp250500$start<250)
Chemo.cp250500$Time2 <- Chemo.cp250500$Rx*(Chemo.cp250500$start>=250 & Chemo.cp250500$start<500)
Chemo.cp250500$Time3 <- Chemo.cp250500$Rx*(Chemo.cp250500$start>=500)


Y250500=Surv(Chemo.cp250500$start, Chemo.cp250500$SurvTime, Chemo.cp250500$Status)

ChemoPh4<-coxph(Y250500~Time1 +  Time2 + Time3 + cluster(id), data=Chemo.cp250500)

summary(ChemoPh4)



##Why doesn't this give the same answer as above?
ChemoPh5<-coxph(Surv(Chemo.cp250500$start, Chemo.cp250500$SurvTime, Chemo.cp250500$Status)~Time1 +  Time2 + Time3, data=Chemo.cp250500)

summary(ChemoPh5)



