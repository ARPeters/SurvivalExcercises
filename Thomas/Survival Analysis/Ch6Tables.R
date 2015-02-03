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
dsChemo <- read.table("http://statweb.stanford.edu/~olshen/hrp262spring01/spring01Assignments/chemo.txt", skip = 16)

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
#using page 648 of text as a reference

ChemoPhSplit=survSplit(dsChemo,cut=250,end="SurvTime", event="Status",start="start", id="id")

ChemoPhSplit$time1=ChemoPhSplit