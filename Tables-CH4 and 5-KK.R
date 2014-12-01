library(survival)
library(foreign)

################################
#CHAPTER 4: Practice
################################

#Reading in vets dataset
dsVets <- read.dta("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/vets.dta")
names(dsVets)<-c("tx", "Large", "Adeno", "Small", "Squamous", "survt", "perf", "DisDur", "age", "priortx", "status")

#Fitting full COX PH Model
coxphPractice1<-coxph(Surv(dsVets$survt, dsVets$status==1)~dsVets$tx+dsVets$Large+dsVets$Adeno+dsVets$Small+dsVets$perf+dsVets$DisDur+dsVets$age+dsVets$priortx, ties="breslow")
coxphPractice1

#Testing PH Assumption for each predictor

cox.zph(coxphPractice1)


dsVetsEvents<-dsVets[dsVets$status==1,]
coxphPractice1Events<-coxph(Surv(dsVetsEvents$survt, dsVetsEvents$status==1)~dsVetsEvents$tx+dsVetsEvents$Large+dsVetsEvents$Adeno+dsVetsEvents$Small+dsVetsEvents$perf+dsVetsEvents$DisDur+dsVetsEvents$age+dsVetsEvents$priortx, ties="breslow")

cox.zph(coxphPractice1Events, transform="rank")

#Fitting Reduced COX PH Model
coxphPractice4b<-coxph(Surv(dsVets$survt, dsVets$status==1)~dsVets$tx+dsVets$Small+dsVets$perf+dsVets$DisDur+dsVets$age+dsVets$priortx, ties="breslow")
coxphPractice4b

cox.zph(coxphPractice4b, transform="rank")

#Adding a predictor that divides subjects in high performance (perf>=50) and low performance (perf<50) groups
perfHigh<-ifelse(dsVets$perf>=50, 1,0)
dsVets<-cbind(dsVets, perfHigh)

#Plotting log-log survival curve for each performance group.
survtest<-survfit(Surv(dsVets$survt, dsVets$status==1)~dsVets$perfHigh)
plot(survtest, fun="cloglog", )

#Add Observed versus Expected tomorrow when I have access to other code.

################################
#CHAPTER 4: Test
################################

#Reading in Addicts Dataset
dsAddicts<- read.dta("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/addicts.dta")

#Fitting Full Cox PH Model
coxphTest1<-coxph(Surv(dsAddicts$survt, dsAddicts$status==1)~dsAddicts$clinic+dsAddicts$prison+dsAddicts$dose, ties="breslow")
coxphTest1

#Testing PH Assumption for each predictor
dsAddictsEvents<-dsAddicts[dsAddicts$status==1,]
coxphTest1Events<-coxph(Surv(dsAddictsEvents$survt, dsAddictsEvents$status==1)~dsAddictsEvents$clinic+dsAddictsEvents$prison+dsAddictsEvents$dose, ties="breslow")
cox.zph(coxphTest1Events, transform="rank")

#Fitting a cox ph model stratifying on clinic
coxphAddictsClinic<-coxph(Surv(dsAddicts$survt, dsAddicts$status==1)~dsAddicts$prison+dsAddicts$dose+strata(dsAddicts$clinic), ties="breslow")
coxphAddictsClinic


################################
#CHAPTER 5: Practice
################################

#Reading in Anderson (Leukemia) Dataset
dsAnderson<- read.dta("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/anderson.dta")


coxphPractice5<-coxph(Surv(dsAnderson$survt, dsAnderson$status==1)~dsAnderson$sex+dsAnderson$logwbc+dsAnderson$rx, ties="breslow")
coxphPractice5

cox.zph(coxphPractice5, transform="rank") ##NOt sure why this one is dead on, and others were off. Revisit.

