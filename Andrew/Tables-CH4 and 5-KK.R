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

################################
#CHAPTER 4: Test
################################

#Reading in Addicts Dataset
dsAddicts<- read.dta("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/addicts.dta")

#Fitting Full Cox PH Model
coxphTest1<-coxph(Surv(dsAddicts$survt, dsAddicts$status==1)~dsAddicts$clinic+dsAddicts$prison+dsAddicts$dose, ties="breslow")
coxphTest1

cox.zph(coxphTest1)

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

coxphPractice1Full<-coxph(Surv(dsVets$survt, dsVets$status)~dsVets$tx+dsVets$Large+dsVets$Adeno+dsVets$Small+dsVets$perf+dsVets$DisDur+dsVets$age+dsVets$priortx, ties="breslow")
coxphPractice1Full

coxphPractice1Reduced<-coxph(Surv(dsVets$survt, dsVets$status)~dsVets$tx+dsVets$Small+dsVets$perf+dsVets$DisDur+dsVets$age+dsVets$priortx, ties="breslow")
coxphPractice1Reduced

#Question3
perfStatus<-ifelse(dsVets$perf>59, 1, 0)
dsVets<-cbind(dsVets, perfStatus)

coxphVetsStrata3<-coxph(Surv(dsVets$survt, dsVets$status)~dsVets$tx+dsVets$DisDur+dsVets$age+dsVets$priortx +strata(dsVets$Small, dsVets$perfStatus), ties="breslow")
coxphVetsStrata3

#Question 10:

coxphVetsStrata10<-coxph(Surv(dsVets$survt, dsVets$status)~dsVets$tx+dsVets$DisDur+dsVets$age+dsVets$priortx+
                           dsVets$tx*strata(dsVets$Small, dsVets$perfStatus)+dsVets$DisDur*strata(dsVets$Small, dsVets$perfStatus)+dsVets$age*strata(dsVets$Small, dsVets$perfStatus)+dsVets$priortx*strata(dsVets$Small, dsVets$perfStatus)
                         , ties="breslow")

coxphVetsStrata10<-coxph(Surv(dsVets$survt, dsVets$status)~
                           dsVets$tx*strata(dsVets$Small, dsVets$perfStatus)+dsVets$DisDur*strata(dsVets$Small, dsVets$perfStatus)+dsVets$age*strata(dsVets$Small, dsVets$perfStatus)+dsVets$priortx*strata(dsVets$Small, dsVets$perfStatus)
                         , ties="breslow")

summary(coxphVetsStrata10)

plot(coxphVetsStrata10)

################################
#CHAPTER 5: Test
################################

#Question 1:
coxphTest1<-coxph(Surv(dsAddicts$survt, dsAddicts$status)~dsAddicts$clinic+dsAddicts$prison+dsAddicts$dose, ties="breslow")
coxphTest1

cox.zph(coxphTest1, transform="rank")

#Question 2:
coxphTest2<-coxph(Surv(dsAddicts$survt, dsAddicts$status)~dsAddicts$prison+dsAddicts$dose + strata(dsAddicts$clinic), ties="breslow")
coxphTest2

cox.zph(coxphTest2, transform="rank")

#Question 5
coxphTest5<-coxph(Surv(dsAddicts$survt, dsAddicts$status)~dsAddicts$prison+dsAddicts$dose
                  +dsAddicts$prison*strata(dsAddicts$clinic)+dsAddicts$dose*strata(dsAddicts$clinic), ties="breslow")

aov(coxphTest2, coxphTest5)
?qchisq()
pchisq(1.88, df=2, lower.tail=FALSE)
