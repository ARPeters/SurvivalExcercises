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

#Fitting a Poisson regression model
glm_pr_Practice1<-glm(status~tx+Large+Adeno+Small+perf+DisDur+age+priortx +offset(log(survt)), family="poisson", data=dsVets)
glm_pr_Practice1


#Testing PH Assumption for each predictor
zphRes <- cox.zph(coxphPractice1, transform="rank")

#To recreate the table results from K&K, you need to use the "unscaled" Schoenfield residuals as demonstrated below
scaledRes <- residuals(coxphPractice1,type="scaledsch")
unscaledRes <- residuals(coxphPractice1,type="schoenfeld")
cbind(oldRes,newRes)
cbind(zphRes$y[,1],scaledRes[,1],unscaledRes[,1])
apply(unscaledRes,2,function(z) { summary(lm(zphRes$x~z))$coefficients[2,4] } )

dsVetsEvents<-dsVets[dsVets$status==1,]
coxphPractice1Events<-coxph(Surv(dsVetsEvents$survt, dsVetsEvents$status==1)~dsVetsEvents$tx+dsVetsEvents$Large+dsVetsEvents$Adeno+dsVetsEvents$Small+dsVetsEvents$perf+dsVetsEvents$DisDur+dsVetsEvents$age+dsVetsEvents$priortx, ties="breslow")

cox.zph(coxphPractice1Events, transform="rank")

#Fitting Reduced COX PH Model
coxphPractice4b<-coxph(Surv(dsVets$survt, dsVets$status==1)~dsVets$tx+dsVets$Small+dsVets$perf+dsVets$DisDur+dsVets$age+dsVets$priortx, ties="breslow")
coxphPractice4b

cox.zph(coxphPractice4b, transform="rank")


#Fitting a Poisson regression model
glm_pr_Practice4b<-glm(status~tx+Small+perf+DisDur+age+priortx +offset(log(survt)), family="poisson", data=dsVets)
glm_pr_Practice4b

#Adding a predictor that divides subjects in high performance (perf>=50) and low performance (perf<50) groups
perfHigh<-ifelse(dsVets$perf>=50, 1,0)
dsVets<-cbind(dsVets, perfHigh)


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


#Fitting a Poisson regression model
glm_pr_Test1<-glm(status~clinic+prison+dose+offset(log(survt)), family="poisson", data=dsAddicts)
glm_pr_Test1


#Fitting a cox ph model stratifying on clinic
coxphAddictsClinic<-coxph(Surv(dsAddicts$survt, dsAddicts$status==1)~dsAddicts$prison+dsAddicts$dose+strata(dsAddicts$clinic), ties="breslow")
coxphAddictsClinic

#Fitting a Poisson regression model
glm_pr_Clinic<-glm(status~prison*strata(clinic)+dose*strata(clinic)+offset(log(survt)), family="poisson", data=dsAddicts)
glm_pr_Clinic


################################
#CHAPTER 5: Practice
################################

coxphPractice1Full<-coxph(Surv(dsVets$survt, dsVets$status)~dsVets$tx+dsVets$Large+dsVets$Adeno+dsVets$Small+dsVets$perf+dsVets$DisDur+dsVets$age+dsVets$priortx, ties="breslow")
coxphPractice1Full

glm_pr_Practice1Full<-glm(status~tx+Large+Adeno+Small+perf+DisDur+age+priortx +offset(log(survt)), family="poisson", data=dsVets)
glm_pr_Practice1Full


coxphPractice1Reduced<-coxph(Surv(dsVets$survt, dsVets$status)~dsVets$tx+dsVets$Small+dsVets$perf+dsVets$DisDur+dsVets$age+dsVets$priortx, ties="breslow")
coxphPractice1Reduced

glm_pr_Practice1Reduced<-glm(status~tx+Small+perf+DisDur+age+priortx+offset(log(survt)), family="poisson", data=dsVets)
glm_pr_Practice1Reduced


#Question3
perfStatus<-ifelse(dsVets$perf>59, 1, 0)
dsVets<-cbind(dsVets, perfStatus)

coxphVetsStrata3<-coxph(Surv(dsVets$survt, dsVets$status)~dsVets$tx+dsVets$DisDur+dsVets$age+dsVets$priortx +strata(dsVets$Small, dsVets$perfStatus), ties="breslow")
coxphVetsStrata3

#Fitting a Poisson regression model
# Neither of these is a great fit; not sure how to reconcile this with a stratified cox model. 
glm_pr_Strata3<-glm(status~tx+DisDur+age+priortx+offset(log(survt))+strata(Small, perfStatus), family="poisson", data=dsVets)
glm_pr_Strata3

glm_pr_Strata3<-glm(status~tx*strata(Small, perfStatus)+DisDur*strata(Small, perfStatus)+age*strata(Small, perfStatus)+priortx*strata(Small, perfStatus)+offset(log(survt))+strata(Small, perfStatus), family="poisson", data=dsVets)
glm_pr_Strata3

#Question 10:

coxphVetsStrata10<-coxph(Surv(survt, status)~tx+DisDur+age+priortx+
                           tx*strata(Small, perfStatus)+DisDur*strata(Small, perfStatus)+age*strata(Small, perfStatus)+priortx*strata(Small, perfStatus)
                         ,ties="breslow", data=dsVets)
coxphVetsStrata10

glm_pr_Strata10<-glm(status~tx+DisDur+age+priortx+offset(log(survt))+tx*strata(Small, perfStatus)+DisDur*strata(Small, perfStatus)
                            +age*strata(Small, perfStatus)+priortx*strata(Small, perfStatus),
                     family="poisson", data=dsVets)
glm_pr_Strata10


################################
#CHAPTER 5: Test
################################

coxphTest1<-coxph(Surv(dsAddicts$survt, dsAddicts$status)~dsAddicts$clinic+dsAddicts$prison+dsAddicts$dose, ties="breslow")
coxphTest1

glm_pr_Test1<-glm(status~clinic+prison+dose+offset(log(survt)), family="poisson", data=dsAddicts)
glm_pr_Test1


cox.zph(coxphTest1, transform="rank")

coxphTest2<-coxph(Surv(dsAddicts$survt, dsAddicts$status)~dsAddicts$prison+dsAddicts$dose + strata(dsAddicts$clinic), ties="breslow")
coxphTest2

glm_pr_Test2<-glm(status~prison+dose+offset(log(survt)) + strata(clinic), family="poisson", data=dsAddicts)
glm_pr_Test2


cox.zph(coxphTest2, transform="rank")


coxphTest5<-coxph(Surv(dsAddicts$survt, dsAddicts$status)~dsAddicts$prison+dsAddicts$dose
                  +dsAddicts$prison*strata(dsAddicts$clinic)+dsAddicts$dose*strata(dsAddicts$clinic), ties="breslow")
coxphTest5

glm_pr_Test5<-glm(status~prison+dose+offset(log(survt)) + prison*strata(clinic)+dose*strata(clinic), family="poisson", data=dsAddicts)
glm_pr_Test5
