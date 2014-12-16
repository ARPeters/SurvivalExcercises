library(survival)
library(foreign)

################################
#CHAPTER 4: Practice
################################

#Reading in vets dataset
dsVets <- read.dta("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/vets.dta")
names(dsVets)<-c("tx", "Large", "Adeno", "Small", "Squamous", "survt", "perf", "DisDur", "age", "priortx", "status")

#Fitting full COX PH Model
coxphPractice1<-coxph(Surv(dsVets$survt, dsVets$status==1)~tx+Large+Adeno+Small+perf+DisDur+age+priortx, ties="breslow", data=dsVets)
coxphPractice1

#Fitting a Poisson regression model
glm_pr_Practice1<-glm(status~tx+Large+Adeno+Small+perf+DisDur+age+priortx +offset(log(survt)), family="poisson", data=dsVets)
glm_pr_Practice1

#Testing PH Assumption for each predictor
cox.zph(coxphPractice1)

dsVetsEvents<-dsVets[dsVets$status==1,]
coxphPractice1Events<-coxph(Surv(dsVetsEvents$survt, dsVetsEvents$status==1)~tx+Large+Adeno+Small+perf+DisDur+age+priortx, 
                            ties="breslow", data=dsVetsEvents)

cox.zph(coxphPractice1Events, transform="rank")

################################
#CHAPTER 4: Test
################################

#Reading in Addicts Dataset
dsAddicts<- read.dta("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/addicts.dta")

#Fitting Full Cox PH Model
coxphTest1<-coxph(Surv(dsAddicts$survt, dsAddicts$status==1)~clinic+prison+dose, ties="breslow", data=dsAddicts)
coxphTest1

cox.zph(coxphTest1)

#Testing PH Assumption for each predictor
dsAddictsEvents<-dsAddicts[dsAddicts$status==1,]
coxphTest1Events<-coxph(Surv(dsAddictsEvents$survt, dsAddictsEvents$status==1)~clinic+prison+dose, ties="breslow", data=dsAddictsEvents)
cox.zph(coxphTest1Events, transform="rank")

#Fitting a Poisson regression model
glm_pr_Test1<-glm(status~clinic+prison+dose+offset(log(survt)), family="poisson", data=dsAddicts)
glm_pr_Test1

#Fitting a cox ph model stratifying on clinic
coxphAddictsClinic<-coxph(Surv(dsAddicts$survt, dsAddicts$status==1)~prison+dose+strata(clinic), ties="breslow", data=dsAddicts)
coxphAddictsClinic

#Fitting a Poisson regression model
glm_pr_Clinic<-glm(status~prison*strata(clinic)+dose*strata(clinic)+offset(log(survt)), family="poisson", data=dsAddicts)
glm_pr_Clinic


################################
#CHAPTER 5: Practice
################################

coxphPractice1Full<-coxph(Surv(dsVets$survt, dsVets$status==1)~tx+Large+Adeno+Small+perf+DisDur+age+priortx, ties="breslow", data=dsVets)
coxphPractice1Full

glm_pr_Practice1Full<-glm(status~tx+Large+Adeno+Small+perf+DisDur+age+priortx +offset(log(survt)), family="poisson", data=dsVets)
glm_pr_Practice1Full

coxphPractice1Reduced<-coxph(Surv(dsVets$survt, dsVets$status==1)~tx+Small+perf+DisDur+age+priortx, ties="breslow", data=dsVets)
coxphPractice1Reduced

glm_pr_Practice1Reduced<-glm(status~tx+Small+perf+DisDur+age+priortx+offset(log(survt)), family="poisson", data=dsVets)
glm_pr_Practice1Reduced


#Question3
perfStatus<-ifelse(dsVets$perf>59, 1, 0)
dsVets<-cbind(dsVets, perfStatus)

coxphVetsStrata3<-coxph(Surv(dsVets$survt, dsVets$status)~tx+DisDur+age+priortx +strata(Small, perfStatus), ties="breslow", data=dsVets)
coxphVetsStrata3

#Fitting a Poisson regression model
# Neither of these is a great fit; not sure how to reconcile this with a stratified cox model. 
glm_pr_Strata3<-glm(status~tx+DisDur+age+priortx+offset(log(survt))+strata(Small, perfStatus), family="poisson", data=dsVets)
glm_pr_Strata3

glm_pr_Strata3<-glm(status~tx*strata(Small, perfStatus)+DisDur*strata(Small, perfStatus)+age*strata(Small, perfStatus)+
                      priortx*strata(Small, perfStatus)+offset(log(survt))+strata(Small, perfStatus), family="poisson", data=dsVets)
glm_pr_Strata3

#Question 10:
coxphVetsStrata10<-coxph(Surv(dsVets$survt, dsVets$status)~tx+DisDur+age+priortx+
                           tx*strata(Small, perfStatus)+DisDur*strata(Small, perfStatus)+
                           age*strata(Small, perfStatus)+priortx*strata(Small, perfStatus)
                         , ties="breslow", data=dsVets)

glm_pr_Strata10<-glm(status~tx+DisDur+age+priortx+offset(log(survt))+tx*strata(Small, perfStatus)+DisDur*strata(Small, perfStatus)
                     +age*strata(Small, perfStatus)+priortx*strata(Small, perfStatus),
                     family="poisson", data=dsVets)

coxphVetsStrata10<-coxph(Surv(dsVets$survt, dsVets$status)~
                           tx*strata(Small, perfStatus)+DisDur*strata(Small, perfStatus)+age*strata(Small, perfStatus)+
                           priortx*strata(Small, perfStatus)
                         , ties="breslow", data=dsVets)

glm_pr_Strata10<-glm(status~offset(log(survt))+tx*strata(Small, perfStatus)+DisDur*strata(Small, perfStatus)
                     +age*strata(Small, perfStatus)+priortx*strata(Small, perfStatus),
                     family="poisson", data=dsVets)

summary(coxphVetsStrata10)

plot(coxphVetsStrata10)

################################
#CHAPTER 5: Test
################################

#Question 1:
coxphTest1<-coxph(Surv(dsAddicts$survt, dsAddicts$status)~clinic+prison+dose, ties="breslow", data=dsAddicts)
coxphTest1

cox.zph(coxphTest1, transform="rank")

glm_pr_Test1<-glm(status~clinic+prison+dose+offset(log(survt)), family="poisson", data=dsAddicts)
glm_pr_Test1

#Question 2:
coxphTest2<-coxph(Surv(dsAddicts$survt, dsAddicts$status)~prison+dose + strata(clinic), ties="breslow", data=dsAddicts)
coxphTest2

cox.zph(coxphTest2, transform="rank")

glm_pr_Test2<-glm(status~ prison + dose + offset(log(survt)) + strata(clinic), family="poisson", data=dsAddicts)
glm_pr_Test2

#Question 5
coxphTest5<-coxph(Surv(dsAddicts$survt, dsAddicts$status)~prison+dose
                  +prison*strata(clinic)+dose*strata(clinic), ties="breslow", data=dsAddicts)

glm_pr_Test5<-glm(status~prison +dose + offset(log(survt)) + prison*strata(clinic) + dose*strata(clinic),
                  family="poisson", data=dsAddicts)
glm_pr_Test5
