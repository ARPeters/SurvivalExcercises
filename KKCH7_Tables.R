library(survival)
library(foreign)
library(SurvRegCensCov)


#####################################################
##Recreating table from ch7 exercise questions 8-11
#####################################################
#Getting and formatting data
dsVetsColumns<-c("Tx", "Survt", "Perf", "DD", "Age", "Priortx", "Status")

dsVets<-read.table("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/vets.dat", col.names=(dsVetsColumns))
dsVets<-dsVets[-138,]
dsVets$Tx<-ifelse(dsVets$Tx==10001 | dsVets$Tx==10010 | dsVets$Tx==10100 |dsVets$Tx== 11000, c("standard"), c("test"))

#Fitting AFT Form
ExpAFTVets<-survreg(Surv(dsVets$Survt, dsVets$Status==1)~dsVets$Tx + dsVets$Perf + dsVets$DD + dsVets$Age + dsVets$Priortx, dist='loglogistic')
summary(ExpAFTVets)

#####################################################
##Recreating table from ch7 Test questions 18 - 19
#####################################################
## Getting data

AddictColumns=c("ID", "Clinic", "Status", "Survt", "Prison", "Dose")
dsAddicts <- read.table("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/addicts.dat", skip=23, col.names=(AddictColumns))

## Weibull AFT form
WeibullAFTAddicts<-survreg(Surv(dsAddicts$Survt, dsAddicts$Status)~dsAddicts$Clinic + dsAddicts$Prison + dsAddicts$Dose + dsAddicts$Prison*dsAddicts$Dose, dist='weibull')
summary(WeibullAFTAddicts)

## Weibull log relative-hazard form (From AFT FOrm)
WeibullAFTAddicts$s
WeibullAFTAddicts$coef/(-1*WeibullAFTAddicts$s)

## Weibull log relative hazard form (from WeibullReg() function)
WeibullAFTAddicts_Other_Function<-WeibullReg(Surv(dsAddicts$Survt, dsAddicts$Status)~dsAddicts$Clinic + dsAddicts$Prison + dsAddicts$Dose + dsAddicts$Prison*dsAddicts$Dose, data=dsAddicts)
WeibullAFTAddicts_Other_Function$HR
log(WeibullAFTAddicts_Other_Function$HR)

#####################################################
##Recreating table from ch7 Test questions 11 - 17
#####################################################

## Weibull regression AFT form with gamma frailty
WeibullAFTAddicts_Frailty<-survreg(Surv(dsAddicts$Survt, dsAddicts$Status)~dsAddicts$Clinic + dsAddicts$Prison + dsAddicts$Dose + dsAddicts$Prison*dsAddicts$Dose + frailty.gamma(dsAddicts$Clinic), dist='weibull')
summary(WeibullAFTAddicts_Frailty)
