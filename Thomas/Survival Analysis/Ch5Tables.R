library(survival)
library(foreign)


################################
#CHAPTER 5: Practice
################################

#Reading in Anderson (Leukemia) Dataset
dsAnderson<- read.dta("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/anderson.dta")


coxphPractice5<-coxph(Surv(dsAnderson$survt, dsAnderson$status==1)~dsAnderson$sex+dsAnderson$logwbc+dsAnderson$rx, ties="breslow")
coxphPractice5

cox.zph(coxphPractice5, transform="rank")
