library(survival)
library(foreign)

################################
#Ch 4 Practice Questions
################################

#Read vets dataset
#File Layout
# Column 1: Treatment (standard = 1, test = 2)
# Column 2: Cell type 1 (large = 1, other = 0)
# Column 3: Cell type 2 (adeno = 1, other = 0)
# Column 4: Cell type 3 (small = 1, other = 0)
# Column 5: Cell type 4 (squamous = 1, other = 0)
# Column 6: Survival time (days)
# Column 7: Performance status (0 = worst, ..., 100 = best)
# Column 8: disease duration (months)
# Column 9: Age
# Column 10: Prior therapy (none = 0, some = 10)
# Column 11: Status (0 = censored, 1 = died)

dsVets <- read.dta("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/vets.dta")


names(dsVets)<-c("Treatment", "LargeCell", "AdenoCell", "SmallCell", "SquamousCell", "SurvivalTime", "PerformanceStatus", 
                 "DiseaseDuration", "Age", "PriorTherapy", "Status")

###Question:  How do you make a variable "readable" with a space in the variable name:
###           i.e. dsVets$Survival Time versus dsVets$SurvivalTime

#Practice Question 3


Ch4Practice3 <- coxph(Surv(dsVets$SurvivalTime, dsVets$Status==1) ~ dsVets$Treatment + dsVets$LargeCell + dsVets$AdenoCell + 
                        dsVets$SmallCell + dsVets$PerformanceStatus + dsVets$DiseaseDuration + dsVets$Age + dsVets$PriorTherapy, ties="breslow")
summary(Ch4Practice3)





#Testing PH 

cox.zph(Ch4Practice3, transform= "rank")

#
################################
#Ch 4 Test Questions
################################

#Read vets dataset
#File Layout
# Column 1: Subject ID
# Column 2: Clinic (1 or 2)
# Column 3: Survival Status (0 = censored, 1 = departed from clinic)
# Column 4: Survival time (days)
# Column 5: Prison record ( 0 = none, 1 = any)
# Column 6: Maximum methadone dose (mg/day)


dsAddicts<- read.dta("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/addicts.dta")

names(dsAddicts)<-c("Subject", "Clinic", "Status", "SurvivalTime", "Prison", "Dose")

#Test Question 1
Ch4Test1 <- coxph(Surv(SurvivalTime, Status) ~ Clinic + Prison + Dose, ties="breslow", data=dsAddicts)

summary(Ch4Test1)


Ch4Test1Poisson <- glm(Status ~ Clinic + Prison + Dose + offset(log(SurvivalTime)), (family=poisson), data=dsAddicts)
summary(Ch4Test1Poisson)

#dsAddictsOrd <- dsAddicts[order(dsAddicts$SurvivalTime,-dsAddicts$Status),]
#dsAddictsNew <- dsAddictsOrd[!duplicated(dsAddictsOrd$SurvivalTime),]

eventTimes <- unique(dsAddicts$SurvivalTime[dsAddicts$Status==1])
eventTimes <- eventTimes[order(eventTimes)]

require(plyr)

createPTable <- function(d){  
  dNew <- d
  for(i in 1:length(eventTimes)){
    dNew[i,] <- d
    dNew[i,"r"] <- i
    dNew[i,"tr"] <- eventTimes[i]
    dNew[i,"dir"] <- ifelse(i==1,min(d$SurvivalTime,eventTimes[i]),min(d$SurvivalTime,eventTimes[i]) - eventTimes[i-1])
    dNew[i,"yir"] <- 0
    if(d$SurvivalTime <= eventTimes[i]) {
      if(d$Status %in% 1) dNew[i,"yir"] <- 1
      break      
    }      
  }    
  return(dNew)
}

ptProcessDat <- ddply(.data=dsAddicts,.variables=.(Subject),.fun = createPTable)
ptProcessDat[ptProcessDat$Subject %in% c(166,111),]
dim(ptProcessDat)
colnames(dsAddicts)

#The counting process version of a Cox regression model
Ch4Test1PoissonNew <- glm(yir ~ I(as.factor(r)) + Clinic + Prison + Dose + offset(I(log(dir))), family=poisson(link = "log"), data=ptProcessDat)
summary(Ch4Test1PoissonNew)
#compare to the coxph output
summary(Ch4Test1)





Ch4Test1Poisson <- glm(SurvivalTime ~ Clinic + Prison + Dose + offset(log(Dose)), (family=poisson), data=dsAddicts)


summary(Ch4Test1Poisson)


Ch4Test1Poisson <- glm(SurvivalTime ~ Clinic + Prison + Dose, (family=poisson), data=dsAddicts)


summary(Ch4Test1Poisson)


#Test Question 1
Ch4Test1 <- coxph(Surv(SurvivalTime, Status==1) ~ Clinic, ties="breslow", data=dsAddicts)

summary(Ch4Test1)




Ch4Test1Poisson <- glm(SurvivalTime ~ offset(log(Clinic)), (family=poisson), data=dsAddicts)


summary(Ch4Test1Poisson)








