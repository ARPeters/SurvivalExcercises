library(survival)
library(foreign)

rm(list=ls(all=TRUE))
################################
#CHAPTER 5: Practice
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


#adding a subject ID variable for dsVets

numrows <- nrow(dsVets)

dsVets$Subject <- c(1:numrows)


###Question:  How do you make a variable "readable" with a space in the variable name:
###           i.e. dsVets$Survival Time versus dsVets$SurvivalTime

#Practice Question 1


Ch5Practice1a <- coxph(Surv(SurvivalTime, Status==1) ~ Treatment + LargeCell + AdenoCell + 
                        SmallCell + PerformanceStatus + DiseaseDuration + Age + PriorTherapy, data = dsVets, ties="breslow")
summary(Ch5Practice1a)


Ch5Practice1b <- coxph(Surv(SurvivalTime, Status==1) ~ Treatment +  
                         SmallCell + PerformanceStatus + DiseaseDuration + Age + PriorTherapy, data = dsVets, ties="breslow")
summary(Ch5Practice1b)


##Create New Variables Z1 Small cell
##                     Z2 Performance Status High (60 or above) or Low (Below 60)

dsVets$Z1 = dsVets$SmallCell
dsVets$Z2 <- ifelse(dsVets$PerformanceStatus > 59, 1, 0)



Ch5Practice3 <- coxph(Surv(SurvivalTime, Status==1) ~ Treatment + DiseaseDuration + Age + PriorTherapy + strata(Z1, Z2), data = dsVets, ties="breslow")
summary(Ch5Practice3)

Ch5Practice3a <- coxph(Surv(SurvivalTime, Status) ~ Treatment + DiseaseDuration + Age + PriorTherapy + strata(Z1), data = dsVets, ties="breslow")
summary(Ch5Practice3a)




#GLM Procedure
require(plyr)

createEventTimes <- function(d){
	eventTimes <- unique(d$SurvivalTime[d$Status==1])
	eventTimes <- eventTimes[order(eventTimes)]
	return(eventTimes)
}

createPTable <- function(d){
  eventTimes <- createEventTimes(dsVets[dsVets$Z1 %in% unique(d$Z1),])
  #eventTimes <- createEventTimes(dsVets)
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

ctProcessDat <- ddply(.data=dsVets,.variables=.(Subject,Z1,Z2),.fun = createPTable)
ctProcessDat[ctProcessDat$Subject %in% c(166,111),]
max(ctProcessDat$r)
ctProcessDat$rNew <- paste0(ctProcessDat$r,ctProcessDat$Z1,ctProcessDat$Z2)
dim(ctProcessDat)
colnames(dsVets)
table(ctProcessDat$rNew)

#The counting process version of a Cox regression model
#Ch5Practice3PoissonNewA <- glm(yir ~ I(as.factor(r)):strata(Z1) + Treatment + DiseaseDuration + Age + PriorTherapy + offset(I(log(dir))), family=poisson(link = "log"), data=ctProcessDat)
Ch5Practice3PoissonNewB <- glm(yir ~ I(as.factor(rNew)) + Treatment + DiseaseDuration + Age + PriorTherapy + offset(I(log(dir))), family=poisson(link = "log"), data=ctProcessDat)
summary(Ch5Practice3PoissonNewB)

summary(Ch5Practice3)$coefficients[,c(1,3:5)]
#summary(Ch5Practice3PoissonNewA)$coefficients[2:5,]
summary(Ch5Practice3PoissonNewB)$coefficients[(nrow(summary(Ch5Practice3PoissonNewB)$coefficients)-3):nrow(summary(Ch5Practice3PoissonNewB)$coefficients),]

#ctProcessDat[order(ctProcessDat$r,ctProcessDat$Z1,ctProcessDat$Subject),c("Subject","Z1","r")][1:200,]




#Ch5Practice10 <- coxph(Surv(SurvivalTime, Status==1) ~ Treatment + DiseaseDuration + Age + PriorTherapy + 
#                         DiseaseDuration:Z1 + Age:Z1 + PriorTherapy:Z1 + 
#                         DiseaseDuration:Z2 + Age:Z2 + PriorTherapy:Z2 + 
#                         DiseaseDuration:Z1:Z2 + Age:Z1:Z2 + PriorTherapy:Z1:Z2 + 
#                         Treatment:Z1 + Treatment:Z1 + Treatment:Z1:Z2 + 
#                         strata(Z1, Z2), data = dsVets, ties="breslow")
Ch5Practice10 <- coxph(Surv(SurvivalTime, Status==1) ~ Treatment*strata(Z1,Z2) + DiseaseDuration*strata(Z1,Z2) + Age*strata(Z1,Z2) + PriorTherapy*strata(Z1,Z2), 
		data = dsVets, ties="breslow")
summary(Ch5Practice10)

Ch5Practice10PoissonNew <- glm(yir ~ I(as.factor(rNew)) + Treatment + DiseaseDuration + Age + PriorTherapy + offset(I(log(dir))) +
				+ Treatment:Z1 + DiseaseDuration:Z1 + Age:Z1 + PriorTherapy:Z1 +
				+ Treatment:Z2 + DiseaseDuration:Z2 + Age:Z2 + PriorTherapy:Z2 +
				+ Treatment:Z1:Z2 + DiseaseDuration:Z1:Z2 + Age:Z1:Z2 + PriorTherapy:Z1:Z2, family=poisson(link = "log"), data=ctProcessDat)
summary(Ch5Practice10PoissonNew)


cor(dsVets[,c()])



