library(survival)
library(foreign)


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



Ch5Practice10 <- coxph(Surv(SurvivalTime, Status==1) ~ Treatment + DiseaseDuration + Age + PriorTherapy + 
                         DiseaseDuration:Z1 + Age:Z1 + PriorTherapy:Z1 + 
                         DiseaseDuration:Z2 + Age:Z2 + PriorTherapy:Z2 + 
                         DiseaseDuration:Z1:Z2 + Age:Z1:Z2 + PriorTherapy:Z1:Z2 + 
                         Treatment:Z1 + Treatment:Z1 + Treatment:Z1:Z2 + 
                         strata(Z1, Z2), data = dsVets, ties="breslow")
summary(Ch5Practice10)

cor(dsVets[,c()])



