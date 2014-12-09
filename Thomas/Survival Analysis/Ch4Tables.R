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
Ch4Test1 <- coxph(Surv(dsAddicts$SurvivalTime, dsAddicts$Status==1) ~ dsAddicts$Clinic + dsAddicts$Prison + dsAddicts$Dose, ties="breslow")

summary(Ch4Test1)

