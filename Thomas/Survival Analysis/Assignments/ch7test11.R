rm(list=ls(all=TRUE))
#load necessary packages
library(survival)


#Ch7 Test Questions 11-17
#Using the addicts.dat data set described in the presentation:
# File layout
# Column 1: Subject ID
# Column 2: Clinic (1 or 2)
# Column 3: Survival status (0 = censored, 1 = departed from clinic)
# Column 4: Survival time in days
# Column 5: Prison record (0 = none, 1 = any)
# Column 6: Methadone dose (mg/day)

# Column 6: New Variable: prisdose = prison record * dose

#Read in dataset
ch7test1 <- read.table("S:\\CCAN\\CCANResEval\\Methods & Stat Training\\Thomas\\Survival Analysis\\Data Files\\addicts.txt", skip = 19)

#Rename remaining columns
names(ch7test1)[1]  <- "subjectid"
names(ch7test1)[2]  <- "clinic"
names(ch7test1)[3]  <- "censorstatus"
names(ch7test1)[4]  <- "survivaltime"
names(ch7test1)[5]  <- "prison"
names(ch7test1)[6]  <- "dose"

ch7test1$prisdose <- ch7test1$prison * ch7test1$dose

#recreating the Weibull regression tables

## Weibull AFT form
AddictsWeibull<-survreg(Surv(ch7test1$survivaltime, ch7test1$censorstatus)~ch7test1$clinic + ch7test1$prison + ch7test1$dose + ch7test1$prison*ch7test1$dose, dist='weibull')
summary(AddictsWeibull)

## Weibull AFT form using product pris*dose variable
AddictsWeibull<-survreg(Surv(ch7test1$survivaltime, ch7test1$censorstatus)~ch7test1$clinic + ch7test1$prison + ch7test1$dose + ch7test1$prisdose, dist='weibull')
summary(AddictsWeibull)


AddictsWeibull

## Weibull log relative-hazard form (From AFT FOrm)
AddictsWeibull$s
AddictsWeibull$coef/(-1*AddictsWeibull$s)

