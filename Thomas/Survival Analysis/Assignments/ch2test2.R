rm(list=ls(all=TRUE))
#load necessary packages
library(survival)


#Ch2 Test Question 2
#Using the addicts.dat data set described in the presentation:
# File layout
# Column 1: Subject ID
# Column 2: Clinic (1 or 2)
# Column 3: Survival status (0 = censored, 1 = departed from clinic)
# Column 4: Survival time in days
# Column 5: Prison record (0 = none, 1 = any)
# Column 6: Methadone dose (mg/day)

#a.  Compute and plot the KM plots for the two categories of the "clinic" variable and comment on the extent to which they differ.

#Read in dataset
ch2test2 <- read.table("S:\\CCAN\\CCANResEval\\Methods & Stat Training\\Thomas\\Survival Analysis\\Data Files\\addicts.txt", skip = 19)

#Rename remaining columns
names(ch2test2)[1]  <- "subjectid"
names(ch2test2)[2]  <- "clinic"
names(ch2test2)[3]  <- "censorstatus"
names(ch2test2)[4]  <- "survivaltime"
names(ch2test2)[5]  <- "prisonrecord"
names(ch2test2)[6]  <- "methadonedose"

#KM plots for clinic
Surv(ch2test2$survivaltime, ch2test2$censorstatus==1)
summary(survfit(Surv(ch2test2$survivaltime, ch2test2$censorstatus==1)~ch2test2$clinic))
kmfit1 <- survfit(Surv(ch2test2$survivaltime, ch2test2$censorstatus==1)~ch2test2$clinic)
plot(kmfit1, col=c("black", "blue"))

#b. A printout of the log-rank and Wilcoxon tests (using Stata) is provided below.  What are your conclusions from this printout?
#The Log-rank chi2 is equal to 27.89 with a p-value = .0000.  It appears that the two groups do NOT have the same survival curves
#The chi2 and p-value correspond to the KM plot.

#c. Compute and evaluate KM curves and the log-rank test for comparing suitably chosen categores of the variable "Methadone dose."
#   Explain how you determined the categories for this variable.

#Calculating the 5 number summary
summary(ch2test2$methadonedose)

#I chose to partition the data in quartiles.  Mainly because I don't know squat about methadone and the exercise will play out the same
#regardless of my chosen categories.  In real life, I would consult a person who knows more about methadone than I do.
#methadonegroup = 1 corresponds to methadonedose < 50mg/day
#methadonegroup = 2 corresponds to 50 <= methadonedose < 60mg/day
#methadonegroup = 3 corresponds to 60 <= methadonedose < 70mg/day
#methadonegroup = 4 corresponds to methadonedose >= 70mg/day


#Creating the methadone dose partitions
ch2test2$methadonegroup <- ifelse(ch2test2$methadonedose < 50, 1, ifelse(ch2test2$methadonedose>=50 & ch2test2$methadonedose < 60, 2, 
                          ifelse(ch2test2$methadonedose>=60 & ch2test2$methadonedose < 70, 3, 4)))

#KM plots for methadonegroup
Surv(ch2test2$survivaltime, ch2test2$censorstatus==1)
summary(survfit(Surv(ch2test2$survivaltime, ch2test2$censorstatus==1)~ch2test2$methadonegroup))
kmfit2 <- survfit(Surv(ch2test2$survivaltime, ch2test2$censorstatus==1)~ch2test2$methadonegroup)
plot(kmfit2, col=c("black", "red", "green", "blue"))

#Log Rank

survdiff(Surv(ch2test2$survivaltime, ch2test2$censorstatus==1)~ch2test2$methadonegroup)
survdiff(Surv(ch2test2$survivaltime, ch2test2$censorstatus==1)~ch2test2$methadonegroup, rho=1)


















#Split columns
ch2test1$treatment <- sapply(strsplit(as.character(ch2test1$V1),""),function(x) x[1])
ch2test1$large     <- sapply(strsplit(as.character(ch2test1$V1),""),function(x) x[2])
ch2test1$adeno     <- sapply(strsplit(as.character(ch2test1$V1),""),function(x) x[3])
ch2test1$small     <- sapply(strsplit(as.character(ch2test1$V1),""),function(x) x[4])
ch2test1$squamous  <- sapply(strsplit(as.character(ch2test1$V1),""),function(x) x[5])

#Delete 1st Column and reorder columns
ch2test1 <- ch2test1[, c(8, 9, 10, 11, 12, 2, 3, 4, 5, 6, 7) ]

#Rename remaining columns
names(ch2test1)[6]  <- "survivaltime"
names(ch2test1)[7]  <- "performancestatus"
names(ch2test1)[8]  <- "diseaseduration"
names(ch2test1)[9]  <- "age"
names(ch2test1)[10] <- "priortherapy"
names(ch2test1)[11] <- "censorstatus"

#KM plots for cell type 1 (large)
#is this giving the survival plot and the confidence intervals?
Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)
summary(survfit(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~1))
kmfit1 <- survfit(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~1)
plot(kmfit1)

#KM plots for cell type 1 (large)
Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)
summary(survfit(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~ch2test1$large))
kmfit2 <- survfit(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~ch2test1$large)
plot(kmfit2)




#b.  Obtain KM plots for the four categories of cell type-large, adeno, small, and squamous.  
#    Note that you will need to recode the data to define a single variable which numerically distinguishes the four categories (e.g., 1 = large, 2= adeno, etc.)
#    As in part a, compare the four KM curves.  Also carry out the log-rank for the equality of the four cuves and draw conclusions.

#Creating New Variable celltype

ch2test1$celltype <- ifelse(ch2test1$large==1, 1, ifelse(ch2test1$adeno==1, 2, ifelse(ch2test1$small==1, 3, ifelse(ch2test1$squamous==1, 4, 0))))

#KM plots for all cell types
Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)
summary(survfit(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~ch2test1$celltype))
kmfit3 <- survfit(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~ch2test1$celltype)
plot(kmfit3)











