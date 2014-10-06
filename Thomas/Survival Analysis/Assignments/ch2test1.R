rm(list=ls(all=TRUE))
#load necessary packages
library(survival)


#Ch2 Test Question 1
#Using the vets.dat data set described in the presentation:
# File layout
# Column 1: Treatment (standard = 1, test = 2)
# Column 2: Cell type 1 (large = 1, other = 0)
# Column 3: Cell type 2 (adeno = 1, other = 0)
# Column 4: Cell type 3 (small = 1, other = 0)
# Column 5: Cell type 4 (squamous = 1, other = 0)
# Column 6: Survival time (days)
# Column 7: Performance Status (0 = worst . . . 100 = best)
# Column 8: Disease duration (months)
# Column 9: Age
# Column 10: Prior therapy (none = 0, some = 1)
# Column 11: Status (0 = censored, 1 = died)

#a.  Obtain KM plots for the two categories of the variable cell type 1 ( 1 = large, 0 = other).
#    Comment on how the two curves compare with each other.
#    Carry out the log-rank, and draw con clusions from the test(s).

#Read in dataset
ch2test1 <- read.table("S:\\CCAN\\CCANResEval\\Methods & Stat Training\\Thomas\\Survival Analysis\\Data Files\\vets.txt")

#Split columns
newNames <- c("treatment","large","adeno","small","squamous")
any(!sapply(strsplit(as.character(ch2test1$V1),""),function(x) sum(nchar(x))) == 5) #check that all v1 rows have 5 characters
ch2test1[,newNames] <- t(sapply(strsplit(as.character(ch2test1$V1),""),function(x) x))
# ch2test1$large     <- sapply(strsplit(as.character(ch2test1$V1),""),function(x) x[2])
# ch2test1$adeno     <- sapply(strsplit(as.character(ch2test1$V1),""),function(x) x[3])
# ch2test1$small     <- sapply(strsplit(as.character(ch2test1$V1),""),function(x) x[4])
# ch2test1$squamous  <- sapply(strsplit(as.character(ch2test1$V1),""),function(x) x[5])


#Delete 1st Column and reorder columns
ch2test1 <- ch2test1[, c(8:12, 2:7) ]

#Rename remaining columns
names(ch2test1)[6]  <- "survivaltime"
names(ch2test1)[7]  <- "performancestatus"
names(ch2test1)[8]  <- "diseaseduration"
names(ch2test1)[9]  <- "age"
names(ch2test1)[10] <- "priortherapy"
names(ch2test1)[11] <- "censorstatus"

#KM plots for cell type 1 (large)
#is this giving the survival plot and the confidence intervals?
sObj <- Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)
summary(survfit(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~1))
kmfit1 <- survfit(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~1)
plot(kmfit1)

#KM plots for cell type 1 (large)
Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)
summary(survfit(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~ch2test1$large))
kmfit2 <- survfit(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~ch2test1$large)
plot(kmfit2, col=c("black", "blue"))

survdiff(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~ch2test1$large)

#b.  Obtain KM plots for the four categories of cell type-large, adeno, small, and squamous.  
#    Note that you will need to recode the data to define a single variable which numerically distinguishes the four categories (e.g., 1 = large, 2= adeno, etc.)
#    As in part a, compare the four KM curves.  Also carry out the log-rank for the equality of the four cuves and draw conclusions.

#Creating New Variable celltype

ch2test1$celltype <- ifelse(ch2test1$large==1, 1, ifelse(ch2test1$adeno==1, 2, ifelse(ch2test1$small==1, 3, ifelse(ch2test1$squamous==1, 4, 0))))


#KM plots for all cell types
Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)
summary(survfit(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~ch2test1$celltype))
kmfit3 <- survfit(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~ch2test1$celltype)
plot(kmfit3, col=c("black", "red", "green", "blue"))


survdiff(Surv(ch2test1$survivaltime, ch2test1$censorstatus==1)~ch2test1$celltype)









