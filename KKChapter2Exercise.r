# Kleinbaum & Klein, Chapter 2: Kaplan-Meier Survival Estimates

# Loads in necessary packages
library(survival)
library(foreign)

# Practice Exercise 1-B
# Reads in .csv datafile of relevent data
#    Column 1 variable: Time
#    Column 2 variable: Status (0=censored, 1=event occured)
#    Column 3 variable: group (0="CH=0", 1="CH=1")
KMPracticeQ1Data <- read.csv("KMPracticeQ1Data.csv")

# Sets column names to variable names, and then creates survival object from KMPracticeQ1Data
attach(KMPracticeQ1Data)
KMPracticeQ1Surv<-Surv(time, status==1)


# survfit function breaks the survival times into two groups based on the variable "group" and 
#    estimates KM curves foreach group.
#    plot function plots both survival functions on same graph. Set to include confidence intervals.
plot(survfit(KMPracticeQ1Surv~group), col=c("Black", "Blue"), conf.int=TRUE)

# Uses the log-rank statistic to formally test the hypothesis that the survival curves for each group are all equivalent.
survdiff(KMPracticeQ1Surv~group)

detach(KMPracticeQ1Data)

#####################################################################################

# Practice Exercise 2-A and 2-B

# Reads in .csv, attaches column names to variable names.
#    Note: For purposes of this question, only three of the variables were relevent; only these three are included
#    in the data set. 
#    Column 1 variable: time
#    Column 2 variable: event (0=censored, 1=event occured)
#    Column 3 variable: lgwbc (log white bloodcell count; the variable to be categorized.)
KMPracticeQ2Data <- read.csv("KMPracticeQ2Data.csv")
attach(KMPracticeQ2Data)

# Cuts the "logwbc" variable into three domains (0-2.30, 2.31-3.00, 3.01-max value), and then labels the domains
#    as "Low", "Medium," and "High." and then assigns those values to a vector. This vector is then bound to the
#    original data set using cbind.
LWBCCategory <- cut(logwbc, breaks=c(0, 2.3, 3.0, (max(logwbc))), labels=c("Low", "Medium", "High"))
KMPracticeQ2DataWithCategories <- cbind(KMPracticeQ2Data, LWBCCategory)

# Creats a survival object from time data, splits that data into groups using the LWBCCategory variable, and then estimates
#    the survival curves for each category.
KMPracticeQ2Surv <- Surv(time, event==1)
summary(survfit(KMPracticeQ2Surv~LWBCCategory, data=KMPracticeQ2DataWithCategories))

# Plots the different survival curves on one graph, and then formally tests the hypothesis that all categories have
#    the same survival curve using the log rank statistic.
plot(survfit(KMPracticeQ2Surv~LWBCCategory), col=c("Black", "Blue", "Red"), conf.int=TRUE)
survdiff(KMPracticeQ2Surv~LWBCCategory)

detach(KMPracticeQ2Data)

#####################################################################################

#Test Question 1:A-B

#Reads in vets data and attaches column names to column variables.
# Column 2: ct1 (Large cell type; 0=False, 1=TRUE)
# Column 3: ct2 (Adeno cell type; 0=False, 1=TRUE)
# Column 4: ct3 (Small cell type; 0=False, 1=TRUE)
# Column 5: ct4 (Squamous cell type; 0=False, 1=TRUE)
# Column 6: Survt (survival time)
# Column 11: status (0 = event censored, 1 = event occured)

Vets<- read.dta("vets.dta")
attach(Vets)

# Creats three dummy variables corresponding to the cell categories
#    CellCat2 takes the ct2 variable and converts the "1"(TRUE) values to 2.
#    CellCat3 takes the ct3 variable and converts the "1"(TRUE) values to 3.
#    CellCat4 takes the ct4 variable and converts the "1"(TRUE) values to 4.
#    Celltype is the sum of the vectors ct1, CellCat2, CellCat3, and CellCat4. 
#    The levels of this new vector are then changed to the actual cell type names.
CellCat2 <- ifelse((ct2) == 1, 2, ct2)
CellCat3 <- ifelse((ct3) == 1, 3, ct3)
CellCat4 <- ifelse((ct4) == 1, 4, ct4)
CellType <- ct1 + CellCat2 + CellCat3 + CellCat4
levels(CellType) <- c("Large", "Adeno", "Small", "Squamous")

# Creates a survobject, calculates the survival curves for for two groups based on cell types ("Large" and other),
#    plots the two different survival curves, and then formally tests the difference between them. 
VetSurvObject <- Surv(survt, status==1)
summary(survfit(VetSurvObject~ct1))
plot(survfit(VetSurvObject~ct1), col=c("Black", "Red"), conf.int=TRUE)
survdiff(VetSurvObject~ct1)


# Creates a survobject, calculates the survival curves for each cell type group, plots the four different 
#    survival curves and then formally tests the difference between them.
summary(survfit(VetSurvObject~CellType))
plot(survfit(VetSurvObject~CellType), col=c("Black", "Red", "Blue", "Green"))
survdiff(VetSurvObject~CellType)

detach(Vets)

#####################################################################################

# Test Question 2:A & C

# Reads in the Addicts data and attaches column names to column variables.
#    Column 1: Subject ID
#    Column 2: Clinic (1 =  Clinic 1, 2 = Clinic 2)
#    Column 3: Status (0 = Censored, 1 = Departed from Clinic)
#    Column 4: survt (Survival time in days)
#    Column 5: prison (0 =  none, 1= any)
#    Column 6: dose (Dose of Methadone in mg/day)
Addicts <- read.dta("Addicts.dta")
attach(Addicts)

# Creates a survival object based on survival time "survt" and whether the patients departed clinic or were censored, "status". 
#     Splits the survival object into two groups based on clinic attended, and then calculates the Kaplan-Meier survival estimates for each group.
#     The two survival curves are then plotted on a single graph. 
AddictSurvObject <- Surv(survt, status==1)
summary(survfit(AddictSurvObject~clinic))
plot((survfit(AddictSurvObject~clinic)), col=c("Black", "Red"), conf.int=TRUE)
head(Addicts)

# Creates a new variable, DoseCategory, then divides the variable "dose" into one of four categories:
#    (0, 45], (45, 59], (59, 67], (67, 111].
DoseCategory <- cut(dose, breaks=c(0, 45,59,67,111))

# Splits the survival data into four groups based on methadone dose category, DoseCategory, and calculates Kaplan-Meier estimate for each group.
#    This data is then plotted on the same graph.
#    Uses the log-rank statistic to formally test the hypothesis that the survival curves for each group are all equivalent.
summary(survfit(AddictSurvObject~DoseCategory))
plot(survfit(AddictSurvObject~DoseCategory), col=c("Black", "Red", "Blue", "Green"))
survdiff(AddictSurvObject~DoseCategory)

detach(Addicts)
