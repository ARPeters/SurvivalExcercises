library(survival)
library(foreign)
setwd("~/GitHub")

###########################################################################
#KK Chapter 6: Practice Questions
###########################################################################

#anderson.dat

#Survival times in weeks (in remission) of 42 leukemia patients in 
#clinical trial to compare treatment with placebo.  Data from Freireich et al.,
#"The effect of 6-mercaptopurine on the duration of steroid-induced remissions
#in acute leukemia," Blook 21, 699-716, 1963.

#Column 1 = survival time (weeks)
#Column 2 = status (0=censored, 1=relapse)
#Column 3 = sex (1=male, 0=female)
#Column 4 = log WBC
#Column 5 = Rx (1=placebo, 0=treatment)

dsAnderson<-read.csv("./SurvivalExercises/Andrew/dsAnderson.csv")
colnames(dsAnderson)<-c("subject", "survt", "status", "sex", "logWBC", "rx")

AndersonPh<-coxph(Surv(survt, status==1)~sex+logWBC+rx, data=dsAnderson, ties="breslow")
cox.zph(AndersonPh, transform="rank")

# 1) Which of the variables in the model fitted above are time-independent
#    and which are time-dependent?

#    The probability of the sex variable having a constant effect on the hazard
#    is below 0.05; this suggest that it's schoenfield residuals have a correlation with time that is unlikely to be the 
#   result of random chance. 

#2) Based on this printout, is the PH assumption satisfied for the
#   model being fit? Explain briefly.

#   The predictions made by the model do not seem to have a significant correlation with time (p(PH GLOBAL)p=0.2393). This p-value is 
#   not statistically significant as a whole, but is elevated, likely due to the influence of the sex variable. 

#3) Suppose you want to use an extended Cox model to assess
#   the PH assumption for all three variables in the above model.
#   State the general form of an extended Cox model that will
#   allow for this assessment.

#   h(t, X(t))= h-null(t)exp(B1(sex)+B2(logWBC)+B3(rx)+epsilon1(sex)g1(t)+epsilon1(sex)g1(t)+epsilon2(logWBC)g2(t)+epsilon3(rx)g3(t))
#   gx(t) represents a function of time

#4) Suppose you wish to assess the PH assumption for the Sex
#   variable using a heaviside function approach designed to
#   yield a constant hazard ratio for less than 15 weeks of followup
#   and a constant hazard ratio for 15 weeks or more of followup.
#   State two equivalent alternative extended Cox models that
#   will carry out this approach, one model containing one heaviside
#   function and the other model containing two heaviside
#   functions.


#   h(t, X(t))= h-null(t)exp(B2(logWBC)+B3(rx)+epsilon1(sex)g1(t)+epsilon2(sex)g2(t)
#   or
#   h(t, X(t))= h-null(t)exp(B1(sex)+B2(logWBC)+B3(rx)+epsilon1(sex)g1(t)

#5) 


eventTimes <- unique(dsAnderson$survt[dsAnderson$status==1])
eventTimes <- eventTimes[order(eventTimes)]

require(plyr)

createPTable <- function(d){  
  dNew <- d
  for(i in 1:length(eventTimes)){
    dNew[i,] <- d
    dNew[i,"r"] <- i
    dNew[i,"tr"] <- eventTimes[i]
    dNew[i,"dir"] <- ifelse(i==1,min(d$survt,eventTimes[i]),min(d$survt,eventTimes[i]) - eventTimes[i-1])
    dNew[i,"yir"] <- 0
    if(d$survt <= eventTimes[i]) {
      if(d$status %in% 1) dNew[i,"yir"] <- 1
      break      
    }      
  }    
  return(dNew)
}

ptProcessAnderson <- ddply(.data=dsAnderson,.variables=.(subject),.fun = createPTable)

Int1<-list()
length(Int1)<-length(ptProcessAnderson$survt)

for(i in 1:length(ptProcessAnderson$tr)){
  Int1[i]<-ifelse(ptProcessAnderson$tr[i]<15, ptProcessAnderson$sex[i]*1, 0)
}

Int2<-list()
length(Int2)<-length(ptProcessAnderson$survt)

for(i in 1:length(ptProcessAnderson$tr)){
  Int2[i]<-ifelse(ptProcessAnderson$tr[i]>=15, ptProcessAnderson$sex[i]*1, 0)
}

head(ptProcessAnderson)
ptProcessAnderson<-cbind(ptProcessAnderson, as.integer(unlist(Int1)), as.integer(unlist(Int2)))
colnames(ptProcessAnderson)<-c("subject", "survt", "status", "sex", "logWBC", "rx","r", "tr", "dir", "yir", "Int1", "Int2")
poissonAndersos5<-glm(yir ~ I(as.factor(r)) + logWBC + rx + Int1 + Int2 + offset(I(log(dir))), family=poisson(link = "log"), data=ptProcessAnderson)

#   Using the above computer results, carry out a test of hypothesis,
#   estimate the hazard ratio, and obtain 95% confidence
#   interval for the treatment effect adjusted for log WBC and
#   the time-dependent Sex variables. What conclusions do you
#   draw about the treatment effect?

#   The treatment variable, rx, has a significant effect on the hazard rate, p<0.004.
#   The hazard ratio associated with treatment is estimated to be 3.822, but has  a wide confidence interval, 1.533 to 9.526. 
#   The placebo group, rx=1, has a significantly higher hazard rate, but our estimate of the exact ratio is not very exact. 


#6) We now consider an alternative approach to controlling for
#   Sex using an extended Cox model. We define an interaction
#   term between sex and time that allows for diverging survival
#   curves over time.
#   For the situation just described, write down the extended Cox
#   model, which contains Rx, log WBC, and Sex as main effects
#   plus the product term sex ? time.

#   h(t, X(t))= h-null(t)exp(B1(sex)+B2(logWBC)+B3(rx)+epsilon1(sex*t)

                         
#7) Using the model described in question 6, express the hazard
#   ratio for the effect of Sex adjusted for Rx and log WBC at 8
#   and 16 weeks.

#   At t=8 weeks:  hr(sex)= exp(B1(sex)+8*epsilon(sex))
#   At t=16 weeks: hr(sex)= exp(B1(sex)+16*epsilon(sex))

#8)
poissonAnderson8<-glm(yir ~ I(as.factor(r)) + sex + logWBC + rx + sex:tr + offset(I(log(dir))), family=poisson(link = "log"), data=ptProcessAnderson)
summary(poissonAnderson8)
#coxphAnderson8<-coxph(Surv(ptProcessAnderson$tr, ptProcessAnderson$yir)~sex + logWBC + rx + sex*tr, data=ptProcessAnderson)

#   Based on the above results, describe the hazard ratio estimate
#   for the treatment effect adjusted for the other variables in the
#   model, and summarize the results of the significance test and
#   interval estimate for this hazard ratio. How do these results
#   compare with the results previously obtained when a heaviside
#   function approach was used? What does this comparison
#   suggest about the drawbacks of using an extended Cox model
#   to adjust for variables not satisfying the PH assumption?

#   Based on the slightly different model above (continuous time function, rather than a heaviside function)
#   the treatment variable rx has a hazard ratio of 2.984, p<0.022, with a confidence interval of 1.467 to 7.626.
#   While the overall conclusion remains the same, the exact estimate, p-value, and confidence interval are all slightly
#   different. 

#   This suggests that the selection of a time function can change the characteristics of the model; care must be taken to select
#   a time function that is appropriate to the study. 

#9) 


poissonAnderson9<-glm(yir ~ I(as.factor(r)) + logWBC + rx + strata(sex) + offset(I(log(dir))), family=poisson(link = "log"), data=ptProcessAnderson)
#coxAnderson9<-coxph(Surv(ptProcessAnderson$tr, ptProcessAnderson$yir)~logWBC + rx + strata(sex), data=ptProcessAnderson)


#   The following gives an edited printout of computer results
#   using a stratified Cox procedure that stratifies on the Sex
#   variable but keeps Rx and log WBC in the model.Compare the 
#   results of the above printout with previously
#   provided results regarding the hazard ratio for the effect of
#   Rx. Is there any way to determine which set of results is more
#   appropriate? Explain.

#   This model gives yet another set of values regarding the treatment effect (B=0.93, Hratio=2.537, p<-.048, conf.int=(1.006, 6.396))
#   and shows similar divergence for the logWBC variable as well.
#   
#   We can select the best model using model goodness-of-fit statistics, or by graphing the observed survival curves and estimated
#   survival curves to select which model fits the data the best. Or by looking at AUC?




###########################################################################
#KK Chapter 6: Test Questions
###########################################################################

#  http://statweb.stanford.edu/~olshen/hrp262spring01/spring01Assignments/chemo.txt

#  Survival times in days from a clinical trial on gastric carcinoma, 
#  involving 90 patients randomized to either chemotherapy alone or to a 
#  combination of chemotherapy and radiation.  Data from Stablein et al., 
#  "The analysis of survival data with nonproportional hazard functions," 
#  Controlled Clinical Trials 2, 149-159, 1981.

#  Column 1 = Rx (1=chemotherapy, 2=chemotherapy and radiation)
#  Column 2 = status (0=censored, 1=died)
#  Column 3 = survival time (days)

dsChemo<-read.csv("./SurvivalExercises/Andrew/dsChemo.csv")
colnames(dsChemo)<-c("subject", "rx", "status", "survt")



#1) A plot of the log-log Kaplan-Meier curves for each
#   treatment group is shown below. Based on this plot, what
#   would you conclude about the PH assumption regarding
#   the treatment group variable? Explain.
#   scurveChemo<-survfit(Surv(survt, status==1)~rx, data=dsChemo)
#   summary(scurveChemo)

scurveChemo<-survfit(Surv(survt, status==1)~rx, data=dsChemo)
summary(scurveChemo)
plot(scurveChemo)


#   The ph assumption appears to be violated; the survival functions for the two treatment groups appear to diverge
#   early on, and then reconverge around the t=800 days, when there are not many individuals left in either risk set.


#2) The following is an edited printout of computer results
#   obtained when fitting the PH model containing only the
#   treatment group variable. Based on these results, what
#   would you conclude about the PH assumption regarding
#   the treatment group variable? Explain.

ChemoPh<-coxph(Surv(survt, status==1)~rx, data=dsChemo, ties="breslow")
cox.zph(ChemoPh, transform="rank")

#   The results here differ slightly from the textbook, because this dataset contains 95 subjects (even though the page
#   I copied it from says that it is a dataset of 90 subjects).
#   Still, the PH assumption seems to have been violated. The schoenfield residuals of the treatment variable show
#   a significant correlation with time, that is unlikely to be the result of chance, p(PH)=0.00105

#3) 

eventTimes <- unique(dsChemo$survt[dsChemo$status==1])
eventTimes <- eventTimes[order(eventTimes)]

createPTable <- function(d){  
  dNew <- d
  for(i in 1:length(eventTimes)){
    dNew[i,] <- d
    dNew[i,"r"] <- i
    dNew[i,"tr"] <- eventTimes[i]
    dNew[i,"dir"] <- ifelse(i==1,min(d$survt,eventTimes[i]),min(d$survt,eventTimes[i]) - eventTimes[i-1])
    dNew[i,"yir"] <- 0
    if(d$survt <= eventTimes[i]) {
      if(d$status %in% 1) dNew[i,"yir"] <- 1
      break      
    }      
  }    
  return(dNew)
}

ptProcessChemo <- ddply(.data=dsChemo,.variables=.(subject),.fun = createPTable)
head(ptProcessChemo)

# Tx1<-list()
# length(Tx1)<-length(ptProcessChemo$yir)
# 
# for(i in 1:length(ptProcessChemo$rx)){
#   Tx1[i]<-ifelse(ptProcessChemo$tr[i]<250, ptProcessChemo$rx[i]*1, 0)
# }
# 
# Tx2<-list()
# length(Tx2)<-length(ptProcessChemo$yir)
# 
# 
# for(i in 1:length(ptProcessChemo$rx)){
#   Tx2[i]<-ifelse(250<=ptProcessChemo$tr[i] & ptProcessChemo$tr[i]<500, ptProcessChemo$rx[i]*1, 0)
# }
# 
# Tx3<-list()
# length(Tx3)<-length(ptProcessChemo$yir)
# 
# 
# for(i in 1:length(ptProcessChemo$rx)){
#   Tx3[i]<-ifelse(ptProcessChemo$tr[i]>=500, ptProcessChemo$rx[i]*1, 0)
# }
# 
# ptProcessChemo<-cbind(ptProcessChemo, as.numeric(unlist(Tx1)),  as.numeric(unlist(Tx2)),  as.numeric(unlist(Tx3)))
# colnames(ptProcessChemo)<-c("subject", "rx", "status", "survt", "r", "tr", "dir", "yir","Time1", "Time2", "Time3")

ptProcessChemo$Time1 <- ifelse(ptProcessChemo$tr<250, ptProcessChemo$rx*1, 0)
ptProcessChemo$Time2 <- ifelse(250<=ptProcessChemo$tr & ptProcessChemo$tr<500, ptProcessChemo$rx*1, 0)
ptProcessChemo$Time3 <- ifelse(ptProcessChemo$tr>=500, ptProcessChemo$rx*1, 0)
head(ptProcessChemo)

poissonChemo3<-glm(yir ~ I(as.factor(r)) + Time1 + Time2 + Time3 + offset(I(log(dir))), family=poisson(link = "log"), data=ptProcessChemo)
summary(poissonChemo3)

#   The following printout shows the results from using a
#   heaviside function approach with an extended Cox model
#   to fit these data. The model used product terms of the
#   treatment variable (Tx) with each of three heaviside functions.
#   The first product term (called Time1) involves a
#   heaviside function for the period from 0 to 250 days,
#   the second product term (i.e., Time2) involves the period
#   from 250 to 500 days, and the third product term (i.e.,Time3) 
#   involves the open-ended period from 500 days and
#   beyond.
#   Write down the hazard function formula for the extended
#   Cox model being used, making sure to explicitly define the
#   heaviside functions involved.

#   h(t, X(t))= h-null(t)exp(epsilon1(rx)g1(t)+epsilon2(rx)g2(t)+epsilon3(rx)g3(t)
#   where
#   g1(t)= 1 at t<250; and 0 elsewhere
#   g2(t)= 1 at 250<=t<500; and 0 elsewhere
#   g3(t)= 1 at t>500; and 0 elsewhere

#4) Based on the printout, describe the hazard ratios in each
#   of the three time intervals, evaluate each hazard ratio for
#   significance, and draw conclusions about the extent of the
#   treatment effect in each of the three time intervals considered

#   In the first time interval, the hazard ratio associated with the treatment variable 
#   rx has a hazard ratio of 0.221, p<0.001, with a confidence interval of (0.089 to 0.545).
#   This seems to suggest that those subjects who receive both radiation and chemo die off lesser rates than those who just
#   receive just chemo. 

#   The hazard ratio loses its significance across the other time intervals. This could be an artifact of the decreased risk
#   set in later intervals, or could be evidence that the protective effects of receiving both lessens over time. I am inclined
#   to think the former, due to the decreasing sample size across time intervals. 


#5) Inspection of the printout provided in question 3 indicates
#   that the treatment effect in the second and third intervals
#   appears quite similar. Consequently, another analysis was
#   considered that uses only two intervals, from 0 to 250 days
#   versus 250 days and beyond.Write down the hazard function
#   formula for the extended Cox model that considers
#   this situation (i.e., containing two heaviside functions).
#   Also, write down an equivalent alternative hazard function
#   formula which contains the main effect of treatment
#   group plus one heaviside function variable.

#   h(t, X(t))= h-null(t)exp(epsilon1(rx)g1(t)+epsilon2(rx)g2(t)
#   where
#   g1(t)= 1 at t<250; and 0 elsewhere
#   g2(t)= 1 at 250<=t<500; and 0 elsewhere

#   Or
#   h(t, X(t))= h-null(t)exp(B1(rx)+epsilon1(rx*g1(t))
#   where
#   g1(t)= 1 at t>250; and 0 elsewhere


#6) For the situation described in question 5, the computer
#   results are provided below. Based on these results,
#   describe the hazard ratios for the treatment effect below
#   and above 250 days, summarize the inference results
#   for each hazard ratio, and draw conclusions about the
#   treatment effect within each time interval.
#   Time-Dependent Cox Regression Analysis


Time1_2<-list()
length(Time1_2)<-length(ptProcessChemo$yir)


for(i in 1:length(ptProcessChemo$rx)){
  Time1_2[i]<-ifelse(ptProcessChemo$tr[i]<250, ptProcessChemo$rx*1, 0)
}

Time2_2<-list()
length(Time2_2)<-length(ptProcessChemo$yir)


for(i in 1:length(ptProcessChemo$rx)){
  Time2_2[i]<-ifelse(ptProcessChemo$tr[i]>=250, ptProcessChemo$rx*1, 0)

}

ptProcessChemo<-cbind(ptProcessChemo, as.numeric(unlist(Time1_2)),  as.numeric(unlist(Time2_2)))
head(ptProcessChemo)
colnames(ptProcessChemo)<-c("subject", "rx", "status", "survt", "r", "tr", "dir", "yir","Time1", "Time2", "Time3", "Time1_2", "Time2_2")

poissonChemo6<-glm(yir ~ I(as.factor(r)) + Time1_2 + Time2_2 + offset(I(log(dir))), family=poisson(link = "log"), data=ptProcessChemo)

#   For the first time interval, the hazard ratio associated with the treatment variable
#   is 0.221, p=0.001. This is relatively unchanged from the model that contained three time intervals. 

#   For the second time interval, the hazard ratio assoicated with the treatment variable
#   is 1.532, p=0.176. The difference between the two different groups has increased, technically, but fails
#   to reach significance. This suggests, again, that the difference between the two washes out over time.
#   This could still be the result of the decreased sample size, but since there are two divisions rather than three,
#   the second group had a better chance of hitting significance, and still failed to hit a significant p-value.
#   I'm inclined to say that this is more evidence that the effect of the treatment decreases over time. 