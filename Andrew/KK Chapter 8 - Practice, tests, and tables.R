#Chapter 8: Recurrent Events

library(survival)
library(survrec)
#####################################
#Practice Questions
#####################################

#1) A recurrent event is an event (i.e. failure) that can occure mor ethan once over the follow0up on a given subject
#   TRUE

#2) The Counting Process approach is appropriate if a given sbject can experience more than one different type of 
#   event over follow-up?
#   FALSE

#3) In the data layout for the Counting Process approach, a subject who has additional follow-up time after 
#   having failed at time t(j), does not drop out of the risk set after time t(j).
#   TRUE

#4) The Couting Process approach requires hte use of a stratified Cox model
#   FALSE

#5) Using the Counting Process approach, if exactly two subjects fail at month t=10, but both these subjects have later
#   recurrent events, then the number in the risk set at the next ordered failure time does not decrease becuse of these two failures. 
#   TRUE

#6) The goal of robust estimation for the counting process approach is to adjust estimated regression coefficients to accoutn for the
#   correlation of observation within subjects when previously no such correlation was assumed. 
#   FALSE

#7) Robust estimation is recommended for the counting process approach but not for the alternative stratified Cox 
#   approachs for analyzing recurrent event survival time. 
#   FALSE

#8) The p-value obtained from using a robust standard error will always be larger than the corresponding p-value from using a 
#   non-robust standard error.
#   FALSE

#9) The marginal approach uses the exact same (start, stop) data layout format used for the counting process approach, except that 
#   for the marginal approach, the model used is a stratified Cox PH model rather than a standard PH model.
#   FALSE

#10) Suppose the maximum number of failures occuring for a given subject is five in a dataset to be analyzed using the marginal approach. 
#    Then a subject who failed only twice will contribute five lines of data corresponding to his or her two failures and the three 
#    additional failures that coud have possibly occorued for this subject. 
#    TRUE

#11) Suppose the maximum number of failures occuring for a given subject is five in a dataset to be analyzed using the conditional 1 approach.
#    Then an interaction Stratified Cox model used to carry out this analysis will have the following general model form:
#    H(x,t)= hnullg(t) exp(B1g*X1+B2g*X2+...+BpgXp); g = 1,2,3,4,5.
#    TRUE

#12) Suppose a no-interaction Stratified Cox model using the conditional 1 approach is found (using a likelihood ratio test) not statistically 
#    different froma  corresponding interaction stratified cox model. Then if the no-interaction model is used, it will not be possible to 
#    separate out the effects of predictors within each stratum representing the recurring events on a given subject.
#    TRUE

#13) In choosing betwen the conditional 1 and the marginal approaches, the marginal approach would be preferred provided the different strata 
#    clearly represent different event types.
#    TRUE

#14) When using an interaction stratified Cox model to analyze recurrent event data, the estimated regression coefficients and corresponding 
#    standard errors for the first stratum always will be identical for conditional 1, conditional 2, and marginal approaches. 
#    TRUE

#15) The choice among the Counting Process, conditional 1, conditional 2, and marginal approaches depends upon whether a no-interaction 
#    stratified Cox or an interaction stratified Cox model is more appropriate for one's data. 
#    FALSE

#16) Suppose that Allie, Sally, and Callie are the only three subjects in the dataset shown below. All three subjects have two recurrent events
#    that occur at different event times

ID<-c("A", "A", "S", "S", "C", "C")
Status<-c("1")
Stratum<-c(1,2,1,2,1,2)
Start<-c(0, 70, 0, 20, 0, 10)
Stop<-c(70, 90, 20, 30, 10, 40)
tx<-c(1,1,0,0,1,1)

ds<-cbind(ID, Status, Stratum, Start, Stop, tx)
ds<-as.data.frame(ds)
ds


#    Fill in the following data layout describing survival (in weeks) to the first event (stratum 1). Recall that m(j) and q(j) denote the number
#    of failures and censored observations at time t(j). The survival probabilities in the last column use the KM product limit formula.

survObject16<-Surv(time=as.numeric(as.vector(ds$Start)), time2=as.numeric(as.vector((ds$Stop))), event=ds$Status==1)
summary(survfit(survObject16~strata(ds$Stratum), data=ds))


#17  Plot the survival curve that corresponds to the data layout obtained for question 16
plot(survfit(survObject16~strata(ds$Stratum), data=ds), col=c("Black", "Red"), xlab=c("Time, t"), ylab=c("Survival Probability, S(t)"))
legend(x=50, y=0.8, legend=c("Stratum 1 = Black"))


#18)  Fill in the following data layout describing survival (in weeks) from the first to second event using the Conditional approach.

ds$gaptime<-(as.numeric(as.vector(ds$Stop))-as.numeric(as.vector(ds$Start)))
survObject18<-Surv(ds$gaptime, ds$Status==1)
summary(survfit(survObject18~strata(ds$Stratum), data=ds))


#19)  Plot the survival curve that corresponds to the data layout obrtained for Question 18
plot(survfit(survObject18~strata(ds$Stratum), data=ds), col=c("Black", "Red"), xlab=c("Time, t"), ylab=c("Survival Probability, S(t)"))
legend(x=40, y=0.8, legend=c("Stratum 2 = Red"))

#20) Fill in the following data layout descibing survival (in weeks) to the second event using the marginal aproach.
survObject20<-Surv(time= as.numeric(as.vector((ds$Stop))), event=ds$Status==1)
summary(survfit(survObject20~strata(ds$Stratum), data=ds))

#21) Plot the survival curve that corresponds to the data layout obrained for Question 20.
plot(survfit(survObject20~strata(ds$Stratum), data=ds), col=c("Black", "Red"), xlab=c("Time, t"), ylab=c("Survival Probability, S(t)"))
legend(x=40, y=0.8, legend=c("Stratum 2 = Red"))

#22) To what extend do the three plots obtained in Questions 17, 19, and 21 differ?
#    
#     The survival curve in question 16 is interested solely in time until the first event. The model in question 18 is interesed in survival
#     times, where time is defined as starting at the occurence of the first event and stopping at the second event. The final model is 
#     interested in time until the second event, where all subjects are condsidered at risk of the second event from the start of the study. 

#####################################
#Test Questions
#####################################

#1) Suppose that Bonnie and Lonnie are the only two subjects in the dataset swown below, where both subjects have two recurrent events that 
#   occur at dfferent times. 

ID<-c("B", "B", "R", "R")
Status<-1
Stratum<-c(1, 2, 1, 2)
Start<-c(0, 12, 0, 20)
Stop<-c(12, 16, 20, 23)

ds<-cbind(ID, Status, Stratum, Start, Stop)
ds<-as.data.frame(ds)
ds

#A) Fill in the empty cels in the following data layuut describing survival time to the first event (stratum 1):
survObject<-Surv(time=as.numeric(as.vector(ds$Start)), time2=as.numeric(as.vector(ds$Stop)), event=ds$Status==1)

survlayout1A<-summary(survfit(survObject~strata(Stratum), data=ds))
survlayout1A

#B) Why will the layout given in part A be the same regardless of whether the analysis approach is the counting process, stratified CP, gap time, 
#   or marginal approaches?

#   The other approaches are all geared towards allowing for recurrent events. All methods necessarily give the same estimates regarding each 
#   subject's first event. 

#C) Fill in the epty cells in the following data layout describing survival time from the first to the second event (stratum 2) using the stratified
#   Counting Process

survObject<-Surv(time=as.numeric(as.vector(ds$Stop)), event=ds$Status==1)
survlayout1C<-summary(survfit(survObject~strata(Stratum), data=ds))
survlayout1C

#D) Fill in the empty cells in the following data layout describing survival time from the first to the second event (stratum 2) using the gap-time approach

ds$gaptime<-as.numeric(as.vector(ds$Stop))-as.numeric(as.vector(ds$Start))

survObject<-Surv(time=as.numeric(as.vector(ds$gaptime)), event=ds$Status==1)
survlayout1D<-summary(survfit(survObject~strata(Stratum), data=ds))
survlayout1D

#E) Fill in the empty cells in the following data layout describing survival time from the first to the second event using the marginal approach
survObject<-Surv(time=as.numeric(as.vector(ds$Stop)), event=ds$Status==1)
survlayout1E<-summary(survfit(survObject~strata(Stratum), data=ds))
survlayout1E

#F) For the Stratfed Counting Process approach discribed in part c, determine which of the following choices is correct.
#   i: L(R)onnie is in the risk set when Bonnie gets her second event. 

#G) For the Gap Time approach described in part d, determine which of the following choices is correct. 
#   ii: Bonnie is in the risk set when L(R)onnie gets her second event. 

#H) For the Marginal Approach described in part e, determine which of the following choices is correct:
#   i: L(R)onnie is in the risk set when Bonnie gets her second event. 

#2)

dat8_2 <- c("01 1 0 39 0 0 12 1 0 39 0 1
01 1 39 66 0 0 12 1 39 80 0 1
01 1 66 97 0 0 12 0 80 107 0 1
02 1 0 34 0 1 13 1 0 36 0 1
02 1 34 65 0 1 13 1 36 64 0 1
02 1 65 100 0 1 13 1 64 95 0 1
03 1 0 36 0 0 14 1 0 46 0 1
03 1 36 67 0 0 14 1 46 77 0 1
03 1 67 96 0 0 14 0 77 111 0 1
04 1 0 40 0 0 15 1 0 61 0 1
04 1 40 80 0 0 15 1 61 79 0 1
04 0 80 111 0 0 15 0 79 111 0 1
05 1 0 45 0 0 16 1 0 57 0 1
05 1 45 68 0 0 16 0 57 79 0 1
05 . 68 . 0 0 16 . 79 . 0 1
06 1 0 33 0 1 17 1 0 37 0 1
06 1 33 66 0 1 17 1 37 76 0 1
06 1 66 96 0 1 17 0 76 113 0 1
07 1 0 34 0 1 18 1 0 58 0 1
07 1 34 67 0 1 18 1 58 67 0 1
07 1 67 93 0 1 18 0 67 109 0 1
08 1 0 39 0 1 19 1 0 58 1 1
08 1 39 72 0 1 19 1 58 63 1 1
08 1 72 102 0 1 19 1 63 106 1 1
09 1 0 39 0 1 20 1 0 45 1 0
09 1 39 79 0 1 20 1 45 72 1 0
09 0 79 109 0 1 20 1 72 106 1 0
10 1 0 36 0 0 21 1 0 48 1 0
10 1 36 65 0 0 21 1 48 81 1 0
10 1 65 96 0 0 21 1 81 112 1 0
11 1 0 39 0 0 22 1 0 38 1 1
11 1 39 78 0 0 22 1 38 64 1 1
11 1 78 108 0 0 22 1 64 97 1 1
23 1 0 51 1 1 30 1 0 57 1 0
23 1 51 69 1 1 30 1 57 78 1 0
23 0 69 98 1 1 30 1 78 99 1 0
24 1 0 43 1 1 31 1 0 44 1 1
24 1 43 67 1 1 31 1 44 74 1 1
24 0 67 111 1 1 31 1 74 96 1 1
25 1 0 46 1 0 32 1 0 38 1 1
25 1 46 66 1 0 32 1 38 71 1 1
25 1 66 110 1 0 32 1 71 105 1 1
26 1 0 33 1 1 33 1 0 38 1 1
26 1 33 68 1 1 33 1 38 64 1 1
26 1 68 96 1 1 33 1 64 97 1 1
27 1 0 51 1 1 34 1 0 38 1 1
27 1 51 97 1 1 34 1 38 63 1 1
27 0 97 115 1 1 34 1 63 99 1 1
28 1 0 37 1 0 35 1 0 49 1 1
28 1 37 79 1 0 35 1 49 70 1 1
28 1 79 93 1 0 35 0 70 107 1 1
29 1 0 41 1 1 36 1 0 34 1 1
29 1 41 73 1 1 36 1 34 81 1 1
29 0 73 111 1 1 36 1 81 97 1 1")

dat8_2b <- data.frame(matrix(as.numeric(unlist(strsplit(dat8_2,split="\\s|\\n"))),ncol=6,byrow=T))
colnames(dat8_2b) <- c("id","event","start","stop","tx","smoking")
dat8_2c <- dat8_2b[order(dat8_2b$id,dat8_2b$start),]
rm(dat8_2b,dat8_2)

#CP approach
cpMod2 <- coxph(data=dat8_2c, Surv(time=start, time2=stop,event=event==1) ~ tx+smoking+cluster(id),ties="breslow")
#cpMod2 <- coxph(data=dat8_2c, Surv(time=start, time2=stop,event=event==1) ~ tx+smoking,ties="breslow",robust=T)
summary(cpMod2)

####################################
# NOTE: WE ARE UNABLE TO REPLICATE THE K&K RESULTS TABLES
# BUT, WHEN WE TRY TO REPLICATE THE CP MODEL IN THE APPENDIX (p. 566), OUR APPROACH SEEMS FINE
require(foreign)
dsBladder <- read.dta("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/bladder.dta")
cpApxMod <- coxph(data=dsBladder[!dsBladder$start==dsBladder$stop,], Surv(time=start, time2=stop,event=event==1) ~ tx+num+size+cluster(id),ties="breslow")
summary(cpApxMod)
scpApxMod <- coxph(data=dsBladder[!dsBladder$start==dsBladder$stop,], Surv(time=start, time2=stop,event=event==1) ~ tx+num+size+cluster(id)+strata(interval),
                   ties="breslow")
summary(scpApxMod)
# MAKES ME THINK K&K RESULTS TABLES ARE WRONG IN CHPT 8
####################################

#Stratified CP approach
dat8_2c$eventOcc <- rep(1:3,36)
scpMod2 <- coxph(data=dat8_2c, Surv(time=start, time2=stop,event=event==1) ~ tx+smoking+cluster(id)+strata(eventOcc),ties="breslow")
summary(scpMod2)

#Gap approach
dat8_2c$gapTime <- dat8_2c$stop - dat8_2c$start
dat8_2c$newStart <- 0
head(dat8_2c)
gapMod2 <- coxph(data=dat8_2c, Surv(time=gapTime,event=event==1) ~ tx+smoking+cluster(id)+strata(eventOcc),ties="breslow")
#gapMod2 <- coxph(data=dat8_2c, Surv(time=newStart, time2=gapTime,event=event==1) ~ tx+smoking+cluster(id)+strata(eventOcc),ties="breslow")
summary(gapMod2)


#Marginal approach
require(plyr)
lastStop <- function(d){
  d$newStop <- ifelse(is.na(d$stop),max(d$stop,na.rm=T),d$stop)
  d$newEvent <- ifelse(is.na(d$event),0,d$event)
  return(d)
}
dat8_2d <- ddply(dat8_2c,"id",lastStop)
head(dat8_2d,50)
mrgMod2 <- coxph(data=dat8_2d, Surv(time=newStop, event=newEvent==1) ~ tx+smoking+cluster(id)+strata(eventOcc),ties="breslow")
summary(mrgMod2)

#A) State the hazard function formula for the no-interaction model used to fit the CP appraoch. 
#    H(t,x)= hnull(t)*exp(B1*tx+B2*Smoking)

#B) Based on the counting process approach, what do you conclude about the effect of treatment (tx)

#   Based on the counting process, the treatment variable, tx, appears to NOT have a significant effect on hazard
#   when smoking is accounted for, B1=0.0839, X-square(1)=0.655, p=0.4182

#C) State the hazard function formulas for the no interaction and interaction Stratified Cox models corresponding to 
#   the use of the Marginal approach for fitting these data.

#   No-interaction model: h(x)=hnull(t)g*exp(B1*tx+B2*Smoking); g=1,2,3
#   Interaction model:h(x)=hnull(t)g*exp(B1*tx+B2*Smoking+B3g*tx+B4g*Smoking); g=1,2,3

#D) Table T.1 gives results for "no-interaction" Stratified Cox models because likelihood ratio tests comparing 
#   a "no-interaction" with an "interaction" stratified Cox model were not significant. Describe the LR test used 
#   for the marginal model (full and reduced models, null hypothesis, test-statistic, distribution of test statistic 
#   under the null).

#   The likelihood ratio is a test statistic of goodness of fit, comparing a full and reduced model. The test statistic,
#   -2ln(Likelihood(full)/Likelihood(reduced)) follows an approximate chi-square distribution with degrees of freedom
#   equal to the difference in the number of parameters of each model. In this case, the full model includes the 
#   predictor variables treatment (tx), smoking, and an interaction between the strata and the two predictor variables (B3 & B4),
#   and the reduced model would contain only the first two parameters, B1 and B2. Using the likelihood statistics for each model,
#   we conduct the likelihood ratio and compare it to a chi-squared distribution with 2 (four paramaters - 2 paramaters) degrees
#   of freedom. The p-value of this statistic is then used to test the null hypothesis that there is no significant difference
#   between these two models (i.e. that there is no interaction effect between strata and predictor variables).

#E) How can you criticize the use of a no-interaction Stratified Cox model for any of the Stratified Cox appraoches, despite 
#   finding that the above likelihood ratio test was not significant?

#   Any model that includes interaction models between strata and predictor variables is necessarly controlling for such interactions
#   when estimating other parameters. Even if the interactions are not significant, including them in the model can still produce
#   more accurate estimates of the other parameters in the model. 

#F) Based on the study description given earlier, wy does it make sense to recommend the Cox Proportional approach 
#   over the other alternative approaches?

#   Because the researchers are considering each event as being the same type of event, and are not interested in the
#   order of events. 

#G) Under what circumstances would you recommend using the Marginal approach instead of the Cox Proportional Hazard approach?

#   You would use the Marginal approach when you are interested in categorically different events, and consider
#   all subjects to be at risk for each type of event from the start of observation. 

#H) In table T.2, why does the number in the risk set remain unchanged through failure time 68, even though 
#   50 events occur up to that time?

#   Table T.2 shows survival times of subjects from start date up until their third event, but the researchers in this case
#   are apparently not interested in the order of events. All subjects remain in a single risk set until their third event. 
#   Any subject who failed was still considered "at risk" until their third event. 

#I) Why does the number in the risk set change from 31 to 26 when going from time 96 to 97?

#   Because five subjects experience their third and final event on day 96, and are removed from the risk set.

#J) Why is the number of failures equal to 3 and the number of censored subjects tqual to 1 in the interval 
#   between failure times 79 and 80?

#   Because subject 9 is censored at time t=79, and subjects 15, 16, and 28 all experience their final event at
#   time t=79.

#K) What 5 subjects were censored in the the interval between failure times 111 and 112?

#   Subjects 4, 14, 15, 24, and 29.

#L) Describe the eventhistory for subject 5, including his/her effect on changes in the risk set.

#  Subject 5 experiences their first and second event at times t=45 and t=68 respectively. After that,
#  they were apparently lost to follow up, and are no longer part of the risk set. 

#M) Suppose the survival probabilities shown in table T.3 are plotted on the y-axis versus corresponding ordered
#   failure times on the x-axis. What is plotted by such a curve?

#   iii: Pr(T>t) where T = time to any event from study entry.

#N) Can you criticize the use of the product limit formula for S(t(f)) in table T.3? Explain briefly?

#   This use of the product limit formula depends upon all subjects being part of a single risk set.
#   In not stratifying on event order (and not including interactions between strata and predictors)
#   estimates of survival probabilities are based on the assumption that the order of events has no effect
#   on survival probabilities or parameter estimates. 

#O) 
#i)     tf  nf  mf  qf  S(t(f))
#       51  7   2   0   (5/7)*.19=0.14
#       57  5   2   0   (3/5)*.14=0.084
#       58  3   2   0   (1/3)*.084=.03
#       60  1   1   0   0

#ii)    tf  nf  mf  qf  S(t(f))
#       40  6   1   0   (5/6)*.17=.14
#       41  5   1   0   (4/5)*.14=.11
#       42  4   1   0   (3/4)*.11=.08
#       46  3   1   0   (2/3)*.08=.05
#       47  2   1   0   (1/2)*.05=.025

#iii)   tf  nf  mf  qf  S(t(f))
#       79  9   4   0   (5/9)*.25=.14
#       80  7   2   0   (5/7)*.14=.10
#       81  5   2   0   (3/5)*.1=.06
#       97  3   2   0   (1/3)*.06=.02

#P) The survival curves corresponding to each of the data layouts (ab, b, c) described in Question O (#14) 
#   will be different. Why?

#   They will be different becuase each is based on a different understanding of the recurring events. The first table,a,
#   is time until the first event from study entry. The second models survival time until the second event, starting from 
#   the first event; this suggests that the first and second events are fundamentally different, because of their order.
#   The third table is models time from the start of observation to the second event, suggesting that events 1, 2, and 3 are
#   all fundamentally different types of events, and that subjects are at risk for all events from the start of observation. 

