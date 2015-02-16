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

