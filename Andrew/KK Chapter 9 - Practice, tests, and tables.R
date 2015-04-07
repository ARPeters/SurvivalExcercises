#Chapter 9: Competing Risks
rm(list = ls(all.names = TRUE))

library(survival)
library(survrec)
#####################################
#Practice Questions
#####################################


#1) A competing risk is an event-type that can occur simultaneiously with another event of interest in
#   the same subject

#   FALSE

#2) An example of competing risks survival data is a study in which patients receiving radiotherapy for
#   head and neck cancer may either die from their cancer or from some other cause of death. 

#   TRUE

#3) If all competing risks in a given study are different causes of death, then it is possible to have 
#   both competing risks and recurrent events in the same study.

#   FALSE


#4) Suppose patients with advanced-stage cancer may die after surgery before their hospital stay is long enough
#   to get a hospital infection. Then such deaths from surery reduce the hospital's burden of infection control.

#   TRUE

#5) The typical approach for analyzing competing risks using a Cox PH model involves fitting separate models for
#   each competing risk ignoring the other competing risks

#   FALSE

#6) Suppose that a cause-specific risk of interest is development of lung metastasis and competing risk is 
#   local recurrence of a lung tumor. Then a patient who develops a local recurrence is treated as a failure
#   in a competing risk analysis. 

#   FALSE

#7) When there are no competing risks, then any study subject in the risk set at a given time has the same risk
#   for failing as any other subject in the risk set with the same values for covariate predictors at time t. 

#   FALSE

#8) If, when analyzing competing risks survival data, it is assumed that censoring is noninformative, then a
#   subject in the risk set at time t is as likely to fail from any competing risk as to be lost to follow-up

#   TRUE

#9) When a sensitivity analysis indicates that a worst-case scenario gives meaningfully different results from
#   an analysis that assumes indeendence of competing risks, then there is evidence that the independence
#   assumption is violated. 

#   FALSE

#10)The typical competing risk analysis assumes that competing risks are independent even if this assumption is
#   not true. 

#   TRUE

#11)The Cumulative Inciudence Curve proves risk estimates for the occurence of a cause-specific event in the
#   presence of competing risks.

#   TRUE

#12)CIC=1-Kaplan-Meier curve

#   FALSE

#13)A CIC for a cause-specific event that ignores the control of covariates does not require the assumption of 
#   independent competing risks. 

#   TRUE

#14)A cumulative Probability Curve gives the probability of experiencing an event c by time t, from risk c, 
#   given that an individual has experienced any other competing risks by time t. 

#   FALSE

#15)If CIC(sub-c)=.4, then CPC=.4/.6=.667

#   FALSE

#16)The Lunn-McNeil approach fits a single stratified Cox model using an augmented dataset to obtain the same 
#   results as obtained by fitting separate Cox models for each cause-specific competing risk.

#   TRUE

#17)An advantage of the Lunn-McNeil approach over the approach that fits separate Cox models is that the LM 
#   approach allows for testing whether a no-interaction SC model might be preferable to an interaction SC model.

#   TRUE

#18)Given the LM Model stratified on two cause-specific events, cancer and CVD:...

#   TRUE

#19)Given the LM(alt) model for two cause-specific events, cancer and CVD:

#   TRUE

#20)THe LMu model that would result if hte LM model of Quewstion 18 were changed to an unstratified Cox PH
#   model were changed to an unstratified Cox PH model can be written as follows: 

#   FALSE


# Consider a hypothetical study of the effect of a bone marrow transplant for leukemia on leukemia-free
# survival, where transplant failures can be of one of two types: relapse of leukemia and nonrelapse death
# (without prior relapse of leukemia). Suppose that in a hospital A, 100 patients undergo such a  transplant
# and that within the first 4 years post-translplant, 60 die without relapse by year 2 and 20 relapse during 
# year 4. Suppose that in hospital B, 100 patients undergo such a transplant but post-transplant, there are 
# 20 non-relapse deaths by year 1, 15 relapss during year 2, 40 non-relapse deaths between years 3 and 4, 
# and 5 relapses during year 4.

#21)What are the competing risks in this study?

#   Leukemia relapse and non-relapse death. 

#22)What is the proportion of initial patients in hospitals A and B, respectively, that have leukemia relapse
#   by 4 years?

#   Hospital A: 20 of the 100 patients, or 0.2
#   Hospital B: 20 of the 100 patients, or 0.2

#23)How have both tables treated the competing risk for nonrelapse death in the calculation of the KM 
#   probabilities.

#   Both tables treat it as censoring. 

#24)Why are the KM Probabilities different at 4 years for each hospital?

#   In hospital A, all of the events and censorings happen at once, leaving just on calculation of KM
#   at one sample size.

#   In hospital B, the events and censorings happen in two waves. That means that the KM is adjusted at
#   multiple points in time, with multiple multiple sampling sizes, and is cumulative across the two adjustment
#   times (i.e. S(t=1), S(t=1)*S(t=2)).

#25)Compute the CIC curves for each hospital using the following tables

tfa<-c(0,2,4)
nfa<-c(100,40,40)
mfa<-c(0,0,20)
hazhata<-c(0,0,0.5)
survhata<-c(NA,1,0.4)
ihata<-c(NA,0,0.2)
CICa<-c(NA,0,0.2)
dsA<-cbind(tfa,nfa,mfa,hazhata,survhata,ihata,CICa)
dsA

tfb<-c(0,1,2,3,4)
nfb<-c(100,80,80,65,25)
mfb<-c(0,0,15,0,5)
hazhatb<-c(0,0,(15/80),0,5/25)
survhatb<-c(NA,1,80/100,65/100,25/100)
ihatb<-c(NA, 0, 0.15, 0, 0.5)
CICb<-c(NA,0,0.15,0.15,0.20)
dsB<-cbind(tfb,nfb,mfb,hazhatb,survhatb,ihatb,CICb)
dsB

#26)Why are the CIC probabilities the same at 4 years?

#   The CIC curves are the same at four years because they give the cumulative risk at each year. While the timing of leukemia
#   relapses differs across time, the Cumulative Incidence cumulates to the same number at year 4 in both hospitals. 

#   Consider a hypothetical study to assess the effect of a new hospital infection control strategy for patients who undergo
#   heart transplant surgery in a given hospital. The exposure variable of interest is a binary variable Group (G): G = 0 
#   for those patients receiving heart transplants from 1992 through 1995 when the previous hospital control strategy was used;
#   G = 1 for those patients receiving heart transplants from 1996 through 1999 when the new hospital infection control strategy 
#   was adopted. The primary event of interest is getting a hospital infection after surgery. A competing risk is death during 
#   recovery from surgery without getting a hospital infection. Control variables being considered are tissue mismatch score (TMS) 
#   at transplant and AGE at transplant. The outcome variable of interest is time (DAYS after surgery) until a patient developed
#   a hospital infection.

#27)State a cause-specific no-interaction Cox PH model for assessing the effect of group status (G) on time until a hospital infection
#   event.

# h(t,x)=hnull*exp(B1*TMS + B2*AGE + B3*G); deaths without infection are considered censored. 

#28)When fitting hte model given in Question 27, which patients should be censored?

#   Patients who die without a hospital infection are considered censored. 

#29)Describe or provide a table that would show how the data on the ith patient should be augmented for input into a Lunn-McNeil model
#   for analysis. 

#   In a Lunn-McNeil model, the table would include two binary outcome events, one for each of the competing risks of interest. 

#30)State a Lunn-McNeil model that can be used with an augmented dataset htaht will provide identical results to those obtained
#   from useing the model of Question 27.

#   h(t,x)=hnull*exp(B1*TMS + B2*AGE + B3*G + B4*D2*TMS + B5*D2*AGE + B6*D2*G);
#   D2=0 if patient develops an infection
#   D2=1 if patient dies without infection

#31)For the LM model of Question 30, what is the formula for the hazard ratio for the group effect G, controlling for TMS and AGE

#   haz=exp(B3*G); G=1

#32)Describe how you would test whether a no-interaction SC LM Model would be more appropriate than an interaction SC LM Model

#   You would compare the two with a likelihood ratio: the full model is that of question 30, the reduced model is that of 
#   question 27. You would calculate the likelihood ratio and treat it as a chi-squared stat with 3 degrees of freedom and
#   use the p-value to accept or reject the null hypothesis that the interaction variables add nothing to the model. 

#33)State a Lunn-McNeil model that can be used with an augmented dataset that will provide identical results to those obtained
#   from usig the model of Question 27.

#   h(t,x)=hnull*exp(B1*D1*TMS + B2*D1*AGE + B3*D1*G + B4*D2*TMS + B5*D2*AGE + B6*D2*G)
#   D2=0 if patient develops an infection
#   D2=1 if patient dies without infection

#34)For the LM Model of question 33, what is the formula for hte hazar ratio for the group effect G, controlling for TMS and AGE?

#   haz=exp(B3)