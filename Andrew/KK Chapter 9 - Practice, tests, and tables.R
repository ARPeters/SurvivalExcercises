#Chapter 9: Competing Risks
rm(list = ls(all.names = TRUE))

library(survival)
library(survrec)
library(cmprsk)

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

#####################################
#Test Questions
#####################################

# The dataset shown below describes a hypothetical study of
# recurrent bladder cancer. The entire dataset contained 53
# patients, each with local bladder cancer tumors who are
# followed for up to 30 months after transurethral surgical
# excision. Three competing risks being considered are local
# recurrence of bladder cancer tumor (event ¼ 1), bladder
# metastasis (event ¼ 2), or other metastasis (event ¼ 3).
# The variable time denotes survival time up to the occurrence
# of one of the three events or censorship from loss to
# follow-up, withdrawal, or end of study. The exposure variable
# of interest is drug treatment status (tx, 0 = placebo,
# 1 = treatment A), The covariates listed here are initial
# number of tumors (num) and initial size of tumors (size)
# in centimeters.

data9<-c("1 1 8 1 1 1
2 0 1 0 1 3
3 0 4 1 2 1
4 0 7 0 1 1
5 0 10 1 5 1
6 2 6 0 4 1
7 0 10 1 4 1
8 0 14 0 1 1
9 0 18 1 1 1
10 3 5 0 1 3
11 0 18 1 1 3
12 1 12 0 1 1
13 2 16 1 1 1
14 0 18 0 1 1
15 0 23 1 3 3
16 3 10 0 1 3
17 1 15 1 1 3
18 0 23 0 1 3
19 2 3 1 1 1
20 3 16 0 1 1
21 1 23 1 1 1
22 1 3 0 3 1
23 2 9 1 3 1
24 2 21 0 3 1
25 0 23 1 3 1
26 3 7 0 2 3
27 3 10 1 2 3
28 1 16 0 2 3
29 1 24 1 2 3
30 1 3 0 1 1
31 2 15 1 1 1
32 2 25 0 1 1
33 0 26 1 1 2
34 1 1 0 8 1
35 0 26 1 8 1
36 1 2 0 1 4
37 1 26 1 1 4
38 1 25 0 1 2
39 0 28 1 1 2
40 0 29 0 1 4
41 0 29 1 1 2
42 0 29 0 4 1
43 3 28 1 1 6
44 1 30 0 1 6
45 2 2 1 1 5
46 1 17 0 1 5
47 1 22 1 1 5
48 0 30 0 1 5
49 3 3 1 2 1
50 2 6 0 2 1
51 3 8 1 2 1
52 3 12 0 2 1
53 0 30 1 2 1")

data9b<-data.frame(matrix(as.numeric(unlist(strsplit(data9, split="\\s|\\n"))), ncol=6,byrow=TRUE))
colnames(data9b)<-c("ID", "Event", "Time", "TX", "NUM", "Size")

#1) Suppose you wish to use these data to determine
#   the effect of tx on survival time for the cause-specific
#   event of a local recurrence of bladder cancer. State a
#   no-interaction Cox PH model for assessing this relationship
#   that adjusts for the covariates num and size.

#   haz(x,t)=hnull(x)exp(B1*TX + B2*NUM + B3*Size); events other than local recurrence (event=1) are considered censored.

#2) When fitting the model given in Question 1, which subjects are considered censored?

#   Bladder metastasis (event=2) and other metastasis (event=3) are considered censored. 

#3) How would you modify your answers to Questions 1
#   and 2 if you were interested in the effect of tx on
#   survival time for the cause-specific event of finding
#   metastatic bladder cancer?

#   I would treat local recurrence (event=1) and other mestasis (event=3) as censored and fit essentially the same Cox PH model. 

#4) For the model considered in Question 1, briefly
#   describe how to carry out a sensitivity analysis to determine
#   how badly the results from fitting this model
#   might be biased if the assumption of independent competing
#   risks is violated.

#   To see how biased this model might be, we would compare our Cox PH model to those for which the independence assumption has been
#   thoroughly violated. This could be done by considering all events to be the single event type of interest, or by changing the
#   survival times of event types 2 and 3 to the last possible time, essentially treating them as event-free. 
#   We would then calculate a Cox PH model for one or both of these assumptions and compare them to the Cox PH model from question 1.
#   This does not give any evidence of dependence or independence, but it does offer a view of how biased our first model might be
#   under worst-case conditions. 

#5) 
data9_5tx1<-c("0 27 0 0 — — —
2 27 0 0 1 0 0
3 26 0 0 .9630 0 0
4 24 0 0 .8889 0 0
8 23 1 .0435 .8889 .0387 .0387
9 21 0 0 .8116 0 .0387
10 20 0 0 .7729 0 .0387
15 17 1 .0588 .7343 .0432 .0819
16 15 0 0 .6479 0 .0819
18 14 0 0 .6047 0 .0819
22 12 1 .0833 .6047 .0504 .1323
23 11 1 .0910 .5543 .0504 .1827
24 8 1 .1250 .5039 .0630 .2457
26 7 1 .1429 .4409 .0630 .3087
28 4 0 0 .3779 0 .3087
29 2 0 0 .2835 0 .3087
30 1 0 0 .2835 0 .3087")

ds9_5tx1<-data.frame(matrix(as.numeric(unlist(strsplit(data9_5tx1, split="\\s|\\n"))), ncol=7,byrow=TRUE))
colnames(ds9_5tx1)<-c("tf", "nf", "d1f", "h1" ,"S1", "I1", "CIC1")
ds9_5tx1

data9_5tx0<-c("0 26 0 0 — — —
1 26 1 .0400 1 .0400 .0400
2 24 1 .0417 .9615 .0400 .0800
3 23 2 .0870 .9215 .0801 .1601
5 21 0 0 .8413 0 .1601
6 20 0 0 .8013 0 .1601
7 18 0 0 .7212 0 .1601
10 16 0 0 .6811 0 .1601
12 15 1 .0667 .6385 .0426 .2027
14 13 0 0 .6835 0 .2027
16 12 1 .0833 .5534 .0461 .2488
17 10 1 .1000 .4612 .0461 .2949
18 9 0 0 .4150 0 .2949
21 8 0 0 .4150 0 .2949
23 7 0 0 .3632 0 .2949
25 6 1 .1667 .3632 .0605 .3554
29 4 0 0 .2421 0 .3554
30 2 1 0 .2421 0 .3554")


ds9_5tx0<-data.frame(matrix(as.numeric(unlist(strsplit(data9_5tx0, split="\\s|\\n"))), ncol=7,byrow=TRUE))
colnames(ds9_5tx0)<-c("tf", "nf", "d1f", "h1" ,"S1", "I1", "CIC1")
ds9_5tx0

#5A)  Verify the CIC1 calculation provided at failure time
#     tf = 8 for persons in the treatment group (tx = 1);
#     that is, use the original data to compute h1(tf), S(tf1),
#     I1(tf), and CIC1(tf), assuming that the calculations
#     made up to this failure time are correct.

#     h1(tf=8)  = (1/23) =  0.04347
#     S(tf1=8)  = (26/27)*(24/26) = 0.88889
#     I1(tf=8)  = (h1(tf=8))*(S1(tf=8))=(1/23)*(24/27)=0.03865
#     CIC(tf=8) = 0 + 0.03865 = 0.03865

#5B)  Verify the CIC1 calculation provided at failure time
#     tf = 25 for persons in the treatment group (tx = 0).

#     h1(tf=25)  = (1/6) =  0.1667
#     S(tf1=25)  = S(21)*P(tf>23|tf>=23)=(.4150)*(7/8) = 0.36312
#     I1(tf=25)  = (h1(tf=25))*(S1(tf=25))=(1/6)*(0.36312)=0.0605
#     CIC(tf=25) = CIC(tf=23) + I(tf=25) = .2949 + 0.0605

#5C)  Interpret the CIC1 values obtained for both tthe treatment and placebo groups at tf=30. 

#     For the treatment group, there is an estimated probability of .3087 that any given individual will have experienced 
#     a recurrence of bladder cancer by time tf=30, controlling for the competing risk of bladder or other metastasis. 


#     For the placebo group, there is an estimated probability of .3554 that any given individual will have experienced 
#     a recurrence of bladder cancer by time tf=30, controlling for local recurrence or other metastasis. 


#6A)  The following output was obtained using separate models for each of the 3 event-types

ds6<-data9b
ds6$CR<-0
ds6$CM<-0
ds6$OM<-0

for(i in 1:length(ds6$CR)){
  ds6$CR[i]<-ifelse(ds6$Event[i]==1, 1, 0)
  ds6$CM[i]<-ifelse(ds6$Event[i]==2, 1, 0)
  ds6$OM[i]<-ifelse(ds6$Event[i]==3, 1, 0)
  
}

hazard6a<-coxph(Surv(Time, CR)~TX+NUM+Size, data=ds6)
hazard6b<-coxph(Surv(Time, CM)~TX+NUM+Size, data=ds6)
hazard6c<-coxph(Surv(Time, OM)~TX+NUM+Size, data=ds6)

hazard6a
hazard6b
hazard6c

#     What is the effect of treatment on survial from having a local recurrence of bladder cancer? Is it significant?
#     HR(tx=1, df=1) = 0.535, p=0.25; Not significant.

hazard6a

#6B)  What is the effect of treatment on survial from developing metastatic bladder cancer? Is it significant?
#     HR(tx=1, df=2) = 0.987, p=0.985; Not significant.

hazard6b

#6C)  What is the effect of treatment on survial from other metastatic cancer? Is it significant?
#     HR(tx=1, df=3) = 0.684, p=0.575; Not significant.

hazard6c

#7A)  Below is the ouput from fitting a LM model to the bladder cancer data.
#     State the hazard model formula for the LM model used for the ouput?

#     h(t,x)=hnull*exp(B1*tx+B2*num+B3*size
#                     +B4*txd2+B5*numd2+B6sized2
#                     +B7*txd3+B7*numd3+B7sized3)

#7B)  Determine the estimated hazard ratios for the effect of each of the 3 cause-specific events based
#     on the above output.

#     HR(tx=1, df=1)=0.535
#     HR(tx=1, df=2)=exp(-0.6258+0.6132)= 0.987
#     HR(tx=1, df=3)=exp(-0.6258+0.2463)= 0.684

#7C)  Verify that the stimated hazard ratios copmuted in part B are identical to the hazar rations computed in 
#     question 6.

#     Verified.

#8A)   Below is the output from fitting an alternate Lunn-McNeil model to the bladder cancer data.
#     State the hazard model formula for the LM model used for the ouput?

#     h(t,x)=hnull*exp(B1*txd1+B2*numd1+B3*sized1
#                     +B4*txd2+B5*numd2+B6*sized2
#                     +B7*txd3+B8*numd3+B9*sized3



#8B)  Determine the estimated hazard ratios for the effect of each of the 3 cause-specific events based
#     on the above output.

#     HR(tx=1, df=1)=0.535
#     HR(tx=1, df=2)=exp(-0.0127)= 0.987
#     HR(tx=1, df=3)=exp(-0.3796)= 0.684

#7C)  Verify that the stimated hazard ratios copmuted in part B are identical to the hazar rations computed in 
#     questions 6 and 7.

#     Verified.

#9)   State the formula for a no-interaction SC LM model for these data.

#     h(t,x)=hnull-g*exp(B1*tx+B2*num+B3*size); g=1,2,3

#10)  Describe how you would test whether a no-interaction SC LM model would be more appropriate than an interaction
#     SC LM model.

#     I would perform a likelihood ratio test, comparing the no-interaction model with the full model.


