library(survival)
library(foreign)

rm(list=ls(all=TRUE))
################################
# CHAPTER 9 Practice
################################

# Answer questions 1 to 15 as true or false.

################################
# Question 1
################################

# A competing risk is an event-type (i.e., failure status) that can occur simultaneously with another 
# event of interest on the same subject.

# False

# Page 430:
# Competing risks occur when there are at least two possible ways that a person can fail, but only one
# failure type can actually occur.

################################
# Question 2
################################

# An example of competing risks survival data is a study in which patients receiving radiotherapy for head and 
# neck cancer may either die from their cancer or from some other cause of death.

# True

################################
# Question 3 
################################

# If all competing risks in a given study are different causes of death, then it is possible to have both competing 
# risks and recurrent events in the same study.

# False

# Page 430:

# Death is a one time event and cannot be recurrent.

################################
# Question 4
################################

# Suppose patients with advanced-stage cancer may die after surgery before their hospital stay is long enough to 
# get a hospital infection. Then such deaths from surgery reduce the hospital’s burden of infection control.

# True

################################
# Question 5
################################

# The typical approach for analyzing competing risks using a Cox PH model involves fitting separate models for 
# each competing risk ignoring the other competing risks.

# False

# Page 434:

# The typical approach for analyzing competing risks data uses the Cox (PH) model to separately estimate hazards and 
# corresponding hazard ratios for each failure type, treating the other (competing) failure types as censored in addition 
# to those who are censored from loss to follow-up or withdrawal.

# Referred to as Method 1

################################
# Question 6
################################

# Suppose that a cause-specific risk of interest is development of lung metastasis, and a competing risk is local recurrence 
# of a lung tumor. Then a patient who develops a local recurrence is treated as a failure in a competing risk analysis.

# False

# Page 434:

# The patient would be treated as censored.

################################
# Question 7
################################

# When there are no competing risks, then any study subject in the risk set at a given time has the same risk for failing 
# as any other subject in the risk set with the same values for covariate predictors at time t.

# False/True?

# Page 438

# According to answer key, it is false.  Says that it would be true if the censoring is independent.

#????????????????????????????????

################################
# Question 8
################################

# If, when analyzing competing risks survival data, it is assumed that censoring is independent, then a subject in the 
# risk set at time t is as likely to fail from any competing risk as to be lost to follow-up.

# True

# Page 438

################################
# Question 9
################################

# When a sensitivity analysis indicates that a worst-case scenario gives meaningfully different results from an analysis 
# that assumes independence of competing risks, then there is evidence that the independence assumption is violated.

# False

# Page 441

# If, on the other hand, the sensitivity analysis provides results that meaningfully differ from results obtained 
# under the independence assumption, the investigator learns only the extremes to which the results could be biased
# without adjusting for the actual bias.

################################
# Question 10
################################

# The typical competing risk analysis assumes that competing risks are independent even if this assumption is not true.

# True

# Page 443

# The typical survival analysis assumes that the independence assumption is satisfied when there are
# competing risks, even if this is not the case.

################################
# Question 11
################################

# The Cumulative Incidence Curve (CIC) provides risk estimates for the occurrence of a cause-specific event in 
# the presence of competing risks.

# True

# Starting on Page 444

# ??????????????????????  Maybe more explanation?

################################
# Question 12
################################

# CIC = 1 - KM, where KM denotes the Kaplan–Meier curve.

# False

# Page 447

# In the simplest case, if there is only 1 case CIC = 1 - KM.

################################
# Question 13
################################

# A CIC for a cause-specific event that ignores the control of covariates does not require the assumption of independent 
# competing risks.

# True

# ?????????????????

################################
# Question 14
################################

# A Cumulative Probability Curve (CPC) gives the probability of experiencing an event c by time t, given that an 
# individual has experienced any of the other competing risks by time t.

# False

# Page 453 

# Put simply, the CPCc is the probability of experiencing an event c by time t, given that an
# individual has not experienced any of the other competing risks by time t.


################################
# Question 15
################################

# If CICc = .4, then CPC = .4/.6 = .667.

# False

# Page 454

# CPCc = CICc/(1-CICc')

################################
# Question 16
################################

# The Lunn–McNeil (LM) approach fits a single stratified Cox model using an augmented dataset to obtain the same 
# results as obtained by fitting separate Cox models for each cause specific competing risk.

# True

# Page 455

################################
# Question 17
################################

# An advantage of the Lunn–McNeil (LM) approach over the approach that fits separate Cox models is that the LM approach 
# allows for testing whether a no-interaction SC model might be preferable to an interaction SC model.

# True

# ?????????????????????

################################
# Question 18
################################

#Given the LM model stratified on two cause specific events, Cancer and CVD:
 
# h'g(t,X)= h'0g(t)exp[B1Rx + B2Age + theta1(DxRx) +theta2(DxAge)],
# 
# g = 1, 2 where
# D = 0 if Ca and = 1 if CVD
# 
# then
# 
# HRcvd(Rx = 1 vs. Rx = 0) = exp[B1 + theta1]

# True

# ????????????????????????????
################################
# Question 19
################################

#??????????????????????????????

################################
# Question 20
################################

#??????????????????????????????

################################
# Questions 21-26
################################

# Consider a hypothetical study of the effect of a bone marrow transplant for leukemia on leukemia-free survival,
# where transplant failures can be of one of two types: relapse of leukemia and nonrelapse death (without prior
# relapse of leukemia). Suppose that in hospital A, 100 patients undergo such a transplant and that within the
# first 4 years post-transplant, 60 die without relapse by year 2 and 20 relapse during year 4. Suppose that in hospital
# B, 100 patients undergo such a transplant but posttransplant, there are 20 non-relapse deaths by year 1, 15
# relapses during year 2, 40 non-relapse deaths between years 3 and 4, and 5 relapses during year 4.


################################
# Question 21
################################

# What are the competing risks in this study?

# The competing risks are: relapse of leukemia and nonrelapse death (without prior relapse of leukemia)

################################
# Question 22
################################

# What is the proportion of initial patients in hospitals A and B, respectively, that have leukemia relapse by 4 years?

# Hospital A = 20/100
# Hospital B = 20/100

################################
# Question 23
################################

# How have both tables treated the competing risk for nonrelapse death in the calculation of the KM probabilities?

# Both have treated the competing risk for nonrelapse death as a censored event.

################################
# Question 24
################################

# Why are the KM probabilities different at 4 years for each hospital?

# The KM probabilities are different at year 4 because even though they had the same proportion of leukemia relapses,
# the competing risks caused different censorship patterns.

################################
# Question 25
################################

# Compute the CIC curves for each hospital using the following tables.


# Hospital A

# tf  nf  mf  hca(tf) s(tf-1) Ica(tf) CIC(tf)

# 0   100 0   0
# 2   40  0   0       1       0       0
# 4   40  20  0.5     0.4     0.2     0.2


# Hospital B

# tf  nf  mf  hca(tf) s(tf-1) Ica(tf) CIC(tf)

# 0   100 0   0
# 1   80  0   0       1       0       0
# 2   80  15  0.1875  0.8     0.15    0.15
# 3   65  0   0       0.65    0       0.15
# 4   25  5   0.20    0.25    0.05    0.20

################################
# Question 26
################################

# Why are the CIC probabilities the same at 4 years?

# The CIC probabilities are marginal probabilites that are not influenced by any patterns of censorship.

################################
# Questions 27-34
################################

# Consider a hypothetical study to assess the effect of a new hospital infection control strategy for patients who
# undergo heart transplant surgery in a given hospital. The exposure variable of interest is a binary variable Group
# (G): G = 0 for those patients receiving heart transplants from 1992 through 1995 when the previous hospital control
# strategy was used; G = 1 for those patients receiving heart transplants from 1996 through 1999 when the new
# hospital infection control strategy was adopted. The primary event of interest is getting a hospital infection after
# surgery. A competing risk is death during recovery from surgery without getting a hospital infection. Control variables
# being considered are tissue mismatch score (TMS) at transplant and AGE at transplant. The outcome variable of
# interest is time (DAYS after surgery) until a patient developed a hospital infection.

################################
# Question 27
################################

# State a cause-specific no-interaction Cox PH model for assessing the effect of group status (G) on time
# until a hospital infection event.

# hHI(t, X) = h0(t)exp[B1HIG + B2HITMS + B3HIAge]

# HI indicates a hospital infection

################################
# Question 28
################################

# When fitting the model given in Question 27, which patients should be considered censored?

# Patients dying during recovery from surgery without getting a hospital infection.  Also censor patients
# who are lost to follow-up or withdraw from the study.

################################
# Question 29
################################

# Describe or provide a table that would show how the data on the ith patient should be augmented for input
# into a Lunn–McNeil (LM) model for this analysis

# Subj Time Status  D1  D2  G   TMS   AGE
# i     ti  e1i     1   0   Gi  TMSi  AGEi
# i     ti  e2i     0   0   Gi  TMSi  AGEi
# 
# e1i = 1 if the ith subject develops a hospital infection, otherwise 0
# e21 = 1 if the ith sujbect dies after surgery, otherwise 0
# 
# D1 = indicator for infection
# D2 = indicator for death after surgery

################################
# Question 30
################################

# State a LMmodel that can be used with an augmented dataset that will provide identical results to those
# obtained from using the model of Question 27.

#hg(t, X) = h0g(t)exp[B1G + B2TMS + B3Age + Theta21D2G + Theta22D2TMS + Theta23D''2AGe]

################################
# Question 31
################################

# For the LM model of Question 30, what is the formula for the hazard ratio for the group effect G, controlling
# for TMS and AGE.

# HRHI(RX = 1 vs RX = 9) = exp[B1]

################################
# Question 32
################################

# Describe how you would test whether a no-interaction SC LM model would be more appropriate than an
# interaction SC LM model.

# Do a likelihood ratio test to compare a full (SC Interaction LM) model to a reduced (no-interaction SC LM) model

################################
# Question 33
################################

# State a LMalt model that can be used with an augmented dataset that will provide identical results to
# those obtained from using the model of Question 27.

################################
# Question 34
################################

# For the LMalt model of Question 33, what is the formula for the hazard ratio for the group effect G,
# controlling for TMS and AGE?

################################
# Test
################################

The dataset shown below describes a hypothetical study of recurrent bladder cancer. The entire dataset contained 53
patients, each with local bladder cancer tumors who are followed for up to 30 months after transurethral surgical
excision. Three competing risks being considered are local recurrence of bladder cancer tumor (event ¼ 1), bladder
metastasis (event = 2), or other metastasis (event = 3). The variable time denotes survival time up to the occurrence
of one of the three events or censorship from loss to follow-up, withdrawal, or end of study. The exposure variable
of interest is drug treatment status (tx, 0 = placebo, 1 = treatment A), The covariates listed here are initial
number of tumors (num) and initial size of tumors (size) in centimeters.


id  event time  tx  num size
1   1     8     1   1   1
2   0     1     0   1   3
3   0     4     1   2   1
4   0     7     0   1   1
5   0     10    1   5   1
6   2     6     0   4   1
7   0     10    1   4   1
8   0     14    0   1   1
9   0     18    1   1   1
10  3     5     0   1   3
11  0     18    1   1   3
12  1     12    0   1   1
13  2     16    1   1   1
14  0     18    0   1   1
15  0     23    1   3   3
16  3     10    0   1   3
17  1     15    1   1   3
18  0     23    0   1   3
19  2     3     1   1   1
20  3     16    0   1   1
21  1     23    1   1   1
22  1     3     0   3   1
23  2     9     1   3   1
24  2     21    0   3   1
25  0     23    1   3   1
26  3     7     0   2   3
27  3     10    1   2   3
28  1     16    0   2   3
29  1     24    1   2   3
30  1     3     0   1   1
31  2     15    1   1   1
32  2     25    0   1   1
33  0     26    1   1   2
34  1     1     0   8   1
35  0     26    1   8   1
36  1     2     0   1   4
37  1     26    1   1   4
38  1     25    0   1   2
39  0     28    1   1   2
40  0     29    0   1   4
41  0     29    1   1   2
42  0     29    0   4   1
43  3     28    1   1   6
44  1     30    0   1   6
45  2     2     1   1   5
46  1     17    0   1   5
47  1     22    1   1   5
48  0     30    0   1   5
49  3     3     1   2   1
50  2     6     0   2   1
51  3     8     1   2   1
52  3     12    0   2   1
53  0     30    1   2   1

################################
# Question 1
################################

# Suppose you wish to use these data to determine the effect of tx on survival time for the cause-specific
# event of a local recurrence of bladder cancer. State a no-interaction Cox PH model for assessing this relationship
# that adjusts for the covariates num and size.

# h1(t, X) = h01(t)exp[B11tx + b21num + b31size]


################################
# Question 2
################################

# When fitting the model given in Question 1, which subjects are considered censored?

# Censor event 2 (bladder metastasis) and event 3 (other metastasis)

################################
# Question 3
################################

# How would you modify your answers to Questions 1 and 2 if you were interested in the effect of tx on
# survival time for the cause-specific event of finding metastatic bladder cancer?

# Model would be h2 and h02 instead and censor event 1 and event 3

################################
# Question 4
################################

















































