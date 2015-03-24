library(survival)
library(foreign)

rm(list=ls(all=TRUE))
################################
# CHAPTER 8 Test
################################

################################
# Question 1
################################

# Suppose that Bonnie (B) and Lonnie (L) are th only two subjects in the dataset shown below, wher both subjects have two
# recurrent events that occur ar different times.

# ID  Status  Stratum Start Stop
# B   1       1       0     12
# B   1       2       12    16
# L   1       1       0     20
# L   1       2       20    23

################################
# A 
################################

# Fill in the empty cells in the following data layout describing survival time (say, in weeks) to the first event (stratum 1):

# Answer

# t(f)    nf    mf    qf    R(t(f))
# 0       2     0     0     {B, L}
# 12      2     1     0     {B, L}
# 20      1     1     0     {L}

################################
# B 
################################

#Why will the layout given in part a be the same regardless of whether rhe analysis approach is the Counting Process (CP), 
# Stratified CP, Gap Time, or Marginal approaches?

# Answer

# Regardless of the approach, the observation for the first events are identical.

################################
# C 
################################

# Fill in the empty cells in the following data layout describing survival time (say, in weeks) from the first to the 
# second event (stratum 2) using the Stratified CP approach:

# Answer

# t(f)    nf    mf    qf    R((f))
# 0       0     0     0     -
# 16      1     1     0     {B}
# 23      1     1     0     {L}

################################
# D
################################

# Fill in the empty cells in the following data layout describing survival time (say, in weeks) from the first to 
# the second event (stratum 2) using the Gap Time approach:

# Answer

# t(f)    nf    mf    qf    R((f))
# 0       2     0     0     {B, L}
# 3       2     1     0     {B, L}
# 4       1     1     0     {B}

################################
# E
################################

# Fill in the empty cells in the following data layout describiing survival time (say, in weeks) from the
# first to the second event (stratum 2) using the Marginal approach:

# t(f)    nf    mf    qf    R((f))
# 0       2     0     0     {B, L}
# 16      2     1     0     {B, L}
# 23      1     1     0     {L}

################################
# F
################################

# For the Stratified CP approach described in part c,determine which of the following choices is correct. Circle the number 
# corresponding to the one and only one correct choice.

# i. Lonnie is in the risk set when Bonnie gets her second event.
# ii. Bonnie is in the risk set when Lonnie gets her second event.
# iii. Neither is in the risk set for the other’s second event.

# Answer 

# Choice iii.
# Bonnie is at risk for a 2nd event from time 12 to 16
# Lonnie is at risk from time 20 to 23.
# There is no overlap in their risk set.

################################
# G
################################

# For the Gap Time approach described in part d, determine which of the following choices is correct.
# Circle the number corresponding to the one and only one correct choice.

# i. Lonnie is in the risk set when Bonnie gets her second event.
# ii. Bonnie is in the risk set when Lonnie gets her second event.
# iii. Neither is in the risk set for the other’s second event.

# Answer

# Choice ii
# Bonnie is at risk between times 0 to 4
# Lonnie is a trisk between times 0 to 3
# Bonnie is in the risk set during the time when Lonnie gets her 2nd event.

################################
# H
################################

# For the Marginal approach described in part e, determine which of the following choices is correct.
# Circle the number corresponding to the one and only one correct choice.

# i. Lonnie is in the risk set when Bonnie gets her second event.
# ii. Bonnie is in the risk set when Lonnie gets her second event.
# iii. Neither is in the risk set for the other’s second event.

# Answer

# Choice i
# Bonnie is at risk between times 0 to 16
# Lonnie is at risk between times 0 to 23
# Lonnie is i the risk set when Bonnie gets her 2nd event

################################
# Question 2
################################

# The dataset shown below in the counting process layout comes from a clinical trial involving 36 heart attack
# patients between 40 and 50 years of age with implanted defibrillators who were randomized to one of two treatment
# groups (tx, ¼ 1 if treatment A, ¼ 0 if treatment B) to reduce their risk for future heart attacks over a 
# 4-month period. The event of interest was experiencing a “high energy shock” from the defibrillator. The outcome
# is time (in days) until an event occurs. The covariate of interest was Smoking History 
# (1 ¼ ever smoked, 0 ¼ never smoked). Questions about the analysis of this dataset follow.

################################
# A
################################

# State the hazard function formula for the nointeraction model used to fit the CP approach.

# Answer

# h(t, X) = h0(t)exp[Btx + ysmoking]

# tx = 1 if treatment A, 0 if trt B
# Smoking status = 1 if ever smoke, 0 if never smoke

################################
# B
################################

# Based on the CP approach, what do you conclude about the effect of treatment (tx)? Explain briefly using the results in Table T.1.

# Answer

# There is no significatn effect of trt status adjusted for smoking.  
# HR = 1.087
# P-Value = .4182

################################
# C
################################

# State the hazard function formulas for the no-interaction and interaction SC models corresponding to the use of 
# the Marginal approach for fitting these data.

# Answer

# Interaction Model
# hb{t, X} = h0g(t)exp[Bgtx + ygsmoking], g = 1, 2, 3

# No-Interaction Model
# hb{t, X} = h0g(t)exp[Btx + ysmoking], g = 1, 2, 3

################################
# D
################################

# Table T.1 gives results for “no-interaction” SC models because likelihood ratio (LR) tests comparing a “no-interaction” 
# with an “interaction” SC model were not significant. Describe the (LR) test used for the marginal model (full and reduced models,
# null hypothesis, test statistic, distribution of test statistic under the null).

# Answer

# LR = -2lnLR-(-21nLF)
# Approx x^2 with 4df

# H0: no-interaction SC model is appropriate, whereR denotes the reduced (no interaction SC) model and F denotes the full (interaction SC) model

################################
# E
################################

# How can you criticize the use of a no-interaction SC model for any of the SC approaches, despite the finding that the 
# above likelihood ratio test was not significant?

# Answer

# The use ofa non-interactionmodel does not allow you to obtain stratum-specific HR estimates, even when you assume strata are important.

################################
# F
################################

# Based on the study description given earlier, why does it make sense to recommend the CCP approach
# over the other alternative approaches?

# Answer

# The CP approach makes sence because the recurrent events on teh same subject (defibrillator shock) are the same types of events
# regardless of when they occurred.

################################
# G
################################

# Under what circumstances/assumptions would you recommend using the Marginal approach instead of the CP approach?

# Answer

# The marginal approach would be used if you determined that the recurrent events on the same subject were different.

################################
# H
################################

# In Table T.2, why does the number in the risk set (nf) remain unchanged through failure time (i.e., day) 68, 
# even though 50 events occur up to that time?

# Answer

# The number in the risk set remains unchanged because the subjects who had already failed by this time were still at risk for a 
# subsequent event.

################################
# I
################################

# Why does the number in the risk set change from 31 to 26 when going from time 96 to 97?

# Answer

# Subjects 9, 15, and 28 fail for the second time at 79 days, subject #16 is censored at 79 days.

################################
# J
################################

# Why is the number of failures (mf) equal to 3 and the number of censored subjects equal to 1 in the interval
# between failure times 79 and 80?

# Answer

# Subjects 3,6, 10,26, and 31 all fail for the third time at day 98 and are not followed afterwards.

################################
# K
################################

# What 5 subjects were censored in the interval between failure times 111 and 112?

# Answer

# Subjects 4, 14, 15, 24, and 29

################################
# L
################################

# Describe the event history for subject #5, including his or her effect on changes in the risk set.

# Answer

# First event at 45 days
# Second event at 68 days
# Drops out of study at 68 days
# causes the risk set to change from 36 to 35 after 68 days.

################################
# M
################################

# What is being plotted by such a curve? (Circle one or more choices.)

# i. Pr(T1 > t) where T1 = time to first event from study entry.
# ii. Pr(T > t) where T = time from any event to the next recurrent event.
# iii. Pr(T > t) where T = time to any event from study entry.
# iv. Pr(not failing prior to time t).
# v. None of the above.

# Answer

# v. None of the above.

################################
# N
################################

# Can you criticize the use of the product limit formula for S(t(f)) in Table T.3? Explain briefly.

# Answer

# Product limit formula does not apply to CP data


################################
# O
################################

# Use Table T.2 to complete the data layouts for plotting the following survival curves.

# i. S1(t) ¼ Pr(T1 > t) where T1 ¼ time to first event from study entry


# t(f) nf mf qf S(t(f)) = S(t(f1))  Pr(T1 > t|T1  t)
# 0    36 0  0  1.00
# 33   36 2  0  0.94
# 34   34 3  0  0.86
# 36   31 3  0  0.78
# 37   28 2  0  0.72
# 38   26 4  0  0.61
# 39   22 5  0  0.47
# 40   17 1  0  0.44
# 41   16 1  0  0.42
# 43   15 1  0  0.39
# 44   14 1  0  0.36
# 45   13 2  0  0.31
# 46   11 2  0  0.25
# 48   9  1  0  0.22
# 49   8  1  0  0.19

# 51   7  2  0  .19 * 5/7 = .14
# 57   5  2  0  .14 * 3/5 = .08
# 58   3  2  0  .08 * 1/3 = .03
# 61   1  1  0  .03 * 0/1 = 0

# ii. Gap Time S2c(t) = Pr(T2c > t) where T2c = time to second event from first event.

# t(f) nf mf qf S(t(f)) = S(t(f1))  Pr(T1 > t|T1  t)
# 0    36 0  0  1.00
# 5    36 1  0  0.97
# 9    35 1  0  0.94
# 18   34 2  0  0.89
# 20   32 1  0  0.86
# 21   31 2  1  0.81
# 23   28 1  0  0.78
# 24   27 1  0  0.75
# 25   26 1  0  0.72
# 26   25 2  0  0.66
# 27   23 2  0  0.60
# 28   21 1  0  0.58
# 29   20 1  0  0.55
# 30   19 1  0  0.52
# 31   18 3  0  0.43  
# 32   15 1  0  0.40
# 33   14 5  0  0.26  
# 35   9  1  0  0.23  
# 39   8  2  0  0.17

# 40
# 41
# 42
# 46
# 47

#???????????????????????????????????????????????????????????????????

# Marginal S2m(t) = Pr(T2m > t) where T2m = time to second event from study entry.

# t(f) nf mf qf S(t(f)) = S(t(f 1))  Pr(T1 > t|T1  t)
# 0    36 0  0  1.00
# 63   36 2  0  0.94                34, 19
# 64   34 3  0  0.86                13, 22, 33 
# 65   31 2  0  0.81                2, 10
# 66   29 3  0  0.72                6, 1, 25 
# 67   26 4  0  0.61                7, 3, 24, 18 
# 68   22 2  0  0.56                26, 5
# 69   20 1  0  0.53                23
# 70   19 1  0  0.50                35
# 71   18 1  0  0.47                32
# 72   17 2  0  0.42                8,20 
# 73   15 1  0  0.39                29
# 74   14 1  0  0.36                31
# 76   13 1  0  0.33                17
# 77   12 1  0  0.31                14
# 78   11 2  0  0.25                11, 30

# 79   9  3  1  .25 * 6/9 = .17     28, 9, 16 # 15
# 80   5  2  0  .17 * 3/5 = .10     12, 4 
# 81   3  2  0  .10 * 1/3 = .03     21, 36 
# 97   1  1  0  .03 * 0/1 = 0       27 
 

################################
# P
################################

# The survival curves corresponding to each of the data layouts (a, b, c) described in Question 14 will be different. Why?

# Answer

# Simple Answer: they're using different numbers.
# Less Simple Answer: They are describing different survival functions.





























































































