#Chapter 10: Design Issues
rm(list = ls(all.names = TRUE))

library(survival)
library(survrec)
library(cmprsk)

#####################################
#Practice Questions
#####################################

#1) In a randomized trial comparing 2 groups, the null hypothesis (H0) 
#   assumes that the survival experience in the groups is different.

#   FALSE

#2) The effect size is typically determined in the form of a difference of hazard rates.

#   FALSE

#3) Suppose that a time-to-event study is designed to determine if there is reduction in annual
#   hazard rates from 5% to 2.5%. Then the effect size of interest defined in terms of a ratio measure is 2.

#   TRUE

#4) The P-value is the probability that the null hypothesis is true given the observed study data.

#   FALSE

#5) Consider a two-group randomized clinical trial, in which the randomization process is not 
#   systematically flawed. Then the use of a P-value to compare the distributions of a known risk 
#   factor (e.g., gender) in each group is uninformative.

#   TRUE

#6) When determining sample size for a time-to-event study, one typically first determines the 
#   expected (i.e., required) number of events (NEV) prior to determining the study size required (N).

#   TRUE

#7) When carrying out a time-to-event study, one typically recruits the study participants prior to 
#    observing the number of events that actually occur.

#   TRUE

#8) A formula for the total sample size (N) for a time-to-event study can be derived by multiplying 
#   the required number of events (NEV) by the probability (pEV) that a study subject will get an
#   event since study entry, i.e., N = NEV * pEV.

#   FALSE

#9) Suppose the allocation ratio (R = N1/N0) for subjects into two treatment groups is 1/2, and 
#   it is determined that the total study size (N) required is 300.Then N1=100 and N0=200.

#   TRUE

#10)  Suppose that it is determined that the total study size required (N) is 300 without considering 
#     the possibility of loss to follow-up during the study. If, nevertheless, the investigators expect 
#     that 20% of study subjects would be lost to follow-up, then to adjust for possible loss to follow-up,
#     the sample size should be increased to 1500 subjects.  

#     FALSE; n=300/(1-0.2) = 375

#     Consider a randomized trial in which the accrual period (A) is 2 years and the follow-up period (F) is
#     3 years. Suppose further that the allocation ratio (R) is 2, a = 0.05, b =.20, and the study aim is to
#     increase the median survival time (mi) in the control group from 1.5 years to 2.2 years in the intervention
#     group.

#11)  Assuming an exponential survival rate in both groups, transform the given median survival times (mi) 
#     to corresponding hazard rates.

#     Control Group: hazard = ln(R)/m0 = log(2)/(1.5) = 0.462
#     Treatment Gropu: hazard = ln(R)/m1 = log(2)/(2.2) = 0.3151

#12)  Based on your answer to question 11, what is the effect size?

#     Effect size, delta, = hazard1/hazard0=0.462/0.3151 = 1.466

#13)  Determine the number of events (NEV) required for the study.

#     NEV = ((z[1-a/2] + z[1-b/2])(RDelta+1)/(sqrt(R)*(Delta-1)))^2 =
            ((1.96 + 0.84)*(((2)*(1.467))+1)/(sqrt(2)*((1.467)-1)))^2
#         = 278.1769 = 279

#14)  Determine the probability of getting an event (pEVi) for each group, i = 0,1.

#     For Control Group: Pev0 = (1 - (1/(h0*A))*(e(-h0*F)-e(-h0(A+F)))) 
                                (1 - (1/(0.462*2))*(exp(-0.462*3)-exp(-0.462*(2+3)))) 
#                             = 0.8368

#     For Control Group: Pev1 = (1 - (1/(h0*A))*(e(-h0*F)-e(-h0(A+F)))) 
                                (1 - (1/(0.3151*2))*(exp(-0.3151*3)-exp(-0.3151*(2+3)))) 
#                             = 0.7117

#15)  Determine the total sample size (N) required for this study

#     N = Nev/(((R/(R+1))*Pev1) + ((1/(R+1))*Pev0))  
          (279)/(((2/(2+1))*(0.7117)) + ((1/(2+1))*(0.8368)))  
        = 371

#16)  Determine the sample sizes in each group (i.e., N0 and N1).

#     N1 =
           (2/(2+1))*(371)
         = 247.3 
  
#     N0 = 
           (247.3)/2
         = 123.65

#17)  Based on your answer to question 14, how would you adjust your total sample size to consider
#     loss-to-follow-up of 25%?

#     Nlofadj = N/(1-Plof) = 
                (371)/(1-0.25)
#             = 494.667

#18)  Based on your answer to question 17, determine the sample sizes required in each group (i.e., N0 and N1).
#     N1 =
          (2/(2+1))*(495)
#        = 330 

#     N0 = 
          (330)/2
#        = 165

#19)  Based on your answer to question 17, how would you adjust your total sample size to consider cross-over
#     percentages of dc = .05 and dt = 0.10?

#     Nittadj = (N/(1-dc-dt))^2 = 
                495/(((1 - 0.05) - 0.1)^2) 
#             = 582.4

#20)  Based on your answer to question 17, determine the sample sizes required in each group (i.e., N0 and N1).

#     N1 =
          (2/(2+1))*(583)
#        = 389 

#     N0 = 
          (389)/2
#        = 194

#####################################
#Test Questions
#####################################

#Consider a randomized trial in which the accrual period (A) is 2 years and the follow-up period (F) is 2 years. 
# Suppose further that the allocation ratio (R) is 2, a = 0.05, b = .10, and the study aim is to reduce hazard rate
# in the control group from 10% to 5% in the intervention group.

#1) Use Formula 1 (see detailed outline) to determine the required number of events (NEV), total study size (N),
#   and sample sizes required in each group (i.e., N0 and N1).

#     NEV = ((z[1-a/2] + z[1-b/2])(RDelta+1)/(sqrt(R)*(Delta-1)))^2 =
          ((1.96 + 1.282)*(((2)*(2))+1)/(sqrt(2)*((2)-1)))^2
#         = 132


#     N = Nev/(((R/(R+1))*(1-e(-Delta*alpha*MF))) + ((1/(R+1))*(1-e(-alpha*MF))))  
          (132)/(((2/(2+1))*(1-exp(-2*0.05*3))) + ((1/(2+1))*(1-exp(-.05*3))))  
#       = 602

#     N1 =
            (2/(2+1))*(602)
#        = 401 

#     N0 = 
          (401)/2
#        = 201

#2) Use Formula 2 (see detailed outline) to determine the required number of events (NEV), 
#   total study size (N), and sample sizes required in each group (i.e., N0 and N1).


#     For Control Group: Pev0 = (1 - (1/(h0*A))*(e(-h0*F)-e(-h0(A+F)))) 
                                (1 - (1/(0.1*2))*(exp(-0.1*2)-exp(-0.1*(2+2)))) 
#                             = 0.2579

#     For Control Group: Pev1 = (1 - (1/(h0*A))*(e(-h0*F)-e(-h0(A+F)))) 
                                (1 - (1/(0.05*2))*(exp(-0.05*2)-exp(-0.05*(2+2)))) 
#                             = 0.1389

#     N = Nev/(((R/(R+1))*Pev1) + ((1/(R+1))*Pev0))  
          (132)/(((2/(2+1))*(0.1389)) + ((1/(2+1))*(0.2579)))  
#       = 739

#     N1 =
          (2/(2+1))*(739)
#        = 492 

#     N0 = 
          (492)/2
#        = 246

#3) The two are slightly different. The first form uses the median follow-up time to estimate the number of expected events,
#   and gives a slightly smaller estimate. The second assumes that subjects enter the study at times that follow an exponential
#   distribution. 

#4) Based on your answer to question 2, how would you adjust your total sample size to consider loss-to-follow-up of 25%?

#     Nlofadj = N/(1-Plof) = 
                (739)/(1-0.25)
#             = 985.33, so 986.

#5) Based on your answer to question 4, determine the sample sizes required in each group (i.e., N0 and N1).

#     N1 =
          (2/(2+1))*(986)
#        = 658. 

#     N0 = 
          (658)/2
#        = 246

#6) Based on your answer to question 4, how would you adjust your total sample size to consider cross-over proportions
#   of dc = .05 and dt = 0.10?
#     Nittadj = N/(1-dc-dt) = 
                986/((1 - 0.05 - 0.1)^2) 
#             = 1365


#7) Based on your answer to question 6, determine the sample sizes required in each group (i.e., N0 and N1).

#     N1 =
          (2/(2+1))*(1365)
#        = 910 

#     N0 = 
          910/2
#        = 455

#8) Using the total study size (N) calculated in question 6, these study subjects will need to be recruited 
#   at an accrual rate of r1⁄4N/A per year, where A1⁄42. If this accrual rate is not feasible, i.e., you couldn’t 
#   find r subjects per year, how can you adjust your sample size to make your study feasible?

#   The two obvious solutions are to either increase the amount of time you spend accruing subjects, or the amount
#   of time spent in follow-up. Of the two, increasing the follow-up time tends to have a greater effect on the 
#   required number of subjects (pg 512).
