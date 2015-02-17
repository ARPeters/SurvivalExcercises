library(survival)
library(foreign)

rm(list=ls(all=TRUE))
################################
# CHAPTER 7 Test
################################

################################
# Question 1
################################

# T or F
# The accelerated failure time model and proportional hazards model are both additive models.

# Page 298
# False

# The AFT model has a multiplicative effect with survival time.
# The PH model has a multiplicative effect with hazard.


################################
# Question 2
################################

# T or F
#If the survival function is known, then the hazard function can be ascertained (and vice versa).

# Page 
# ????


################################
# Question 3
################################

# T or F
# If survival time follows a Weibull distribution then a plot of the ln[-ln S(t)] against ln(t) should be a straight line.

# Page 305
# True

# Useful Weibull property:
# ln[-ln S(t)] is linear with ln(t)
# This enables graphical evaluation usingKM survival estimates.


################################
# Question 4
################################

# T or F
# If the acceleration failure time (AFT) assumption holds in a log-logistic model then the proportional hazards assumption also holds.

# Page 310
# False

# The log-logistic AFT is not a PH model.  It is a proportional odds model.
# Note for myself: a proportional odds survival model is a model in which the survival odds ration is assumed to remain constant over time.


################################
# Question 5
################################

# T or F
# If the acceleration factor for the effect of an exposure (exposed vs. unexposed) is greater than one, then the exposure is harmful to the survival.

# Page 303
# False

# An acceleration factor greater than one for the effect of an exposure implies that being exposed (i.e., TRT = 1), is beneficial to survival.
# A Hazard Ration greater than one implies being exposed is harmful to survival (and vice versa).


################################
# Question 6
################################

# T or F
# Let S0(t) be the survial function for unexposed subjects (E = 0) and let S1(t) be the survival function for exposed subjects (E = 1).  If gamma is the acdelration factor comparing E = 1 vs. E = 0, theh S0(t) = S1(gammat).

# Page 298 
# True

# Smoker illustraction


################################
# Question 7
################################

# T or F
# Frailty models are designed to provide an approach to account for unobserved individual-legel characteristics.

# Page 327
# True

# The frailty alpha is is an unovserved multiplicative effect on the hazard function assumed to follow some distribution g(alpha) with alpha > 0 and the mea of alpha equal to 1.  The variance of alpha is a parameter (theta) that is typically estimated from the data.


################################
# Question 8
################################

# T or F
# If you include a gamma distributed frailty component to the model, then you will see an additional parameter estimate for the variance of the frailty in the model output.

# Page 329
# True


################################
# Question 9
################################

# T or F
# If survival time T follows a Weibull distribution, then ln(T) also follows a Weibull distribution.

# Page
# ??????


################################
# Question 10
################################

# T or F
# If a subject is lost to follow-up after 5 years, then the subject is left-censored.

# Page 318
# False

# The individual would be right censored.


################################
# Question 11
################################

# Estimate the acceleration factor with a 95% confidence interval comparing Clinic = 2 vs. Clinic = 1.  Interpret this result.

# Acceleration Factor
exp(.698)
# 2.009729

# 95% Confidence Interval

exp(.698 + 1.96*(.158))
# Upper Bound 2.739239

exp(.698 - 1.96*(.158))
# Lower Bound 1.474502

# The acceleration factor of 2 suggesta that the time off heroin is doubled for those in clinic 2 versus clinic 1.

################################
# Question 12
################################

# Estimate the hazard ratio with a 95% confidence interval comparing Clinic = 2 vs. Clinic = 1.  Interpret this resutlt.

# Hazard Ratio

exp (-.957)
# .3840433

# 95% Confidence Interval

exp(-.957 + 1.96*(.213))
# Upper Bound .583028

exp(-.957 - 1.96*(.213))

# Lower Bound .2529711

# The hazard ratio of .38 suggests that the hazard of going back on heroin is reduced by a factor of .38 for those in clinic 2 versus clinic 1.

################################
# Question 13
################################

# Estimate the coefficient for CLINIC in the PH Weibull model using the results reported in the output from the AFT form of the model. Hint: the coefficients for a Weibull PH and AFT model are related bj ¼ ajp for the jth covariate.

# clinic
-(.698*1.370467)
# -0.956586

# prison
-(.145*1.370467)
# -0.1987177

# dose
-(.027*1.370467)
# -0.03700261

# _cons
-(3.977*1.370467)
# -5.450347


################################
# Question 14
################################

# Is the product term PRISDOSE included in the model to account for potential interaction or potential confounding of the effect of CLINIC on survival?

#?????


################################
# Question 15
################################

# Use the output to estimate the median survival time for a patient fromCLINIC ¼ 2 who has a prison record and receives a methadone dose of 50 mg/day. Hint:   use the relationship that t ¼ [ln S(t)]1/p  (1/l1/p) for a Weibull model.

# clinic = 2
# Prison = 1
# dose = 50
# prisdose = 50




################################
# Question 16
################################

################################
# Question 17
################################
