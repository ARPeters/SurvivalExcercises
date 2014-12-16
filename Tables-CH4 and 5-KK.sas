*Chapter 4: Practice
*Importing vets data;
PROC IMPORT OUT= WORK.DS 
            DATAFILE= "C:\Users\APETERS4\Documents\GitHub\SurvivalExercises\Andrew\Vets.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc print;
run;

*Chapter 4 Practice 1
*Fitting Full Cox PH Model;
proc phreg data=ds;
model survt*status(0)= tx Large Adeno Small perf DisDur age priortx / ties=breslow;
OUTPUT out=phtest RESSCH=schtx schLarge schAdeno schSmall schperf schDisDur schage schpriortx;
run;

*Testing PH Assumption for each predictor;
data EventsOnly;
set phtest;
if status=1;
run;

proc rank data=EventsOnly out=Ranked ties=mean;
var survt;
ranks timerank;
run;

proc corr data=ranked NOSIMPLE;
VAR schtx schLarge schAdeno schSmall schperf schDisDur schage schpriortx;
with timerank;
run;


proc phreg data=ds;
model survt*status(0)= tx Large Adeno Small perf DisDur age priortx / ties=breslow;
run;


*Approximating a piecewise exponential model with Bayes estimation;
proc phreg data=ds;
model survt*status(0)= tx Large Adeno Small perf DisDur age priortx / ties=breslow;
BAYES PIECEWISE NBI=0 NMC=1;
run;

*Chapter 4: Test;
*Importing addicts data;
PROC IMPORT OUT= WORK.DS2 
            DATAFILE= "C:\Users\APETERS4\Documents\GitHub\SurvivalExercises\Andrew\Addicts.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc print;
run;

*Chapter 4, Test question 1;
*Fitting full Cox PH Model;
proc phreg data=ds2;
model survt*status(0)= clinic prison dose / ties=breslow;
OUTPUT out=phtestAddicts RESSCH=schClinic schPrison schDose;
run;

*Testing ph assumption;
data AddictsEventsOnly;
set phtestAddicts;
if status=1;
run;

proc rank data=AddictsEventsOnly out=AddictsRanked ties=mean;
var survt;
ranks AddictsTimerank;
run;

proc corr data=AddictsRanked NOSIMPLE;
VAR schClinic schPrison schDose;
with AddictsTimerank;
run;

*Estimating piecewise exponential model with Bayes estimate;
proc phreg data=ds2;
model survt*status(0)= clinic prison dose / ties=breslow;
BAYES PIECEWISE NBI=0 NMC=1;
run;



*Chapter 4, Test question 3;
*Stratifying on clinic;
proc phreg data=ds2;
model survt*status(0)= prison dose / ties=breslow;
strata clinic;
run;

*Cannont estimate piecewise exponential model with strata using BAYES statement;

*Chapter 5 Practice;

*Chapter 5 Practice question 1 Table 1;
*Full Model;
proc phreg data=ds;
model survt*status(0)= tx Large Adeno Small perf DisDur age priortx / ties=breslow;
run;

*Chapter 5 Practice question 1 Table 2;
*Reduced Model;
proc phreg data=ds;
model survt*status(0)= tx Small perf DisDur age priortx / ties=breslow;
OUTPUT out=phtestVetsReduced RESSCH=schtx schSmall schperf schDisDur schage schpriortx;
run;

*Testing PH Assumption for each predictor;
data EventsOnly;
set phtestVetsReduced;
if status=1;
run;

proc rank data=EventsOnly out=RankedVetsReduced ties=mean;
var survt;
ranks timerank;
run;

proc corr data=RankedVetsReduced NOSIMPLE;
VAR schtx schSmall schperf schDisDur schage schpriortx;
with timerank;
run;

*Approximating exponential piecewise model with BAYES statement;
proc phreg data=ds;
model survt*status(0)= tx Small perf DisDur age priortx / ties=breslow;
BAYES PIECEWISE NBI=0 NMC=1;
run;


*Chapter 5 Practice question 3;
*Stratifying on Small and perStatus Variables;
data VetsWithPerfStatus;
set ds;
IF perf>59 THEN perfStatus=1; ELSE perfStatus=0;
run;

proc phreg data=VetsWithPerfStatus;
model survt*status(0)= tx DisDur age priortx / ties=breslow;
STRATA Small perfStatus;
run;

*Chapter 5 Practice question 10;
*Lot of interactions with strata;
proc phreg data=VetsWithPerfStatus;
model survt*status(0) = tx DisDur age priortx 
						tx*Small DisDur*Small age*Small priortx*Small 
						tx*perfStatus DisDur*perfStatus age*perfStatus priortx*perfStatus 
						tx*Small*perfStatus DisDur*Small*perfStatus age*Small*perfStatus priortx*Small*perfStatus;
STRATA Small perfStatus;
run;

*Chapter 5 Test question 1;
proc phreg data=ds2;
model survt*status(0)= clinic prison dose / ties=breslow;
run;


*Approximating exponential piecewise model with BAYES statement;
proc phreg data=ds2;
model survt*status(0)= clinic prison dose / ties=breslow;
BAYES PIECEWISE NBI=0 NMC=1;
run;


*Chapter 5 Test question 2;
proc phreg data=ds2;
model survt*status(0)= prison dose / ties=breslow;
strata clinic;
run;


*Chapter 5 Test question 5;
proc phreg data=ds2;
model survt*status(0)= prison dose prison*clinic dose*clinic / ties=breslow;
strata clinic;
run;
