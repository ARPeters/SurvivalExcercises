**Importing vets data;

PROC IMPORT OUT= WORK.DS 
            DATAFILE= "C:\Users\APETERS4\Documents\GitHub\SurvivalExerci
ses\Vets.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc print;
run;

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

*Fitting Reduced Cox PH Model;
proc phreg data=ds;
model survt*status(0)= tx Small perf DisDur age priortx / ties=breslow;
OUTPUT out=phtestR RESSCH=schtx  schSmall schperf schDisDur schage schpriortx;
run;

*Testing PH Assumption for each predictor;
data EventsOnlyR;
set phtestR;
if status=1;
run;

proc rank data=EventsOnlyR out=RankedR ties=mean;
var survt;
ranks timerank;
run;

proc corr data=RankedR NOSIMPLE;
VAR schtx schSmall schperf schDisDur schage schpriortx;
with timerank;
run;



*Preparing to stratify by high/low performers;
data perfHigh;
set ds;
if perf>=50 THEN perfHigh=1; ELSE perfHigh=0;
run;

proc print;
run;

*Graphing Loglog survival curves, stratified on perfoermance;

ODS GRAPHICS ON;
proc lifetest data=perfHigh plot=LLS;
TIME survt*status(0);
strata perfHigh;
run;
ODS GRAPHICS OFF;

/*
Come back to this

data perfStatus;
input perfHigh;
datalines;
1
;


ODS GRAPHICS ON;
proc phreg data=perfHigh plots(OVERLAY=ROW)=s;
model survt*status(0)= tx Large Adeno Small DisDur age priortx perfHigh / ties=breslow;
strata perfHigh;
BASELINE COVARIATES=perfStatus;
run;

ODS GRAPHICS OFF;

*/

*Importing addicts data;
PROC IMPORT OUT= WORK.DS2 
            DATAFILE= "C:\Users\APETERS4\Documents\GitHub\SurvivalExercises\Addicts.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc print;
run;

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

*Stratifying on clinic;
proc phreg data=ds2;
model survt*status(0)= prison dose / ties=breslow;
strata clinic;
run;
