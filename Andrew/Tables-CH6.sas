/*KK Chapter 6: Practice
Anderson data
*/

PROC IMPORT OUT= WORK.DS 
            DATAFILE= "C:\Users\Psychometrics\Documents\GitHub\SurvivalE
xercises\Andrew\dsAnderson.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc print;
run;

*Fitting Full Cox PH Model;
proc phreg data=ds;
model survt*status(0)= sex logWBC rx / ties=breslow;
OUTPUT out=phtest RESSCH= schSex schLogWbc schRx;
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
VAR schSex schLogWbc schRx;
with timerank;
run;

*Fitting an EXTENDED cox model;
proc phreg data=ds;
model survt*status(0)= logWBC rx Int1 Int2 / ties=breslow;
IF survt <15 THEN Int1=sex*1; Else Int1 = sex*0;
IF survt >=15 THEN Int2=sex*1; Else Int2 = sex*0;
run;

*Come back to this one;
proc phreg data=ds;
model survt*status(0)=sex logWBC rx sexSurvt/ ties=breslow;
sexSurvt=sex*survt;
run;

*Stratifying on sex variable;
proc phreg data=ds;
model survt*status(0)= logWBC rx / ties=breslow;
Strata sex;
run;


/*KK Chapter 6: Test
Chemo data
*/

PROC IMPORT OUT= WORK.DS 
            DATAFILE= "C:\Users\Psychometrics\Documents\GitHub\SurvivalExercises\Andrew\dsChemo.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc print;
run;

*Fitting Full Cox PH Model;
*Should expect some difference due to data set not being exactly what it says on the tin;
proc phreg data=ds;
model survt*status(0)= rx / ties=breslow;
OUTPUT out=phtest RESSCH= schRx ;
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
VAR schRx;
with timerank;
run;

*Fitting an EXTENDED cox model;
proc phreg data=ds;
model survt*status(0)= Int1 Int2 Int3 / ties=breslow;
IF survt <250 THEN Int1=rx*1; Else Int1 = rx*0;
IF 250<=survt<500 THEN Int2=rx*1; Else Int2 = rx*0;
IF survt>=500 THEN Int3=rx*1; Else Int3 = rx*0;
run;

proc phreg data=ds;
model survt*status(0)= Int1 Int2 / ties=breslow;
IF survt <250 THEN Int1=rx*1; Else Int1 = rx*0;
IF survt>=250 THEN Int2=rx*1; Else Int2 = rx*0;
run;


