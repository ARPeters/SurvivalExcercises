proc import datafile="C:\Users\APETERS4\Desktop\recid.csv"
out=recid
dbms=csv
replace;
getnames=yes;
run;

*Fitting to log normal regression model using variables fin age race wexp mar paro prio;
proc lifereg data=recid;
MODEL week*arrest(0)=fin age race wexp mar paro prio
/ DISTRIBUTION = LNORMAL;
probplot;
run;

*Fitting to exponential regression model using variables fin age race wexp mar paro prio;
proc lifereg data=recid;
MODEL week*arrest(0)=fin age race wexp mar paro prio
/ DISTRIBUTION = EXPONENTIAL;
probplot;
run;


*Fitting to weibull regression model using variables fin age race wexp mar paro prio;
proc lifereg data=recid;
MODEL week*arrest(0)=fin age race wexp mar paro prio
/ DISTRIBUTION = WEIBULL;
probplot;
run;

*Fitting to log-logistic regression model using variables fin age race wexp mar paro prio;
proc lifereg data=recid;
MODEL week*arrest(0)=fin age race wexp mar paro prio
/ DISTRIBUTION = LLOGISTIC;
probplot;
run;

*Fitting to gamma regression model using variables fin age race wexp mar paro prio;
proc lifereg data=recid;
MODEL week*arrest(0)=fin age race wexp mar paro prio
/ DISTRIBUTION = GAMMA;
run;


*Recategorizing education variable from five levels (2,3,4,5,6) into three (3,4,5);
data recid;
set recid;
if educ = 2 then educ = 3;
if educ = 6 then educ = 5;
run;

proc lifereg data=recid;
CLASS educ;
MODEL week*arrest(0)=fin age race wexp mar paro prio educ
/ DISTRIBUTION = WEIBULL;
run;

*Fitting no variables, just to get likelihood ratio test of the null that all covariates have a coefficient of zero;
proc lifereg data=recid;
MODEL week*arrest(0)= / DISTRIBUTION = WEIBULL covb;
run;

*Creating some missing data for the sake of the next demonstration;
proc sort data=recid out=recid2;
by descending arrest;
data recid1ft;
set recid2;
if _N_ le 30 then week =.;
run;

*Creating upper and lower time variables to help SAS accomodate lower and interval censored observations;
data recid3;
	set recid1ft;
	if arrest=1 AND week ne . then do;
		upper=week;
		lower=week;
	end;

	if arrest=1 and week = . then do;
		upper=52;
		lower=.;
	end;

	if arrest = 0 then do;
		upper=.;
		lower=52;
	end;
run;

*Estimating weibull model with new set of data with missingness;
proc lifereg data=recid3;
MODEL (lower, upper)=fin age race wexp mar paro prio
/ DISTRIBUTION = WEIBULL;
run;

*Recoding to allow for interval-censored data;
data recidint;
	set recid;
	if arrest=1 then do;
		upper=week;
		lower=week-.9999;
	end;
	if arrest=0 then do;
		upper=.;
		lower=52;
	end;
run;

*Estimating weibull model with set of interval-censored data;
proc lifereg data=recidint;
MODEL (lower, upper)=fin age race wexp mar paro prio
/ DISTRIBUTION = WEIBULL;
run;

*Predicting median survival times;
proc lifereg data=recid;
MODEL week*arrest(0)=fin age race wexp mar paro prio
/ DISTRIBUTION = WEIBULL;
output out=a p=median std=s;
run;

proc print data=a;
var week arrest _prob_ median s;
run;


*Pual Allison's macro for predicting probability of survival to a specific time;
%macro predict (outest=, out=_last_, xbeta=, time=);
	data _pred_;
	_p_=1;
	set &outest point=_p_;
	set &out;
	lp=&xbeta;
	t=&time;
	gamma=1/_scale_;
	alpha=exp(-lp*gamma);
	prob=0;
	_dst_=upcase(_dist_);
		if _dist_='WEIBULL' or _dist_='EXPONENTIAL' or _dist_='EXPONENT' then prob=exp(-alpha*t**gamma);
		if _dist_='LOGNORMAL' or _dist_='LNORMAL' then prob=1-probnorm((log(t)-lp)/_scale_);
		if _dist_='LLOGISTIC' or _dist_='LLOGISTC' then prob=1/(1+alpha*t**gamma);
		if _dist_='GAMMA' then do;
	d=_shape1_;
	k=1/(d*d);
	u=(t*exp(-lp))**gamma;
	prob=1-probgam(k*u**d,k);
		if d lt 0 then prob=1-prob;
	end;
	drop lp gamma alpha _dist_ _scale_ intercept _shape_ _model_ _name_ _type_ _status_ _prob_ _lnlike_ d k u;
run;

proc print data=_pred_;
run;
%mend predict;

*Predicting probability of each individual of surviving until week 30;
proc lifereg data=recid outest=a;
model week*arrest(0) = fin age race wexp mar paro prio
/ d=weibull;
output out=b xbeta=lp;
run;
%predict (outest=a, out=b, xbeta=lp, time=30);


*Pual Allison's macro for predicing hazard;
%macro lifehaz(outest=,out=,obsno=0,xbeta=lp);

*   %lifehaz(outest=name1,out=name2,xbeta=name3,obsno=1);
*   Author: Paul D. Allison, U. of Pennsylvania, allison@ssc.upenn.edu.;   
data;
  set &outest;
  call symput('time',_NAME_);
run;
proc means data=&out noprint;
  var &time &xbeta;
  output out=_c_ min(&time)=min max(&time)=max mean(&xbeta)=mean;
run;
data;
  set &outest;
  call symput('model',_dist_);
  s=_scale_;
  d=_shape1_;
  _y_=&obsno;
  set _c_ (keep=min max mean);
  if _y_=0 then m=mean;
  else do;
    set &out (keep=&xbeta) point=_y_;
    m=&xbeta;
  end;
  inc=(max-min)/300;
  g=1/s;
  alph=exp(-m*g);
  _dist_=upcase(_dist_);
if _dist_='LOGNORMAL' or _dist_='LNORMAL'  then do;
  do t=min to max by inc;
  z=(log(t)-m)/s;
  f=exp(-z*z/2)/(t*s*sqrt(2*3.14159));
  Surv=1-probnorm(z);
  h=f/Surv;
  output;
  end;
end;
else if _dist_='GAMMA' then do;
  k=1/(d*d);
  do t=min to max by inc;
  u=(t*exp(-m))**(1/s);
  f=abs(d)*(k*u**d)**k*exp(-k*u**d)/(s*gamma(k)*t);
  Surv=1-probgam(k*u**d,k);
  if d lt 0 then Surv=1-Surv;
  h=f/Surv;
  output;
  end;
end;
else if _dist_='WEIBULL' or _dist_='EXPONENTIAL' or _dist_='EXPONENT'  then do;
  do t=min to max by inc;
  h=g*alph*t**(g-1);
  output;
  end;
end;
else if _dist_='LLOGISTIC' or _dist_='LLOGISTC' then do;
  do t=min to max by inc;
  h=g*alph*t**(g-1)/(1+alph*t**g);
  output;
  end;
end;
else put 'ERROR:DISTRIBUTION NOT FITTED BY LIFEREG';
run;
proc gplot;
  plot h*t / haxis=axis2 vaxis=axis1 vzero;
  symbol1 i=join v=none c=black;
  axis1 label=(f=titalic angle=90 'Hazard');
  axis2 label=(f=titalic justify=c 'time' f=titalic justify=c "&model");
run; quit;
%mend lifehaz;
*Predicting hazard probability associated with week 30 for each individual;
proc lifereg data=recid outest=a;
model week*arrest(0) = fin age race wexp mar paro prio
/ d=weibull;
output out=b xbeta=lp;
run;
%lifehaz (outest=a, out=b, xbeta=lp);



*Breaking data up into intervals. This allows the hazard to stay constant within an interval but vary across intervals. Thus, can still use cox model;
data quarter;
	set recid;
	quarter=CEIL(week/13);
	do j=1 to quarter;
		time=13;
		event=0;
		if j=quarter and arrest=1 then do;
			event=1;
			time=week-13*(quarter-1);
		end;
	output;
end;
run;

*Runs lifereg on interval data, using exponential model;
proc lifereg data=quarter;
class j;
model time*event(0)=fin age race wexp mar paro prio j
	/ d=EXPONENTIAL covb;
run;

*Runs Bayesian analysis on  weibull model of survival;
ods html;
ods graphics on;
proc lifereg data=recid;
MODEL week*arrest(0)=fin age race wexp mar paro prio
/ DISTRIBUTION = WEIBULL;
BAYES;
run;
ods graphics off;
ods html close;
