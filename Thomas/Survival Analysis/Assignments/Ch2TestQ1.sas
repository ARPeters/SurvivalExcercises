libname survival 'C:\Users\twilson\Documents\GitHub\SurvivalExercises\Thomas\Survival Analysis\';

data survivalVets;
	set survival.vets;
run;



proc freq data=survivalVets;
	table TX;
run;

proc lifetest data=survivalVets plots=s(test);
	time survt*status(0);
	strata ct1;
run;


