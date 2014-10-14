libname survival 'C:\Users\twilson\Documents\GitHub\SurvivalExercises\Thomas\Survival Analysis\';

data survivalVets;
	set survival.vets;
run;



proc freq data=survivalVets;
	table TX;
run;

proc lifetest data=survivalVets;
	time survt*status(0);
run;


