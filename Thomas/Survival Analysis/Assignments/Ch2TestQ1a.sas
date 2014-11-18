libname survival 'C:\Users\twilson\Documents\GitHub\SurvivalExercises\Thomas\Survival Analysis\';

data survivalVets;
	set survival.vets
run;

proc print data=survival.survivalVets;
run;