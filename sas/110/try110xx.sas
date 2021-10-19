proc contents data = WORK.TRY110xx;
	run;

proc format;
value melon_seeds_l 
  1="jiaotang"
  2="hongzao" 
  3="yuanwei"
  4="bumai";
run;

proc freq data = WORK.TRY110xx;
	format melon_seeds melon_seeds_l.;
	table melon_seeds;
run;

proc means data = WORK.TRY110xx;
	format melon_seeds melon_seedsl.;
	var price xx= WORK.TRY110xx;
	run;

proc logistic data = WORK.TRY110xx;
	model melon_seeds = price xx/ link = glogit;
run;
