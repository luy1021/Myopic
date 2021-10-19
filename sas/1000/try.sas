proc contents data = WORK.TRY;
	run;

proc format;
value melon_seeds_l 
  1="jiaotang"
  2="hongzao" 
  3="yuanwei"
  4="bumai";
run;

proc freq data = WORK.TRY;
	format melon_seeds melon_seeds_l.;
	table melon_seeds;
run;

proc means data = WORK.TRY;
	format melon_seeds melon_seedsl.;
	var price lnCD1= WORK.TRY;
	run;

proc logistic data = WORK.TRY;
	model melon_seeds = price lnCD1/ link = glogit;
run;
