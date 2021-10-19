proc contents data = WORK.TRY110nop;
	run;

proc format;
value melon_seeds_l 
  1="jiaotang"
  2="hongzao" 
  3="yuanwei"
  4="bumai";
run;

proc freq data = WORK.TRY110nop;
	format melon_seeds melon_seeds_l.;
	table melon_seeds;
run;

proc means data = WORK.TRY110nop;
	format melon_seeds melon_seedsl.;
	var price= WORK.TRY110nop;
	run;

proc logistic data = WORK.TRY110nop;
	model melon_seeds = price/ link = glogit;
run;
