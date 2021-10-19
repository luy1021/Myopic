proc contents data = WORK.TRY500NOP;
	run;

proc format;
value melon_seeds_l 
  1="jiaotang"
  2="hongzao" 
  3="hetao"
  4="yuanwei"
  5="bumai";
run;

proc freq data = WORK.TRY500NOP;
	format melon_seeds melon_seeds_l.;
	table melon_seeds;
run;

proc means data = WORK.TRY500NOP;
	format melon_seeds melon_seedsl.;
	var price= WORK.TRY500NOP;
	run;

proc logistic data = WORK.TRY500NOP;
	model melon_seeds = price/ link = glogit;
run;
