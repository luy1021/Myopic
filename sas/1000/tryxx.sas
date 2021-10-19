proc contents data = WORK.TRYXX;
	run;

proc format;
value melon_seeds_l 
  1="jiaotang"
  2="hongzao" 
  3="yuanwei"
  4="bumai";
run;

proc freq data = WORK.TRYXX;
	format melon_seeds melon_seeds_l.;
	table melon_seeds;
run;

proc means data = WORK.TRYXX;
	format melon_seeds melon_seedsl.;
	var price xx= WORK.TRYXX;
	run;

proc logistic data = WORK.TRYXX;
	model melon_seeds = price xx/ link = glogit;
run;
