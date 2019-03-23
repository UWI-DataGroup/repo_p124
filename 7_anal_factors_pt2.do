** analysis of nutribase database for factor analysis/dietary pattern (DP) comparison 
** This first version08 created by Angie Rose 17-mar-2019 

** stata version control
version  13

**Initialising the STATA log and allow auto page scrolling
capture  {
	program drop _all
    drop _all
	log close
	}
	

**Begin a STATA logfile
log using "logfiles\logfiles 2_anal_factors.smcl", replace

**Auto page scroll 
set more off

use "data\factors_v13.dta" ,clear
tab foodcat ,m

** This dofile is to perform number (2) below:
********************************************************************************
** 1. First we have to create the new food categories in this db: each cat=var
**    The food cats are based on the table created by RH in the draft of the paper (v1 ppr 3)
** 2. Then we will have to work out total g per day of each cat per participant
** 3. Then estimate the mean daily intake in g for each cat with 95% CI

********************************************************************************

********************************************************************************
** STEP 1: Calculate mean g per day for each cat 
********************************************************************************
drop if cat_fact==.
collapse(sum) grams , by(subject pid cat_fact)
collapse(mean) grams , by(pid cat_fact) // gives mean daily rate per pt per cat

// ameans grams if cat_fact==1

tab cat_fact ,m

forvalues num=1(1)30 {

tab cat_fact if cat_fact==`num'
ameans grams if cat_fact==`num'
}
 
