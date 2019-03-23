** analysis of nutribase database for factor analysis/dietary pattern (DP) comparison 
** This first version08 created by Angie Rose & Rachel Harris 16-mar-2019 

** stata version control
version  13

**Initialising the STATA log and allow auto page scrolling
capture  {
	program drop _all
    drop _all
	log close
	}
	

**Direct STATA to your dofile folder
//cd "C:\Statistics\analysis\Salt\nutribase\analysis\versions\version05\"

**Begin a STATA logfile
log using "logfiles\logfiles 1_anal_factors.smcl", replace

**Auto page scroll 
set more off

use "data\prep_salt_2.dta" ,clear
tab foodcat ,m

********************************************************************************
** 1. First we have to create the new food categories in this db: each cat=var
**    The food cats are based on the table created by RH in the draft of the paper (v1 ppr 3)
** 2. Then we will have to work out total g per day of each cat per participant
** 3. Then estimate the mean daily intake in g for each cat with 95% CI

********************************************************************************

********************************************************************************
** STEP 1: Create the new food categories
********************************************************************************

********************************************************************************
** poultry: to be split into fried chicken vs all other poultry
********************************************************************************
sort foodname
list foodname if foodcat==1 // NA

gen cat_fact=2 if (foodcat==1 & regexm(foodname, "FRIED CHI")) 
tab cat_fact ,m

** check it worked
tab foodname if cat_fact==2 ,m
replace cat_fact=1 if (foodcat==1 & cat_fact!=2)
tab cat_fact ,m		
							  
tab foodcat ,m

********************************************************************************
** dairy: to split into high- and low-fat dairy
********************************************************************************
tab foodname if foodcat==2 ,m

replace cat_fact=3 if foodcat==2 & (regexm(foodname, "BLENDED") | regexm(foodname, "CHEDDAR") | ///
					   regexm(foodname, "WHOLE") | regexm(foodname, "ENSURE") | ///
					   regexm(foodname, "EVAPORATED M") | regexm(foodname, "FLAVOUR") | /// 
					   regexm(foodname, "ICE CRE") | regexm(foodname, "CONDENSED") | ///
					   regexm(foodname, "GOAT"))
					   
replace cat_fact=4 if foodcat==2 & (regexm(foodname, "LESS FAT") | regexm(foodname, "NONFAT") | ///
					   regexm(foodname, "SKIM") | regexm(foodname, "SWISS") | ///
					   regexm(foodname, "LOWFAT"))

** check it worked
tab foodname if cat_fact==3 ,m
tab foodname if cat_fact==4 ,m
tab cat_fact ,m

********************************************************************************
** bread: to split into wholegrains vs refined grains
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==3 ,m

replace cat_fact=5 if foodcat==3 & (regexm(foodname, "BAKES") | regexm(foodname, "BISCUIT") | ///
					   regexm(foodname, "CROISS") | regexm(foodname, "CROUT") | ///
					   regexm(foodname, "DUMPLIN") | regexm(foodname, "PLAIN") | /// 
					   regexm(foodname, "WRAP") | regexm(foodname, "PANCA") | ///
					   regexm(foodname, "PITA") | regexm(foodname, "PUMPK") | ///
					   regexm(foodname, "ROTI") | regexm(foodname, "TURK") | ///
					   regexm(foodname, "WHITE") | regexm(foodname, "WONTON"))
					   
replace cat_fact=6 if foodcat==3 & (regexm(foodname, "WHEAT") | regexm(foodname, "GRAIN"))
	
** check it worked
tab foodname if cat_fact==5 ,m
tab foodname if cat_fact==6 ,m
tab cat_fact ,m


********************************************************************************
** ground provisions: take out french fries and wedges
** as well as coucou which goes to cat_fact=6
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==4 ,m

replace cat_fact=7 if foodcat==4 & (regexm(foodname, "FRENCH") | regexm(foodname, "HASH"))	
replace cat_fact=6 if foodcat==4 & (regexm(foodname, "COUCOU"))	
	   
replace cat_fact=8 if foodcat==4 & (cat_fact!=7 & cat_fact!=6) 

** check it worked
tab foodname if cat_fact==7 ,m
tab foodname if cat_fact==8 ,m
tab cat_fact ,m


********************************************************************************
** noodles and pasta: separate into whole vs refined as above for bread
** except for macaroni pie which gets its own category
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==5 ,m // I think they all become refined!!

replace cat_fact=9 if foodcat==5 & (regexm(foodname, "MACARONI PIE") | regexm (foodname, "MACARONI &"))
replace cat_fact=5 if foodcat==5 & cat_fact!=9
	
** check it worked
tab foodname if cat_fact==5 ,m
tab foodname if cat_fact==9 ,m
tab cat_fact ,m


********************************************************************************
** rice: separate into whole vs refined as above for bread
** except for rice and peas which gets its own category
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==6 ,m 

replace cat_fact=10 if foodcat==6 & (regexm(foodname, "RICE AND") | regexm(foodname, "PILAU"))
replace cat_fact=6 if foodcat==6 & (regexm(foodname, "BROWN"))
replace cat_fact=5 if foodcat==6 & (cat_fact!=10 & cat_fact!=6)
	
** check it worked
tab foodname if cat_fact==5 ,m
tab foodname if cat_fact==6 ,m
tab foodname if cat_fact==9 ,m
tab cat_fact ,m


********************************************************************************
** crackers: separate into whole vs refined as above for bread
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==7 ,m 

replace cat_fact=6 if foodcat==7 & (regexm(foodname, "RYE") | regexm(foodname, "WHEAT"))
replace cat_fact=5 if foodcat==7 & (cat_fact!=6)
					   
** check it worked
tab foodname if cat_fact==5 ,m
tab foodname if cat_fact==6 ,m
tab cat_fact ,m

********************************************************************************
** cereals: separate into whole vs refined as above for bread
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==8 ,m 

replace cat_fact=5 if foodcat==8 & (regexm(foodname, "APPLE") | regexm(foodname, "KRISPIES") | ///
					  regexm(foodname, "BUNS") | regexm(foodname, "SPECIAL") | ///
					  regexm(foodname, "FROOT") | regexm(foodname, "FROSTED") | ///
					  regexm(foodname, "CHEERIO"))

replace cat_fact=6 if foodcat==8 & (cat_fact!=5)
	
** check it worked
tab foodname if cat_fact==5 ,m
tab foodname if cat_fact==6 ,m
tab cat_fact ,m

********************************************************************************
** vegetables: leave as is! but pull out corn to whole grains
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==9 ,m 

replace cat_fact=6 if foodcat==9 & (regexm(foodname, "CORN"))

replace cat_fact=11 if foodcat==9 & (cat_fact!=6)
					   
** check it worked
tab foodname if cat_fact==5 ,m
tab foodname if cat_fact==6 ,m
tab foodname if cat_fact==11 ,m
tab cat_fact ,m

********************************************************************************
** fruit: leave as is! 
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==10 ,m 

replace cat_fact=12 if foodcat==10 

** check it worked
tab foodname if cat_fact==12 ,m
tab cat_fact ,m

********************************************************************************
** all fish: leave as is! but pull out fried fish, fish cakes and fish sticks
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==11 ,m 

replace cat_fact=13 if foodcat==11 & (regexm(foodname, "FRIED") | regexm(foodname, "CAKE") | ///
						regexm(foodname, "STICK"))

replace cat_fact=14 if foodcat==11 & (cat_fact!=13)
		
** check it worked
tab foodname if cat_fact==13 ,m
tab foodname if cat_fact==14 ,m
tab cat_fact ,

********************************************************************************
** all fish: leave as is! but pull out fried fish, fish cakes and fish sticks
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==11 ,m 

replace cat_fact=13 if foodcat==11 & (regexm(foodname, "FRIED") | regexm(foodname, "CAKE") | ///
						regexm(foodname, "STICK"))

replace cat_fact=14 if foodcat==11 & (cat_fact!=13)
	
** check it worked
tab foodname if cat_fact==13 ,m
tab foodname if cat_fact==14 ,m
tab cat_fact ,m

********************************************************************************
** red meat: pull out processed meats (done already just check it)
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==12 ,m 
tab foodname if foodcat==14 ,m 

replace cat_fact=15 if foodcat==12 
replace cat_fact=16 if foodcat==14 // now add pizza to processed meats! 
replace cat_fact=16 if foodcat==27
	 
** check it worked
tab foodname if cat_fact==15 ,m
tab foodname if cat_fact==16 ,m
tab cat_fact ,m

********************************************************************************
** eggs is eggs
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==13 ,m 

replace cat_fact=17 if foodcat==13 
			   
** check it worked
tab foodname if cat_fact==17 ,m
tab cat_fact ,m


********************************************************************************
** cake is cake but add cookies and sweetbreads
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==15 ,m 
tab foodname if foodcat==16 ,m 

replace cat_fact=18 if (foodcat==15 | foodcat==16)
	   
** check it worked
tab foodname if cat_fact==18 ,m
tab cat_fact ,m


********************************************************************************
** snacks
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==17 ,m 

replace cat_fact=19 if foodcat==17 
			
** check it worked
tab foodname if cat_fact==19 ,m
tab cat_fact ,m


********************************************************************************
** nuts & legumes: separate into two groups
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==18 ,m 

replace cat_fact=20 if foodcat==18 & (regexm(foodname, "ALMOND") | regexm(foodname, "BUSH") | ///
									regexm(foodname, "CASHEW") | regexm(foodname, "WALNUT") | ///
									regexm(foodname, "SEED") ) 
replace cat_fact=20 if foodcat==38 

replace cat_fact=21 if foodcat==18 &  cat_fact!=20
	 						   		   
** check it worked
tab foodname if cat_fact==20 ,m
tab foodname if cat_fact==21 ,m
tab cat_fact ,m


********************************************************************************
** butter, marge and mayo: put into one group ADD OILS
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==19 ,m 
tab foodname if foodcat==20 ,m 
tab foodname if foodcat==34 ,m 
tab foodname if foodcat==21 ,m 

replace cat_fact=22 if foodcat==19 
replace cat_fact=22 if foodcat==20 
replace cat_fact=22 if foodcat==21 
replace cat_fact=22 if foodcat==34 & (regexm(foodname, "MAYO") | regexm(foodname, "CAES") | ///
									regexm(foodname, "TARTA")) 
replace cat_fact=23 if foodcat==34 &  cat_fact!=22
	 						   
		   
** check it worked
tab foodname if cat_fact==22 ,m
tab foodname if cat_fact==23 ,m
tab cat_fact ,m

********************************************************************************
** soy products
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==22 ,m 

replace cat_fact=24 if foodcat==22 
				
** check it worked
tab foodname if cat_fact==24 ,m
tab cat_fact ,m


********************************************************************************
** candy, sweets & chocolate
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==23 ,m 

replace cat_fact=25 if foodcat==23 

** check it worked
tab foodname if cat_fact==25 ,m
tab cat_fact ,m

********************************************************************************
** candy, sweets & chocolate
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==25 ,m 

replace cat_fact=26 if foodcat==25 
replace cat_fact=26 if foodcat==26 & regexm(foodname, "GELATI") 

** check it worked
tab foodname if cat_fact==26 ,m
tab cat_fact ,m

********************************************************************************
** sugar-sweetened beverages 
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==32 ,m 

replace cat_fact=28 if foodcat==32 | (foodcat==28 & regexm(foodname, "LATE DRINK"))
replace cat_fact=28 if cat_fact==3 & (regexm(foodname, "FLAV"))
			
** check it worked
tab foodname if cat_fact==28 ,m
tab cat_fact ,m

********************************************************************************
** beverages with no added sugar
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==30 ,m 
tab foodname if foodcat==28 ,m 

replace cat_fact=27 if foodcat==30 | (foodcat==28 & cat_fact!=28)
		
label values cat_fact factor_lab

					
** check it worked
tab foodname if cat_fact==27 ,m
tab cat_fact ,m


********************************************************************************
** alcohol
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==31 ,m 

replace cat_fact=29 if foodcat==31 
	
** check it worked
tab foodname if cat_fact==29 ,m
tab cat_fact ,m


********************************************************************************
** hydrogenated fats - find them!
********************************************************************************
tab foodcat ,m
tab foodname if foodcat==26 ,m 

replace cat_fact=30 if foodcat==26 & regexm(foodname, "CREAMER") 

label define factor_lab 1 "poultry" 2 "fried chicken" 3 "dairy, high-fat" 4 "dairy, low-fat" ///
						5 "refined grains" 6 "whole grains" 7 "French fries" ///
						8 "ground provisions" 9 "macaroni pie" 10 "rice and peas" ///
						11 "vegetables" 12 "fruit" 13 "fried fish" 14 "fish (not fried)" ///
						15 "red meat" 16 "processed meat" 17 "eggs" 18 "cake and sweetbread" ///
						19 "snacks" 20 "nuts" 21 "legumes" 22 "butter, oils, marge & mayo" ///
						23 "condiments, sauces, dressings" 24 "soy products" ///
						25 "candy, sweets & chocolates" 26 "sugar" 27 "beverages with no added sugar"  ///
						28 "SSB" 29 "alcohol" 30 "hydrogenated fats"
						
label values cat_fact factor_lab
			
** check it worked
tab foodname if cat_fact==30 ,m
tab cat_fact ,m


********************************************************************************
** save the factors analysis dataset
save "data\factors_v13.dta" ,replace
saveold "data\factors_v12.dta" ,replace
 