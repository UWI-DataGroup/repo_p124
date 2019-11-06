** HEADER -----------------------------------------------------
**  DO-FILE METADATA
    //  algorithm name			    ncdrisc_obesity_001.do
    //  project:				    Food Sovereignty and Obesity in SIDS
    //  analysts:				    Ian HAMBLETON
    // 	date last modified	        04-JUN-2019
    //  algorithm task			    Preparing the Obesity datasets from NCD-RisC Collaboration

    ** General algorithm set-up
    version 15
    clear all
    macro drop _all
    set more 1
    set linesize 80

    ** Set working directories: this is for DATASET and LOGFILE import and export
    ** DATASETS to encrypted SharePoint folder
    local datapath "X:/The University of the West Indies/DataGroup - repo_data/data_p124"
    ** LOGFILES to unencrypted OneDrive folder (.gitignore set to IGNORE log files on PUSH to GitHub)
    local logpath X:/OneDrive - The University of the West Indies/repo_datagroup/repo_p124

    ** Close any open log file and open a new log file
    capture log close
    log using "`logpath'\weighted_auc_001", replace
** HEADER -----------------------------------------------------

*Load CORE dataset
** use "C:\Hotnanalysis\hotn_v4.1\hotn_v41RPAQ.dta", clear
use "`datapath'/version01/1-input/hotn_v41.dta", clear


** -------------------------------------------------------------------------------------------------------------------- 
** DATA PREPARATION
** -------------------------------------------------------------------------------------------------------------------- 

* THREE AGE GROUPS
* AGE IN 3 BANDS (25-44, 45-64, 65+)
gen age_gr2 =.
replace age_gr2= 1 if agey >=25 & agey <45
replace age_gr2= 2 if agey >=45 & agey <65
replace age_gr2= 3 if agey >=65 & agey <.
label variable age_gr2 "Age in 3 bands"
label define age_gr2 1 "25 - <45 years" 2 "45 - <65 years" 3 "65 and over years"
label values age_gr2 age_gr2
order age_gr2, after(agey)

** SEX indicators
gen female = (sex==1) if !missing(sex)
gen male = (sex==2) if !missing(sex)

** AGE indicators
gen age25 = (age_gr2==1) if !missing(age_gr2)
gen age45 = (age_gr2==2) if !missing(age_gr2)
gen age65 = (age_gr2==3) if !missing(age_gr2)
 
 
** -------------------------------------------------------------------------------------------------------------------- 
** Full survey weighting 
** -------------------------------------------------------------------------------------------------------------------- 
svyset ed [pweight=wfinal1_ad], strata(region) 




** -------------------------------------------------------------------------------------------------------------------- 
** OVERWEIGHT & OBESITY
** Table 15: Prevalence of overweight and obesity based on BMI measurement in the Barbadian population aged 25 years and over
** Table 16: Distribution of BMI in the Barbadian population aged 25 years and older
** Table 17: Prevalence of central obesity in the Barbadian population aged 25 years and over
** -------------------------------------------------------------------------------------------------------------------- 

**BMI
gen ht = height/100
gen bmi = weight/(ht*ht)
label var ht "height in m"
label var bmi "Body mass index"
**overeight
gen ow = 0
replace ow = 1 if bmi >=25 & bmi <.
replace ow =. if height ==. | weight ==.
label variable ow "overweight"
label define ow 0 "no" 1 "yes"
label values ow ow
**obese
gen ob=0
replace ob = 1 if bmi >=30 & bmi <.
replace ob =. if height ==. | weight ==.
tab ob, miss
label define ob 0 "not obese" 1 "obese"
label variable ob "obesity"
label values ob ob
**obesity categories
gen ob4 = 0
replace ob4 = 1 if bmi>=25
replace ob4 = 2 if bmi>=30
replace ob4 = 3 if bmi>=35
replace ob4 = 4 if bmi>=40
replace ob4 = . if weight==. | height==.
label variable ob4 "obesity category"
label define ob4 0 "not obese" 1 "bmi: 25-<30" 2 "bmi: 30-<35" 3 "bmi:35-<40" 4 "bmi: >40"
label values ob4 ob4

**Central obesity
**waist circumference
gen waist = (wc1+wc2)/2 if wc3==. 
replace waist =(wc1+wc2+wc3)/3 if wc3!=.
lab var waist "Mean waist circ"




** -------------------------------------------------------------------------------------------------------------------- 
** PRE-DIABETES AND DIABETES 
** Table 24: Mean fasting glucose (mmo/l) in the Barbadian population aged 25 years and over
** Table 25: Prevalence of pre-diabetes using World Health Organization Criteria (fasting plasma glucose > 6.1 mmol/l and < 7.0 mmol/l) 
** Table 26: Prevalence of pre-diabetes using American Diabetes Association Criteria (fasting plasma glucose > 5.6 mmol/l and < 7.0 mmol/l)
** Table 27: Prevalence of known diabetes by doctor diagnosis 
** Table 28: Prevalence of diabetes by doctor diagnosis and/or fasting plasma glucose > 7 mmol/l
** -------------------------------------------------------------------------------------------------------------------- 

** Several new diabetes variables produced
** diabkn		Doctor diagnosed diabetes
** diab_trt		Diagnosed diabetes by treatment status (insulin/oral meds, diabetes no meds, not diabetes)
** diabfpg		Diagnosis diabetes based of FPG
** diabkfpg		Reported as diagnosed + new diabetes based on FPG
** diabhba		Diagnosed diabetes based on HbA1C
** dbfpg_uk		Undiagnosed diabetes based on FPG
** dbhba_uk 		Undiagnosed diabetes based on HbA1C
** dball_uk		Undiagnosed diabetes based on FPG OR HbA1C
** PRE-DIABETES
** glyfpg_who	Pre-diabetes. WHO criteria. FPG (normal <=6.0, pre  6.0 to 6.9, diabetes  7.0 and higher)
** glyhba_who	Pre-diabetes. WHO criteria. HbA1C (normal <=5.9, pre  6.0 to 6.4, diabetes  6.5 and higher)
** glyall_who		Pre-diabetes. WHO criteria. FPG or HbA1C 
** glyfpg_ada	Pre-diabetes. ADA criteria. FPG (normal <=5.5, pre  5.6 to 6.9, diabetes  7.0 and higher)
** glyhba_ada	Pre-diabetes. ADA criteria. HbA1C (normal <=5.6, pre  5.7 to 6.4, diabetes  6.5 and higher)
** glyall_ada		Pre-diabetes. ADA criteria. FPG or HbA1C 
** HBA1C CONTROL
** hba7 			HbA1C < 7
** hba8 			HbA1C < 8
** hba10 		HbA1C < 10

** DIABETES - previously doctor diagnosed
gen diabkn=.
replace diabkn=1 if diab==1 & diabp!=1
replace diabkn=2 if diabkn!=1 & diab!=.
tab diabkn

** DIABETES - reported diagnosis
gen diab_trt =.
replace diab_trt=1 if diabi==1 | diabpill==1
replace diab_trt=2 if diab_trt!=1 & diabkn==1
replace diab_trt=3 if diab_trt!=1 & diab_trt!=2
lab val diab_trt diab_trt
tab diab_trt
lab var diab_trt "Diagnosed diabetes by trtment status"
lab def diab_trt 1 "ins/oha" 2 "KDM no meds" 3 "Not KDM"

** DIABETES based on fasting glucose
recode fplas (1/6.9 =  0) (7.0/23 = 1), gen(diabfpg)
lab var diabfpg "Diabetes based on fasting glucose"
lab def diabyn 0 "not diabetes" 1 "diabetes"
lab val diabfpg diabyn

** Diabetes - reported diagnosis plus newly diagnosed on fplas
gen diabkfpg =.
replace diabkfpg = 1 if diabkn==1 | diabfpg==1
replace diabkfpg = 0 if diabkn==2 & diabfpg==0
lab val diabkfpg diabyn
tab diabkfpg
lab var diabkfpg "Total diabetes based on known plus fasting glucose"

** DIABETES based on HBA1c
recode hba1c (1/6.4=0) (6.5/14=2), gen(diabhba)
lab var diabhba "Diabetes based on HBA1c"
lab val diabhba diabyn

** PREDIABETES WHO criteria - fpg 
recode fplas (1/6.0=1) (6.1/6.9=2) (7.0/23=3), gen(glyfpg_who)
replace glyfpg_who=3 if diabkn==1
replace glyfpg_who=1 if diabp==1
lab val glyfpg_who gly
lab var glyfpg_who "WHO categories of hyperglycaemia based on FPG"
lab def gly 1 "normal" 2 "'predm'" 3 "diabetes"

** PREDIABETES WHO criteria - HBA1c 
recode hba1c (1/5.9=1) (6.0/6.4=2) (6.5/14=3), gen(glyhba_who)
replace glyhba_who=3 if diabkn==1
replace glyhba_who=1 if diabp==1
lab val glyhba_who gly
lab var glyhba_who "WHO categories of hyperglycaemia based on HBA1c"

** PREDIABETES - WHO on either FPG or HBA1c
gen glyall_who=.
replace glyall_who = 3 if glyfpg_who==3 | glyhba_who==3
replace glyall_who = 2 if (glyfpg_who==2 | glyhba_who==2) & glyall_who!=3
replace glyall_who = 1 if (glyfpg_who==1 | glyhba_who==1) & (glyall_who!=3 & glyall_who!=2)
lab val glyall_who gly
lab var glyall_who "WHO categories of hyperglycaemia based on HBA1c/FPG"

** PREDIABETES ADA criteria - fpg 
recode fplas (1/5.5=1) (5.6/6.9=2) (7.0/23=3), gen(glyfpg_ada)
replace glyfpg_ada=3 if diabkn==1
replace glyfpg_ada=1 if diabp==1
lab val glyfpg_ada gly
lab var glyfpg_ada "ADA categories of hyperglycaemia based on FPG"

** PREDIABETES ADA criteria - HBA1c 
recode hba1c (1/5.6=1) (5.7/6.4=2) (6.5/14=3), gen(glyhba_ada)
replace glyhba_ada=3 if diabkn==1
replace glyhba_ada=1 if diabp==1
lab val glyhba_ada gly 
lab var glyhba_ada "ADA categories of hyperglycaemia based on HBA1c"

** PREDIABETES - ADA on either FPG or HBA1c
gen glyall_ada=.
replace glyall_ada = 3 if glyfpg_ada==3 | glyhba_ada==3
replace glyall_ada = 2 if (glyfpg_ada==2 | glyhba_ada==2) & glyall_ada!=3
replace glyall_ada = 1 if (glyfpg_ada==1 | glyhba_ada==1) & (glyall_ada!=3 & glyall_ada!=2)
lab val glyall_ada gly
lab var glyall_ada "ADA categories of hyperglycaemia based on HBA1c/FPG"

*UNDIAGNOSED diabetes
gen dbfpg_uk =.
replace dbfpg_uk =1 if diabkn!=1 & glyfpg_who==3
replace dbfpg_uk = 2 if dbfpg_uk !=1 & glyfpg_who!=.
lab val dbfpg_uk dbfpg_uk
lab var dbfpg_uk "Unknown DM based on FPG"
lab def dbfpg_uk 1 "UK DM" 2 "Other"

gen dbhba_uk =.
replace dbhba_uk =1 if diabkn!=1 & glyhba_who==3
replace dbhba_uk = 2 if dbhba_uk !=1 & glyhba_who!=.
lab val dbhba_uk dbhba_uk
lab var dbhba_uk "Unknown DM based on HBA"
lab def dbhba_uk 1 "UK DM" 2 "Other"

gen dball_uk=.
replace dball_uk =1 if dbhba_uk==1 | dbfpg_uk==1
replace dball_uk =2 if dball_uk!=1 & dbhba_uk!=. & dbfpg_uk!=.
lab val dball_uk dbhba_uk
tab dball_uk
lab var dball_uk "Unknown DM based on HBA or FPG"

*HBA1c control, less than 7
recode hba1c (1/6.9 = 1) (7.0/14=0), gen(hba7)
lab var hba7 "hba1c less than 7"
lab def hba7 1 "<7" 0 ">=7"
lab val hba7 hba7

*HBA1c control, less than 8
recode hba1c (1/7.9 = 1) (8.0/14=0), gen(hba8)
lab var hba8 "hba1c less than 8"
lab def hba8 1 "<8" 0 ">=8"
lab val hba8 hba8 

*HBA1c control, less than 10
recode hba1c (1/9.9 = 1) (10.0/14=0), gen(hba10)
lab var hba10 "hba1c less than 10"
lab def hba10 1 "<10" 0 ">=10"
lab val hba10 hba10 



**-----------------------------------------------------------------------
** (01) 
** TABULATION of DIABETES by AGE/SEX
** CONFIRMING DIABETES DEFINITION: COMPARING TO MOH REPORT TABLE
** Table 28: Prevalence of diabetes by doctor diagnosis and/or fasting plasma glucose > 7 mmol/l
**-----------------------------------------------------------------------
svy, subpop(age25): tab diabkfpg sex , percent ci col
svy, subpop(age45): tab diabkfpg sex , percent ci col
svy, subpop(age65): tab diabkfpg sex , percent ci col
svy: tab diabkfpg sex , percent ci col




**-----------------------------------------------------------------------
** (02) 
** EXAMPLE UNWEIGHTED SENS / SPEC / AUC 
** Using the example of: DIABETES OUTCOME for two BMI cutpoints (>=25 kg/m2, >=30 kg/m2)
**-----------------------------------------------------------------------
** Calculate BMI indicators for cutpoints between 25 and 33
forval x = 20(1)35 {
	gen bmi`x'=0
	replace bmi`x' = 1 if bmi>=`x'
	}
** Unweighted sensitivity and specificity (BMI>=25, and BMI>=30)
roctab diabkfpg bmi25 , detail table  summary 
roctab diabkfpg bmi30 , detail table  summary


**-----------------------------------------------------------------------
** (03)
** EXAMPLE WEIGHTED SENS / SPEC
** Using the example of: DIABETES OUTCOME for two BMI cutpoints (>=25 kg/m2, >=30 kg/m2)
** 
** analysis note
** The problem we have here is that ROC commands in Stata only allow frequency weights (fweights - which need to be integers)
** We ideally want to apply sampling weights (pweights - which can be fractional)
** We use different commands to apply -pweights-
**-----------------------------------------------------------------------

** EXAMPLE for BMI>=30
** Weighted logistic regression
** Save the probability of a positive outcome into variable "phat" 
svyset ed [pweight=wfinal1_ad], strata(region) 
quietly svy: logistic diabkfpg bmi30 if sex==1
predict phat

** WEIGHTED SENS / SPEC
senspec diabkfpg phat [pweight=wfinal1_ad]  if sex==1, sensitivity(sens) specificity(spec)
replace sens = sens*100
replace spec = spec*100
table bmi30 if bmi30==1, c(mean sens mean spec) format(%9.1f)



**-----------------------------------------------------------------------
** (04)
** EXAMPLE WEIGHTED AUC
** Apply SomersD statistic
** SomersD is a measure of rank association between two variables
** When the outcome is binary, SomersD becomes AUC as a special case. 
** The SomersD command is therefore useful as it allows "pweights" rather than just
** "fweights" allowed in the Stata ROC commands (see above analysis note)  
**-----------------------------------------------------------------------
somersd diabkfpg phat [pweight=wfinal1_ad] if sex==1, transf(roc) cluster(ed) cimatrix(cil)
matrix b = e(b)
local auc = b[1,1] * 100
local auc_l`x' = cil[1,2]*100
local auc_u`x' = cil[1,3]*100
di   "Area under the Curve: " %4.1f `auc`x'' "  (95% CI " %4.1f `auc_l`x'' ", " %4.1f `auc_u`x'' ")"


**-----------------------------------------------------------------------
** (05)
** AUTOMATED WEIGHTED SENS / SPEC  
** Now automation creates a larger table with sens / spec for a range of BMI cutpoints
** Would repeat for different endpoints and for WC instead of BMI
**-----------------------------------------------------------------------
tempfile diab_sens
postfile diabse sex tot1 tot2 bmi sens spec using `diab_sens'
forval s = 1(1)2 {
	local bmi = 20
	forval x = 20(1)35 {
		qui {
		count if diabkfpg<. & sex==`s'
		local num1 = r(N)
		count if diabkfpg<. & sex==`s' & bmi`x'==1
		local num2 = r(N)		
		senspec diabkfpg bmi`x' [pweight=wfinal1_ad] if sex==`s', sensitivity(se`x'_`s') specificity(sp`x'_`s')
		replace se`x'_`s' = . if se`x'_`s'==1
		replace sp`x'_`s' = . if sp`x'_`s'==0 
		egen sens`x'_`s' = min(se`x'_`s') 
		egen spec`x'_`s' = min(sp`x'_`s') 
		replace sens`x'_`s' = sens`x'_`s'*100 
		replace spec`x'_`s' = spec`x'_`s'*100
		post diabse (`s') (`num1') (`num2') (`bmi') (sens`x'_`s') (spec`x'_`s')
		local bmi = `bmi' + 1
		noi dis "." _cont	
			}
		}		
	}
postclose diabse
** Tabulation of weighted Sensitivity / Specificity for increasing BMI cutpoints (Outcome: diabkfpg)
preserve
	use `diab_sens', clear
	table bmi,  by(sex) c(mean tot1 mean tot2 mean sens`x' mean spec`x') format(%9.1f)
restore


**-----------------------------------------------------------------------
** (06)
** AUTOMATED WEIGHTED AUC
** Now automation creates a larger table with AUC (95% CI) for a range of BMI cutpoints
** Would repeat for different endpoints and for WC instead of BMI
**-----------------------------------------------------------------------

** Set up temporary file to store Area Under Curve statistics
tempfile diab_auc
** Post results from looping to the temporary file
postfile diabauc sex tot1 tot2 bmi auc aucl aucu using `diab_auc'
** Estimate separately for women and men
forval s = 1(1)2 {
	** Initialize BMI at 20 kg/m2
	local bmi = 20
	** Loop BMI in increments of 1 between 20 and 35
	forval x = 20(1)35 {
	qui {
		** Count number of cases (num1) by sex
		count if diabkfpg<. & sex==`s'
		local num1 = r(N)
		** Count number of cases (num2) by sex and BMI category
		count if diabkfpg<. & sex==`s' & bmi`x'==1
		local num2 = r(N)
		** SomersD is the core of this algorithm
		** Somers’ D is a measure of association between two variables, 
		** and plays a central role as a parameter behind rank or nonparametric statistical methods.
		** It allows survey weights - allowing us more flexibility than the built-in Stata ROC commands
		** SomersD has many extensions, and we specify "auc" which is a synonym for Harrel's C statistic
		** If the first variable of varlist is a binary indicator of a disease and the other variable(s)
		** is/are quantitative predictors for that disease, then Harrell’s c is the area under the ROC curve	
		somersd diabkfpg bmi`x' [pweight=wfinal1_ad] if sex==`s', transf(roc) cluster(ed) cimatrix(cil`x')
		** Extract matrix elements and post these elements to the temporary Stata file
		matrix b`x' = e(b)
		local auc`x' = b`x'[1,1] * 100
		local auc_l`x' = cil`x'[1,2]*100
		local auc_u`x' = cil`x'[1,3]*100
		di   "Area under the Curve: " %4.1f `auc`x'' "  (95% CI " %4.1f `auc_l`x'' ", " %4.1f `auc_u`x'' ")"
		post diabauc (`s') (`num1') (`num2') (`bmi') (`auc`x'') (`auc_l`x'') (`auc_u`x'')
		** Increase local BMI macro by 1 unit ready for next iteration
		local bmi = `bmi' + 1
		** Display "Stata working" dots
		noi dis "." _cont	
		}
		}
	}
** Close the temporary AUC file, then load the file to memory and tabulate results
postclose diabauc
preserve
	use `diab_auc', clear
	table bmi,  by(sex) c(mean tot1 mean tot2 mean auc mean aucl mean aucu) format(%9.1f)

**-----------------------------------------------------------------------
** (07)
** Visual comparison of AUC curves (with 95% CIs)
** Investigating difference by looking at CI overlap
**-----------------------------------------------------------------------
#delimit ;
	gr twoway 
		/// Coverage by wealth
		(rspike aucl aucu bmi if sex==1, hor lc(gs6) lw(0.25))
		(sc bmi auc if sex==1,  msize(4) m(o) mlc(gs0) mfc("253 174 97") mlw(0.1))
		,
			graphregion(color(gs16)) ysize(5)

			xlab(40(5)70, labs(4) nogrid glc(gs16))
			xscale(fill ) 
			xtitle("AUC (%) with 95% CI", size(4) margin(l=2 r=2 t=5 b=2)) 
			xmtick(40(1)70)
			
			ylab(,
			labs(4) tstyle(major_notick) nogrid glc(gs16) angle(0) format(%9.0f))
			yscale( lw(vthin) reverse range(20(1)36)) 
			ytitle("BMI cutpoint", size(4) margin(l=2 r=5 t=2 b=2)) 
			ymtick(20(1)35)
			legend(off)
			;
#delimit cr

restore


**-----------------------------------------------------------------------
** (08)
** Formal comparison of two AUC curves
**-----------------------------------------------------------------------
somersd diabkfpg bmi20-bmi35 [pweight=wfinal1_ad] if sex==1, transf(roc) cluster(ed) cimatrix(cid)
** Eg. comparing bmi25 with bmi30
lincom bmi25 - bmi30

	