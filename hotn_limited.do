** HEADER -----------------------------------------------------
**  DO-FILE METADATA
    //  algorithm name			    hotn_limited.do
    //  project:				    Health of the Nation
    //  analysts:				    Ian HAMBLETON
    // 	date last modified	    	21-NOV-2019
    //  algorithm task			    De-identification - HotN

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
    local logpath "X:/OneDrive - The University of the West Indies/repo_datagroup/repo_p124"

    ** Close any open log file and open a new log file
    capture log close
    log using "`logpath'\hotn_limited", replace
** HEADER -----------------------------------------------------

use "`datapath'\version01\4-curated\hotn_v41", clear

** Drop identifying features 
drop iid doi consent ist dob occ_text occ_code waddress _merge

** Drop unecessary variables 
drop pid_str rstatus ethnico pocco semp_text sempb employerb phelp  
drop occm occ_code_retired ind_text ind_code indm oil_other 
drop doctor4_other crf01_pm21 response ift v1_ok 
drop bp_arm_reason bp_taken_reason 
drop ht_taken_reason1 ht_taken_reason2 
drop wt_taken_reason1 wt_taken_reason2 
drop wc_taken_reason1 wc_taken_reason2 
drop hc_taken_reason1 hc_taken_reason2 
drop pm_comments

** Age to 5-year bands
gen age5 = . 
replace age5 = 1 if agey<30
replace age5 = 2 if agey>=30 & agey<35
replace age5 = 3 if agey>=35 & agey<40
replace age5 = 4 if agey>=40 & agey<45
replace age5 = 5 if agey>=45 & agey<50
replace age5 = 6 if agey>=50 & agey<55
replace age5 = 7 if agey>=55 & agey<60
replace age5 = 8 if agey>=60 & agey<65
replace age5 = 9 if agey>=65 & agey<70
replace age5 = 10 if agey>=70 & agey<75
replace age5 = 11 if agey>=75 & agey<80
replace age5 = 12 if agey>=80
#delimit ; 
    label define _age5  1 "<30"  
                        2 "30 to <35"
                        3 "35 to <40"
                        4 "40 to <45"
                        5 "45 to <50"
                        6 "50 to <55"
                        7 "55 to <60"
                        8 "60 to <65"
                        9 "65 to <70"
                        10 "70 to <75"
                        11 "75 to <80"
                        12 "80+";
#delimit cr 
label values age5 _age5 
replace agey = int(agey) 
drop agey 
order age5, after(sex)
label var age5 "Age in 5-year bands"

** Randomly re-number PID 
set seed 2019
gen pid_shock = runiformint(1,100)
gen pidr = pid + pid_shock 
order pidr, after(pid) 
drop pid_shock pid  
rename pidr pid 
label var pid "Unique participant ID - limited dataset version"

** Randomly re-number EDs
set seed 2019
gen ed_shock = runiformint(1,100)
bysort ed: replace ed_shock = ed_shock[1]
gen edr = ed + ed_shock 
order edr, after(ed) 
drop ed_shock ed 
rename edr ed 
label var ed "Enumeration District - limited dataset version"

** Save de-identified dataset 
label data "Health of the Nation NCD risk factor survey: limited dataset" 
save "`datapath'\version01\4-curated\hotn_v41_limited", replace
