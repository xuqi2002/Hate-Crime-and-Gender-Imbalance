*************************************************************
*Purpose: Merge context data and generate additional vars
*Stata Version: 16
*************************************************************

*-------------------------------
*generate merged_context_1.dta
*-------------------------------
use "source_data/unemployment.dta", clear

*unemployment gendergap
*----------------------
gen unemp_gendergap = unemp_men/unemp_female
label var unemp_gendergap "male unemployment rate / female unemployment rate"

rename ags_dist ags_county // "county" and "district" used as synonyms
label var ags_c " Identifier for County"
save "source_data/merged_context_1.dta",replace

*-----------------------------
*generate merged_context_2.dta
*-----------------------------

*unemployment data
*-------------------
use "source_data/unemployment.dta", clear

gen unemp_gendergap = unemp_men/unemp_female
label var unemp_gendergap "male unemployment rate / female unemployment rate"

save "source_data/temp1.dta", replace

*refugees by gender
*-------------------
use "source_data/refugee_gender.dta", clear

egen ref_male = rowtotal(all_male0_3 all_male3_6 all_male6_15 all_male15_18 all_male18_25 all_male25_30 all_male30_40 all_male40_50 all_male50_65 all_male65_75 all_male75_up)
gen pc_ref_male = ref_male*100/all_totref
label var pc_ref_male "% male refugees, of all refugees"
label var ref_male "total number of male refugees (all ages)"

save "source_data/temp2.dta", replace

*education
*----------
use "source_data/education.dta", clear

*high degree
gen pc_hidegree_all2011 = pop15_high_degree*100/pop15_total
label var pc_hidegree_all2011 "% population with university entrance exam, incl. still in school" // (census 2011)

save "source_data/temp3.dta", replace


* sector
*--------
use "source_data/sectors.dta", clear

gen pc_manufacturing = no_manufacturing/no_employed
label var pc_manufacturing "pc_manufacturing"

save "source_data/temp4.dta", replace

*merge data
*----------
/*
this is a more comprehensive dataset - start by using a master data file
that includes the ags year combinations we need
*/

use  "source_data/population.dta"

merge 1:1 ags year using "source_data/temp1.dta" // unemployment
drop _m 
merge 1:1 ags year using "source_data/temp2.dta" // refugees
drop _m
merge m:1 ags using "source_data/temp3.dta" // education in 2011 this m:1 merge
drop _m
merge 1:1 ags year using "source_data/temp4.dta" //sectors
drop _m

rename ags_dist ags_county // "county" and "district" used as synonyms
label var ags_c " Identifier for County"

save "source_data/merged_context_2.dta",replace
erase "source_data/temp1.dta"
erase "source_data/temp2.dta"
erase "source_data/temp3.dta"
erase "source_data/temp4.dta"
