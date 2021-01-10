* Creating variables
* Author: Nozomi Nakajima
* Date: Dec 2020


drop _all
clear all
set mem 30g
set maxvar 11000
set matsize 11000
set more off

tempfile a b 

*-----------------------------------------------------------------------------*
use "$data/panel_student_long.dta", clear
*------------------------------------------------------------------------------*


*------------------------------------------------------------------------------*
* Clean variables
*------------------------------------------------------------------------------*

* Gender

gen female = 1 - ap02
la var female "Child is female (1=Yes)"
drop ap02

* Student perception of teaching practices *

gen ap27h_flip = 1-ap27h
	
polychoricpca ap27g ap27j ap27h_flip ap27i, score(gen_teach) nscore(1)
	drop _*
	
  summ gen_teach if year==2008 & exp_1==0
	local mean_gen_teach = r(mean)
	local sd_gen_teach = r(sd)
	gen class_teach = (gen_teach - `mean_gen_teach') / `sd_gen_teach' if !missing(exp_1)

  summ gen_teach if year==2010 & exp_2==0
	local mean_gen_teach = r(mean)
	local sd_gen_teach = r(sd)
	replace class_teach = (gen_teach - `mean_gen_teach') / `sd_gen_teach' if !missing(exp_2)
	drop gen_teach	
	

la var class_teach "Teaching index (S.D.)"

drop ap27g ap27j ap27h ap27h_flip ap27i
	
	
* Wealth variable * 
*------------------------------------------------------------------------------*

polychoricpca ap61b ap61c ap61d ap61e ap61f ap61h ap61i ap61l, score(gen_wealth) nscore(1)
drop _*

	summ gen_wealth if year==2007 & exp_1==0
		local mean_gen_wealth = r(mean)
		local sd_gen_wealth = r(sd)
		gen zwealth1 = (gen_wealth - `mean_gen_wealth') / `sd_gen_wealth' if !missing(exp_1) 
		
	summ gen_wealth if year==2009 & exp_2==0
		local mean_gen_wealth = r(mean)
		local sd_gen_wealth = r(sd)
		gen zwealth2 = (gen_wealth - `mean_gen_wealth') / `sd_gen_wealth' if !missing(exp_2) 		

	gen zwealth = zwealth1 if !missing(exp_1)
	replace zwealth = zwealth2 if !missing(exp_2)
	label var zwealth "Household wealth index (S.D.)"

summ zwealth gen_wealth ap61* if year == 2007 & exp_1 == 0
summ zwealth gen_wealth ap61* if year == 2009 & exp_2 == 0

drop zwealth1 zwealth2 gen_wealth
drop ap61b ap61c ap61d ap61e ap61f ap61h ap61i ap61l


* Maternal education *
*------------------------------------------------------------------------------*

tab ap51

gen mothered = 0
replace mothered = 1 if ap51 < 5 
lab var mothered "Mother completed primary education (1=Yes)"


* Parental response at home *
*------------------------------------------------------------------------------*

summ ap53a ap53b 


	

// Standardize ENLACE scores using baseline AGE group as reference
*------------------------------------------------------------------------------*

gen punt_esp_STD = .
la var punt_esp_STD "Spanish score (SD)"

gen punt_mat_STD = .
la var punt_mat_STD "Math score (SD)"


// Spanish

foreach g in 3 4 5 {

summ punt_esp if year == 2007 & exp_1 == 0 & grado == `g'
	local mean = r(mean)
	local sd = r(sd)

gen punt_esp`g'_STD1 = (punt_esp - `mean') / `sd' if !missing(exp_1) & grado == `g'

	}

replace punt_esp_STD = punt_esp3_STD1 if !missing(exp_1) & grado == 3
replace punt_esp_STD = punt_esp4_STD1 if !missing(exp_1) & grado == 4 
replace punt_esp_STD = punt_esp5_STD1 if !missing(exp_1) & grado == 5 

foreach g in 3 4 5 {

summ punt_esp if year == 2009 & exp_2 == 0 & grado == `g'
	local mean = r(mean)
	local sd = r(sd)

gen punt_esp`g'_STD2 = (punt_esp - `mean') / `sd' if !missing(exp_2) & grado == `g'

	}

replace punt_esp_STD = punt_esp3_STD2 if !missing(exp_2) & grado == 3 
replace punt_esp_STD = punt_esp4_STD2 if !missing(exp_2) & grado == 4  
replace punt_esp_STD = punt_esp5_STD2 if !missing(exp_2) & grado == 5  


// Math

foreach g in 3 4 5 {

summ punt_mat if year == 2007 & exp_1 == 0 & grado == `g'
	local mean = r(mean) 
	local sd = r(sd)

gen punt_mat`g'_STD1 = (punt_mat-`mean') / `sd' if !missing(exp_1) & grado == `g'

}
replace punt_mat_STD = punt_mat3_STD1 if !missing(exp_1) & grado == 3 
replace punt_mat_STD = punt_mat4_STD1 if !missing(exp_1) & grado == 4
replace punt_mat_STD = punt_mat5_STD1 if !missing(exp_1) & grado == 5 

foreach g in 3 4 5 {

summ punt_mat if year == 2009 & exp_2 == 0 & grado == `g'
	local mean = r(mean)
	local sd = r(sd)

gen punt_mat`g'_STD2 = (punt_mat-`mean') / `sd' if !missing(exp_2) & grado == `g'

}
replace punt_mat_STD = punt_mat3_STD2 if !missing(exp_2) & grado == 3 
replace punt_mat_STD = punt_mat4_STD2 if !missing(exp_2) & grado == 4 
replace punt_mat_STD = punt_mat5_STD2 if !missing(exp_2) & grado == 5 
	
	
	
summ punt_esp_STD punt_mat_STD if year == 2007 & exp_1 == 0
summ punt_esp_STD punt_mat_STD if year == 2007 & exp_1 == 1

summ punt_esp_STD punt_mat_STD if year == 2009 & exp_2 == 0
summ punt_esp_STD punt_mat_STD if year == 2009 & exp_2 == 1

drop punt_esp3_STD1-punt_mat5_STD2

order estado-punt_mat female ap03-ap48 ap51 mothered zwealth ap53a ap53b ap31 class_teach punt_esp_STD punt_mat_STD 


*------------------------------------------------------------------------------*
** Missing covariates
*------------------------------------------------------------------------------*

* Code missing as mean with dummy indicator for missing

foreach var of varlist punt_esp-punt_mat_STD {
	
	* generate missing indicator
	
	gen `var'_m = `var' ==.
	
	
	* recode to mean if missing
	summ `var'
	local mean = round(r(mean),1)
	replace `var' = `mean' if `var'_m == 1
}

*------------------------------------------------------------------------------*
save "$data/panel_student_long_clean.dta", replace
*------------------------------------------------------------------------------*
