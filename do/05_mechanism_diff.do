
* Analysis: Difference in mechanisms

* Author: Nozomi Nakajima
* Date: Dec 2020


drop _all
clear all
set mem 30g
set maxvar 11000
set matsize 11000
set more off


tempfile a b c d e

*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
* Effect on trust & blame (parent perspective)
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

use "$data/panel_parent_long_clean.dta", clear


*	Shorten label of key variables	
*------------------------------------------------------------------------------*
rename (trustte pt_job_pca)(mech1 mech2) 

save `a', replace


*	Reshape to include baseline variables	
*------------------------------------------------------------------------------*
keep estado year cct mech1 mech2 

reshape wide mech1 mech2, i(cct) j(year)	 

	keep cct *2007 *2009
	
save `b', replace


use `a', clear

merge m:1 cct using `b'

drop _merge

*------------------------------------------------------------------------------*
** Bootstrap
*------------------------------------------------------------------------------*  

foreach var of varlist mech1 mech2 {

* Experiment 1
*----------------*

bootstrap, reps(500) cluster(cct) saving($data/`var'_exp1.dta, replace) seed(02139): ///
  reg `var' i.exp_1 `var'2007 if year == 2008 & drop == 0, robust cluster(cct)

reg `var' i.exp_1 `var'2007 if year == 2008 & drop == 0, robust cluster(cct)
  global b_`var'_exp1: di %6.3fc _b[1.exp_1]  
  
  
* Experiment 2
*----------------*

bootstrap, reps(500) cluster(cct) saving($data/`var'_exp2.dta, replace) seed(02139): ///
  reg `var' i.exp_2 `var'2009 if year == 2010 & drop == 0, robust cluster(cct)
 
reg `var' i.exp_2 `var'2009 if year == 2010 & drop == 0, robust cluster(cct)
  global b_`var'_exp2: di %6.3fc _b[1.exp_2]
 
}  


* "Experiment" 3
*----------------*
* Only keep 2010 variables that are not outcomes but plausible predictors  

keep if year == 2010 & !missing(exp_3) & modalidad == "General" & drop == 0

keep estado cct modalidad exp_1 exp_2 exp_3 id_parent drop ///
pb03 pb06 pb07 pb16a pb18 pb21 pb27 pb37 pb39 ///
pb03_m pb06_m pb07_m pb16a_m pb18_m pb21_m pb27_m ///
mech1 mech2 ///
mech12009 mech22009

save `a', replace


* Gather prior/2009 school-level variables  

use "$data/panel_parent_long_clean.dta", clear

keep if year == 2009 & !missing(exp_3) & modalidad == "General" & drop == 0

keep estado cct modalidad exp_1 exp_2 exp_3 id_parent drop ///
prom3_esp-v914_m

foreach var of varlist prom3_esp-v914_m {
	
	rename `var' `var'2009
	
}

save `b', replace 


use `a', clear
merge 1:1 cct using `b'
drop _merge

keeporder estado-pb39 pb03_m-pb27_m prom3_esp2009-v914_m2009 mech12009 mech22009 mech1 mech2 drop 


* Create binary variable for categorical variable

tab pb16a, gen(pb16a_)
drop pb16a pb16a_1
la var pb16a_2 "Parent edu primary"
la var pb16a_3 "Parent edu secondary"
la var pb16a_4 "Parent edu college"
la var pb16a_5 "Parent edu university"

tab pb18, gen(pb18_)
drop pb18 pb18_1
la var pb18_2 "Election"
la var pb18_3 "Other"

order estado-id_parent drop pb03-pb07 pb16a_2-pb16a_5 pb18_2 pb18_3 pb21-v914_m2009 mech12009 mech22009 mech1 mech2 
 
 
* Generate interactions of variables

unab vars : pb03 - pb39
local nvar : word count `vars'
forval i = 1/`nvar' {
  forval j = 1/`=`i'-1' {
    local x : word `i' of `vars'
    local y : word `j' of `vars'
    generate `x'X`y' = `x' * `y'
  }
}

order estado-v914_m2009 pb06Xpb03-pb39Xpb37 mech12009 mech22009 mech1 mech2 


* Estimation 

// lasso step 1	

foreach x in 1 2 {
	
lasso linear mech`x' pb03-mech22009, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
}
	
	
// lasso step 2

lasso linear exp_3 pb03-mech22009, selection(plugin) rseed(02472)
	local B1 `e(allvars_sel)'
	
	
// lasso step 3 - main
	
foreach x in 1 2 {
	
bootstrap, reps(500) cluster(cct) saving($data/mech`x'_exp3.dta, replace) seed(02139): ///
  reg mech`x' i.exp_3 `A`x'' `B1', robust cluster(cct)

reg mech`x' i.exp_3 `A`x'' `B1', robust cluster(cct)
  global b_mech`x'_exp3: di %6.3fc _b[1.exp_3] 
  
}



*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
* Effect on educational outcomes (teacher perspective)
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
use "$data/panel_teacher_long_clean.dta", clear


*	Shorten label of key variables	
*------------------------------------------------------------------------------*
rename (trust tp_job_pca) (mech3 mech4) 

* generate group level lagged mean

foreach var of varlist mech3 mech4 { 
	
egen mean`var'1 = mean(`var') if year == 2007, by(cct)
egen mean`var'2007 = min(mean`var'1), by(cct)

egen mean`var'2 = mean(`var') if year == 2009, by(cct)
egen mean`var'2009 = min(mean`var'2), by(cct)

}
drop meanmech31 meanmech32 meanmech41 meanmech42 


*------------------------------------------------------------------------------*
** Bootstrap
*------------------------------------------------------------------------------*  

foreach var of varlist mech3 mech4 {

* Experiment 1
*----------------*

bootstrap, reps(500) cluster(cct) saving($data/`var'_exp1.dta, replace) seed(02139): ///
  reg `var' i.exp_1 mean`var'2007 if year == 2008 & drop == 0, robust cluster(cct)

reg `var' i.exp_1 mean`var'2007 if year == 2008 & drop == 0, robust cluster(cct)
  global b_`var'_exp1: di %6.3fc _b[1.exp_1]  
  
  
* Experiment 2
*----------------*

bootstrap, reps(500) cluster(cct) saving($data/`var'_exp2.dta, replace) seed(02139): ///
  reg `var' i.exp_2 mean`var'2009 if year == 2010 & drop == 0, robust cluster(cct)
 
reg `var' i.exp_2 mean`var'2009 if year == 2010 & drop == 0, robust cluster(cct)
  global b_`var'_exp2: di %6.3fc _b[1.exp_2]
 
}  


* "Experiment" 3
*----------------*
* Only keep 2010 variables that are not outcomes but plausible predictors  

keep if year == 2010 & modalidad == "General" & !missing(exp_3) & drop == 0

keeporder estado-mb16 mb34a mb36a mb28 mb03_m-mb16_m mb34a_m mb36a_m mb28_m teacheredu_col teacheredu_uni meanmech32009 meanmech42009 ///
mech3 mech4 

save `a', replace


* Append school-level average test score from 2009

use "$data/panel_school.dta", clear

keep if year == 2009 & modalidad == "General" & !missing(exp_3) & drop == 0

keep cct prom3_esp-prom5_mat 

foreach var of varlist prom3_esp-prom5_mat {
	
	rename `var' `var'2009
	
}

save `b', replace


use `a', clear

merge m:1 cct using `b'
	drop _merge

	
* Create binary variable for categorical variable
tab grado, gen(grado_)
  drop grado grado_1
  la var grado_2 "Grade 4"
  la var grado_3 "Grade 5"
           

order estado-drop grado_2 grado_3 mb03-meanmech42009 ///
prom3_esp2009-prom5_mat2009 mech3 mech4

 
* Generate squared of continuous variables
foreach var of varlist prom3_esp2009 prom4_esp2009 prom5_esp2009 prom3_mat2009 prom4_mat2009 prom5_mat2009 {
	
	gen `var'_2 = `var'*`var'	

}

* Generate interactions of variables
unab vars : mb03 mb06 mb16 mb34a mb36a mb28 teacheredu_col teacheredu_uni
local nvar : word count `vars'
forval i = 1/`nvar' {
  forval j = 1/`=`i'-1' {
    local x : word `i' of `vars'
    local y : word `j' of `vars'
    generate `x'X`y' = `x' * `y'
  }
}


order estado-prom5_mat2009 prom3_esp2009_2-teacheredu_uniXteacheredu_col mech3 mech4


* Estimation

// lasso step 1	

foreach x in 3 4 {
	
lasso linear mech`x' grado_2-teacheredu_uniXteacheredu_col, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
}
	
	
// lasso step 2

lasso linear exp_3 grado_2-teacheredu_uniXteacheredu_col, selection(plugin) rseed(02472)
	local B2 `e(allvars_sel)'
	macro list _B2
	
	
// lasso step 3 - main
	
foreach x in 3 4 {
	
eststo: reg mech`x' i.exp_3 `A`x'' `B2', robust cluster(cct)
   
bootstrap, reps(500) cluster(cct) saving($data/mech`x'_exp3.dta, replace) seed(02139): ///
  reg mech`x' i.exp_3 `A`x'' `B2', robust cluster(cct)

reg mech`x' i.exp_3 `A`x'' `B2', robust cluster(cct)
  global b_mech`x'_exp3: di %6.3fc _b[1.exp_3]    
}


*------------------------------------------------------------------------------*
** Compare across models
*------------------------------------------------------------------------------*  
	
foreach yvar in mech1 mech2 mech3 mech4 {
	
	* POVERTY
	
	tempfile a b c d
	
	use "$data/`yvar'_exp1.dta", clear
		rename _bs_2 exp1
		gen sim = _n
		keep exp1 sim
		save `a', replace
 
	use "$data/`yvar'_exp2.dta", clear
		rename _bs_2 exp2
		gen sim = _n
		keep exp2 sim
		save `b', replace
		
	use "$data/`yvar'_exp3.dta", clear
		rename _bs_2 exp3
		gen sim = _n
		keep exp3 sim
		save `c', replace	
			
	use	`a', clear
		merge 1:1 sim using `b'
		drop sim _merge
		
		gen delta12 = exp1 - exp2		
		summ delta12
		global delta12m_`yvar' = r(mean)
		global delta12se_`yvar' = r(sd)
	
	use	`b', clear
		merge 1:1 sim using `c'
		drop sim _merge
		
		gen delta23 = exp2 - exp3		
		summ delta23
		global delta23m_`yvar' = r(mean)
		global delta23se_`yvar' = r(sd)
		
	use	`c', clear
		merge 1:1 sim using `a'
		drop sim _merge
		
		gen delta31 = exp3 - exp1		
		summ delta31
		global delta31m_`yvar' = r(mean)
		global delta31se_`yvar' = r(sd)					
}

putexcel set "$data/diff.xls", replace
putexcel A1="y"   B1="x" C1="coef" D1="se" E1="ll" F1="ul"

putexcel A2="mech1"  B2="1"  C2=${delta12m_mech1}  D2=${delta12se_mech1}  E2=${delta12m_mech1}-1.96*${delta12se_mech1}  F2=${delta12m_mech1}+1.96*${delta12se_mech1}
putexcel A3="mech1"  B3="2"  C3=${delta23m_mech1}  D3=${delta23se_mech1}  E3=${delta23m_mech1}-1.96*${delta23se_mech1}  F3=${delta23m_mech1}+1.96*${delta23se_mech1}
putexcel A4="mech1"  B4="3"  C4=${delta31m_mech1}  D4=${delta31se_mech1}  E4=${delta31m_mech1}-1.96*${delta31se_mech1}  F4=${delta31m_mech1}+1.96*${delta31se_mech1}

putexcel A5="mech2"  B5="1"  C5=${delta12m_mech2}  D5=${delta12se_mech2}  E5=${delta12m_mech2}-1.96*${delta12se_mech2}  F5=${delta12m_mech2}+1.96*${delta12se_mech2}
putexcel A6="mech2"  B6="2"  C6=${delta23m_mech2}  D6=${delta23se_mech2}  E6=${delta23m_mech2}-1.96*${delta23se_mech2}  F6=${delta23m_mech2}+1.96*${delta23se_mech2}
putexcel A7="mech2"  B7="3"  C7=${delta31m_mech2}  D7=${delta31se_mech2}  E7=${delta31m_mech2}-1.96*${delta31se_mech2}  F7=${delta31m_mech2}+1.96*${delta31se_mech2}

putexcel A8="mech3"  B8="1"  C8=${delta12m_mech3}  D8=${delta12se_mech3}  E8=${delta12m_mech3}-1.96*${delta12se_mech3}  F8=${delta12m_mech3}+1.96*${delta12se_mech3}
putexcel A9="mech3"  B9="2"  C9=${delta23m_mech3}  D9=${delta23se_mech3}  E9=${delta23m_mech3}-1.96*${delta23se_mech3}  F9=${delta23m_mech3}+1.96*${delta23se_mech3}
putexcel A10="mech3" B10="3" C10=${delta31m_mech3} D10=${delta31se_mech3} E10=${delta31m_mech3}-1.96*${delta31se_mech3} F10=${delta31m_mech3}+1.96*${delta31se_mech3}

putexcel A11="mech4" B11="1" C11=${delta12m_mech4} D11=${delta12se_mech4} E11=${delta12m_mech4}-1.96*${delta12se_mech4} F11=${delta12m_mech4}+1.96*${delta12se_mech4}
putexcel A12="mech4" B12="2" C12=${delta23m_mech4} D12=${delta23se_mech4} E12=${delta23m_mech4}-1.96*${delta23se_mech4} F12=${delta23m_mech4}+1.96*${delta23se_mech4}
putexcel A13="mech4" B13="3" C13=${delta31m_mech4} D13=${delta31se_mech4} E13=${delta31m_mech4}-1.96*${delta31se_mech4} F13=${delta31m_mech4}+1.96*${delta31se_mech4}

import excel "/Users/nakajimaemiko/Desktop/Harvard Year3/AGE Mexico/new analysis/data/diff.xls", sheet("Sheet1") firstrow clear
destring x, replace

// mech1
twoway (scatter coef x if y=="mech1", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="mech1"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.4 0.4)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.42 1 "Double grant -") ///
	   text(-0.45 1 "Information") ///
	   text(-0.42 2 "Information -") ///
	   text(-0.45 2 "Single grant") ///
	   text(-0.42 3 "Single grant -") ///
	   text(-0.45 3 "Double grant") /// 
	   legend(off)
	   graph export "mech1_diff.pdf", replace
	   
// mech2
twoway (scatter coef x if y=="mech2", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="mech2"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-1.5 1.5)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-1.50 1 "Double grant -") ///
	   text(-1.6 1 "Information") ///
	   text(-1.50 2 "Information -") ///
	   text(-1.6 2 "Single grant") ///
	   text(-1.50 3 "Single grant -") ///
	   text(-1.6 3 "Double grant") /// 
	   legend(off)
	   graph export "mech2_diff.pdf", replace		
			
// mech3
twoway (scatter coef x if y=="mech3", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="mech3"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.35 0.35)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.35 1 "Double grant -") ///
	   text(-0.38 1 "Information") ///
	   text(-0.35 2 "Information -") ///
	   text(-0.38 2 "Single grant") ///
	   text(-0.35 3 "Single grant -") ///
	   text(-0.38 3 "Double grant") /// 
	   legend(off)
	   graph export "mech3_diff.pdf", replace	
	   
// mech4
twoway (scatter coef x if y=="mech4", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="mech4"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-1 1)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-1.0 1 "Double grant -") ///
	   text(-1.1 1 "Information") ///
	   text(-1.0 2 "Information -") ///
	   text(-1.1 2 "Single grant") ///
	   text(-1.0 3 "Single grant -") ///
	   text(-1.1 3 "Double grant") /// 
	   legend(off)
	   graph export "mech4_diff.pdf", replace	   
