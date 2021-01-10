
* Analysis: Difference in treatment effect on parental involvement

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
* Effect of parental empowerment on parental involvement
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
use "$data/panel_parent_long_clean.dta", clear


*	Shorten label of key variables	
*------------------------------------------------------------------------------*
rename (pb51_1a pb51_1e pb51_1h pb48) ///
(pv1 pv2 pv3 pv4) 

save `a', replace


*	Reshape to include baseline variables	
*------------------------------------------------------------------------------*
keep estado year cct modalidad exp_1 exp_2 exp_3 drop turnover id_parent ///
	 pv1 pv2 pv3 pv4

reshape wide id_parent turnover ///
	pv1 pv2 pv3 pv4, i(cct) j(year)	 

	keep cct *2007 *2009
	
save `b', replace


use `a', clear

merge m:1 cct using `b'

drop _merge

estimates drop _all


*------------------------------------------------------------------------------*
** Bootstrap
*------------------------------------------------------------------------------*  

foreach var of varlist pv1 pv2 pv3 pv4 {

* Experiment 1
*----------------*

bootstrap, reps(100) cluster(cct) saving($data/`var'_exp1.dta, replace) seed(02139): ///
  reg `var' i.exp_1 `var'2007 if year == 2008 & drop == 0, robust cluster(cct)

reg `var' i.exp_1 `var'2007 if year == 2008 & drop == 0, robust cluster(cct)
  global b_`var'_exp1: di %6.3fc _b[1.exp_1]  
  
* Experiment 2
*----------------*

bootstrap, reps(100) cluster(cct) saving($data/`var'_exp2.dta, replace) seed(02139): ///
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
pb03_m pb06_m pb07_m pb16a_m pb18_m pb21_m pb27_m pb37_m pb39_m ///
pv1 pv2 pv3 pv4 ///
pv12009 pv22009 pv32009 pv42009 

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

keeporder estado-id_parent drop pb03-pb39 pb03_m-pb27_m pv42009-v914_m2009 pv1 pv2 pv3 pv4 


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
  
order estado-id_parent drop pb03-pb07 pb16a_2-pb18_3 pb21-pb37 pb39 pb03_m-pb27_m pv42009-v914_m2009 pv1 pv2 pv3 pv4 
 
 
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

order estado-v914_m2009 pb06Xpb03-pb39Xpb37 pv1 pv2 pv3 pv4


* Estimation

// lasso step 1	

foreach x in 1 2 3 4 {
	
lasso linear pv`x' pb03-pb39Xpb37, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
}
	
	
// lasso step 2

lasso linear exp_3 pb03-pb39Xpb37, selection(plugin) rseed(02472)
	local B `e(allvars_sel)'
	macro list _B
	
	
// lasso step 3 - main
	
foreach x in 1 2 3 4 {
	
bootstrap, reps(100) cluster(cct) saving($data/pv`x'_exp3.dta, replace) seed(02139): ///
  reg pv`x' i.exp_3 `A`x'' `B', robust cluster(cct)

reg pv`x' i.exp_3 `A`x'' `B', robust cluster(cct)
  global b_pv`x'_exp3: di %6.3fc _b[1.exp_3] 
   
}



*------------------------------------------------------------------------------*
** Compare across models
*------------------------------------------------------------------------------*  
	
foreach yvar in pv1 pv2 pv3 pv4 {
	
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

putexcel A2="pv1"  B2="1"  C2=${delta12m_pv1}  D2=${delta12se_pv1}  E2=${delta12m_pv1}-1.96*${delta12se_pv1}  F2=${delta12m_pv1}+1.96*${delta12se_pv1}
putexcel A3="pv1"  B3="2"  C3=${delta23m_pv1}  D3=${delta23se_pv1}  E3=${delta23m_pv1}-1.96*${delta23se_pv1}  F3=${delta23m_pv1}+1.96*${delta23se_pv1}
putexcel A4="pv1"  B4="3"  C4=${delta31m_pv1}  D4=${delta31se_pv1}  E4=${delta31m_pv1}-1.96*${delta31se_pv1}  F4=${delta31m_pv1}+1.96*${delta31se_pv1}

putexcel A5="pv2"  B5="1"  C5=${delta12m_pv2}  D5=${delta12se_pv2}  E5=${delta12m_pv2}-1.96*${delta12se_pv2}  F5=${delta12m_pv2}+1.96*${delta12se_pv2}
putexcel A6="pv2"  B6="2"  C6=${delta23m_pv2}  D6=${delta23se_pv2}  E6=${delta23m_pv2}-1.96*${delta23se_pv2}  F6=${delta23m_pv2}+1.96*${delta23se_pv2}
putexcel A7="pv2"  B7="3"  C7=${delta31m_pv2}  D7=${delta31se_pv2}  E7=${delta31m_pv2}-1.96*${delta31se_pv2}  F7=${delta31m_pv2}+1.96*${delta31se_pv2}

putexcel A8="pv3"  B8="1"  C8=${delta12m_pv3}  D8=${delta12se_pv3}  E8=${delta12m_pv3}-1.96*${delta12se_pv3}  F8=${delta12m_pv3}+1.96*${delta12se_pv3}
putexcel A9="pv3"  B9="2"  C9=${delta23m_pv3}  D9=${delta23se_pv3}  E9=${delta23m_pv3}-1.96*${delta23se_pv3}  F9=${delta23m_pv3}+1.96*${delta23se_pv3}
putexcel A10="pv3" B10="3" C10=${delta31m_pv3} D10=${delta31se_pv3} E10=${delta31m_pv3}-1.96*${delta31se_pv3} F10=${delta31m_pv3}+1.96*${delta31se_pv3}

putexcel A11="pv4" B11="1" C11=${delta12m_pv4} D11=${delta12se_pv4} E11=${delta12m_pv4}-1.96*${delta12se_pv4} F11=${delta12m_pv4}+1.96*${delta12se_pv4}
putexcel A12="pv4" B12="2" C12=${delta23m_pv4} D12=${delta23se_pv4} E12=${delta23m_pv4}-1.96*${delta23se_pv4} F12=${delta23m_pv4}+1.96*${delta23se_pv4}
putexcel A13="pv4" B13="3" C13=${delta31m_pv4} D13=${delta31se_pv4} E13=${delta31m_pv4}-1.96*${delta31se_pv4} F13=${delta31m_pv4}+1.96*${delta31se_pv4}

import excel "/Users/nakajimaemiko/Desktop/Harvard Year3/AGE Mexico/new analysis/data/diff.xls", sheet("Sheet1") firstrow clear
destring x, replace

// pv1
twoway (scatter coef x if y=="pv1", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="pv1"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.25 0.35)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.45 1 "Double grant -") ///
	   text(-0.48 1 "Information") ///
	   text(-0.45 2 "Information -") ///
	   text(-0.48 2 "Single grant") ///
	   text(-0.45 3 "Single grant -") ///
	   text(-0.48 3 "Double grant") ///
	   legend(off)
	   graph export "pv1_diff.pdf", replace
	   
// pv2
twoway (scatter coef x if y=="pv2", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="pv2"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.25 0.35)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.27 1 "Double grant -") ///
	   text(-0.3 1 "Information") ///
	   text(-0.27 2 "Information -") ///
	   text(-0.3 2 "Single grant") ///
	   text(-0.27 3 "Single grant -") ///
	   text(-0.3 3 "Double grant") ///
	   legend(off)
	   graph export "pv2_diff.pdf", replace		
			
// pv3
twoway (scatter coef x if y=="pv3", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="pv3"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.25 0.35)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.4 1 "Double grant -") ///
	   text(-0.43 1 "Information") ///
	   text(-0.4 2 "Information -") ///
	   text(-0.43 2 "Single grant") ///
	   text(-0.4 3 "Single grant -") ///
	   text(-0.43 3 "Double grant") /// 
	   legend(off)
	   graph export "pv3_diff.pdf", replace	
	   
// pv4
twoway (scatter coef x if y=="pv4", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="pv4"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-10 10)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-10 1 "Double grant -") ///
	   text(-11 1 "Information") ///
	   text(-10 2 "Information -") ///
	   text(-11 2 "Single grant") ///
	   text(-10 3 "Single grant -") ///
	   text(-11 3 "Double grant") /// 
	   legend(off)
	   graph export "pv4_diff.pdf", replace	   
