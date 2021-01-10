
* Analysis: Difference in treatment effect on educational outcomes

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
* Effect on educational outcomes (school-level)
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
use "$data/panel_school.dta", clear


*	Shorten label of key variables	
*------------------------------------------------------------------------------*
rename (frate_tot_both rrate_tot_both drate_tot_both) ///
(ed1 ed2 ed3) 

save `a', replace


*	Reshape to include baseline variables	
*------------------------------------------------------------------------------*
keep estado year cct modalidad group exp_1 exp_2 exp_3 ed1 ed2 ed3 ///
prom3_esp prom4_esp prom5_esp prom3_mat prom4_mat prom5_mat

reshape wide ed1 ed2 ed3 prom3_esp prom4_esp prom5_esp prom3_mat prom4_mat prom5_mat, i(cct) j(year)	 

	keep cct *2007 *2009
	rename (ed12007 ed22007 ed32007 ed12009 ed22009 ed32009) ///
	(meaned12007 meaned22007 meaned32007 meaned12009 meaned22009 meaned32009 )
	
save `b', replace


use `a', clear

merge m:1 cct using `b'

drop _merge


estimates drop _all

*------------------------------------------------------------------------------*
** Bootstrap
*------------------------------------------------------------------------------*  

foreach var of varlist ed1 ed2 ed3 {

* Experiment 1
*----------------*

bootstrap, reps(100) cluster(cct) saving($data/`var'_exp1.dta, replace) seed(02139): ///
  reg `var' i.exp_1 mean`var'2007 if year == 2008 & drop == 0, robust cluster(cct)

reg `var' i.exp_1 mean`var'2007 if year == 2008 & drop == 0, robust cluster(cct)
  global b_`var'_exp1: di %6.3fc _b[1.exp_1]  
  
  
* Experiment 2
*----------------*

bootstrap, reps(100) cluster(cct) saving($data/`var'_exp2.dta, replace) seed(02139): ///
  reg `var' i.exp_2 mean`var'2009 if year == 2010 & drop == 0, robust cluster(cct)
 
reg `var' i.exp_2 mean`var'2009 if year == 2010 & drop == 0, robust cluster(cct)
  global b_`var'_exp2: di %6.3fc _b[1.exp_2]
   
}


* "Experiment" 3
*----------------*
* Only keep 2010 variables that are not outcomes but plausible predictors  

keep if year == 2010 & modalidad == "General" & !missing(exp_3) & drop == 0

keeporder cct modalidad year exp_1 exp_2 exp_3 drop ///
teachedu_col teachedu_col_m teachedu_uni teachedu_uni_m ///
v912 v912_m v913 v913_m v914 v914_m ///
prom3_esp2009 prom4_esp2009 prom5_esp2009 prom3_mat2009 prom4_mat2009 prom5_mat2009 ///
meaned12009 meaned22009 meaned32009 ///
ed1 ed2 ed3 

save `a', replace


* Merge parent data (predicts treatment)

use "$data/panel_parent_long_clean.dta", clear

keep if year == 2010 & modalidad == "General" & !missing(exp_3) & drop == 0

keeporder cct pb03 pb06 pb07 pb16a pb18 pb21 pb27 ///
pb03_m pb06_m pb07_m pb16a_m pb18_m pb21_m pb27_m 

save `b', replace


use `a', clear
merge 1:1 cct using `b'
drop _merge

order cct-meaned32009 pb03-pb27_m ed1 ed2 ed3

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

* Generate squred of continuous variables
foreach var of varlist teachedu_col teachedu_uni prom3_esp2009 prom4_esp2009 prom5_esp2009 prom3_mat2009 prom4_mat2009 prom5_mat2009 {
	
	gen `var'_2 = `var'*`var'	

}

order cct-pb07 pb16a_2 pb16a_3 pb16a_4 pb16a_5 pb18_2 pb18_3 pb21-pb27_m teachedu_col_2-prom5_mat2009_2 ed1 ed2 ed3

* Generate interactions of variables
unab vars : teachedu_uni pb03 pb06 pb07 pb21 pb27
local nvar : word count `vars'
forval i = 1/`nvar' {
  forval j = 1/`=`i'-1' {
    local x : word `i' of `vars'
    local y : word `j' of `vars'
    generate `x'X`y' = `x' * `y'
  }
}

order cct-prom5_mat2009_2 pb03Xteachedu_uni-pb27Xpb21 ed1 ed2 ed3



* Estimation

// lasso step 1	

foreach x in 1 2 3 {
	
lasso linear ed`x' teachedu_col-pb27Xpb21, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	macro list _A`x'
}
	
	
// lasso step 2

lasso linear exp_3 teachedu_col-pb27Xpb21, selection(plugin) rseed(02472)
	local B1 = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	macro list _B1
	
	
	
foreach x in 1 2 3 {
	
bootstrap, reps(100) cluster(cct) saving($data/ed`x'_exp3.dta, replace) seed(02139): ///
  reg ed`x' i.exp_3 `A`x'' `B1', robust cluster(cct)

reg ed`x' i.exp_3 `A`x'' `B1', robust cluster(cct)
  global b_ed`x'_exp3: di %6.3fc _b[1.exp_3]  
  
}  


*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
* Effect on educational outcomes (student-level)
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
use "$data/panel_student_long_clean.dta", clear


*	Shorten label of key variables	
*------------------------------------------------------------------------------*
rename (punt_esp_STD punt_mat_STD ap31) (ed4 ed5 ed6) 


* generate group level lagged mean

foreach var of varlist ed4 ed5 ed6 { 
	
egen mean`var'1 = mean(`var') if year == 2007, by(cct)
egen mean`var'2007 = min(mean`var'1), by(cct)

egen mean`var'2 = mean(`var') if year == 2009, by(cct)
egen mean`var'2009 = min(mean`var'2), by(cct)

}

*------------------------------------------------------------------------------*
** Bootstrap
*------------------------------------------------------------------------------*  

foreach var of varlist ed4 ed5 ed6 {

* Experiment 1
*----------------*

bootstrap, reps(100) cluster(cct) saving($data/`var'_exp1.dta, replace) seed(02139): ///
  reg `var' i.exp_1 i.grado female zwealth mothered mean`var'2007 female_m zwealth_m mothered_m if year == 2008 & drop == 0, robust cluster(cct)

reg `var' i.exp_1 i.grado female zwealth mothered mean`var'2007 female_m zwealth_m mothered_m if year == 2008 & drop == 0, robust cluster(cct)
  global b_`var'_exp1: di %6.3fc _b[1.exp_1]  
  
* Experiment 2
*----------------*

bootstrap, reps(100) cluster(cct) saving($data/`var'_exp2.dta, replace) seed(02139): ///
  reg `var' i.exp_2 i.grado female zwealth mothered mean`var'2009 female_m zwealth_m mothered_m if year == 2010 & drop == 0, robust cluster(cct)
 
reg `var' i.exp_2 i.grado female zwealth mothered mean`var'2009 female_m zwealth_m mothered_m if year == 2010 & drop == 0, robust cluster(cct)
  global b_`var'_exp2: di %6.3fc _b[1.exp_2]
 
}  

 
 
*------------------------------------------------------------------------------*
** Experiment 3
*------------------------------------------------------------------------------*  
* Merge data of teachers

merge m:1 cct year grado using "$data/panel_teacher_long_clean.dta"

drop if _merge == 2
	* _merge == 2 are teachers not in cct, year, grado of student sample

tab exp_1 year if _merge == 1
tab exp_2 year if _merge == 1
	* _merge == 1 are exp_2 schools in 2007 & 2008 (no survey)

drop _merge


* Only keep 2010 variables that are not outcomes but plausible predictors  

keep if year == 2010 & modalidad == "General" & !missing(exp_3) & drop == 0
	  
keeporder estado-id_student female ap03-ap51 ap03_m-ap51_m zwealth ///
mb03 mb06 mb16 mb34a mb36a mb28 mb03_m mb06_m mb16_m mb34a_m mb36a_m mb28_m ///
teacheredu_col teacheredu_uni ///
meaned42009 meaned52009 meaned62009 ///
ed4 ed5 ed6 

* Create binary variable for categorical variable
tab grado, gen(grado_)
  drop grado grado_1
  la var grado_2 "Grade 4"
  la var grado_3 "Grade 5"

tab ap51, gen(ap51_) 
  drop ap51 ap51_1
  la var ap51_2 "Mother education: Prepatoria"
  la var ap51_3 "Mother education: Secundaria"
  la var ap51_4 "Mother education: Primaria completa"
  la var ap51_5 "Mother education: No termino primaria"
  la var ap51_6 "Mother education: No fue a la escuela"
  la var ap51_7 "Mother education: No se"


order estado-female grado_2 grado_3 ap03-ap48 ap51_2-ap51_7 ap03_m-ed6
  
* Generate interactions of variables
unab vars : grado_2-ap51_7 mb03-mb28
local nvar : word count `vars'
forval i = 1/`nvar' {
  forval j = 1/`=`i'-1' {
    local x : word `i' of `vars'
    local y : word `j' of `vars'
    generate `x'X`y' = `x' * `y'
  }
}

order estado-meaned62009 grado_3Xgrado_2-mb28Xmb36a ed4 ed5 ed6


* Estimation

// lasso step 1	

foreach x in 4 5 6 {
	
lasso linear ed`x' grado_2-mb28Xmb36a, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
}
	
	
// lasso step 2

lasso linear exp_3 grado_2-mb28Xmb36a, selection(plugin) rseed(02472)
	local B2 = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")

	
// lasso step 3 - main
	
foreach x in 4 5 6 {

bootstrap, reps(100) cluster(cct) saving($data/ed`x'_exp3.dta, replace) seed(02139): ///
  reg ed`x' i.exp_3 `A`x'' `B2', robust cluster(cct)

reg ed`x' i.exp_3 `A`x'' `B2', robust cluster(cct)
  global b_ed`x'_exp3: di %6.3fc _b[1.exp_3] 
  
}


*------------------------------------------------------------------------------*
** Compare across models
*------------------------------------------------------------------------------*  
	
foreach yvar in ed1 ed2 ed3 ed4 ed5 ed6 {
	
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

putexcel A2="ed1"  B2="1"  C2=${delta12m_ed1}  D2=${delta12se_ed1}  E2=${delta12m_ed1}-1.96*${delta12se_ed1}  F2=${delta12m_ed1}+1.96*${delta12se_ed1}
putexcel A3="ed1"  B3="2"  C3=${delta23m_ed1}  D3=${delta23se_ed1}  E3=${delta23m_ed1}-1.96*${delta23se_ed1}  F3=${delta23m_ed1}+1.96*${delta23se_ed1}
putexcel A4="ed1"  B4="3"  C4=${delta31m_ed1}  D4=${delta31se_ed1}  E4=${delta31m_ed1}-1.96*${delta31se_ed1}  F4=${delta31m_ed1}+1.96*${delta31se_ed1}

putexcel A5="ed2"  B5="1"  C5=${delta12m_ed2}  D5=${delta12se_ed2}  E5=${delta12m_ed2}-1.96*${delta12se_ed2}  F5=${delta12m_ed2}+1.96*${delta12se_ed2}
putexcel A6="ed2"  B6="2"  C6=${delta23m_ed2}  D6=${delta23se_ed2}  E6=${delta23m_ed2}-1.96*${delta23se_ed2}  F6=${delta23m_ed2}+1.96*${delta23se_ed2}
putexcel A7="ed2"  B7="3"  C7=${delta31m_ed2}  D7=${delta31se_ed2}  E7=${delta31m_ed2}-1.96*${delta31se_ed2}  F7=${delta31m_ed2}+1.96*${delta31se_ed2}

putexcel A8="ed3"  B8="1"  C8=${delta12m_ed3}  D8=${delta12se_ed3}  E8=${delta12m_ed3}-1.96*${delta12se_ed3}  F8=${delta12m_ed3}+1.96*${delta12se_ed3}
putexcel A9="ed3"  B9="2"  C9=${delta23m_ed3}  D9=${delta23se_ed3}  E9=${delta23m_ed3}-1.96*${delta23se_ed3}  F9=${delta23m_ed3}+1.96*${delta23se_ed3}
putexcel A10="ed3" B10="3" C10=${delta31m_ed3} D10=${delta31se_ed3} E10=${delta31m_ed3}-1.96*${delta31se_ed3} F10=${delta31m_ed3}+1.96*${delta31se_ed3}

putexcel A11="ed4" B11="1" C11=${delta12m_ed4} D11=${delta12se_ed4} E11=${delta12m_ed4}-1.96*${delta12se_ed4} F11=${delta12m_ed4}+1.96*${delta12se_ed4}
putexcel A12="ed4" B12="2" C12=${delta23m_ed4} D12=${delta23se_ed4} E12=${delta23m_ed4}-1.96*${delta23se_ed4} F12=${delta23m_ed4}+1.96*${delta23se_ed4}
putexcel A13="ed4" B13="3" C13=${delta31m_ed4} D13=${delta31se_ed4} E13=${delta31m_ed4}-1.96*${delta31se_ed4} F13=${delta31m_ed4}+1.96*${delta31se_ed4}

putexcel A14="ed5" B14="1" C14=${delta12m_ed5} D14=${delta12se_ed5} E14=${delta12m_ed5}-1.96*${delta12se_ed5} F14=${delta12m_ed5}+1.96*${delta12se_ed5}
putexcel A15="ed5" B15="2" C15=${delta23m_ed5} D15=${delta23se_ed5} E15=${delta23m_ed5}-1.96*${delta23se_ed5} F15=${delta23m_ed5}+1.96*${delta23se_ed5}
putexcel A16="ed5" B16="3" C16=${delta31m_ed5} D16=${delta31se_ed5} E16=${delta31m_ed5}-1.96*${delta31se_ed5} F16=${delta31m_ed5}+1.96*${delta31se_ed5}

putexcel A17="ed6" B17="1" C17=${delta12m_ed6} D17=${delta12se_ed6} E17=${delta12m_ed6}-1.96*${delta12se_ed6} F17=${delta12m_ed6}+1.96*${delta12se_ed6}
putexcel A18="ed6" B18="2" C18=${delta23m_ed6} D18=${delta23se_ed6} E18=${delta23m_ed6}-1.96*${delta23se_ed6} F18=${delta23m_ed6}+1.96*${delta23se_ed6}
putexcel A19="ed6" B19="3" C19=${delta31m_ed6} D19=${delta31se_ed6} E19=${delta31m_ed6}-1.96*${delta31se_ed6} F19=${delta31m_ed6}+1.96*${delta31se_ed6}

import excel "/Users/nakajimaemiko/Desktop/Harvard Year3/AGE Mexico/new analysis/data/diff.xls", sheet("Sheet1") firstrow clear
destring x, replace

// ed1
twoway (scatter coef x if y=="ed1", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="ed1"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.08 0.08)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.08 1 "Double grant -") ///
	   text(-0.09 1 "Information") ///
	   text(-0.08 2 "Information -") ///
	   text(-0.09 2 "Single grant") ///
	   text(-0.08 3 "Single grant -") ///
	   text(-0.09 3 "Double grant") /// 
	   legend(off)
	   graph export "ed1_diff.pdf", replace
	   
// ed2
twoway (scatter coef x if y=="ed2", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="ed2"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.05 0.05)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.050 1 "Double grant -") ///
	   text(-0.055 1 "Information") ///
	   text(-0.050 2 "Information -") ///
	   text(-0.055 2 "Single grant") ///
	   text(-0.050 3 "Single grant -") ///
	   text(-0.055 3 "Double grant") /// 
	   legend(off)
	   graph export "ed2_diff.pdf", replace		
			
// ed3
twoway (scatter coef x if y=="ed3", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="ed3"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.05 0.05)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.050 1 "Double grant -") ///
	   text(-0.055 1 "Information") ///
	   text(-0.050 2 "Information -") ///
	   text(-0.055 2 "Single grant") ///
	   text(-0.050 3 "Single grant -") ///
	   text(-0.055 3 "Double grant") /// 
	   legend(off)
	   graph export "ed3_diff.pdf", replace	
	   
// ed4
twoway (scatter coef x if y=="ed4", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="ed4"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.5 0.5)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.50 1 "Double grant -") ///
	   text(-0.54 1 "Information") ///
	   text(-0.50 2 "Information -") ///
	   text(-0.54 2 "Single grant") ///
	   text(-0.50 3 "Single grant -") ///
	   text(-0.54 3 "Double grant") /// 
	   legend(off)
	   graph export "ed4_diff.pdf", replace	   
	   
// ed5
twoway (scatter coef x if y=="ed5", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="ed5"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.5 0.5)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.50 1 "Double grant -") ///
	   text(-0.54 1 "Information") ///
	   text(-0.50 2 "Information -") ///
	   text(-0.54 2 "Single grant") ///
	   text(-0.50 3 "Single grant -") ///
	   text(-0.54 3 "Double grant") /// 
	   legend(off)
	   graph export "ed5_diff.pdf", replace
	   
// ed6
twoway (scatter coef x if y=="ed6", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="ed6"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.2 0.2)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.20 1 "Double grant -") ///
	   text(-0.22 1 "Information") ///
	   text(-0.20 2 "Information -") ///
	   text(-0.22 2 "Single grant") ///
	   text(-0.20 3 "Single grant -") ///
	   text(-0.22 3 "Double grant") /// 
	   legend(off)
	   graph export "ed6_diff.pdf", replace	   
