
* Analysis: Difference in treatment effect on child inputs

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
* Parenting & teaching behavior
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

u "$data/panel_student_long_clean.dta", clear

merge m:1 cct year grado using "$data/panel_teacher_long_clean.dta"

drop if _merge == 2
	* _merge == 2 are teachers not in cct, year, grado of student sample

tab exp_1 year if _merge == 1
tab exp_2 year if _merge == 1
	* _merge == 1 are exp_2 schools in 2007 & 2008 (no survey)

drop _merge

*	Shorten label of key variables	
*------------------------------------------------------------------------------*
rename (ap53a ap53b mb31a class_teach) ///
(c1 c2 c3 c4) 

* generate group level lagged mean

foreach var of varlist c1 c2 c3 c4 { 
	
egen mean`var'1 = mean(`var') if year == 2007, by(cct)
egen mean`var'2007 = min(mean`var'1), by(cct)

egen mean`var'2 = mean(`var') if year == 2009, by(cct)
egen mean`var'2009 = min(mean`var'2), by(cct)

}
drop meanc11 meanc12 meanc21 meanc22 meanc31 meanc32 meanc41 meanc42


*------------------------------------------------------------------------------*
** Bootstrap
*------------------------------------------------------------------------------*  

foreach var of varlist c1 c2 c3 c4 {

* Experiment 1
*----------------*

bootstrap, reps(100) cluster(cct) saving($data/`var'_exp1.dta, replace) seed(02139): ///
  reg `var' i.exp_1 grado female zwealth mothered mean`var'2007 female_m zwealth_m mothered_m if year == 2008 & drop == 0, robust cluster(cct)

reg `var' i.exp_1 grado female zwealth mothered mean`var'2007 female_m zwealth_m mothered_m if year == 2008 & drop == 0, robust cluster(cct)
  global b_`var'_exp1: di %6.3fc _b[1.exp_1]  

  
* Experiment 2
*----------------*

bootstrap, reps(100) cluster(cct) saving($data/`var'_exp2.dta, replace) seed(02139): ///
  reg `var' i.exp_2 grado female zwealth mothered mean`var'2009 female_m zwealth_m mothered_m if year == 2010 & drop == 0, robust cluster(cct)
 
reg `var' i.exp_2 grado female zwealth mothered mean`var'2009 female_m zwealth_m mothered_m if year == 2010 & drop == 0, robust cluster(cct)
  global b_`var'_exp2: di %6.3fc _b[1.exp_2]  
  
}

* "Experiment" 3
*----------------*

* Only keep 2010 variables that are not outcomes but plausible predictors  

keep if year == 2010 & modalidad == "General" & !missing(exp_3) & drop == 0
	  
keeporder estado-id_student female ap03-ap51 ap03_m-ap51_m zwealth ///
mb03 mb06 mb16 mb34a mb36a mb28 mb03_m mb06_m mb16_m mb34a_m mb36a_m mb28_m ///
teacheredu_col teacheredu_uni ///
meanc12009 meanc22009 meanc32009 meanc42009 ///
c1 c2 c3 c4 


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
  

order estado-female grado_2 grado_3 ap03-ap48 ap51_2-ap51_7 ap03_m-c4

					 
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


order estado-meanc42009 grado_3Xgrado_2-mb28Xmb36a c1-c4



* Estimation 

// lasso step 1	

foreach x in 1 2 3 4 {
	
lasso linear c`x' grado_2-mb36aXmb34a, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
}
	
	
// lasso step 2

lasso linear exp_3 grado_2-mb36aXmb34a, selection(plugin) rseed(02472)
	local B `e(allvars_sel)'
	macro list _B
	
	
// lasso step 3 - main
	
foreach x in 1 2 3 4 {
	
bootstrap, reps(100) cluster(cct) saving($data/c`x'_exp3.dta, replace) seed(02139): ///
  reg c`x' i.exp_3 `A`x'' `B', robust cluster(cct)

reg c`x' i.exp_3 `A`x'' `B', robust cluster(cct)
  global b_c`x'_exp3: di %6.3fc _b[1.exp_3] 
}


*------------------------------------------------------------------------------*
** Compare across models
*------------------------------------------------------------------------------*  
	
foreach yvar in c1 c2 c3 c4 {
	
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

putexcel A2="c1"  B2="1"  C2=${delta12m_c1}  D2=${delta12se_c1}  E2=${delta12m_c1}-1.96*${delta12se_c1}  F2=${delta12m_c1}+1.96*${delta12se_c1}
putexcel A3="c1"  B3="2"  C3=${delta23m_c1}  D3=${delta23se_c1}  E3=${delta23m_c1}-1.96*${delta23se_c1}  F3=${delta23m_c1}+1.96*${delta23se_c1}
putexcel A4="c1"  B4="3"  C4=${delta31m_c1}  D4=${delta31se_c1}  E4=${delta31m_c1}-1.96*${delta31se_c1}  F4=${delta31m_c1}+1.96*${delta31se_c1}

putexcel A5="c2"  B5="1"  C5=${delta12m_c2}  D5=${delta12se_c2}  E5=${delta12m_c2}-1.96*${delta12se_c2}  F5=${delta12m_c2}+1.96*${delta12se_c2}
putexcel A6="c2"  B6="2"  C6=${delta23m_c2}  D6=${delta23se_c2}  E6=${delta23m_c2}-1.96*${delta23se_c2}  F6=${delta23m_c2}+1.96*${delta23se_c2}
putexcel A7="c2"  B7="3"  C7=${delta31m_c2}  D7=${delta31se_c2}  E7=${delta31m_c2}-1.96*${delta31se_c2}  F7=${delta31m_c2}+1.96*${delta31se_c2}

putexcel A8="c3"  B8="1"  C8=${delta12m_c3}  D8=${delta12se_c3}  E8=${delta12m_c3}-1.96*${delta12se_c3}  F8=${delta12m_c3}+1.96*${delta12se_c3}
putexcel A9="c3"  B9="2"  C9=${delta23m_c3}  D9=${delta23se_c3}  E9=${delta23m_c3}-1.96*${delta23se_c3}  F9=${delta23m_c3}+1.96*${delta23se_c3}
putexcel A10="c3" B10="3" C10=${delta31m_c3} D10=${delta31se_c3} E10=${delta31m_c3}-1.96*${delta31se_c3} F10=${delta31m_c3}+1.96*${delta31se_c3}

putexcel A11="c4" B11="1" C11=${delta12m_c4} D11=${delta12se_c4} E11=${delta12m_c4}-1.96*${delta12se_c4} F11=${delta12m_c4}+1.96*${delta12se_c4}
putexcel A12="c4" B12="2" C12=${delta23m_c4} D12=${delta23se_c4} E12=${delta23m_c4}-1.96*${delta23se_c4} F12=${delta23m_c4}+1.96*${delta23se_c4}
putexcel A13="c4" B13="3" C13=${delta31m_c4} D13=${delta31se_c4} E13=${delta31m_c4}-1.96*${delta31se_c4} F13=${delta31m_c4}+1.96*${delta31se_c4}

import excel "/Users/nakajimaemiko/Desktop/Harvard Year3/AGE Mexico/new analysis/data/diff.xls", sheet("Sheet1") firstrow clear
destring x, replace


// c1
twoway (scatter coef x if y=="c1", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="c1"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.12 0.12)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.18 1 "Double grant -") ///
	   text(-0.2 1 "Information") ///
	   text(-0.18 2 "Information -") ///
	   text(-0.2 2 "Single grant") ///
	   text(-0.18 3 "Single grant -") ///
	   text(-0.2 3 "Double grant") /// 
	   legend(off)
	   graph export "c1_diff.pdf", replace
	   
// c2
twoway (scatter coef x if y=="c2", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="c2"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.25 0.25)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.25 1 "Double grant -") ///
	   text(-0.28 1 "Information") ///
	   text(-0.25 2 "Information -") ///
	   text(-0.28 2 "Single grant") ///
	   text(-0.25 3 "Single grant -") ///
	   text(-0.28 3 "Double grant") /// 
	   legend(off)
	   graph export "c2_diff.pdf", replace		
			
// c3
twoway (scatter coef x if y=="c3", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="c3"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-2.4 2.4)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-2.4 1 "Double grant -") ///
	   text(-2.6 1 "Information") ///
	   text(-2.4 2 "Information -") ///
	   text(-2.6 2 "Single grant") ///
	   text(-2.4 3 "Single grant -") ///
	   text(-2.6 3 "Double grant") /// 
	   legend(off)
	   graph export "c3_diff.pdf", replace	
	   
// c4
twoway (scatter coef x if y=="c4", msymbol(S) msize(medium)) ///
       (rcap ll ul x  if y=="c4"), ///
	   xscale(range(0 4)) xlabel(none, nogrid) xtitle(" ") ///
	   yscale(range(-0.35 0.35)) ylabel(, nogrid) ///
	   ytitle("Difference in treatment effect") ///
	   yline(0, lpattern(dash)) ///
	   text(-0.3 1 "Double grant -") ///
	   text(-0.33 1 "Information") ///
	   text(-0.3 2 "Information -") ///
	   text(-0.33 2 "Single grant") ///
	   text(-0.3 3 "Single grant -") ///
	   text(-0.33 3 "Double grant") /// 
	   legend(off)
	   graph export "c4_diff.pdf", replace	   

