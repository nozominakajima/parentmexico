
* Analysis: Post Double Selection (PDS) LASSO for educational outcomes

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
* School-level outcomes
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

*------------------------------------------------------------------------------*  

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



*------------------------------------------------------------------------------*
* Post-double selection LASSO - plugin lambda
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 1 2 3 {
	
lasso linear ed`x' teachedu_col-pb27Xpb21, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

// lasso step 2

lasso linear exp_3 teachedu_col-pb27Xpb21, selection(plugin) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	macro list _B

// lasso step 3 - main
	
foreach x in 1 2 3 {
	
reg ed`x' exp_3 `A`x'' `B', robust cluster(cct)
  local length = cond(`e(F)' == ., `e(df_m)', `e(df_m)'-1)

* save results
mat list r(table)
mat PDS`x' = r(table)[1,1], r(table)[2,1], r(table)[5,1], r(table)[6,1], `length'
mat rown PDS`x' = "plug-in (`length')"
mat coln PDS`x' = b se ll ul ncontrol
mat list PDS`x'
   
}


*------------------------------------------------------------------------------*
* Post-double selection LASSO - cv lambda
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 1 2 3 {
	
lasso linear ed`x' teachedu_col-pb27Xpb21, selection(cv, folds(10)) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

// lasso step 2

lasso linear exp_3 teachedu_col-pb27Xpb21, selection(cv, folds(10)) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	macro list _B

// lasso step 3 - main
	
foreach x in 1 2 3 {
	
reg ed`x' exp_3 `A`x'' `B', robust cluster(cct)
  local length = cond(`e(F)' == ., `e(df_m)', `e(df_m)'-1)

* save results
mat list r(table)
mat CV`x' = r(table)[1,1], r(table)[2,1], r(table)[5,1], r(table)[6,1], `length'
mat rown CV`x' = "cv lambda (`length')"
mat coln CV`x' = b se ll ul ncontrol
mat list CV`x'
   
}


*------------------------------------------------------------------------------*
* Post-double selection LASSO - lambda1se
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 1 2 3 {
	
lasso linear ed`x' teachedu_col-pb27Xpb21, selection(cv, serule) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

// lasso step 2

lasso linear exp_3 teachedu_col-pb27Xpb21, selection(cv, serule) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	macro list _B

// lasso step 3 - main
	
foreach x in 1 2 3 {
	
reg ed`x' exp_3 `A`x'' `B', robust cluster(cct)
  local length = cond(`e(F)' == ., `e(df_m)', `e(df_m)'-1)

* save results
mat list r(table)
mat SE`x' = r(table)[1,1], r(table)[2,1], r(table)[5,1], r(table)[6,1], `length'
mat rown SE`x' = "1 se lambda (`length')"
mat coln SE`x' = b se ll ul ncontrol
mat list SE`x'
   
}


*------------------------------------------------------------------------------*	
* Post-double selection LASSO - adaptive 
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 1 2 3 {
	
lasso linear ed`x' teachedu_col-pb27Xpb21, selection(adaptive) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

// lasso step 2

lasso linear exp_3 teachedu_col-pb27Xpb21, selection(adaptive) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	macro list _B

// lasso step 3 - main
	
foreach x in 1 2 3 {
	
reg ed`x' exp_3 `A`x'' `B', robust cluster(cct)
  local length = cond(`e(F)' == ., `e(df_m)', `e(df_m)'-1)

* save results
mat list r(table)
mat AD`x' = r(table)[1,1], r(table)[2,1], r(table)[5,1], r(table)[6,1], `length'
mat rown AD`x' = "adaptive (`length')"
mat coln AD`x' = b se ll ul ncontrol
mat list AD`x'
   
}

*------------------------------------------------------------------------------*
* Combine results
*------------------------------------------------------------------------------*
	
mat M9 = (PDS1 \ CV1 \ SE1 \ AD1)
mat M10 = (PDS2 \ CV2 \ SE2 \ AD2)
mat M11 = (PDS3 \ CV3 \ SE3 \ AD3)	


*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
** Student-level outcomes
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

*------------------------------------------------------------------------------*
* Post-double selection LASSO - plugin lambda
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 4 5 6 {
	
lasso linear ed`x' grado_2-mb28Xmb36a, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}
	
	
// lasso step 2

lasso linear exp_3 grado_2-mb28Xmb36a, selection(plugin) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	macro list _B
	
	
// lasso step 3 - main
	
foreach x in 4 5 6 {
	
reg ed`x' exp_3 `A`x'' `B', robust cluster(cct)
  local length = cond(`e(F)' == ., `e(df_m)', `e(df_m)'-1)

* save results
mat list r(table)
mat PDS`x' = r(table)[1,1], r(table)[2,1], r(table)[5,1], r(table)[6,1], `length'
mat rown PDS`x' = "plug-in (`length')"
mat coln PDS`x' = b se ll ul ncontrol
mat list PDS`x'
   
}


*------------------------------------------------------------------------------*
* Post-double selection LASSO - cv lambda
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 4 5 6 {
	
lasso linear ed`x' grado_2-mb28Xmb36a, selection(cv, folds(3)) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}
	
	
// lasso step 2

lasso linear exp_3 grado_2-mb28Xmb36a, selection(cv, folds(3)) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	macro list _B
	
	
// lasso step 3 - main
	
foreach x in 4 5 6 {
	
reg ed`x' exp_3 `A`x'' `B', robust cluster(cct)
  local length = cond(`e(F)' == ., `e(df_m)', `e(df_m)'-1)

* save results
mat list r(table)
mat CV`x' = r(table)[1,1], r(table)[2,1], r(table)[5,1], r(table)[6,1], `length'
mat rown CV`x' = "cv lambda (`length')"
mat coln CV`x' = b se ll ul ncontrol
mat list CV`x'
   
}


*------------------------------------------------------------------------------*
* Post-double selection LASSO - lambda1se
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 4 5 6 {
	
lasso linear ed`x' grado_2-mb28Xmb36a, selection(cv, folds(3) serule) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}
	
	
// lasso step 2

lasso linear exp_3 grado_2-mb28Xmb36a, selection(cv, folds(3) serule) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	macro list _B
	
	
// lasso step 3 - main
	
foreach x in 4 5 6 {
	
reg ed`x' exp_3 `A`x'' `B', robust cluster(cct)
  local length = cond(`e(F)' == ., `e(df_m)', `e(df_m)'-1)
  
* save results
mat list r(table)
mat SE`x' = r(table)[1,1], r(table)[2,1], r(table)[5,1], r(table)[6,1], `length'
mat rown SE`x' = "1 se lambda (`length')"
mat coln SE`x' = b se ll ul ncontrol
mat list SE`x'
   
}

*------------------------------------------------------------------------------*	
* Post-double selection LASSO - adaptive 
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 4 5 6{
	
lasso linear ed`x' grado_2-mb28Xmb36a, selection(adaptive, folds(3)) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}
	
	
// lasso step 2

lasso linear exp_3 grado_2-mb28Xmb36a, selection(adaptive, folds(3)) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	macro list _B
	
	
// lasso step 3 - main
	
foreach x in 4 5 6{
	
reg ed`x' exp_3 `A`x'' `B', robust cluster(cct)
  local length = cond(`e(F)' == ., `e(df_m)', `e(df_m)'-1)

* save results
mat list r(table)
mat AD`x' = r(table)[1,1], r(table)[2,1], r(table)[5,1], r(table)[6,1], `length'
mat rown AD`x' = "adaptive (`length')"
mat coln AD`x' = b se ll ul ncontrol
mat list AD`x'
   
}

*------------------------------------------------------------------------------*
* Combine results
*------------------------------------------------------------------------------*
	
mat M12 = (PDS4 \ CV4 \ SE4 \ AD4)	
mat M13 = (PDS5 \ CV5 \ SE5 \ AD5)	
mat M14 = (PDS6 \ CV6 \ SE6 \ AD6)	


*------------------------------------------------------------------------------*
* Figure
*------------------------------------------------------------------------------*

coefplot (matrix(M9[,1]), ci((M9[,3] M9[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.05 (0.01) 0.05, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off) 
	graph export "edu1_pds.pdf", replace
	
coefplot (matrix(M10[,1]), ci((M10[,3] M10[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.03 (0.01) 0.03, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off) 
	graph export "edu2_pds.pdf", replace	
	
coefplot (matrix(M11[,1]), ci((M11[,3] M11[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.03 (0.01) 0.03, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off) 
	graph export "edu3_pds.pdf", replace

	
coefplot (matrix(M12[,1]), ci((M12[,3] M12[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.5 (0.1) 0.5, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off) 
	graph export "edu4_pds.pdf", replace

coefplot (matrix(M13[,1]), ci((M13[,3] M13[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.5 (0.1) 0.5, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off) 
	graph export "edu5_pds.pdf", replace
	
coefplot (matrix(M14[,1]), ci((M14[,3] M14[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.2 (0.05) 0.2, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off) 
	graph export "edu6_pds.pdf", replace
	
	
	
