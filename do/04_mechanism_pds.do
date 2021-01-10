
* Analysis: Post Double Selection (PDS) LASSO for mechanisms

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
* School-level data
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

*------------------------------------------------------------------------------*
* Post-double selection LASSO - plugin lambda
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 1 2 {

lasso linear mech`x' pb03-mech22009, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2

lasso linear exp_3 pb03-mech22009, selection(plugin) rseed(02472)
	local B `e(allvars_sel)'
	local length_B `: word count `B''
	macro list _B
	
	
// lasso step 3 - main
	
foreach x in 1 2 {
	
reg mech`x' exp_3 `A`x'' `B', robust cluster(cct)
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

foreach x in 1 2 {

lasso linear mech`x' pb03-mech22009, selection(cv) rseed(02472)	
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2
lasso linear exp_3 pb03-mech22009, selection(cv) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	
	
// lasso step 3 - main
	
foreach x in 1 2 {
	
reg mech`x' exp_3 `A`x'' `B', robust cluster(cct)
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

foreach x in 1 2 {

lasso linear mech`x' pb03-mech22009, selection(cv, serule) rseed(02472)	
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2
lasso linear exp_3 pb03-mech22009, selection(cv, serule) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	
	
// lasso step 3 - main
	
foreach x in 1 2 {
	
reg mech`x' exp_3 `A`x'' `B', robust cluster(cct)
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

foreach x in 1 2 {
	
lasso linear mech`x' pb03-mech22009, selection(adaptive) rseed(02472)	
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2
lasso linear exp_3 pb03-mech22009, selection(adaptive) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	
	
// lasso step 3 - main
	
foreach x in 1 2 {
	
reg mech`x' exp_3 `A`x'' `B', robust cluster(cct)
  local length = cond(`e(F)' == ., `e(df_m)', `e(df_m)'-1)
 
* save results
mat list r(table)
mat AD`x' = r(table)[1,1], r(table)[2,1], r(table)[5,1], r(table)[6,1], `length'
mat rown AD`x' = "adaptive (`length')"
mat coln AD`x' = b se ll ul ncontrol
mat list AD`x'
   
}


*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
** Teacher-level data
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


*------------------------------------------------------------------------------*
* Post-double selection LASSO - plugin lambda
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 3 4 {

lasso linear mech`x' grado_2-teacheredu_uniXteacheredu_col, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2

lasso linear exp_3 grado_2-teacheredu_uniXteacheredu_col, selection(plugin) rseed(02472)
	local B `e(allvars_sel)'
	local length_B `: word count `B''
	macro list _B
	
	
// lasso step 3 - main
	
foreach x in 3 4 {
	
reg mech`x' exp_3 `A`x'' `B', robust cluster(cct)
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

foreach x in 3 4 {

lasso linear mech`x' grado_2-teacheredu_uniXteacheredu_col, selection(cv) rseed(02472)	
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2
lasso linear exp_3 grado_2-teacheredu_uniXteacheredu_col, selection(cv) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	
	
// lasso step 3 - main
	
foreach x in 3 4 {
	
reg mech`x' exp_3 `A`x'' `B', robust cluster(cct)
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

foreach x in 3 4 {

lasso linear mech`x' grado_2-teacheredu_uniXteacheredu_col, selection(cv, serule) rseed(02472)	
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2
lasso linear exp_3 grado_2-teacheredu_uniXteacheredu_col, selection(cv, serule) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	
	
// lasso step 3 - main
	
foreach x in 3 4 {
	
reg mech`x' exp_3 `A`x'' `B', robust cluster(cct)
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

foreach x in 3 4 {
	
lasso linear mech`x' grado_2-teacheredu_uniXteacheredu_col, selection(adaptive) rseed(02472)	
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2
lasso linear exp_3 grado_2-teacheredu_uniXteacheredu_col, selection(adaptive) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	
	
// lasso step 3 - main
	
foreach x in 3 4 {
	
reg mech`x' exp_3 `A`x'' `B', robust cluster(cct)
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
	
mat M15 = (PDS1 \ CV2 \ SE1 \ AD1)	
mat M16 = (PDS2 \ CV2 \ SE2 \ AD2)	
mat M17 = (PDS3 \ CV3 \ SE3 \ AD3)	
mat M18 = (PDS4 \ CV4 \ SE4 \ AD4)	


*------------------------------------------------------------------------------*
* Figure
*------------------------------------------------------------------------------*
	
coefplot (matrix(M15[,1]), ci((M15[,3] M15[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-1 (0.5) 1, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off) 
	graph export "mech1_pds.pdf", replace
	
coefplot (matrix(M16[,1]), ci((M16[,3] M16[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-1 (0.5) 1, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off) 
	graph export "mech2_pds.pdf", replace	
	
coefplot (matrix(M17[,1]), ci((M17[,3] M17[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.4 (0.2) 0.4, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off) 
	graph export "mech3_pds.pdf", replace

coefplot (matrix(M18[,1]), ci((M18[,3] M18[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-1 (0.5) 1, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off) 
	graph export "mech4_pds.pdf", replace
		 

