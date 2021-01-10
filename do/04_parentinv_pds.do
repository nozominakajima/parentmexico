
* Analysis: Post Double Selection (PDS) LASSO for parental involvement

* Author: Nozomi Nakajima
* Date: Dec 2020

drop _all
clear all
set mem 30g
set maxvar 11000
set matsize 11000
set more off

set scheme plotplainblind 


tempfile a b c d e

*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
* Data preparation
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


* 	Only keep 2010 variables that are not outcomes but plausible predictors  
*------------------------------------------------------------------------------*
keep if year == 2010 & !missing(exp_3) & modalidad == "General" & drop == 0

keep estado cct modalidad exp_1 exp_2 exp_3 id_parent drop ///
pb03 pb06 pb07 pb16a pb18 pb21 pb27 pb37 pb39 ///
pb03_m pb06_m pb07_m pb16a_m pb18_m pb21_m pb27_m ///
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




*------------------------------------------------------------------------------*
* Post-double selection LASSO - plugin lambda
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 1 2 3 4 {
	
lasso linear pv`x' pb03-pb39Xpb37, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2

lasso linear exp_3 pb03-pb39Xpb37, selection(plugin) rseed(02472)
	local B `e(allvars_sel)'
	local length_B `: word count `B''
	macro list _B
	
	
// lasso step 3 - main
	
foreach x in 1 2 3 4{
	
reg pv`x' exp_3 `A`x'' `B', robust cluster(cct)
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

foreach x in 1 2 3 4 {

lasso linear pv`x' pb03-pb39Xpb37, selection(cv, folds(10)) rseed(02472)	
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2
lasso linear exp_3 pb03-pb39Xpb37, selection(cv, folds(10)) rseed(02472)
	local B `e(allvars_sel)'
	local length_B `: word count `B''
	
	
// lasso step 3 - main
	
foreach x in 1 2 3 4{
	
reg pv`x' exp_3 `A`x'' `B', robust cluster(cct)
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

foreach x in 1 2 3 4 {

lasso linear pv`x' pb03-pb39Xpb37, selection(cv, serule) rseed(02472)	
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2
lasso linear exp_3 pb03-pb39Xpb37, selection(cv, serule) rseed(02472)
	local B `e(allvars_sel)'
	local length_B `: word count `B''
	
	
// lasso step 3 - main
	
foreach x in 1 2 3 4{
	
reg pv`x' exp_3 `A`x'' `B', robust cluster(cct)
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

foreach x in 1 2 3 4 {
	
lasso linear pv`x' pb03-pb39Xpb37, selection(adaptive) rseed(02472)	
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2
lasso linear exp_3 pb03-pb39Xpb37, selection(adaptive) rseed(02472)
	local B `e(allvars_sel)'
	local length_B `: word count `B''
	
	
// lasso step 3 - main
	
foreach x in 1 2 3 4{
	
reg pv`x' exp_3 `A`x'' `B', robust cluster(cct)
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
	
foreach x in 1 2 3 4 {
		
	mat M`x' = (PDS`x' \ CV`x' \ SE`x' \ AD`x')

}
	
mat list M1
mat list M2
mat list M3
mat list M4	


*------------------------------------------------------------------------------*
* Figure
*------------------------------------------------------------------------------*

coefplot (matrix(M1[,1]), ci((M1[,3] M1[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.4 (0.1) 0.4, nogrid labsize(large)) ///
		 ylab(, nogrid labsize(large)) norescaling legend(off) 
	graph export "parentinv1_pds.pdf", replace

coefplot (matrix(M2[,1]), ci((M2[,3] M2[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.4 (0.1) 0.4, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off)
	graph export "parentinv2_pds.pdf", replace	

coefplot (matrix(M3[,1]), ci((M3[,3] M3[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.4 (0.1) 0.4, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off)
	graph export "parentinv3_pds.pdf", replace	

coefplot (matrix(M4[,1]), ci((M4[,3] M4[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-10 (2) 10, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off)
	graph export "parentinv4_pds.pdf", replace	

