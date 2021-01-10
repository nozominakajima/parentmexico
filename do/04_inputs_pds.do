
* Analysis: Post Double Selection (PDS) LASSO for inputs

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
* Data preparation 
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

u "$data/panel_student_long_clean.dta", clear

merge m:1 cct year grado using "$data/panel_teacher_long_clean.dta"

drop if _merge == 2

tab exp_1 year if _merge == 1
tab exp_2 year if _merge == 1

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


order estado-meanc42009 grado_3Xgrado_2-mb28Xmb36a c1 c2 c3 c4


*------------------------------------------------------------------------------*
* Post-double selection LASSO - plugin lambda
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 1 2 3 4 {

lasso linear c`x' grado_2-mb36aXmb34a, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
}

	
// lasso step 2

lasso linear exp_3 grado_2-mb36aXmb34a, selection(plugin) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	macro list _B
	
	
// lasso step 3 - main
	
foreach x in 1 2 3 4 {
	
reg c`x' exp_3 `A`x'' `B', robust cluster(cct)
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

lasso linear c`x' grado_2-mb36aXmb34a, selection(cv, folds(3)) rseed(02472)	
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2
lasso linear exp_3 grado_2-mb36aXmb34a, selection(cv, folds(3)) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	
	
// lasso step 3 - main
	
foreach x in 1 2 3 4{
	
reg c`x' exp_3 `A`x'' `B', robust cluster(cct)
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

lasso linear c`x' grado_2-mb36aXmb34a, selection(cv, folds(3) serule) rseed(02472)	
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2
lasso linear exp_3 grado_2-mb36aXmb34a, selection(cv, folds(3) serule) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	
	
// lasso step 3 - main
	
foreach x in 1 2 3 4 {
	
reg c`x' exp_3 `A`x'' `B', robust cluster(cct)
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
	
lasso linear c`x' grado_2-mb36aXmb34a, selection(adaptive, folds(3)) rseed(02472)	
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_A`x' `: word count `A`x'''
}

	
// lasso step 2
lasso linear exp_3 grado_2-mb36aXmb34a, selection(adaptive, folds(3)) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	local length_B `: word count `B''
	
	
// lasso step 3 - main
	
foreach x in 1 2 3 4 {
	
reg c`x' exp_3 `A`x'' `B', robust cluster(cct)
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
	
mat M5 = (PDS1 \ CV1 \ SE1 \ AD1)
mat M6 = (PDS2 \ CV2 \ SE2 \ AD2)
mat M7 = (PDS3 \ CV3 \ SE3 \ AD3)
mat M8 = (PDS4 \ CV4 \ SE4 \ AD4)
	
mat list M5
mat list M6
mat list M7
mat list M8	


*------------------------------------------------------------------------------*
* Figure
*------------------------------------------------------------------------------*

coefplot (matrix(M5[,1]), ci((M5[,3] M5[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.2 (0.1) 0.2, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off) 
	graph export "input1_pds.pdf", replace

coefplot (matrix(M6[,1]), ci((M6[,3] M6[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.2 (0.1) 0.2, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off)
	graph export "input2_pds.pdf", replace	

coefplot (matrix(M7[,1]), ci((M7[,3] M7[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-3 (1) 3, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off)
	graph export "input3_pds.pdf", replace	

coefplot (matrix(M8[,1]), ci((M8[,3] M8[,4])) ///
		  msymbol(O) mlcolor(black) mfcolor(white) ///
		  ciopts(lcolor(black))) ///
		  , byopts(xrescale legend(off)) xline(0) ///
	     coeflabels(, notick labsize(medlarge)) ///
		 xlab(-0.4 (0.1) 0.4, nogrid labsize(large)) ///
		 ylab(,nogrid labsize(large)) norescaling legend(off)
	graph export "input4_pds.pdf", replace	

	
