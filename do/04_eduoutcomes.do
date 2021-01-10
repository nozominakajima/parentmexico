
* Analysis: Treatment effect on educational outcomes

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
** Experiment 1
*------------------------------------------------------------------------------*  

foreach var of varlist ed1 ed2 ed3 {

foreach year in 2008 2009 2010 {

// main
reg `var' i.exp_1 mean`var'2007 if year == `year', robust cluster(cct)
  global N_`var'`year'_1 = e(N)
  global b_`var'`year'_1: di %6.3fc _b[1.exp_1]
  global se_`var'`year'_1: di %-4.3fc _se[1.exp_1]
  
  test 1.exp_1 = 0
  global p_`var'`year'_1: di %12.3fc r(p)
  global star_`var'`year'_1 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  summ `var' if e(sample) & exp_1 == 0 
  global cm_`var'`year'_1: di %6.3fc r(mean)  
}
}


* multiple hypothesis testing

foreach year in 2008 2009 2010 {
	
// main
 wyoung, cmd( ///
 "reg ed1 exp_1 meaned12007 if year == `year' & drop == 0, robust cluster(cct)" ///
 "reg ed2 exp_1 meaned22007 if year == `year' & drop == 0, robust cluster(cct)" ///
 "reg ed3 exp_1 meaned32007 if year == `year' & drop == 0, robust cluster(cct)") ///
 cluster(cct) familyp(exp_1) bootstraps(100) seed(02472)

 mat A = r(table)
 
 foreach x in 1 2 3 {
	scalar B = A[`x',4]
	global wy_ed`x'`year'_1: di %12.3fc B  
 }
 
} 


*------------------------------------------------------------------------------*  
** Experiment 2
*------------------------------------------------------------------------------*  

foreach var of varlist ed1 ed2 ed3 {

// main
reg `var' i.exp_2 mean`var'2009 if year == 2010, robust cluster(cct)
  global N_`var'2010_2 = e(N)
  global b_`var'2010_2: di %6.3fc _b[1.exp_2]
  global se_`var'2010_2: di %-4.3fc _se[1.exp_2]
  
  test 1.exp_2 = 0
  global p_`var'2010_2: di %12.3fc r(p)
  global star_`var'2010_2 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  summ `var' if e(sample) & exp_2 == 0 
  global cm_`var'2010_2: di %6.3fc r(mean)
  
}  


* multiple hypothesis testing
	
// main
 wyoung, cmd( ///
 "reg ed1 exp_2 meaned12009 if year == 2010 & drop == 0, robust cluster(cct)" ///
 "reg ed2 exp_2 meaned22009 if year == 2010 & drop == 0, robust cluster(cct)" ///
 "reg ed3 exp_2 meaned32009 if year == 2010 & drop == 0, robust cluster(cct)") ///
 cluster(cct) familyp(exp_2) bootstraps(100) seed(02472)

 mat A = r(table)
 
 foreach x in 1 2 3 {
	scalar B = A[`x',4]
	global wy_ed`x'2010_2: di %12.3fc B  
 }
 


*------------------------------------------------------------------------------*
** Experiment 3
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


* Estimation
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 1 2 3 {
	
lasso linear ed`x' teachedu_col-pb27Xpb21, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
}
	
	
// lasso step 2

lasso linear exp_3 teachedu_col-pb27Xpb21, selection(plugin) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	macro list _B
	
	
// lasso step 3 - main
	
foreach x in 1 2 3 {
	
reg ed`x' i.exp_3 `A`x'' `B', robust cluster(cct)
  
  global N_ed`x'2010_3 = e(N)
  global b_ed`x'2010_3: di %6.3fc _b[1.exp_3]
  global se_ed`x'2010_3: di %-4.3fc _se[1.exp_3]
  
  test 1.exp_3 = 0
  global p_ed`x'2010_3: di %12.3fc r(p)
  global star_ed`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  summ ed`x' if e(sample) & exp_3 == 0
  global cm_ed`x'2010_3: di %6.3fc r(mean)
}


  
* multiple hypothesis testing
		
// main
 wyoung, cmd( ///
 "reg ed1 exp_3 `A1' `B', robust cluster(cct)" ///
 "reg ed2 exp_3 `A2' `B', robust cluster(cct)" ///
 "reg ed3 exp_3 `A3' `B', robust cluster(cct)") ///
 cluster(cct) familyp(exp_3) bootstraps(100) seed(02472)

 mat A = r(table)
 
 foreach x in 1 2 3 {
	scalar B = A[`x',4]
	global wy_ed`x'2010_3: di %12.3fc B  
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
** Experiment 1
*------------------------------------------------------------------------------*  

foreach var of varlist ed4 ed5 ed6 {

foreach year in 2008 2009 2010 {

// main
reg `var' i.exp_1 i.grado female zwealth mothered mean`var'2007 female_m zwealth_m mothered_m if year == `year' & drop == 0, robust cluster(cct)
  global N_`var'`year'_1 = e(N)
  global b_`var'`year'_1: di %6.3fc _b[1.exp_1]
  global se_`var'`year'_1: di %-4.3fc _se[1.exp_1]
  
  test 1.exp_1 = 0
  global p_`var'`year'_1: di %12.3fc r(p)
  global star_`var'`year'_1 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
 
  summ `var' if e(sample) & exp_1 == 0 
  global cm_`var'`year'_1: di %6.3fc r(mean)
  
  
// indigenous (ap03 == 1)
reg `var' i.exp_1##i.ap03 i.grado female zwealth mothered mean`var'2007 female_m zwealth_m mothered_m ap03_m if year == `year' & drop == 0, robust cluster(cct)
  matrix A = r(table)
  
  global b1_i`var'`year'_1: di %12.3fc _b[1.exp_1]
  global se1_i`var'`year'_1: di %-4.3fc _se[1.exp_1]
  
  global b2_i`var'`year'_1: di %12.3fc _b[1.exp_1#1.ap03]
  global se2_i`var'`year'_1: di %-4.3fc _se[1.exp_1#1.ap03]
  
  test 1.exp_1 = 0
  global p1_i`var'`year'_1: di %12.3fc r(p)
  global star1_i`var'`year'_1 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  test 1.exp_1#1.ap03 = 0
  global p2_i`var'`year'_1: di %12.3fc r(p)
  global star2_i`var'`year'_1 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))

     
}
}


* multiple hypothesis testing

foreach year in 2008 2009 2010 {
	
// main
 wyoung, cmd( ///
 "reg ed4 exp_1 female zwealth mothered meaned42007 female_m zwealth_m mothered_m if year == `year' & drop == 0, robust cluster(cct)" ///
 "reg ed5 exp_1 female zwealth mothered meaned52007 female_m zwealth_m mothered_m if year == `year' & drop == 0, robust cluster(cct)" ///
 "reg ed6 exp_1 female zwealth mothered meaned62007 female_m zwealth_m mothered_m if year == `year' & drop == 0, robust cluster(cct)") ///
 cluster(cct) familyp(exp_1) bootstraps(100) seed(02472)

 mat A = r(table)
 
 foreach x in 4 5 6{
	scalar B = A[`x'-3,4]
	global wy_ed`x'`year'_1: di %12.3fc B  
 }
 
} 
  
  
*------------------------------------------------------------------------------*  
** Experiment 2
*------------------------------------------------------------------------------*  

foreach var of varlist ed4 ed5 ed6{

// main
reg `var' i.exp_2 i.grado female zwealth mothered mean`var'2009 female_m zwealth_m mothered_m if year == 2010, robust cluster(cct)
  global N_`var'2010_2 = e(N)
  global b_`var'2010_2: di %6.3fc _b[1.exp_2]
  global se_`var'2010_2: di %-4.3fc _se[1.exp_2]
  
  test 1.exp_2 = 0
  global p_`var'2010_2: di %12.3fc r(p)
  global star_`var'2010_2 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  summ `var' if e(sample) & exp_2 == 0 
  global cm_`var'2010_2: di %6.3fc r(mean)
  
// indigenous (ap03 == 1)  
reg `var' i.exp_2##i.ap03 i.grado female zwealth mothered mean`var'2009 female_m zwealth_m mothered_m ap03_m if year == 2010 & drop == 0, robust cluster(cct)
  matrix A = r(table)
  
  global b1_i`var'2010_2: di %12.3fc _b[1.exp_2]
  global se1_i`var'2010_2: di %-4.3fc _se[1.exp_2]
  
  global b2_i`var'2010_2: di %12.3fc _b[1.exp_2#1.ap03]
  global se2_i`var'2010_2: di %-4.3fc _se[1.exp_2#1.ap03]
  
  test 1.exp_2 = 0
  global p1_i`var'2010_2: di %12.3fc r(p)
  global star1_i`var'2010_2 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  test 1.exp_2#1.ap03 = 0
  global p2_i`var'2010_2: di %12.3fc r(p)
  global star2_i`var'2010_2 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))

  }  
  

* multiple hypothesis testing

// main
 wyoung, cmd( ///
 "reg ed4 exp_2 grado female zwealth mothered meaned42009 if year == 2010 & drop == 0, robust cluster(cct)" ///
 "reg ed5 exp_2 grado female zwealth mothered meaned52009 if year == 2010 & drop == 0, robust cluster(cct)" ///
 "reg ed6 exp_2 grado female zwealth mothered meaned62009 if year == 2010 & drop == 0, robust cluster(cct)") ///
 cluster(cct) familyp(exp_2) bootstraps(100) seed(02472)

 mat A = r(table)
 
 foreach x in 4 5 6{
	scalar B = A[`x'-3,4]
	global wy_ed`x'2010_2: di %12.3fc B  
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
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 4 5 6 {
	
lasso linear ed`x' grado_2-mb28Xmb36a, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
}
	
	
// lasso step 2

lasso linear exp_3 grado_2-mb28Xmb36a, selection(plugin) rseed(02472)
	local B = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	
	
// lasso step 3 - main
	
foreach x in 4 5 6 {
	
reg ed`x' i.exp_3 `A`x'' `B', robust cluster(cct)
  
  global N_ed`x'2010_3 = e(N)
  global b_ed`x'2010_3: di %6.3fc _b[1.exp_3]
  global se_ed`x'2010_3: di %-4.3fc _se[1.exp_3]
  
  test 1.exp_3 = 0
  global p_ed`x'2010_3: di %12.3fc r(p)
  global star_ed`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  summ ed`x' if e(sample) & exp_3 == 0
  global cm_ed`x'2010_3: di %6.3fc r(mean)
}


// lasso step 3 - indigenous (ap03 == 1)  
foreach x in 4 5 6 {
	
reg ed`x' i.exp_3##i.ap03 `A`x'' `B', robust cluster(cct)
  matrix A = r(table)
  
  global b1_ied`x'2010_3: di %12.3fc A[1,2]
  global se1_ied`x'2010_3: di %-4.3fc A[2,2]
  
  global b2_ied`x'2010_3: di %12.3fc A[1,8]
  global se2_ied`x'2010_3: di %-4.3fc A[2,8]
  
  test 1.exp_3 = 0
  global p1_ied`x'2010_3: di %12.3fc r(p)
  global star1_ied`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  test 1.exp_3#1.ap03 = 0
  global p2_ied`x'2010_3: di %12.3fc r(p)
  global star2_ied`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 
 
}
  
  		
* multiple hypothesis testing

// main
 wyoung, cmd( ///
 "reg ed4 exp_3 `A4' `B', robust cluster(cct)" ///
 "reg ed5 exp_3 `A5' `B', robust cluster(cct)" ///
 "reg ed6 exp_3 `A6' `B', robust cluster(cct)") ///
 cluster(cct) familyp(exp_3) bootstraps(100) seed(02472)

 mat A = r(table)
 
 foreach x in 4 5 6 {
	scalar B = A[`x'-3,4]
	global wy_ed`x'2010_3: di %12.3fc B  
 }



*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*	Table: Educational outcomes
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

texdoc init eduoutcomes.tex, replace force

tex \begin{tabular}{lcccccc} \toprule

tex  & \multicolumn{3}{c}{School-level} & \multicolumn{3}{c}{Student-level} \\ \cmidrule(l{5pt}r{5pt}){2-4} \cmidrule(l{5pt}r{5pt}){5-7}

tex  & (1) & (2) & (3) & (4) & (5) & (6) \\ 
tex  & \shortstack{Failure\\rate} & \shortstack{Dropout\\rate} & \shortstack{Repetition\\rate} & \shortstack{Spanish\\test (S.D.)} & \shortstack{Math\\test (S.D.)} & \shortstack{Disciplinary\\action}\\ \hline \hline
	
	
tex \multicolumn{4}{l}{\textit{Double grant experiment}} \\ 
	
tex {Treatment (1 year)} & ${b_ed12008_1}${star_ed12008_1} & ${b_ed22008_1}${star_ed22008_1} & ${b_ed32008_1}${star_ed32008_1} & ${b_ed42008_1}${star_ed42008_1} & ${b_ed52008_1}${star_ed52008_1} & ${b_ed62008_1}${star_ed62008_1} \\

tex & (${se_ed12008_1}) & (${se_ed22008_1}) & (${se_ed32008_1}) & (${se_ed42008_1}) & (${se_ed52008_1}) & (${se_ed62008_1}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_ed12008_1} & ${wy_ed22008_1} & ${wy_ed32008_1} & ${wy_ed42008_1} & ${wy_ed52008_1} & ${wy_ed62008_1} \\[0.1cm]


tex {Treatment (2 year)} & ${b_ed12009_1}${star_ed12009_1} & ${b_ed22009_1}${star_ed22009_1} & ${b_ed32009_1}${star_ed32009_1} & ${b_ed42009_1}${star_ed42009_1} & ${b_ed52009_1}${star_ed52009_1} & ${b_ed62009_1}${star_ed62009_1} \\

tex & (${se_ed12009_1}) & (${se_ed22009_1}) & (${se_ed32009_1}) & (${se_ed42009_1}) & (${se_ed52009_1}) & (${se_ed62009_1}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_ed12009_1} & ${wy_ed22009_1} & ${wy_ed32009_1} & ${wy_ed42009_1} & ${wy_ed52009_1} & ${wy_ed62009_1} \\[0.1cm]


tex {Treatment (3 year)} & ${b_ed12010_1}${star_ed12010_1} & ${b_ed22010_1}${star_ed22010_1} & ${b_ed32010_1}${star_ed32010_1} & ${b_ed42010_1}${star_ed42010_1} & ${b_ed52010_1}${star_ed52010_1} & ${b_ed62010_1}${star_ed62010_1} \\

tex & (${se_ed12010_1}) & (${se_ed22010_1}) & (${se_ed32010_1}) & (${se_ed42010_1}) & (${se_ed52010_1}) & (${se_ed62010_1}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_ed12010_1} & ${wy_ed22010_1} & ${wy_ed32010_1} & ${wy_ed42010_1} & ${wy_ed52010_1} & ${wy_ed62010_1} \\[0.1cm]


tex Control mean & ${cm_ed12008_1} & ${cm_ed22008_1} & ${cm_ed32008_1} & ${cm_ed42008_1} & ${cm_ed52008_1} & ${cm_ed62008_1} \\

tex Observations & ${N_ed12008_1} & ${N_ed22008_1} & ${N_ed32008_1} & ${N_ed42008_1} & ${N_ed52008_1}  & ${N_ed62008_1} \\


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Information experiment}} \\ 

tex {Treatment (1 year)} & ${b_ed12010_2}${star_ed12010_2} & ${b_ed22010_2}${star_ed22010_2} & ${b_ed32010_2}${star_ed32010_2} & ${b_ed42010_2}${star_ed42010_2} & ${b_ed52010_2}${star_ed52010_2} & ${b_ed62010_2}${star_ed62010_2} \\

tex & (${se_ed12010_2}) & (${se_ed22010_2}) & (${se_ed32010_2}) & (${se_ed42010_2}) & (${se_ed52010_2}) & (${se_ed62010_2}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_ed12010_2} & ${wy_ed22010_2} & ${wy_ed32010_2} & ${wy_ed42010_2} & ${wy_ed52010_2} & ${wy_ed62010_2} \\[0.1cm]

tex Control mean & ${cm_ed12010_2} & ${cm_ed22010_2} & ${cm_ed32010_2} & ${cm_ed42010_2} & ${cm_ed52010_2} & ${cm_ed62010_2} \\

tex Observations & ${N_ed12010_2} & ${N_ed22010_2} & ${N_ed32010_2} & ${N_ed42010_2} & ${N_ed52010_2}  & ${N_ed62010_2} \\


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Single grant observation}} \\ 
	
tex {Treatment (1 year)} & ${b_ed12010_3}${star_ed12010_3} & ${b_ed22010_3}${star_ed22010_3} & ${b_ed32010_3}${star_ed32010_3} & ${b_ed42010_3}${star_ed42010_3} & ${b_ed52010_3}${star_ed52010_3} & ${b_ed62010_3}${star_ed62010_3} \\

tex & (${se_ed12010_3}) & (${se_ed22010_3}) & (${se_ed32010_3}) & (${se_ed42010_3}) & (${se_ed52010_3}) & (${se_ed62010_3}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_ed12010_3} & ${wy_ed22010_3} & ${wy_ed32010_3} & ${wy_ed42010_3} & ${wy_ed52010_3} & ${wy_ed62010_3} \\[0.1cm]

tex Control mean & ${cm_ed12010_3} & ${cm_ed22010_3} & ${cm_ed32010_3} & ${cm_ed42010_3} & ${cm_ed52010_3} & ${cm_ed62010_3} \\

tex Observations & ${N_ed12010_3} & ${N_ed22010_3} & ${N_ed32010_3} & ${N_ed42010_3} & ${N_ed52010_3}  & ${N_ed62010_3} \\


tex \hline

tex \end{tabular}

texdoc close 


*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*	Table: Table: Treatment x Indigenous (only for student level)
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

texdoc init eduoutcomes_het.tex, replace force

tex \begin{tabular}{lccc} \toprule

tex  & (1) & (2) & (3)\\ 
tex  & \shortstack{Spanish\\test (S.D.)} & \shortstack{Math\\test (S.D.)} & \shortstack{Disciplinary\\action}\\ \hline \hline
	
	
tex \multicolumn{4}{l}{\textit{Double grant experiment}} \\ 
	
tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ied42008_1}${star2_ied42008_1} & ${b2_ied52008_1}${star2_ied52008_1} & ${b2_ied62008_1}${star2_ied62008_1} \\

tex & (${se2_ied42008_1}) & (${se2_ied52008_1}) & (${se2_ied62008_1}) \\[0.1cm]


tex \multicolumn{4}{l}{(2 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ied42009_1}${star2_ied42009_1} & ${b2_ied52009_1}${star2_ied52009_1} & ${b2_ied62009_1}${star2_ied62009_1} \\

tex & (${se2_ied42009_1}) & (${se2_ied52009_1}) & (${se2_ied62009_1}) \\[0.1cm]


tex \multicolumn{4}{l}{(3 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ied42010_1}${star2_ied42010_1} & ${b2_ied52010_1}${star2_ied52010_1} & ${b2_ied62010_1}${star2_ied62010_1} \\

tex & (${se2_ied42010_1}) & (${se2_ied52010_1}) & (${se2_ied62010_1}) \\[0.1cm]


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Information experiment}} \\ 

tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ied42010_2}${star2_ied42010_2} & ${b2_ied52010_2}${star2_ied52010_2} & ${b2_ied62010_2}${star2_ied62010_2} \\

tex & (${se2_ied42010_2}) & (${se2_ied52010_2}) & (${se2_ied62010_2}) \\[0.1cm]


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Single grant observation}} \\ 
	
tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ied42010_3}${star2_ied42010_3} & ${b2_ied52010_3}${star2_ied52010_3} & ${b2_ied62010_3}${star2_ied62010_3} \\

tex & (${se2_ied42010_3}) & (${se2_ied52010_3}) & (${se2_ied62010_3}) \\[0.1cm]

tex \hline

tex \end{tabular}
