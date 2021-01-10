
* Analysis: Testing mechanism

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
** Experiment 1
*------------------------------------------------------------------------------*  

foreach var of varlist mech1 mech2 {

foreach year in 2008 2009 2010 {

// main
reg `var' i.exp_1 `var'2007 if year == `year' & drop == 0, robust cluster(cct)
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
 "reg mech1 exp_1 mech12007 if year == `year' & drop == 0, robust cluster(cct)" ///
 "reg mech2 exp_1 mech22007 if year == `year' & drop == 0, robust cluster(cct)") ///
 cluster(cct) familyp(exp_1) bootstraps(50) seed(02472)

 mat A = r(table)
 
 foreach x in 1 2 {
	scalar B = A[`x',4]
	global wy_mech`x'`year'_1: di %12.3fc B  
 }
 
} 
  

*------------------------------------------------------------------------------*  
** Experiment 2
*------------------------------------------------------------------------------*  

foreach var of varlist mech1 mech2 {

// main
reg `var' i.exp_2 `var'2009 if year == 2010 & drop == 0, robust cluster(cct)
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
 "reg mech1 exp_2 mech12009 if year == 2010 & drop == 0, robust cluster(cct)" ///
 "reg mech2 exp_2 mech22009 if year == 2010 & drop == 0, robust cluster(cct)") ///
 cluster(cct) familyp(exp_2) bootstraps(50) seed(02472)

 mat A = r(table)
 
 foreach x in 1 2 {
	scalar B = A[`x',4]
	global wy_mech`x'2010_2: di %12.3fc B  
 }
 
 

*------------------------------------------------------------------------------* 
** Experiment 3
*------------------------------------------------------------------------------*  

* Only keep 2010 variables that are not outcomes but plausible predictors  

keep if year == 2010 & !missing(exp_3) & modalidad == "General" & drop == 0

keep estado cct modalidad exp_1 exp_2 exp_3 id_parent drop ///
pb03 pb06 pb07 pb16a pb18 pb21 pb27 pb37 pb39 ///
pb03_m pb06_m pb07_m pb16a_m pb18_m pb21_m pb27_m pb37_m pb39_m ///
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
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 1 2 {
	
lasso linear mech`x' pb03-mech22009, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
}
	
	
// lasso step 2

lasso linear exp_3 pb03-mech22009, selection(plugin) rseed(02472)
	local B1 = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
	
	
// lasso step 3 - main
	
foreach x in 1 2 {
	
reg mech`x' i.exp_3 `A`x'' `B1', robust cluster(cct)
  
  global N_mech`x'2010_3 = e(N)
  global b_mech`x'2010_3: di %6.3fc _b[1.exp_3]
  global se_mech`x'2010_3: di %-4.3fc _se[1.exp_3]
  
  test 1.exp_3 = 0
  global p_mech`x'2010_3: di %12.3fc r(p)
  global star_mech`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
summ mech`x' if e(sample) & exp_3 == 0
  global cm_mech`x'_3: di %6.3fc r(mean)
   
}

    
* multiple hypothesis testing
		
// main
 wyoung, cmd( ///
 "reg mech1 exp_3 `A1' `B1', robust cluster(cct)" ///
 "reg mech2 exp_3 `A2' `B1', robust cluster(cct)") ///
 cluster(cct) familyp(exp_3) bootstraps(50) seed(02472)

 mat A = r(table)
 
 foreach x in 1 2 {
	scalar B = A[`x',4]
	global wy_mech`x'2010_3: di %12.3fc B  
 }
 

   
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
* Effect on trust & blame (teacher perspective)
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
** Experiment 1
*------------------------------------------------------------------------------*  

foreach var of varlist mech3 mech4 {

foreach year in 2008 2009 2010 {

// main
reg `var' i.exp_1 mean`var'2007 if year == `year' & drop == 0, robust cluster(cct)
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
 "reg mech3 exp_1 meanmech32007 if year == `year' & drop == 0, robust cluster(cct)" ///
 "reg mech4 exp_1 meanmech42007 if year == `year' & drop == 0, robust cluster(cct)") ///
 cluster(cct) familyp(exp_1) bootstraps(50) seed(02472)

 mat A = r(table)
 
 foreach x in 3 4 {
	scalar B = A[`x'-2,4]
	global wy_mech`x'`year'_1: di %12.3fc B  
 }
 
} 
  

*------------------------------------------------------------------------------*  
** Experiment 2
*------------------------------------------------------------------------------*  

foreach var of varlist mech3 mech4 {

// main
reg `var' i.exp_2 mean`var'2009 if year == 2010 & drop == 0, robust cluster(cct)
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
 "reg mech3 exp_2 meanmech32009 if year == 2010 & drop == 0, robust cluster(cct)" ///
 "reg mech4 exp_2 meanmech42009 if year == 2010 & drop == 0, robust cluster(cct)") ///
 cluster(cct) familyp(exp_2) bootstraps(50) seed(02472)

 mat A = r(table)
 
 foreach x in 3 4 {
	scalar B = A[`x'-2,4]
	global wy_mech`x'2010_2: di %12.3fc B  
 }

 
*------------------------------------------------------------------------------* 
** Experiment 3
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


* Estimation 
*------------------------------------------------------------------------------*

// lasso step 1	

foreach x in 3 4 {
	
lasso linear mech`x' grado_2-teacheredu_uniXteacheredu_col, selection(plugin) rseed(02472)
	local A`x' = cond(`e(k_nonzero_sel)' > 0, "`e(allvars_sel)'", " ")
}
	
	
// lasso step 2

lasso linear exp_3 grado_2-teacheredu_uniXteacheredu_col, selection(plugin) rseed(02472)
	local B2 `e(allvars_sel)'
	
	
// lasso step 3 - main
	
foreach x in 3 4 {
	
reg mech`x' i.exp_3 `A`x'' `B2', robust cluster(cct)
  
  global N_mech`x'2010_3 = e(N)
  global b_mech`x'2010_3: di %6.3fc _b[1.exp_3]
  global se_mech`x'2010_3: di %-4.3fc _se[1.exp_3]
  
  test 1.exp_3 = 0
  global p_mech`x'2010_3: di %12.3fc r(p)
  global star_mech`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
summ mech`x' if e(sample) & exp_3 == 0
  global cm_mech`x'_3: di %6.3fc r(mean)
   
}

    
* multiple hypothesis testing
		
// main
 wyoung, cmd( ///
 "reg mech3 exp_3 `A3' `B2', robust cluster(cct)" ///
 "reg mech4 exp_3 `A4' `B2', robust cluster(cct)") ///
 cluster(cct) familyp(exp_3) bootstraps(50) seed(02472)

 mat A = r(table)
 
 foreach x in 3 4 {
	scalar B = A[`x'-2,4]
	global wy_mech`x'2010_3: di %12.3fc B  
 }




*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*	Table: Mechanisms
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

texdoc init mechanism.tex, replace force

tex \begin{tabular}{lcccc} \toprule

tex  & \multicolumn{2}{c}{Parent's perspective} & \multicolumn{2}{c}{Teacher's perspective} \\ \cmidrule(l{5pt}r{5pt}){2-3} \cmidrule(l{5pt}r{5pt}){4-5}

tex  & (1) & (2) & (3) & (4) \\ 
tex  & \shortstack{Most\\teachers\\can be\\trusted} & \shortstack{Teacher\\Responsibility\\Index (SD)} & \shortstack{Most\\parents\\can be\\trusted} & \shortstack{Parent\\Responsibility\\Index(SD)} \\ \hline \hline
	
	
tex \multicolumn{4}{l}{\textit{Double grant experiment}} \\ 
	
tex {Treatment (1 year)} & ${b_mech12008_1}${star_mech12008_1} & ${b_mech22008_1}${star_mech22008_1} & ${b_mech32008_1}${star_mech32008_1} & ${b_mech42008_1}${star_mech42008_1} \\

tex & (${se_mech12008_1}) & (${se_mech22008_1}) & (${se_mech32008_1}) & (${se_mech42008_1}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_mech12008_1} & ${wy_mech22008_1} & ${wy_mech32008_1} & ${wy_mech42008_1} \\[0.1cm]


tex {Treatment (2 year)} & ${b_mech12009_1}${star_mech12009_1} & ${b_mech22009_1}${star_mech22009_1} & ${b_mech32009_1}${star_mech32009_1} & ${b_mech42009_1}${star_mech42009_1} \\

tex & (${se_mech12009_1}) & (${se_mech22009_1}) & (${se_mech32009_1}) & (${se_mech42009_1}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_mech12009_1} & ${wy_mech22009_1} & ${wy_mech32009_1} & ${wy_mech42009_1} \\[0.1cm]


tex {Treatment (3 year)} & ${b_mech12010_1}${star_mech12010_1} & ${b_mech22010_1}${star_mech22010_1} & ${b_mech32010_1}${star_mech32010_1} & ${b_mech42010_1}${star_mech42010_1} \\

tex & (${se_mech12010_1}) & (${se_mech22010_1}) & (${se_mech32010_1}) & (${se_mech42010_1}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_mech12010_1} & ${wy_mech22010_1} & ${wy_mech32010_1} & ${wy_mech42010_1} \\[0.1cm]


tex Control mean & ${cm_mech12008_1} & ${cm_mech22008_1} & ${cm_mech32008_1} & ${cm_mech42008_1} \\

tex Observations & ${N_mech12008_1} & ${N_mech22008_1} & ${N_mech32008_1} & ${N_mech42008_1} \\


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Information experiment}} \\ 

tex {Treatment (1 year)} & ${b_mech12010_2}${star_mech12010_2} & ${b_mech22010_2}${star_mech22010_2} & ${b_mech32010_2}${star_mech32010_2} & ${b_mech42010_2}${star_mech42010_2} \\

tex & (${se_mech12010_2}) & (${se_mech22010_2}) & (${se_mech32010_2}) & (${se_mech42010_2}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_mech12010_2} & ${wy_mech22010_2} & ${wy_mech32010_2} & ${wy_mech42010_2} \\[0.1cm]


tex Control mean & ${cm_mech12010_2} & ${cm_mech22010_2} & ${cm_mech32010_2} & ${cm_mech42010_2} \\

tex Observations & ${N_mech12010_2} & ${N_mech22010_2} & ${N_mech32010_2} & ${N_mech42010_2} \\


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Single grant observation}} \\ 
	
tex {Treatment (1 year)} & ${b_mech12010_3}${star_mech12010_3} & ${b_mech22010_3}${star_mech22010_3} & ${b_mech32010_3}${star_mech32010_3} & ${b_mech42010_3}${star_mech42010_3} \\

tex & (${se_mech12010_3}) & (${se_mech22010_3}) & (${se_mech32010_3}) & (${se_mech42010_3}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_mech12010_3} & ${wy_mech22010_3} & ${wy_mech32010_3} & ${wy_mech42010_3} \\[0.1cm]


tex Control mean & ${cm_mech12010_3} & ${cm_mech22010_3} & ${cm_mech32010_3} & ${cm_mech42010_3} \\

tex Observations & ${N_mech12010_3} & ${N_mech22010_3} & ${N_mech32010_3} & ${N_mech42010_3} \\


tex \hline

tex \end{tabular}

texdoc close 
