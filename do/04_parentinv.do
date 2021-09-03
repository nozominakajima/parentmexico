
* Analysis: Treatment effect on parental involvement in schools

* Author: Nozomi Nakajima
* Date: June 2019


drop _all
clear all
set mem 30g
set maxvar 11000
set matsize 11000
set more off

cd "/Users/nakajimaemiko/Desktop/Harvard Year3/AGE Mexico/new analysis/output"

global data "/Users/nakajimaemiko/Desktop/Harvard Year3/AGE Mexico/new analysis/data"
global output "/Users/nakajimaemiko/Desktop/Harvard Year3/AGE Mexico/new analysis/output"

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


* Generate binary variable for baseline level of parent participation
*------------------------------------------------------------------------------*

summ pv4 if year == 2007 & !missing(exp_1)

gen bl_pv4 = .
replace bl_pv4 = 0 if year == 2007 & !missing(exp_1) & pv4 < 83
replace bl_pv4 = 1 if year == 2007 & !missing(exp_1) & pv4 >= 83 & pv4 < . 

summ pv4 if year == 2009 & !missing(exp_2)

replace bl_pv4 = 0 if year == 2009 & !missing(exp_2) & pv4 < 88
replace bl_pv4 = 1 if year == 2009 & !missing(exp_2) & pv4 >= 88 & pv4 < . 

egen base = min(bl_pv4), by(cct) 
lab var base "Parent participation is above mean"


*------------------------------------------------------------------------------*
** Experiment 1
*------------------------------------------------------------------------------*  

foreach var of varlist pv1 pv2 pv3 pv4 {

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
  
// indigenous (pb06 == 1)
reg `var' i.exp_1##i.pb06 `var'2007 pb06_m if year == `year' & drop == 0, robust cluster(cct)
  matrix A = r(table)
  
  global b1_i`var'`year'_1: di %12.3fc A[1,2]
  global se1_i`var'`year'_1: di %-4.3fc A[2,2]
  
  global b2_i`var'`year'_1: di %12.3fc A[1,8]
  global se2_i`var'`year'_1: di %-4.3fc A[2,8]
  
  test 1.exp_1 = 0
  global p1_i`var'`year'_1: di %12.3fc r(p)
  global star1_i`var'`year'_1 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  test 1.exp_1#1.pb06 = 0
  global p2_i`var'`year'_1: di %12.3fc r(p)
  global star2_i`var'`year'_1 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))

// parent participation (base  == 1)
reg `var' i.exp_1##i.base `var'2007 if year == `year' & drop == 0, robust cluster(cct)
  matrix A = r(table)
  
  global b3_i`var'`year'_1: di %12.3fc A[1,8]
  global se3_i`var'`year'_1: di %-4.3fc A[2,8]
  
  test 1.exp_1#1.base = 0
  global p3_i`var'`year'_1: di %12.3fc r(p)
  global star3_i`var'`year'_1 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 
  
}
}

  
* multiple hypothesis testing

foreach year in 2008 2009 2010 {
	
// main
 wyoung, cmd( ///
 "reg pv1 exp_1 pv12007 if year == `year' & drop == 0, robust cluster(cct)" ///
 "reg pv2 exp_1 pv22007 if year == `year' & drop == 0, robust cluster(cct)" ///
 "reg pv3 exp_1 pv32007 if year == `year' & drop == 0, robust cluster(cct)" ///
 "reg pv4 exp_1 pv42007 if year == `year' & drop == 0, robust cluster(cct)") ///
 cluster(cct) familyp(exp_1) bootstraps(50) seed(02472)

 mat A = r(table)
 
 foreach x in 1 2 3 4{
	scalar B = A[`x',4]
	global wy_pv`x'`year'_1: di %12.3fc B  
 }
 

} 
  
  
*------------------------------------------------------------------------------*  
** Experiment 2
*------------------------------------------------------------------------------*  

foreach var of varlist pv1 pv2 pv3 pv4 {

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
  
  
// indigenous (pb06 == 1)  
reg `var' i.exp_2##i.pb06 `var'2009 pb06_m if year == 2010 & drop == 0, robust cluster(cct)
  matrix A = r(table)
  
  global b1_i`var'2010_2: di %12.3fc A[1,2]
  global se1_i`var'2010_2: di %-4.3fc A[2,2]
  
  global b2_i`var'2010_2: di %12.3fc A[1,8]
  global se2_i`var'2010_2: di %-4.3fc A[2,8]
  
  test 1.exp_2 = 0
  global p1_i`var'2010_2: di %12.3fc r(p)
  global star1_i`var'2010_2 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  test 1.exp_2#1.pb06 = 0
  global p2_i`var'2010_2: di %12.3fc r(p)
  global star2_i`var'2010_2 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 


// parent participation (base  == 1)
reg `var' i.exp_2##i.base `var'2009 if year == 2010 & drop == 0, robust cluster(cct)
  matrix A = r(table)
  
  global b3_i`var'2010_2: di %12.3fc A[1,8]
  global se3_i`var'2010_2: di %-4.3fc A[2,8]
  
  test 1.exp_2#1.base = 0
  global p3_i`var'2010_2: di %12.3fc r(p)
  global star3_i`var'2010_2 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 
  
}  


* multiple hypothesis testing
	
// main
 wyoung, cmd( ///
 "reg pv1 exp_2 pv12009 if year == 2010 & drop == 0, robust cluster(cct)" ///
 "reg pv2 exp_2 pv22009 if year == 2010 & drop == 0, robust cluster(cct)" ///
 "reg pv3 exp_2 pv32009 if year == 2010 & drop == 0, robust cluster(cct)" ///
 "reg pv4 exp_2 pv42009 if year == 2010 & drop == 0, robust cluster(cct)") ///
 cluster(cct) familyp(exp_2) bootstraps(50) seed(02472)

 mat A = r(table)
 
 foreach x in 1 2 3 4{
	scalar B = A[`x',4]
	global wy_pv`x'2010_2: di %12.3fc B  
 }


// control group difference between indigenous & general schools: 
bysort modalidad: summ pv1 pv2 if year == 2008 & exp_1 == 0 & drop ==0


*------------------------------------------------------------------------------*
** Experiment 3
*------------------------------------------------------------------------------*  


* Only keep 2010 variables that are not outcomes but plausible predictors  

keep if year == 2010 & !missing(exp_3) & modalidad == "General" & drop == 0

keep estado cct modalidad exp_1 exp_2 exp_3 id_parent drop ///
pb03 pb06 pb07 pb16a pb18 pb21 pb27 pb37 pb39 ///
pb03_m pb06_m pb07_m pb16a_m pb18_m pb21_m pb27_m pb37_m pb39_m ///
pv1 pv2 pv3 pv4 ///
pv12009 pv22009 pv32009 pv42009 ///
base

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

keeporder estado-id_parent drop pb03-pb39 pb03_m-pb39_m pv42009-v914_m2009 pv1 pv2 pv3 pv4 base


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
  
order estado-id_parent drop pb03-pb07 pb16a_2-pb18_3 pb21-pb37 pb39 pb03_m-pb39_m pv42009-v914_m2009 pv1 pv2 pv3 pv4 base
 
 
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

order estado-v914_m2009 pb06Xpb03-pb39Xpb37 pv1 pv2 pv3 pv4 base


* Estimation 
*------------------------------------------------------------------------------*

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
	
reg pv`x' i.exp_3 `A`x'' `B', robust cluster(cct)
  
  global N_pv`x'2010_3 = e(N)
  global b_pv`x'2010_3: di %6.3fc _b[1.exp_3]
  global se_pv`x'2010_3: di %-4.3fc _se[1.exp_3]
  
  test 1.exp_3 = 0
  global p_pv`x'2010_3: di %12.3fc r(p)
  global star_pv`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
summ pv`x' if e(sample) & exp_3 == 0
  global cm_pv`x'2010_3: di %6.3fc r(mean)
   
}


// lasso step 3 - indigenous (pb06 == 1)  
foreach x in 1 2 3 4 {
	
reg pv`x' i.exp_3##i.pb06 `A`x'' `B', robust cluster(cct)
  matrix A = r(table)
  
  global b1_ipv`x'2010_3: di %12.3fc A[1,2]
  global se1_ipv`x'2010_3: di %-4.3fc A[2,2]
  
  global b2_ipv`x'2010_3: di %12.3fc A[1,8]
  global se2_ipv`x'2010_3: di %-4.3fc A[2,8]
  
  test 1.exp_3 = 0
  global p1_ipv`x'2010_3: di %12.3fc r(p)
  global star1_ipv`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  test 1.exp_3#1.pb06 = 0
  global p2_ipv`x'2010_3: di %12.3fc r(p)
  global star2_ipv`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 

}

// lasso step 3 - parent participation (base == 1)  
foreach x in 1 2 3 4 {
	
reg pv`x' i.exp_3##i.base `A`x'' `B', robust cluster(cct)
  matrix A = r(table)
    
  global b3_ipv`x'2010_3: di %12.3fc A[1,8]
  global se3_ipv`x'2010_3: di %-4.3fc A[2,8]
    
  test 1.exp_3#1.base = 0
  global p3_ipv`x'2010_3: di %12.3fc r(p)
  global star3_ipv`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 

}

  
* multiple hypothesis testing
		
// main
 wyoung, cmd( ///
 "reg pv1 exp_3 `A1' `B', robust cluster(cct)" ///
 "reg pv2 exp_3 `A2' `B', robust cluster(cct)" ///
 "reg pv3 exp_3 `A3' `B', robust cluster(cct)" ///
 "reg pv4 exp_3 `A4' `B', robust cluster(cct)") ///
 cluster(cct) familyp(exp_3) bootstraps(50) seed(02472)

 mat A = r(table)
 
 foreach x in 1 2 3 4{
	scalar B = A[`x',4]
	global wy_pv`x'2010_3: di %12.3fc B  
 }
  
   
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*   
*	Table: Parental involvement
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

texdoc init parentinv.tex, replace force

tex \begin{tabular}{lcccc} \toprule

tex  & {\shortstack{Organized\\school activities\\ \& events}} & {\shortstack{Met with\\ teachers to discuss \\student performance}} & {\shortstack{Involved in\\school\\decision making}} & {\shortstack{Percent of parents\\regularly attending\\meetings}} \\ 

tex  & (1) & (2) & (3) & (4) \\ \hline
	
tex \multicolumn{4}{l}{\textit{Double grant experiment}} \\ 
	
tex {Treatment (1 year)} & ${b_pv12008_1}${star_pv12008_1} & ${b_pv22008_1}${star_pv22008_1} & ${b_pv32008_1}${star_pv32008_1} & ${b_pv42008_1}${star_pv42008_1} \\

tex & (${se_pv12008_1}) & (${se_pv22008_1}) & (${se_pv32008_1}) & (${se_pv42008_1}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_pv12008_1} & ${wy_pv22008_1} & ${wy_pv32008_1} & ${wy_pv42008_1} \\[0.1cm]


tex {Treatment (2 year)} & ${b_pv12009_1}${star_pv12009_1} & ${b_pv22009_1}${star_pv22009_1} & ${b_pv32009_1}${star_pv32009_1} & ${b_pv42009_1}${star_pv42009_1} \\

tex & (${se_pv12009_1}) & (${se_pv22009_1}) & (${se_pv32009_1}) & (${se_pv42009_1}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_pv12009_1} & ${wy_pv22009_1} & ${wy_pv32009_1} & ${wy_pv42009_1} \\[0.1cm]


tex {Treatment (3 year)} & ${b_pv12010_1}${star_pv12010_1} & ${b_pv22010_1}${star_pv22010_1} & ${b_pv32010_1}${star_pv32010_1} & ${b_pv42010_1}${star_pv42010_1} \\

tex & (${se_pv12010_1}) & (${se_pv22010_1}) & (${se_pv32010_1}) & (${se_pv42010_1}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_pv12010_1} & ${wy_pv22010_1} & ${wy_pv32010_1} & ${wy_pv42010_1} \\[0.1cm]


tex Control mean & ${cm_pv12008_1} & ${cm_pv22008_1} & ${cm_pv32008_1} & ${cm_pv42008_1} \\

tex Observations & ${N_pv12008_1} & ${N_pv22008_1} & ${N_pv32008_1} & ${N_pv42008_1} \\


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Information experiment}} \\ 

tex {Treatment (1 year)} & ${b_pv12010_2}${star_pv12010_2} & ${b_pv22010_2}${star_pv22010_2} & ${b_pv32010_2}${star_pv32010_2} & ${b_pv42010_2}${star_pv42010_2} \\

tex & (${se_pv12010_2}) & (${se_pv22010_2}) & (${se_pv32010_2}) & (${se_pv42010_2}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_pv12010_2} & ${wy_pv22010_2} & ${wy_pv32010_2} & ${wy_pv42010_2} \\[0.1cm]


tex Control mean & ${cm_pv12010_2} & ${cm_pv22010_2} & ${cm_pv32010_2} & ${cm_pv42010_2} \\

tex Observations & ${N_pv12010_2} & ${N_pv22010_2} & ${N_pv32010_2} & ${N_pv42010_2} \\


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Single grant observation}} \\ 
	
tex {Treatment (1 year)} & ${b_pv12010_3}${star_pv12010_3} & ${b_pv22010_3}${star_pv22010_3} & ${b_pv32010_3}${star_pv32010_3} & ${b_pv42010_3}${star_pv42010_3} \\

tex & (${se_pv12010_3}) & (${se_pv22010_3}) & (${se_pv32010_3}) & (${se_pv42010_3}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_pv12010_3} & ${wy_pv22010_3} & ${wy_pv32010_3} & ${wy_pv42010_3} \\[0.1cm]


tex Control mean & ${cm_pv12010_3} & ${cm_pv22010_3} & ${cm_pv32010_3} & ${cm_pv42010_3} \\

tex Observations & ${N_pv12010_3} & ${N_pv22010_3} & ${N_pv32010_3} & ${N_pv42010_3} \\


tex \hline

tex \end{tabular}

texdoc close 


*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*   
*	Table: Treatment x Indigenous 
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

texdoc init parentinv_het.tex, replace force

tex \begin{tabular}{lcccc} \toprule

tex  & {\shortstack{Organized\\school activities\\ \& events}} & {\shortstack{Met with\\ teachers to discuss \\student performance}} & {\shortstack{Involved in\\school\\decision making}} & {\shortstack{Percent of parents\\regularly attending\\meetings}} \\ 

tex  & (1) & (2) & (3) & (4) \\ \hline
	
tex \multicolumn{4}{l}{\textit{Double grant experiment}} \\ 
	
tex \multicolumn{4}{l}{(1 year)} \\ 
  
  tex {Treatment x Indigenous} & ${b2_ipv12008_1}${star2_ipv12008_1} & ${b2_ipv22008_1}${star2_ipv22008_1} & ${b2_ipv32008_1}${star2_ipv32008_1} & ${b2_ipv42008_1}${star2_ipv42008_1} \\

tex & (${se2_ipv12008_1}) & (${se2_ipv22008_1}) & (${se2_ipv32008_1}) & (${se2_ipv42008_1}) \\[0.1cm]


tex \multicolumn{4}{l}{(2 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ipv12009_1}${star2_ipv12009_1} & ${b2_ipv22009_1}${star2_ipv22009_1} & ${b2_ipv32009_1}${star2_ipv32009_1} & ${b2_ipv42009_1}${star2_ipv42009_1} \\

tex & (${se2_ipv12009_1}) & (${se2_ipv22009_1}) & (${se2_ipv32009_1}) & (${se2_ipv42009_1}) \\[0.1cm]


tex \multicolumn{4}{l}{(3 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ipv12010_1}${star2_ipv12010_1} & ${b2_ipv22010_1}${star2_ipv22010_1} & ${b2_ipv32010_1}${star2_ipv32010_1} & ${b2_ipv42010_1}${star2_ipv42010_1} \\

tex & (${se2_ipv12010_1}) & (${se2_ipv22010_1}) & (${se2_ipv32010_1}) & (${se2_ipv42010_1}) \\[0.1cm]


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Information experiment}} \\ 

tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ipv12010_2}${star2_ipv12010_2} & ${b2_ipv22010_2}${star2_ipv22010_2} & ${b2_ipv32010_2}${star2_ipv32010_2} & ${b2_ipv42010_2}${star2_ipv42010_2} \\

tex & (${se2_ipv12010_2}) & (${se2_ipv22010_2}) & (${se2_ipv32010_2}) & (${se2_ipv42010_2}) \\[0.1cm]


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Single grant observation}} \\ 
	
tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ipv12010_3}${star2_ipv12010_3} & ${b2_ipv22010_3}${star2_ipv22010_3} & ${b2_ipv32010_3}${star2_ipv32010_3} & ${b2_ipv42010_3}${star2_ipv42010_3} \\

tex & (${se2_ipv12010_3}) & (${se2_ipv22010_3}) & (${se2_ipv32010_3}) & (${se2_ipv42010_3}) \\[0.1cm]

tex \hline

tex \end{tabular}


*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*   
*	Table: Treatment x Parent Participation
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

texdoc init parentinv_ppar.tex, replace force

tex \begin{tabular}{lcccc} \toprule

tex  & {\shortstack{Organized\\school activities\\ \& events}} & {\shortstack{Met with\\ teachers to discuss \\student performance}} & {\shortstack{Involved in\\school\\decision making}} & {\shortstack{Percent of parents\\regularly attending\\meetings}} \\ 

tex  & (1) & (2) & (3) & (4) \\ \hline
	
tex \multicolumn{4}{l}{\textit{Double grant experiment}} \\ 
	
tex \multicolumn{4}{l}{(1 year)} \\ 
  
  tex {Treatment x Parents} & ${b3_ipv12008_1}${star3_ipv12008_1} & ${b3_ipv22008_1}${star3_ipv22008_1} & ${b3_ipv32008_1}${star3_ipv32008_1} & ${b3_ipv42008_1}${star3_ipv42008_1} \\

tex & (${se3_ipv12008_1}) & (${se3_ipv22008_1}) & (${se3_ipv32008_1}) & (${se3_ipv42008_1}) \\[0.1cm]


tex \multicolumn{4}{l}{(2 year)} \\ 
  
tex {Treatment x Parents} & ${b3_ipv12009_1}${star3_ipv12009_1} & ${b3_ipv22009_1}${star3_ipv22009_1} & ${b3_ipv32009_1}${star3_ipv32009_1} & ${b3_ipv42009_1}${star3_ipv42009_1} \\

tex & (${se3_ipv12009_1}) & (${se3_ipv22009_1}) & (${se3_ipv32009_1}) & (${se3_ipv42009_1}) \\[0.1cm]


tex \multicolumn{4}{l}{(3 year)} \\ 
  
tex {Treatment x Parents} & ${b3_ipv12010_1}${star3_ipv12010_1} & ${b3_ipv22010_1}${star3_ipv22010_1} & ${b3_ipv32010_1}${star3_ipv32010_1} & ${b3_ipv42010_1}${star3_ipv42010_1} \\

tex & (${se3_ipv12010_1}) & (${se3_ipv22010_1}) & (${se3_ipv32010_1}) & (${se3_ipv42010_1}) \\[0.1cm]


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Information experiment}} \\ 

tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Parents} & ${b3_ipv12010_2}${star3_ipv12010_2} & ${b3_ipv22010_2}${star3_ipv22010_2} & ${b3_ipv32010_2}${star3_ipv32010_2} & ${b3_ipv42010_2}${star3_ipv42010_2} \\

tex & (${se3_ipv12010_2}) & (${se3_ipv22010_2}) & (${se3_ipv32010_2}) & (${se3_ipv42010_2}) \\[0.1cm]


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Single grant observation}} \\ 
	
tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Parents} & ${b3_ipv12010_3}${star3_ipv12010_3} & ${b3_ipv22010_3}${star3_ipv22010_3} & ${b3_ipv32010_3}${star3_ipv32010_3} & ${b3_ipv42010_3}${star3_ipv42010_3} \\

tex & (${se3_ipv12010_3}) & (${se3_ipv22010_3}) & (${se3_ipv32010_3}) & (${se3_ipv42010_3}) \\[0.1cm]

tex \hline

tex \end{tabular}
