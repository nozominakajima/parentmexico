
* Analysis: Treatment effect on principal & teacher composition and teaching practices

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
* Parenting & teaching behavior
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


* Get parental involvement variable at baseline for heterogeneity analysis
*------------------------------------------------------------------------------*
preserve

use "$data/panel_parent_long_clean.dta", clear

rename (pb51_1a pb51_1e pb51_1h pb48) ///
(pv1 pv2 pv3 pv4) 

save `a', replace

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

summ pv4 if year == 2007 & !missing(exp_1)

gen bl_pv4 = .
replace bl_pv4 = 0 if year == 2007 & !missing(exp_1) & pv4 < 83
replace bl_pv4 = 1 if year == 2007 & !missing(exp_1) & pv4 >= 83 & pv4 < . 

summ pv4 if year == 2009 & !missing(exp_2)

replace bl_pv4 = 0 if year == 2009 & !missing(exp_2) & pv4 < 88
replace bl_pv4 = 1 if year == 2009 & !missing(exp_2) & pv4 >= 88 & pv4 < . 

gen temp = pv4 if year == 2007 & !missing(exp_1)
replace temp = pv4 if year == 2009 & !missing(exp_2)
egen base = min(temp), by(cct)
*egen base = min(bl_pv4), by(cct) 
lab var base "Parent participation is above mean"

keeporder estado cct year base
save `a', replace

restore

merge m:1 cct year using `a'
drop _merge


*------------------------------------------------------------------------------*
** Experiment 1
*------------------------------------------------------------------------------*  

foreach var of varlist c1 c2 c3 c4 {

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
  
  global b2_i`var'`year'_1: di %12.3fc _b[1.exp_1#1.ap03]
  global se2_i`var'`year'_1: di %-4.3fc _se[1.exp_1#1.ap03]
    
  test 1.exp_1#1.ap03 = 0
  global p2_i`var'`year'_1: di %12.3fc r(p)
  global star2_i`var'`year'_1 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))


// parent participation (base == 1)
reg `var' i.exp_1##c.base i.grado female zwealth mothered mean`var'2007 female_m zwealth_m mothered_m ap03_m if year == `year' & drop == 0, robust cluster(cct)
  
  global b3_i`var'`year'_1: di %12.3fc _b[1.exp_1#c.base]
  global se3_i`var'`year'_1: di %-4.3fc _se[1.exp_1#c.base]
  
  test 1.exp_1#c.base = 0
  global p3_i`var'`year'_1: di %12.3fc r(p)
  global star3_i`var'`year'_1 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
}
}


* multiple hypothesis testing

foreach year in 2008 2009 2010 {
	
// main
 wyoung, cmd( ///
 "reg c1 exp_1 grado female zwealth mothered meanc12007 female_m zwealth_m mothered_m if year == `year' & drop == 0, robust cluster(cct)" ///
 "reg c2 exp_1 grado female zwealth mothered meanc22007 female_m zwealth_m mothered_m if year == `year' & drop == 0, robust cluster(cct)" ///
 "reg c3 exp_1 grado female zwealth mothered meanc32007 female_m zwealth_m mothered_m if year == `year' & drop == 0, robust cluster(cct)" ///
 "reg c4 exp_1 grado female zwealth mothered meanc42007 female_m zwealth_m mothered_m if year == `year' & drop == 0, robust cluster(cct)") ///
 cluster(cct) familyp(exp_1) bootstraps(100) seed(02472)

 mat A = r(table)
 
 foreach x in 1 2 3 4{
	scalar B = A[`x',4]
	global wy_c`x'`year'_1: di %12.3fc B  
 }
 
}



*------------------------------------------------------------------------------*  
** Experiment 2
*------------------------------------------------------------------------------*  

foreach var of varlist c1 c2 c3 c4 {

// main
reg `var' i.exp_2 i.grado female zwealth mothered mean`var'2009 female_m zwealth_m mothered_m if year == 2010 & drop == 0, robust cluster(cct)
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
  
  global b1_i`var'2010_2: di %12.3fc A[1,2]
  global se1_i`var'2010_2: di %-4.3fc A[2,2]
  
  global b2_i`var'2010_2: di %12.3fc A[1,8]
  global se2_i`var'2010_2: di %-4.3fc A[2,8]
  
  test 1.exp_2 = 0
  global p1_i`var'2010_2: di %12.3fc r(p)
  global star1_i`var'2010_2 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  test 1.exp_2#1.ap03 = 0
  global p2_i`var'2010_2: di %12.3fc r(p)
  global star2_i`var'2010_2 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 

  
// parent participation (base == 1)  
reg `var' i.exp_2##c.base i.grado female zwealth mothered mean`var'2009 female_m zwealth_m mothered_m ap03_m if year == 2010 & drop == 0, robust cluster(cct)
  matrix A = r(table)
  
  global b3_i`var'2010_2: di %12.3fc A[1,8]
  global se3_i`var'2010_2: di %-4.3fc A[2,8]
  
  test 1.exp_2#c.base = 0
  global p3_i`var'2010_2: di %12.3fc r(p)
  global star3_i`var'2010_2 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 
  
}  


* multiple hypothesis testing
	
// main
 wyoung, cmd( ///
 "reg c1 exp_2 grado female zwealth mothered meanc12009 female_m zwealth_m mothered_m if year == 2010 & drop == 0, robust cluster(cct)" ///
 "reg c2 exp_2 grado female zwealth mothered meanc22009 female_m zwealth_m mothered_m if year == 2010 & drop == 0, robust cluster(cct)" ///
 "reg c3 exp_2 grado female zwealth mothered meanc32009 female_m zwealth_m mothered_m if year == 2010 & drop == 0, robust cluster(cct)" ///
 "reg c4 exp_2 grado female zwealth mothered meanc42009 female_m zwealth_m mothered_m if year == 2010 & drop == 0, robust cluster(cct)") ///
 cluster(cct) familyp(exp_2) bootstraps(100) seed(02472)

 mat A = r(table)
 
 foreach x in 1 2 3 4{
	scalar B = A[`x',4]
	global wy_c`x'2010_2: di %12.3fc B  
 }

// control group difference between indigenous & non-indigenous schools: 
bysort modalidad: summ c1 c2 if year == 2009 & exp_1 == 0 & drop ==0
	* Maximum gap is .455 - .38 = .075
	* 4.2 pp / 7.5 p.p. = 56 percent


*------------------------------------------------------------------------------*
** Experiment 3
*------------------------------------------------------------------------------*  

* Only keep 2010 variables that are not outcomes but plausible predictors  

keep if year == 2010 & modalidad == "General" & !missing(exp_3) & drop == 0
	  
keeporder estado-id_student female ap03-ap51 ap03_m-ap51_m zwealth ///
mb03 mb06 mb16 mb34a mb36a mb28 mb03_m mb06_m mb16_m mb34a_m mb36a_m mb28_m ///
teacheredu_col teacheredu_uni ///
meanc12009 meanc22009 meanc32009 meanc42009 ///
c1 c2 c3 c4 ///
base


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
  

order estado-female grado_2 grado_3 ap03-ap48 ap51_2-ap51_7 ap03_m-c4 base

					 
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


order estado-meanc42009 grado_3Xgrado_2-mb28Xmb36a c1 c2 c3 c4 base


* Estimation 
*------------------------------------------------------------------------------*

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
	
reg c`x' i.exp_3 `A`x'' `B', robust cluster(cct)
  
  global N_c`x'2010_3 = e(N)
  global b_c`x'2010_3: di %6.3fc _b[1.exp_3]
  global se_c`x'2010_3: di %-4.3fc _se[1.exp_3]
  
  test 1.exp_3 = 0
  global p_c`x'2010_3: di %12.3fc r(p)
  global star_c`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  summ c`x' if e(sample) & exp_3 == 0
  global cm_c`x'2010_3: di %6.3fc r(mean)
}


// lasso step 3 - indigenous (ap03 == 1)  
foreach x in 1 2 3 4 {
	
reg c`x' i.exp_3##i.ap03 `A`x'' `B', robust cluster(cct)
  matrix A = r(table)
  
  global b1_ic`x'2010_3: di %12.3fc A[1,2]
  global se1_ic`x'2010_3: di %-4.3fc A[2,2]
  
  global b2_ic`x'2010_3: di %12.3fc A[1,8]
  global se2_ic`x'2010_3: di %-4.3fc A[2,8]
  
  test 1.exp_3 = 0
  global p1_ic`x'2010_3: di %12.3fc r(p)
  global star1_ic`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))
  
  test 1.exp_3#1.ap03 = 0
  global p2_ic`x'2010_3: di %12.3fc r(p)
  global star2_ic`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 
 
}
  


// lasso step 3 - parent participation (base == 1)  
foreach x in 1 2 3 4 {
	
reg c`x' i.exp_3##c.base `A`x'' `B', robust cluster(cct)
  matrix A = r(table)

  global b3_ic`x'2010_3: di %12.3fc A[1,8]
  global se3_ic`x'2010_3: di %-4.3fc A[2,8]
    
  test 1.exp_3#c.base = 0
  global p3_ic`x'2010_3: di %12.3fc r(p)
  global star3_ic`x'2010_3 = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 
 
}



  
* multiple hypothesis testing
		
// main
 wyoung, cmd( ///
 "reg c1 exp_3 `A1' `B', robust cluster(cct)" ///
 "reg c2 exp_3 `A2' `B', robust cluster(cct)" ///
 "reg c3 exp_3 `A3' `B', robust cluster(cct)" ///
 "reg c4 exp_3 `A4' `B', robust cluster(cct)") ///
 cluster(cct) familyp(exp_3) bootstraps(100) seed(02472)

 mat A = r(table)
 
 foreach x in 1 2 3 4{
	scalar B = A[`x',4]
	global wy_c`x'2010_3: di %12.3fc B  
 }
 


*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*	Table: Student experience
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

texdoc init input.tex, replace force

tex \begin{tabular}{lcccc} \toprule

tex  & \multicolumn{2}{c}{Parenting} & \multicolumn{2}{c}{Teaching} \\ \cmidrule(l{5pt}r{5pt}){2-3} \cmidrule(l{5pt}r{5pt}){4-5} 

tex  & {\shortstack{Aware of\\school\\ assignments}} & {\shortstack{Helps with\\ homework}} & {\shortstack{Days absent\\in past month}} & {\shortstack{Student-\\centered\\instruction}} \\ 

tex  & (1) & (2) & (3) & (4) \\ \hline \hline
	
tex \multicolumn{4}{l}{\textit{Double grant experiment}} \\ 
	
tex {Treatment (1 year)} & ${b_c12008_1}${star_c12008_1} & ${b_c22008_1}${star_c22008_1} & ${b_c32008_1}${star_c32008_1} & ${b_c42008_1}${star_c42008_1} \\

tex & (${se_c12008_1}) & (${se_c22008_1}) & (${se_c32008_1}) & (${se_c42008_1}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_c12008_1} & ${wy_c22008_1} & ${wy_c32008_1} & ${wy_c42008_1} \\[0.1cm]


tex {Treatment (2 year)} & ${b_c12009_1}${star_c12009_1} & ${b_c22009_1}${star_c22009_1} & ${b_c32009_1}${star_c32009_1} & ${b_c42009_1}${star_c42009_1} \\

tex & (${se_c12009_1}) & (${se_c22009_1}) & (${se_c32009_1}) & (${se_c42009_1}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_c12009_1} & ${wy_c22009_1} & ${wy_c32009_1} & ${wy_c42009_1} \\[0.1cm]


tex {Treatment (3 year)} & ${b_c12010_1}${star_c12010_1} & ${b_c22010_1}${star_c22010_1} & ${b_c32010_1}${star_c32010_1} & ${b_c42010_1}${star_c42010_1} \\

tex & (${se_c12010_1}) & (${se_c22010_1}) & (${se_c32010_1}) & (${se_c42010_1}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_c12010_1} & ${wy_c22010_1} & ${wy_c32010_1} & ${wy_c42010_1} \\[0.1cm]


tex Control mean & ${cm_c12008_1} & ${cm_c22008_1} & ${cm_c32008_1} & ${cm_c42008_1} \\

tex Observations & ${N_c12008_1} & ${N_c22008_1} & ${N_c32008_1} & ${N_c42008_1} \\


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Information experiment}} \\ 

tex {Treatment (1 year)} & ${b_c12010_2}${star_c12010_2} & ${b_c22010_2}${star_c22010_2} & ${b_c32010_2}${star_c32010_2} & ${b_c42010_2}${star_c42010_2} \\

tex & (${se_c12010_2}) & (${se_c22010_2}) & (${se_c32010_2}) & (${se_c42010_2}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_c12010_2} & ${wy_c22010_2} & ${wy_c32010_2} & ${wy_c42010_2} \\[0.1cm]


tex Control mean & ${cm_c12010_2} & ${cm_c22010_2} & ${cm_c32010_2} & ${cm_c42010_2} \\

tex Observations & ${N_c12010_2} & ${N_c22010_2} & ${N_c32010_2} & ${N_c42010_2} \\


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Single grant observation}} \\ 
	
tex {Treatment (1 year)} & ${b_c12010_3}${star_c12010_3} & ${b_c22010_3}${star_c22010_3} & ${b_c32010_3}${star_c32010_3} & ${b_c42010_3}${star_c42010_3} \\

tex & (${se_c12010_3}) & (${se_c22010_3}) & (${se_c32010_3}) & (${se_c42010_3}) \\[0.1cm]

tex \hspace{3mm} WY \textit{p}-value & ${wy_c12010_3} & ${wy_c22010_3} & ${wy_c32010_3} & ${wy_c42010_3} \\[0.1cm]


tex Control mean & ${cm_c12010_3} & ${cm_c22010_3} & ${cm_c32010_3} & ${cm_c42010_3} \\

tex Observations & ${N_c12010_3} & ${N_c22010_3} & ${N_c32010_3} & ${N_c42010_3} \\


tex \hline

tex \end{tabular}

texdoc close 

*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*	Table: Treatment x Indigenous
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

texdoc init input_het.tex, replace force

tex \begin{tabular}{lcccc} \toprule

tex  & \multicolumn{2}{c}{Parenting} & \multicolumn{2}{c}{Teaching} \\ \cmidrule(l{5pt}r{5pt}){2-3} \cmidrule(l{5pt}r{5pt}){4-5} 

tex  & {\shortstack{Aware of\\school\\ assignments}} & {\shortstack{Helps with\\ homework}} & {\shortstack{Days absent\\in past month}} & {\shortstack{Student-\\centered\\instruction}} \\ 

tex  & (1) & (2) & (3) & (4) \\ \hline \hline
	
tex \multicolumn{4}{l}{\textit{Double grant experiment}} \\ 
	
tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ic12008_1}${star2_ic12008_1} & ${b2_ic22008_1}${star2_ic22008_1} & ${b2_ic32008_1}${star2_ic32008_1} & ${b2_ic42008_1}${star2_ic42008_1} \\

tex & (${se2_ic12008_1}) & (${se2_ic22008_1}) & (${se2_ic32008_1}) & (${se2_ic42008_1}) \\[0.1cm]


tex \multicolumn{4}{l}{(2 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ic12009_1}${star2_ic12009_1} & ${b2_ic22009_1}${star2_ic22009_1} & ${b2_ic32009_1}${star2_ic32009_1} & ${b2_ic42009_1}${star2_ic42009_1} \\

tex & (${se2_ic12009_1}) & (${se2_ic22009_1}) & (${se2_ic32009_1}) & (${se2_ic42009_1}) \\[0.1cm]


tex \multicolumn{4}{l}{(3 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ic12010_1}${star2_ic12010_1} & ${b2_ic22010_1}${star2_ic22010_1} & ${b2_ic32010_1}${star2_ic32010_1} & ${b2_ic42010_1}${star2_ic42010_1} \\

tex & (${se2_ic12010_1}) & (${se2_ic22010_1}) & (${se2_ic32010_1}) & (${se2_ic42010_1}) \\[0.1cm]


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Information experiment}} \\ 

tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ic12010_2}${star2_ic12010_2} & ${b2_ic22010_2}${star2_ic22010_2} & ${b2_ic32010_2}${star2_ic32010_2} & ${b2_ic42010_2}${star2_ic42010_2} \\

tex & (${se2_ic12010_2}) & (${se2_ic22010_2}) & (${se2_ic32010_2}) & (${se2_ic42010_2}) \\[0.1cm]


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Single grant observation}} \\ 
	
tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Indigenous} & ${b2_ic12010_3}${star2_ic12010_3} & ${b2_ic22010_3}${star2_ic22010_3} & ${b2_ic32010_3}${star2_ic32010_3} & ${b2_ic42010_3}${star2_ic42010_3} \\

tex & (${se2_ic12010_3}) & (${se2_ic22010_3}) & (${se2_ic32010_3}) & (${se2_ic42010_3}) \\[0.1cm]

tex \hline

tex \end{tabular}

texdoc close 


*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*	Table: Treatment x Parents
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

texdoc init input_ppar.tex, replace force

tex \begin{tabular}{lcccc} \toprule

tex  & \multicolumn{2}{c}{Parenting} & \multicolumn{2}{c}{Teaching} \\ \cmidrule(l{5pt}r{5pt}){2-3} \cmidrule(l{5pt}r{5pt}){4-5} 

tex  & {\shortstack{Aware of\\school\\ assignments}} & {\shortstack{Helps with\\ homework}} & {\shortstack{Days absent\\in past month}} & {\shortstack{Student-\\centered\\instruction}} \\ 

tex  & (1) & (2) & (3) & (4) \\ \hline \hline
	
tex \multicolumn{4}{l}{\textit{Double grant experiment}} \\ 
	
tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Parents} & ${b3_ic12008_1}${star3_ic12008_1} & ${b3_ic22008_1}${star3_ic22008_1} & ${b3_ic32008_1}${star3_ic32008_1} & ${b3_ic42008_1}${star3_ic42008_1} \\

tex & (${se3_ic12008_1}) & (${se3_ic22008_1}) & (${se3_ic32008_1}) & (${se3_ic42008_1}) \\[0.1cm]


tex \multicolumn{4}{l}{(2 year)} \\ 
  
tex {Treatment x Parents} & ${b3_ic12009_1}${star3_ic12009_1} & ${b3_ic22009_1}${star3_ic22009_1} & ${b3_ic32009_1}${star3_ic32009_1} & ${b3_ic42009_1}${star3_ic42009_1} \\

tex & (${se3_ic12009_1}) & (${se3_ic22009_1}) & (${se3_ic32009_1}) & (${se3_ic42009_1}) \\[0.1cm]


tex \multicolumn{4}{l}{(3 year)} \\ 
  
tex {Treatment x Parents} & ${b3_ic12010_1}${star3_ic12010_1} & ${b3_ic22010_1}${star3_ic22010_1} & ${b3_ic32010_1}${star3_ic32010_1} & ${b3_ic42010_1}${star3_ic42010_1} \\

tex & (${se3_ic12010_1}) & (${se3_ic22010_1}) & (${se3_ic32010_1}) & (${se3_ic42010_1}) \\[0.1cm]


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Information experiment}} \\ 

tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Parents} & ${b3_ic12010_2}${star3_ic12010_2} & ${b3_ic22010_2}${star3_ic22010_2} & ${b3_ic32010_2}${star3_ic32010_2} & ${b3_ic42010_2}${star3_ic42010_2} \\

tex & (${se3_ic12010_2}) & (${se3_ic22010_2}) & (${se3_ic32010_2}) & (${se3_ic42010_2}) \\[0.1cm]


tex \hline \hline


tex \multicolumn{4}{l}{\textit{Single grant observation}} \\ 
	
tex \multicolumn{4}{l}{(1 year)} \\ 
  
tex {Treatment x Parents} & ${b3_ic12010_3} & ${b3_ic22010_3}${star3_ic22010_3} & ${b3_ic32010_3}${star3_ic32010_3} & ${b3_ic42010_3}${star3_ic42010_3} \\

tex & (${se3_ic12010_3}) & (${se3_ic22010_3}) & (${se3_ic32010_3}) & (${se3_ic42010_3}) \\[0.1cm]

tex \hline

tex \end{tabular}

texdoc close 
