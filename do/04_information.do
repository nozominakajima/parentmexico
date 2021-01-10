
* Analysis: Treatment effect on information

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
* Implementation of information sessions
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

use "$data/panel_parent_long_clean.dta", clear


*	Shorten labels for key variables	
*------------------------------------------------------------------------------*

lab var pb35a "Overview"
lab var pb35b "Role of parents"
lab var pb35c "Community resources"
lab var pb35d "Child development"
lab var pb35e "Action plans"


*	Regress information session
*------------------------------------------------------------------------------*
estimates drop _all

foreach var of varlist pb35a pb35b pb35c pb35d pb35e {
  
* Regression
reg `var' i.exp_2 if year == 2010 & drop == 0, robust cluster(cct)
  global N_`var' = e(N)
  global b_`var': di %6.3fc _b[1.exp_2]
  global se_`var': di %-4.3fc _se[1.exp_2]
  
  test 1.exp_2 = 0
  global p_`var': di %12.3fc r(p)
  global star_`var' = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))

  
* Control mean  
summ `var' if e(sample) & year == 2010 & exp_2 == 0 & drop == 0
  global cm_`var': di %6.3fc r(mean)
 

* Add label variables  
global lbe_`var': var label `var'
  
}  


*	Table: Information delivery
*------------------------------------------------------------------------------*

texdoc init information.tex, replace force

tex \begin{tabular}{lccccc} \toprule
tex  & (1) & (2) & (3) & (4) & (5) \\ 
tex  & \shortstack{Overview} & \shortstack{Role of\\parents} & \shortstack{Community\\resources} & \shortstack{Child\\development} & \shortstack{Action\\plans} \\ \hline \hline
	
tex \multicolumn{6}{l}{\textit{Information experiment}} \\[0.1cm]
	
tex Treatment & ${b_pb35a}${star_pb35a} & ${b_pb35b}${star_pb35b} & ${b_pb35c}${star_pb35c} & ${b_pb35d}${star_pb35d} & ${b_pb35e}${star_pb35e} \\

tex & (${se_pb35a}) & (${se_pb35b}) & (${se_pb35c}) & (${se_pb35d}) & (${se_pb35e}) \\[0.1cm]

tex Control mean & ${cm_pb35a} & ${cm_pb35b} & ${cm_pb35c} & ${cm_pb35d} & ${cm_pb35e} \\

tex Observations & ${N_pb35a} & ${N_pb35b} & ${N_pb35c} & ${N_pb35d} & ${N_pb35e} \\[0.1cm]

tex \hline

tex \end{tabular}

texdoc close 

