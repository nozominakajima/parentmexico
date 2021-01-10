
* Analysis: Attrition

* Author: Nozomi Nakajima
* Date: Dec 2020


drop _all
clear all
set mem 30g
set maxvar 11000
set matsize 11000
set more off


tempfile a b c d e


use "$data/panel_school.dta", clear

*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
* 1. "Differential attrition rate test": determines if attrition rates are different across treatment and control groups
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

summ drop if year==2007 & exp_1 == 0
  global mean10_drop: di %5.3f r(mean)
  global sd10_drop: di %-5.3f r(sd)
  
summ drop if year==2007 & exp_1 == 1
  global mean11_drop: di %5.3f r(mean)
  global sd11_drop: di %-5.3f r(sd)
  
reg drop i.exp_1 if year==2007
  global diff1_drop: di %-6.3fc _b[1.exp_1]
  global se1_drop: di %-4.3fc _se[1.exp_1]
   
  test 1.exp_1 = 0
  global p1_drop: di %12.3fc r(p)
  global star1_drop = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 
	
	
* Experiment 2
  
summ drop if year==2009 & exp_2 == 0
  global mean20_drop: di %5.3f r(mean)
  global sd20_drop: di %-5.3f r(sd)
  
summ drop if year==2009 & exp_2 == 1
  global mean21_drop: di %5.3f r(mean)
  global sd21_drop: di %-5.3f r(sd)
  
reg drop i.exp_2 if year==2009
  global diff2_drop: di %-6.3fc _b[1.exp_2]
  global se2_drop : di %-4.3fc _se[1.exp_2]
   
  test 1.exp_2 = 0
  global p2_drop: di %12.3fc r(p)
  global star2_drop = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))

  
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
* 2. "Selective attrition test": determines if the mean of baseline observable characteristics differs across the treatment and control groups conditional on response status
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
	

u "$data/panel_school.dta", clear

merge 1:1 cct year using "$data/panel_parent_long_clean.dta"

	drop if _merge == 1
	drop _merge 

merge 1:1 cct year using "$data/panel_principal_long_clean.dta"
	
	drop _merge

		
*	Generate school type dummies	
*------------------------------------------------------------------------------*
	
tab modalidad, gen(mod)
encode estado, gen(state)	
	
*	Shorten labels for key variables	
*------------------------------------------------------------------------------*

lab var mod1 "General school (1 = Yes)"
lab var mod2 "Indigenous school (1=Yes)"

lab var parent_prim "Highest edu. is primary (1=Yes)"
lab var pb21 "Years as president"

lab var principaledu_col "Highest edu. is teaching college (1=Yes)" 
lab var principaledu_uni "Highest edu. is university (1=Yes)" 
lab var db28a "Years as principal"

lab var teachedu_col "Prop. with teaching college degree"
lab var teachedu_uni "Prop. with university degree"

lab var frate_tot_both "Failure rate" 
lab var rrate_tot_both "Repetition rate" 
lab var drate_tot_both "Dropout rate"


*------------------------------------------------------------------------------*

estimates drop _all

foreach var of varlist ///
  mod1 mod2 ///
  frate_tot_both rrate_tot_both drate_tot_both ///
  parent_prim pb21 pb06 ///
  teachedu_col teachedu_uni {

* Experiment 1

summ `var' if year==2007 & exp_1 == 0 & drop == 0
  global mean10_`var': di %5.3f r(mean)
  global sd10_`var': di %-5.3f r(sd)
  
summ `var' if year==2007 & exp_1 == 1 & drop == 0
  global mean11_`var': di %5.3f r(mean)
  global sd11_`var': di %-5.3f r(sd)
  
reg `var' i.exp_1 if year==2007 & drop == 0, robust cluster(state)
  global diff1_`var': di %-6.3fc _b[1.exp_1]
  global se1_`var': di %-4.3fc _se[1.exp_1]
   
  test 1.exp_1 = 0
  global p1_`var': di %12.3fc r(p)
  global star1_`var' = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 
	
	
* Experiment 2
  
summ `var' if year==2009 & exp_2 == 0 & drop == 0
  global mean20_`var': di %5.3f r(mean)
  global sd20_`var': di %-5.3f r(sd)
  
summ `var' if year==2009 & exp_2 == 1 & drop == 0
  global mean21_`var': di %5.3f r(mean)
  global sd21_`var': di %-5.3f r(sd)
  
reg `var' i.exp_2 if year==2009 & drop == 0, robust cluster(state)
  global diff2_`var': di %-6.3fc _b[1.exp_2]
  global se2_`var' : di %-4.3fc _se[1.exp_2]
   
  test 1.exp_2 = 0
  global p2_`var': di %12.3fc r(p)
  global star2_`var' = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))

  
* Get variable labels

global label_`var': var label `var'
}


* F-test for experiment 1
reg exp_1 parent_prim pb21 pb06 teachedu_col teachedu_uni frate_tot_both rrate_tot_both drate_tot_both if year == 2007 & drop == 0, robust cluster(state)
testparm parent_prim pb21 teachedu_col teachedu_uni frate_tot_both rrate_tot_both drate_tot_both, equal 
local f1 = r(p)
local f1: di %6.3f `f1'


* F-test for experiment 2
reg exp_2 parent_prim pb21 pb06 teachedu_col teachedu_uni frate_tot_both rrate_tot_both drate_tot_both if year == 2009 & drop == 0, robust cluster(state)
testparm parent_prim pb21 teachedu_col teachedu_uni frate_tot_both rrate_tot_both drate_tot_both, equal 
local f2 = r(p)
local f2: di %6.3f `f2'

* Get N
count if year == 2007 & exp_1 == 0 & drop == 0
	local N1_c = r(N)
	
count if year == 2007 & exp_1 == 1 & drop == 0
	local N1_t = r(N)

count if year == 2009 & exp_2 == 0 & drop == 0
	local N2_c = r(N)
	
count if year == 2009 & exp_2 == 1 & drop == 0
	local N2_t = r(N)	
	
*------------------------------------------------------------------------------*
* Balance table for experiment 1 - Panel A
*------------------------------------------------------------------------------*

texdoc init attritionbalance1.tex, replace force

tex \begin{tabular}{lccccclc} \toprule 

tex & \multicolumn{2}{c}{Control:} & \multicolumn{2}{c}{Treatment:} & \multicolumn{3}{c}{Difference:} \\
tex & \multicolumn{2}{c}{Grant \& Info} & \multicolumn{2}{c}{Double Grant \& Info} & \multicolumn{3}{c}{} \\ \cline{2-8}
tex & Mean & (S.D.) & Mean & (S.D.) & T-C &  & (S.E.) \\ \hline

tex \textit{Attrition rate} \\

tex \hspace{3mm} {Attrition (1=Yes)} & ${mean10_drop} & (${sd10_drop}) & ${mean11_drop} & (${sd11_drop}) & ${diff1_drop} & ${star1_drop} & (${se1_drop}) \\

tex Number of schools & \multicolumn{2}{c}{125} & \multicolumn{2}{c}{125} & & & \\ \hline \hline


tex \textit{Panel A: School characteristics} \\

tex {Type of school}  \\
foreach var of varlist mod1 mod2{
tex \hspace{3mm} ${label_`var'} & ${mean10_`var'} & (${sd10_`var'}) & ${mean11_`var'} & (${sd11_`var'}) & ${diff1_`var'} & ${star1_`var'} & (${se1_`var'}) \\
}

tex {Parent association president} \\
foreach var of varlist parent_prim pb21 pb06{
tex \hspace{3mm} ${label_`var'} & ${mean10_`var'} & (${sd10_`var'}) & ${mean11_`var'} & (${sd11_`var'}) & ${diff1_`var'} & ${star1_`var'} & (${se1_`var'}) \\
}

tex {Teachers} \\  
foreach var of varlist teachedu_col teachedu_uni {  
tex \hspace{3mm} ${label_`var'} & ${mean10_`var'} & (${sd10_`var'}) & ${mean11_`var'} & (${sd11_`var'}) & ${diff1_`var'} & ${star1_`var'} & (${se1_`var'}) \\
  }
  
foreach var of varlist ///
  frate_tot_both rrate_tot_both drate_tot_both { 
tex ${label_`var'} & ${mean10_`var'} & (${sd10_`var'}) & ${mean11_`var'} & (${sd11_`var'}) & ${diff1_`var'} & ${star1_`var'} & (${se1_`var'}) \\
  }  
  
tex Number of schools & \multicolumn{2}{c}{`N1_c'} & \multicolumn{2}{c}{`N1_t'} & & & \\
tex \textit{p-value of joint F-test} & & & & & \multicolumn{3}{c}{\textit{`f1'}} \\ \hline     

texdoc close


*------------------------------------------------------------------------------*
* Balance table for experiment 2 - Panel A
*------------------------------------------------------------------------------*

texdoc init attritionbalance2.tex, replace force

tex \begin{tabular}{lccccclc} \toprule 

tex & \multicolumn{2}{c}{Control:} & \multicolumn{2}{c}{Treatment:} & \multicolumn{3}{c}{Difference} \\
tex & \multicolumn{2}{c}{No Grant \& No Info} & \multicolumn{2}{c}{No Grant \& Info} & \multicolumn{3}{c}{} \\ \cline{2-8}
tex & Mean & (S.D.) & Mean & (S.D.) & T-C & & (S.E.) \\ \hline \hline

tex \textit{Attrition rate} \\

tex \hspace{3mm} {Attrition (1=Yes)} & ${mean20_drop} & (${sd20_drop}) & ${mean21_drop} & (${sd21_drop}) & ${diff2_drop} & ${star2_drop} & (${se2_drop}) \\

tex Number of schools & \multicolumn{2}{c}{100} & \multicolumn{2}{c}{80} & & & \\ \hline

tex \textit{Panel A: School characteristics} \\

tex {Type of school}  \\
foreach var of varlist mod1{
tex \hspace{3mm} ${label_`var'} & ${mean20_`var'} & (${sd20_`var'}) & ${mean21_`var'} & (${sd21_`var'}) & ${diff2_`var'} & ${star2_`var'} & (${se2_`var'}) \\
}

tex {Parent association president} \\
foreach var of varlist parent_prim pb21 parent_prim pb21 pb06{
tex \hspace{3mm} ${label_`var'} & ${mean20_`var'} & (${sd20_`var'}) & ${mean21_`var'} & (${sd21_`var'}) & ${diff2_`var'} & ${star2_`var'} & (${se2_`var'}) \\
}

tex {Teachers} \\  
foreach var of varlist teachedu_col teachedu_uni {  
tex \hspace{3mm} ${label_`var'} & ${mean20_`var'} & (${sd20_`var'}) & ${mean21_`var'} & (${sd21_`var'}) & ${diff2_`var'} & ${star2_`var'} & (${se2_`var'}) \\
  }
  
foreach var of varlist ///
  frate_tot_both rrate_tot_both drate_tot_both { 
tex ${label_`var'} & ${mean20_`var'} & (${sd20_`var'}) & ${mean21_`var'} & (${sd21_`var'}) & ${diff2_`var'} & ${star2_`var'} & (${se2_`var'}) \\
  }  
  
tex Number of schools & \multicolumn{2}{c}{`N2_c'} & \multicolumn{2}{c}{`N2_t'} & & & \\
tex \textit{p-value of joint F-test} & & & & & \multicolumn{3}{c}{\textit{`f2'}} \\ \hline     

texdoc close


*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
* 	Baseline student level characteristics
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

u "$data/panel_student_long_clean.dta", clear


*	Shorten labels for key variables	
*------------------------------------------------------------------------------*

label var ap03 "Indigenous (1=Yes)"
label var female "Female (1=Yes)"
label var zwealth "Household wealth index (S.D.)"

tab grado, gen(grade)
rename grade3 grade5
rename grade2 grade4
rename grade1 grade3
label var grade3 "Grade 3 (1=Yes)"
label var grade4 "Grade 4 (1=Yes)"
label var grade5 "Grade 5 (1=Yes)"

label var punt_esp "Language score"
label var punt_mat "Math score"

*------------------------------------------------------------------------------*
estimates drop _all

foreach var of varlist ///
   grade3 grade4 grade5 ///
   ap03 female zwealth punt_esp punt_mat {

* Experiment 1

summ `var' if year==2007 & exp_1 == 0 & drop == 0
  global mean10_`var': di %5.3f r(mean)
  global sd10_`var': di %-5.3f r(sd)
  
summ `var' if year==2007 & exp_1 == 1 & drop == 0
  global mean11_`var': di %5.3f r(mean)
  global sd11_`var': di %-5.3f r(sd)
  
reg `var' i.exp_1 if year==2007 & drop == 0, robust cluster(cct)
  global diff1_`var': di %-6.3fc _b[1.exp_1]
  global se1_`var': di %-4.3fc _se[1.exp_1]
   
  test 1.exp_1 = 0
  global p1_`var': di %12.3fc r(p)
  global star1_`var' = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," "))) 
	
* Experiment 2
  
summ `var' if year==2009 & exp_2 == 0 & drop == 0
  global mean20_`var': di %5.3f r(mean)
  global sd20_`var': di %-5.3f r(sd)
  
summ `var' if year==2009 & exp_2 == 1 & drop == 0
  global mean21_`var': di %5.3f r(mean)
  global sd21_`var': di %-5.3f r(sd)
  
reg `var' i.exp_2 if year==2009 & drop == 0, robust cluster(cct)
  global diff2_`var': di %-6.3fc _b[1.exp_2]
  global se2_`var' : di %-4.3fc _se[1.exp_2]
   
  test 1.exp_2 = 0
  global p2_`var': di %12.3fc r(p)
  global star2_`var' = cond(r(p)<.01,"***", cond(r(p)<.05,"**", cond(r(p)<0.1,"*"," ")))

  
* Get variable labels

global label_`var': var label `var'  
}  
 
* F-test for experiment 1
reg exp_1 ap03 female zwealth grade3 grade4 grade5 punt_esp punt_mat if year == 2007 & drop == 0, robust cluster(cct)
testparm ap03 female zwealth grade3 grade4 grade5 punt_esp punt_mat, equal 
local f1 = r(p)
local f1: di %6.3f `f1'

* F-test for experiment 2
reg exp_2 ap03 female zwealth grade3 grade4 grade5 punt_esp punt_mat if year == 2009 & drop == 0, robust cluster(cct)
testparm ap03 female zwealth grade3 grade4 grade5 punt_esp punt_mat, equal 
local f2 = r(p)
local f2: di %6.3f `f2'

* Get N
count if year == 2007 & exp_1 == 0 & drop == 0
	local N1_c = r(N)
	
count if year == 2007 & exp_1 == 1 & drop == 0
	local N1_t = r(N)

count if year == 2009 & exp_2 == 0 & drop == 0
	local N2_c = r(N)
	
count if year == 2009 & exp_2 == 1 & drop == 0
	local N2_t = r(N)	

*------------------------------------------------------------------------------*
* Balance table for experiment 1 - Panel B
*------------------------------------------------------------------------------*

texdoc init attritionbalance1.tex, append force

tex \textit{Panel B: Student characteristics} \\

foreach var of varlist ap03 female zwealth grade3 grade4 grade5 punt_esp punt_mat {
tex \indent ${label_`var'} & ${mean10_`var'} & (${sd10_`var'}) & ${mean11_`var'} & (${sd11_`var'}) & ${diff1_`var'} & ${star1_`var'} & (${se1_`var'}) \\
}
  
tex Number of students & \multicolumn{2}{c}{`N1_c'} & \multicolumn{2}{c}{`N1_t'} & & & \\
tex \textit{p-value of joint F-test} & & & & & \multicolumn{3}{c}{\textit{`f1'}} \\ \bottomrule     

tex \end{tabular} 
 
texdoc close


*------------------------------------------------------------------------------*
* Balance table for experiment 2 - Panel B
*------------------------------------------------------------------------------*

texdoc init attritionbalance2.tex, append force

tex \textit{Panel B: Student characteristics} \\

foreach var of varlist ap03 female zwealth grade3 grade4 grade5 punt_esp punt_mat {
tex \indent ${label_`var'} & ${mean20_`var'} & (${sd20_`var'}) & ${mean21_`var'} & (${sd21_`var'}) & ${diff2_`var'} & ${star2_`var'} & (${se2_`var'}) \\
}
  
tex Number of students & \multicolumn{2}{c}{`N2_c'} & \multicolumn{2}{c}{`N2_t'} & & & \\
tex \textit{p-value of joint F-test} & & & & & \multicolumn{3}{c}{\textit{`f2'}} \\ \bottomrule 

tex \end{tabular} 

texdoc close



