
* AGE data prep & analysis
* Author: Nozomi Nakajima
* Date: December 2020

* This is the Master do file that executes all of the do files.

cap log close 
clear
set mem 100m 
set more off 
set seed 1234 


** Set directory files **
*-----------------------*

	global path 	"" // set your own path inside quotes
	global data 	"$path/data"
	global output	"$path/output"
	global do		"$path/do"
	
*------------------------------*
* Clean & create key variables
*------------------------------*
	
do "$do/02_parentvars.do"
	* Cleans & generates parent variables for analysis	
	* Produces "panel_parent_long_clean.dta"
		
do "$do/02_teachervars.do"
	* Cleans & generates teacher variables for analysis	
	* Produces "panel_teacher_long_clean.dta"
	
do "$do/02_studentvars.do"
	* Cleans & generates student variables for analysis	
	* Produces "panel_student_long_clean.dta"	
	
	
*-----------------------*
* Baseline balance table
*-----------------------*

do "$do/03_baseline.do"
	* Estimates balance of vars in experiment 1 and 2
	* Produces Table 1 & Table 2 

	
do "$do/03_selection.do"
	* Checks for common support for non-experimental contrast
	* Produces Figure 3
	
*-----------------------*
* Analysis
*-----------------------*

// Implementation: Expenditure
do "$do/04_expenditure.do"
	* Descriptive statistics of how double grants were allocated
	* Produces Figure 4

	
// Implementation: Information	
do "$do/04_information.do"
	* Regress information session administered by information experiment condition 
	* Produces Table 3
	
// Parental Involvement	
do "$do/04_parentinv.do"
	* Effect of treatments on parental involvement
	* Produces Table 4 & Table A3
	
do "$do/04_parentinv_pds.do"
	* Robustness check of post-double selection lasso estimation 
	* Produces Figure A1 
	
// Parent & Teacher Inputs	
do "$do/04_inputs.do"
	* Effect of treatments on parent & teacher inputs
	* Produces Table 5 & Table A4
	
do "$do/04_inputs_pds.do"
	* Robustness check of post-double selection lasso estimation  
	* Produces Figure A2	
	
// Education outcomes
do "$do/04_eduoutcomes.do"
	* Effect of treatments on educational outcomes
	* Produces Table 6 & Table A5

do "$do/04_eduoutcomes_pds.do"
	* Robustness check of post-double selection lasso estimation  
	* Produces Figure A3

// Mechanisms	
do "$do/04_mechanism.do"
	* Effect of treatments on trust
	* Produces Table 7

do "$do/04_mechanism_pds.do"
	* Robustness check of post-double selection lasso estimation  
	* Produces Figure A4

*----------------------------------------*
* Diff-in-diff across treatments
*----------------------------------------*

do "$do/05_parentinv_diff.do"
	* Difference between treatment effects for parental involvement outcome
	* Produces Figure A5

do "$do/05_inputs_diff.do"
	* Difference between treatment effects for parenting & teaching behavior
	* Produces Figure A6

do "$do/05_eduoutcomes_diff.do"
	* Difference between treatment effects for educational outcomes
	* Produces Figure A7

do "$do/05_mechanism_diff.do"
	* Difference between treatment effects for trust
	* Produces Figure A8

*-----------------------*
* Attrition
*-----------------------*

do "$do/06_attrition.do"
	* Produces Table A1 & Table A2

*---------------------------------*
* Interpret S.D. of index
*---------------------------------*

do "$do/07_interpret.do"
	* Regress constructed index on baseline covariates
	* Produces Table A6





