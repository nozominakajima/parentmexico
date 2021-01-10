
* Creating variables
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
use "$data/panel_teacher_long.dta", clear
*------------------------------------------------------------------------------*


* Education
*------------------------------------------------------------------------------*
gen teacheredu_sec = .
	replace teacheredu_sec = 1 if mb19a > 2 & mb19a < .
	replace teacheredu_sec = 0 if mb19a < 3
	la var teacheredu_sec "Teacher has secondary (Bachillerato) or more"
gen teacheredu_col = .
	replace teacheredu_col = 1 if mb19a > 3 & mb19a < .
	replace teacheredu_col = 0 if mb19a <  4
	la var teacheredu_col "Teacher has teaching college (Normal preescolar/primaria/superior) or more"
gen teacheredu_uni = .
	replace teacheredu_uni = 1 if mb19a > 5 & mb19a < .
	replace teacheredu_uni = 0 if mb19a < 6
la var teacheredu_uni "Teacher has university (licenciatura) or more"


* Trust
*------------------------------------------------------------------------------*	
summ trust


*------------------------------------------------------------------------------*
* Polychoric principal component analysis (polychoricpca) 
* 			& 
* Confirmatory factor analysis (sgem)
*------------------------------------------------------------------------------*

** rescale so that higher values are more positive

	la define mb74 1 "Never" 2 "Sometimes" 3 "Most" 4 "Always", replace

	foreach var in mb74e mb74f mb74g mb74h {
		
	replace `var' = 5 -`var'
	la val `var' mb74 
	
	} 


** generate Teacher-Parent Job Index
*------------------------------------------------------------------------------*
alpha mb74e mb74f mb74g mb74h, std

// PCA:
polychoricpca mb74e mb74f mb74g mb74h, score(gen_1) nscore(1)
drop _*

summ gen_11 if year==2007 & exp_1==0
	local mean_gen_11 = r(mean)
	local sd_gen_11 = r(sd)
	gen teacher_parent_STD1 = (gen_11-`mean_gen_11') / `sd_gen_11' if !missing(exp_1) & year==2007
summ gen_11 if year==2008 & exp_1==0
	local mean_gen_11 = r(mean)
	local sd_gen_11 = r(sd)
	replace teacher_parent_STD1 = (gen_11-`mean_gen_11') / `sd_gen_11' if !missing(exp_1) & year==2008
summ gen_11 if year==2009 & exp_1==0
	local mean_gen_11 = r(mean)
	local sd_gen_11 = r(sd)
	replace teacher_parent_STD1 = (gen_11-`mean_gen_11') / `sd_gen_11' if !missing(exp_1) & year==2009
summ gen_11 if year==2010 & exp_1==0
	local mean_gen_11 = r(mean)
	local sd_gen_11 = r(sd)
	replace teacher_parent_STD1 = (gen_11-`mean_gen_11') / `sd_gen_11' if !missing(exp_1) & year==2010	

summ gen_11 if year==2009 & exp_2==0
	local mean_gen_11 = r(mean)
	local sd_gen_11 = r(sd)
	gen teacher_parent_STD2 = (gen_11-`mean_gen_11') / `sd_gen_11' if !missing(exp_2) & year==2009
summ gen_11 if year==2010 & exp_2==0
	local mean_gen_11 = r(mean)
	local sd_gen_11 = r(sd)
	replace teacher_parent_STD2 = (gen_11-`mean_gen_11') / `sd_gen_11' if !missing(exp_2) & year==2010

gen tp_job_pca = teacher_parent_STD1 if !missing(exp_1)
	replace tp_job_pca = teacher_parent_STD2 if !missing(exp_2) 
	label var tp_job_pca "Teacher-Parent Job Index (SD)"

drop gen_11 teacher_parent_STD1 teacher_parent_STD2


// CFA:
sem (mb74e mb74f mb74g mb74h <- TP_JOB_CFA), method(ml) standardized
	estat eqgof
	estat gof, stats(all)
	predict tp_job_cfa, latent


// Compare PCA and CFA:
corr tp_job_pca tp_job_cfa


reg tp_job_pca exp_1 if year == 2008 & drop == 0, robust cluster(cct)
reg tp_job_pca exp_1 if year == 2009 & drop == 0, robust cluster(cct)
reg tp_job_pca exp_1 if year == 2010 & drop == 0, robust cluster(cct)

reg tp_job_pca exp_2 if year == 2010 & drop == 0, robust cluster(cct)

drop tp_job_cfa

order estado-mb19a teacheredu_sec teacheredu_col teacheredu_uni mb34a-trust tp_job_pca

*------------------------------------------------------------------------------*
** Missing covariates
*------------------------------------------------------------------------------*

* Code missing as mean with dummy indicator for missing

foreach var of varlist mb03-tp_job_pca {
	
	* generate missing indicator
	
	gen `var'_m = `var' ==.
	
	
	* recode to mean if missing
	summ `var'
	local mean = round(r(mean),1)
	replace `var' = `mean' if `var'_m == 1
}


*------------------------------------------------------------------------------*
save "$data/panel_teacher_long_clean.dta", replace
*------------------------------------------------------------------------------*


