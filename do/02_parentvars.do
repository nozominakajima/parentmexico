
* Creating variables
* Author: Nozomi Nakajima
* Date: December 2020


drop _all
clear all
set mem 30g
set maxvar 11000
set matsize 11000
set more off


tempfile a b c d e

*------------------------------------------------------------------------------*
use "$data/panel_parent_long.dta", clear
*------------------------------------------------------------------------------*


*------------------------------------------------------------------------------*
* Education level
*------------------------------------------------------------------------------*

gen parent_prim = 0 if pb16a >= 2 & pb16a < .

	replace parent_prim = 1 if pb16a < 2
	lab var parent_prim "Highest edu. is primary (1=Yes)"
	
	la define parent_prim 0 "No" 1 "Yes"
	la val parent_prim parent_prim
	
	
*------------------------------------------------------------------------------*
* Polychoric principal component analysis (polychoricpca) 
* 			& 
* Confirmatory factor analysis (sgem)
*------------------------------------------------------------------------------*

** rescale so that higher values are more positive

	la def pb62 1 "Never" 2 "Sometimes" 3 "Always", replace

	foreach var in pb62f pb62l pb62n pb62p pb62r pb62s {
	
		replace `var' = 4-`var'
		lab val `var' pb62
	
	} 


** Parent-Teacher Job Index
*------------------------------------------------------------------------------*
alpha pb62f pb62l pb62n pb62p pb62r pb62s, std
 
// PCA:  
polychoricpca pb62f pb62l pb62n pb62p pb62r pb62s, score(gen_1) nscore(1)
drop _*

summ gen_11 if year==2007 & exp_1==0
	local mean_gen_11 = r(mean)
	local sd_gen_11 = r(sd)
	gen parent_teacher_STD1 = (gen_11-`mean_gen_11') / `sd_gen_11' if !missing(exp_1) & year == 2007
summ gen_11 if year==2008 & exp_1==0
	local mean_gen_11 = r(mean)
	local sd_gen_11 = r(sd)
	replace parent_teacher_STD1 = (gen_11-`mean_gen_11') / `sd_gen_11' if !missing(exp_1) & year == 2008
summ gen_11 if year==2009 & exp_1==0
	local mean_gen_11 = r(mean)
	local sd_gen_11 = r(sd)
	replace parent_teacher_STD1 = (gen_11-`mean_gen_11') / `sd_gen_11' if !missing(exp_1) & year == 2009
summ gen_11 if year==2010 & exp_1==0
	local mean_gen_11 = r(mean)
	local sd_gen_11 = r(sd)
	replace parent_teacher_STD1 = (gen_11-`mean_gen_11') / `sd_gen_11' if !missing(exp_1) & year == 2010	


summ gen_11 if year==2009 & exp_2==0
	local mean_gen_11 = r(mean)
	local sd_gen_11 = r(sd)
	gen parent_teacher_STD2 = (gen_11-`mean_gen_11') / `sd_gen_11' if !missing(exp_2) & year == 2009
summ gen_11 if year==2010 & exp_2==0
	local mean_gen_11 = r(mean)
	local sd_gen_11 = r(sd)
	replace parent_teacher_STD2 = (gen_11-`mean_gen_11') / `sd_gen_11' if !missing(exp_2) & year == 2010	

gen pt_job_pca = parent_teacher_STD1 if !missing(exp_1)
	replace pt_job_pca = parent_teacher_STD2 if !missing(exp_2) 
	label var pt_job_pca "Parent-Teacher Job Index"

drop gen_11 parent_teacher_STD1 parent_teacher_STD2

reg pt_job_pca exp_1 if year == 2008 & drop ==0, robust cluster(cct)
reg pt_job_pca exp_1 if year == 2009 & drop ==0, robust cluster(cct)
reg pt_job_pca exp_1 if year == 2010 & drop ==0, robust cluster(cct)
reg pt_job_pca exp_2 if year == 2010 & drop ==0, robust cluster(cct)



// CFA:
sem (pb62f pb62l pb62n pb62p pb62r pb62s <- PT_JOB_CFA), method(ml) standardized
	estat eqgof
	estat gof, stats(all)
	predict pt_job_cfa, latent


// Compare PCA and CFA: 
corr pt_job_pca pt_job_cfa

drop pt_job_cfa

order estado-pb16a parent_prim pb18-pb62s pt_job_pca



*------------------------------------------------------------------------------*
** Parent's trust
*------------------------------------------------------------------------------*

summ trustte	


*------------------------------------------------------------------------------*
** Missing covariates
*------------------------------------------------------------------------------*

* Code missing as mean with dummy indicator for missing

foreach var in pb03 pb06 pb07 pb16a parent_prim pb18 pb21 pb35a pb35b pb35c pb35d pb35e pb27 pb37 pb38 pb39 expenditure_edusupplies expenditure_health expenditure_repair expenditure_upgrade expenditure_utilities expenditure_transport expenditure_construct pb48 pb51_1a pb51_1e pb51_1h pb55_1a pb55_1b pb55_1c pb55_1d pb55_1e pb55_1f pb55_1g pb55_1h trustte pt_job_pca {
	
	* generate missing indicator
	
	gen `var'_m = `var' ==.
	
	
	* recode to mean if missing
	summ `var'
	local mean = round(r(mean),1)
	replace `var' = `mean' if `var'_m == 1

	}


	 
*------------------------------------------------------------------------------*
save "$data/panel_parent_long_clean.dta", replace
*------------------------------------------------------------------------------*
 	 


	
