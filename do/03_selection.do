
* Selection mechanism

* Author: Nozomi Nakajima
* Date: Dec 2020


drop _all
clear all
set mem 30g
set maxvar 11000
set matsize 11000
set more off

set scheme plotplainblind 
*set scheme s1color

tempfile a b c d e

*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
* Selection into experiments (high indigenous population in experiment 1)
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

use "$data/panel_student_long_clean.dta", clear

* indigenous status of students
tab ap03


* collapse to get each school's % indigenous

keep if year == 2009 & !missing(exp_3)

save tempfile, replace
statsby, by(cct modalidad exp_3) : ci mean ap03
	
	
	
* probability of selection to treatment by % indigenous
logit exp_3 mean, robust cluster(cct)
predict phat

twoway (scatter phat mean, mcolor(black%40)), ///
	ytitle("Probability of selection to treatment", height(6) size(vlarge)) ///
	ylabel(0.4(0.2)1, labsize(large) nogrid) ///
	xtitle("Proportion of indigenous students", height(6) size(vlarge)) ///
	xlabel(, labsize(large) nogrid) 
graph export probability.pdf, replace

drop phat

binscatter exp_3 mean, nquantiles(20) linetype(none) mcolor(black) msymbol(circle) ///
	ytitle("Prob. of selection to treatment", height(6) size(vlarge)) ///
	ylabel(, labsize(large) nogrid) ///
	xtitle("Proportion of indigenous students", height(6) size(vlarge)) ///
	xlabel(, labsize(large) nogrid) 
graph export probability.pdf, replace
	
* common support check using histogram of % indigenous
		
drop if modalidad == "Indigena"
	* we first trim all the indigenous schools
	
twoway (kdensity mean if exp_3==1, bwidth(0.043) lpattern(solid) lcolor(black)) ///
       (kdensity mean if exp_3==0, bwidth(0.043) lpattern(dash) lcolor(black)), ///
	   legend(order(1 "Density of treatment group" 2 "Density of comparison group") ///
	   row(2) position(6) size(vlarge)) ///
	   xtitle("Proportion of indigenous students", height(6) size(vlarge)) ///
	   ytitle("Density", height(6) size(vlarge)) ///
	   xlabel(, labsize(large) nogrid) ///
	   ylabel(, labsize(large) nogrid)
	   
graph export cs_kdensity.pdf, replace


summ mean if exp_3 == 0, detail	
return list
local a = round(`r(p75)', 0.01) 
local b = round(`r(p90)', 0.01) 
local c = round(`r(p95)', 0.01) 
local d = round(`r(p99)', 0.01) 
   	   
twoway (histogram mean if exp_3==1 & mean>=`a' & mean <=`d', density width(0.043) color(grey%70)) ///
       (histogram mean if exp_3==0 & mean>=`a' & mean <=`d', density width(0.043) color(black%40)), ///
	   legend(order(1 "Density of treatment group" 2 "Density of comparison group") row(2) position(6) size(vlarge)) ///
	   xtitle("Proportion of indigenous students", height(6) size(vlarge)) ///
	   ytitle("Density", height(6) size(vlarge)) ///
 	   xlabel(`a' `b' `c' `d', labsize(large) nogrid) ///
	   ylabel(, labsize(large) nogrid)
	   
graph export cs_histogram.pdf, replace	   
	   
list cct modalidad exp_3 if mean > `d' 	   
	* no treatment school larger than 99% of comparison schools



