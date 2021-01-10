
* Analysis: Treatment effect on expenditure

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
*	Figure
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

set scheme plotplainblind


use "$data/panel_parent_long_clean.dta", clear

rename (expenditure_edusupplies expenditure_health expenditure_repair expenditure_upgrade expenditure_utilities expenditure_transport expenditure_construct) (e1 e2 e3 e4 e5 e6 e7)

keep if !missing(exp_1)
// only experiment 1 is relevant for grants

keep cct year exp_1 e1 e2 e3 e4 e5 e6 e7

reshape long e, i(exp_1 cct year) j(category)
// each row is a unique school by expenditure category 
   
save tempfile, replace 
 
statsby, by(exp_1 year category) : ci mean e
// collapse data to group (t/c) by category by year

replace year = 1 if year == 2007
replace year = 2 if year == 2008
replace year = 3 if year == 2009
replace year = 4 if year == 2010

drop if year == 1
drop if exp_1 == 0
// only keep treatment schools in 2008-2010

generate catyear = _n
replace catyear = catyear + 1 if catyear > 7
replace catyear = catyear + 1 if catyear > 15

twoway (bar mean catyear if category==1) ///
       (bar mean catyear if category==2) ///
       (bar mean catyear if category==3) ///
	   (bar mean catyear if category==4) ///
	   (bar mean catyear if category==5) ///
       (bar mean catyear if category==6) ///
       (bar mean catyear if category==7) ///
       (rcap ub lb catyear), ///
	   legend(order(1 "Learning related supplies" ///
					2 "Health related supplies" ///
					3 "Repairs" ///
					4 "Upgrades" ///
					5 "Rent & utilities" ///
					6 "Transportation" ///
					7 "Construction") pos(6) col(1)) ///
	   xlabel(4 "1 year" 11 "2 years" 18 "3 years", notick nogrid) ///
	   ylabel(,nogrid) ///
       xtitle("Post treatment years") ytitle("Percent of double grant spent")			
	
graph export expenditure.pdf, replace		


*------------------------------------------------------------------------------*
* How was single grant being allocated in control schools in 2008-2010?
*------------------------------------------------------------------------------*
use "$data/panel_parent_long_clean.dta", clear

rename (expenditure_edusupplies expenditure_health expenditure_repair expenditure_upgrade expenditure_utilities expenditure_transport expenditure_construct) (e1 e2 e3 e4 e5 e6 e7)

keep if !missing(exp_1)
// only experiment 1 is relevant for grants

keep cct year exp_1 e1 e2 e3 e4 e5 e6 e7

reshape long e, i(exp_1 cct year) j(category)
// each row is a unique school by expenditure category 
  
drop if year == 2007
// only keep 2008-2010 when double grant was offered to treatment schools
 
save tempfile, replace 
 
statsby, by(exp_1 category) : ci mean e
// each row is the expenditure category by treatment/control group

generate catyear = _n 

keep if exp_1 == 0
// only keep control schools

twoway (bar mean catyear if category==1) ///
       (bar mean catyear if category==2) ///
       (bar mean catyear if category==3) ///
	   (bar mean catyear if category==4) ///
	   (bar mean catyear if category==5) ///
       (bar mean catyear if category==6) ///
       (bar mean catyear if category==7) ///
       (rcap ub lb catyear), ///
	   legend(order(1 "Learning related supplies" ///
					2 "Health related supplies" ///
					3 "Repairs" ///
					4 "Upgrades" ///
					5 "Rent & utilities" ///
					6 "Transportation" ///
					7 "Construction") pos(6) col(1)) ///
	   xlabel(, notick nogrid) ///
	   ylabel(,nogrid) ///
       xtitle("categories") ytitle("Percent of grant spent")			
	
graph export expenditure_control.pdf, replace
