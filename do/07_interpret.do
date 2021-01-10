
* Interpretation of teaching practices

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
* Student-centered instruction
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

* cleaned data has the index

use "$data/panel_student_long_clean.dta", clear

keep cct year exp_1 exp_2 exp_3 grado id_student class_teach

rename class_teach c4

save `a', replace


* un-cleaned data has the raw variables

use "$data/panel_student_long.dta", clear

gen ap27h_flip = 1-ap27h
	
keep cct year exp_1 exp_2 exp_3 grado id_student ap27g ap27j ap27h_flip ap27i

save `b', replace


* merge

use `a', clear

merge 1:1 cct year grado id_student using `b'

drop _merge


*------------------------------------------------------------------------------*

// baseline measure of student-centered teaching 
summ c4 ap27g ap27j ap27h_flip ap27i if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0)


// items that went into c4
eststo model1: reg ap27g c4 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust

eststo model2: reg ap27j c4 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust 

eststo model3: reg ap27h_flip c4 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust 

eststo model4: reg ap27i c4 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust  


*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
* Effect on trust & blame (parent perspective)
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

use "$data/panel_parent_long_clean.dta", clear

rename (trustte pt_job_pca)(mech1 mech2) 


*------------------------------------------------------------------------------*

// baseline measure of student-centered teaching 
summ mech2 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0)

// items that went into mech2
summ pb62f pb62l pb62n pb62p pb62r pb62s if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0)

foreach var in pb62f pb62l pb62n pb62p pb62r pb62s {
	
	replace `var' = 0 if `var' < 3
	replace `var' = 1 if `var' == 3

}

// how is parent's perspective of teacher's job responsibility correlated with items (at baseline)?
eststo model5: reg pb62f mech2 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust cluster(cct)
  
eststo model6: reg pb62l mech2 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust cluster(cct)

eststo model7: reg pb62n mech2 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust cluster(cct)

eststo model8: reg pb62p mech2 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust cluster(cct)

eststo model9: reg pb62r mech2 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust cluster(cct)

eststo model10: reg pb62s mech2 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust cluster(cct)  

*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
* Effect on educational outcomes (teacher perspective)
*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*
use "$data/panel_teacher_long_clean.dta", clear

rename (trust tp_job_pca) (mech3 mech4) 

// items that went into mech2
summ mb74e mb74f mb74g mb74h if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0)

foreach var in  mb74e mb74f mb74g mb74h {
	
	replace `var' = 0 if `var' < 3
	replace `var' = 1 if `var' >= 3

}

// how is teacher's perspective of parent's job responsibility correlated with items (at baseline)?
eststo model11: reg mb74e mech4 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust cluster(cct)

eststo model12: reg mb74f mech4 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust cluster(cct)

eststo model13: reg mb74g mech4 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust cluster(cct)

eststo model14: reg mb74h mech4 if (year == 2007 & exp_1 == 0) | (year == 2009 & exp_2 == 0), robust cluster(cct)


*------------------------------------------------------------------------------*
*------------------------------------------------------------------------------*

esttab, b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) 

mat list r(coefs)

matrix A = r(coefs)'

matselrc A B , c(1, 2, 3)

matrix list B

esttab matrix(B) using inter.tex, replace 


 
