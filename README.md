# Replication files for “Promoting Parental Involvement in Schools: Evidence from Two Randomized Experiments”

Authors: Felipe Barrera-Osorio, Paul Gertler, Nozomi Nakajima, Harry A. Patrinos

## Overview
This repository contains data and code to replicate the analysis in the paper: Barrera-Osorio, Gertler, Nakajima, and Patrinos. “Promoting Parental Involvement in Schools: Evidence from Two Randomized Experiments”.

## Dataset
The analysis uses data obtained from the National Council for Educational Development (CONAFE) and the Ministry of Public Education (SEP) in Mexico. Survey data is available through the World Bank Microdata Library [2007](https://microdata.worldbank.org/index.php/catalog/1036), [2008](https://microdata.worldbank.org/index.php/catalog/1037), [2009](https://microdata.worldbank.org/index.php/catalog/1038), and [2010](https://microdata.worldbank.org/index.php/catalog/1039). Administrative data (school census and national test scores) may be obtained with Data Use Agreements with CONAFE and SEP. As per data use agreement with CONAFE and SEP, we provide de-identified data that links survey and administrative data as part of our replication files.   

+ “panel_school.dta”
> This file contains school level administrative data.

+ “panel_parent_long.dta”
> This file contains parent association level data.

+ “panel_teacher_long.dta”
> This file contains teacher level data.

+ “panel_student_long.dta”
> This file contains student level data.

## Code
All tables and figures in the paper can be replicated using the following programs:

The program “01_toc.do” is a master do file that runs the programs to clean the data, generate key variables, and analyze the data. Run the programs in order as indicated by this master do file as later parts of the program rely on datasets generated in earlier parts.

The programs "02_parentvars.do", "02_teachervars.do", and "02_studentvars.do" clean and generate variables needed for analysis. 

The program "03_baseline.do" checks for balance in baseline characteristics between treatment and control schools in the two experiments. The file "03_selection.do" clarifies the selection process into the two experiments and performs data checks to set up the analysis across experiments.

The programs "04_expenditure.do" and "04_information.do" checks the implementation of the interventions. 

The programs "04_parentinv.do", "04_inputs.do", and "04_eduoutcomes.do" estimates the treatment effects on parental involvement in schools (parentinv), parenting and teaching practices (inputs), and educational outcomes (eduoutcomes). The file "04_mechanism.do" estimates the effect on trust.

The programs with the suffix pds "04_*_pds.do" performs robustness checks for the post-double selection lasso procedure to compare across experiments.

The programs "05_*.do" estimates the difference in treatment effects across the three contrasts (double grant, information, and single grant). 

The program "06_attrition.do" checks for differences in attrition rates across experimental groups.

The program "07_interpret.do" assists with interpreting standard deviation units of constructed indices.

## Survey questionnaire
The complete survey questionnaires are publicly available here: [2007](https://microdata.worldbank.org/index.php/catalog/1036), [2008](https://microdata.worldbank.org/index.php/catalog/1037), [2009](https://microdata.worldbank.org/index.php/catalog/1038), and [2010](https://microdata.worldbank.org/index.php/catalog/1039).
