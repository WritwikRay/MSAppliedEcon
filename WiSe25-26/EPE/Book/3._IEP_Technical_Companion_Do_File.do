/* Impact Evaluation in Practice, second edition, Technical Companion
do file for examples presented in technical companion, Version 1.0; September 2016 */
clear
clear matrix
set more off
*============================*
*Install packages to update Stata as needed
*============================*
ssc install psmatch2, replace
net install st0366.pkg, all replace force from(http://www.stata-journal.com/software/sj14-4/)
net install sg65.pkg, all replace force from(http://www.stata.com/stb/stb35)
net install sxd4.pkg, all replace force from(http://www.stata.com/stb/stb60)
*============================*
*Specify the access path to the computer folder you will use for the analysis
*============================*
* cd "INSERT THE PATH OF THE FOLDER WHERE YOU SAVED THE DATASET, EG. C:\My Documents\HISP"

*============================*
*Initialize
*============================*
cap log close 
log using "examples_log.txt", replace 
*Open the cleaned data set
use "evaluation.dta" 

*============================*
*Start replicating examples
*============================*

* RANDOMIZED ASSIGNMENT
* In this context, the program is randomized at the village level, and you compare follow-up situation of eligible households in treatment and comparison villages.

*Select the relevant data
use "evaluation.dta", clear
keep if eligible==1

*Example 1 - Randomized Assignment in a Regression Framework (Linear Regression)
reg health_expenditures treatment_locality if round ==1, cl(locality_identifier)

*Example 2 - Testing for Balance in a Baseline Outcome 
reg health_expenditures treatment_locality if round ==0, cl(locality_identifier)

*Example 3 - Testing Balance in a Baseline Covariate
reg age_hh treatment_locality if round ==0, cl(locality_identifier)

*Example 4 - Randomized Assignment in a Regression Framework (Multivariate Regression)
reg health_expenditures treatment_locality age_hh age_sp educ_hh educ_sp female_hh indigenous hhsize dirtfloor bathroom land hospital_distance if round ==1, cl(locality_identifier)

*----------------------------------------------------*
* INSTRUMENTAL VARIABLES, ITT and LATE ESTIMATES 
* In this context, the program is randomized at the village level.
* While everyone is eligible for the program in treatment communities, not everyone participates. 

*Select the relevant data
use "evaluation.dta", clear
drop eligible

* You can estimate 'intent-to-treat estimates', i.e. program impact at the village-level irrespective of who takes up the program or not.
*Example 5 - "Intent-to-Treat" (ITT) Estimates
reg health_expenditures treatment_locality if round ==1, cl(locality_identifier)

* You can back out 'local average treatment effect' estimates on complier units that do take-up the program in treatment communities
*Example 6 - "Local Average Treatment Effect" (Late) 2SLS IV estimates
ivreg health_expenditures (enrolled = treatment_locality) if round ==1, first 

* note that the followign syntax is from more recent command has been introduced in stata for the same estimation
ivregress 2sls health_expenditures (enrolled = treatment_locality) if round ==1, first

*----------------------------------------------------*
* INSTRUMENTAL VARIABLES AND RANDOMIZED PROMOTION 
* In this context, everyone is eligible for the program. You compare what happens in promoted and non-promoted villages.
 
*Select the relevant data
use "evaluation.dta", clear
drop eligible
drop treatment_locality
drop enrolled
 
* Example 7 - 2SLS IV Estimates for Randomized Promotion
ivreg health_expenditures (enrolled_rp = promotion_locality) if round ==1, first

* you could also use a multivariate regression
ivreg health_expenditures (enrolled_rp = promotion_locality) age_hh age_sp educ_hh educ_sp female_hh indigenous hhsize dirtfloor bathroom land hospital_distance if round ==1, first

* note that the followign syntax is from more recent command has been introduced in stata for the same estimation
ivregress 2sls health_expenditures (enrolled_rp = promotion_locality) if round ==1, first

*----------------------------------------------------*
* DIFFERENCE-IN-DIFFERENCES
* In this method, you compare the change in health expenditures over time 
* between enrolled and nonenrolled households in the treatment localities.

*Select the relevant data
use "evaluation.dta", clear
keep if treatment_locality==1

* Example 8 - Difference-in-Differences in a Regression Framework
gen eligible_round=eligible*round
reg health_expenditures eligible_round round eligible, cl(locality_identifier)

* Example 9 - Difference-in-Differences in a Multivariate Regression Framework
reg health_expenditures eligible_round round eligible age_hh age_sp educ_hh educ_sp female_hh indigenous hhsize dirtfloor bathroom land hospital_distance, cl(locality_identifier)
	
* Example 10 - Household Fixed Effect Estimates for Difference-in-Differences
xtset household_identifier round
gen xtenrolled=0
replace xtenrolled=1 if enrolled==1 & round==1

xtreg health_expenditures xtenrolled round, fe vce(cluster locality_identifier)

* Example 11 -  Calculating Difference-in-Differences Estimates by Taking the Difference between Before-After Difference in the Treatment and Comparison Groups
keep health_expenditures treatment_locality locality_identifier enrolled household_identifier round
reshape wide health_expenditures enrolled, i(household_identifier) j(round)
 	
gen dy = health_expenditures1 - health_expenditures0 
replace enrolled0=0
gen dp = enrolled1-enrolled0 
	
reg dy dp if treatment_locality==1, cl(locality_identifier)
	
*----------------------------------------------------*
* REGRESSION DISCONTINUITY DESIGN	
* In this context, you compare health expenditures at follow-up between households just above 
* and just below the poverty index threshold, in the treatment localities.

*Select the relevant data
use "evaluation.dta", clear
keep if treatment_locality==1

* Example 12 - Regression Discontinuity Design Estimates

*Normalize the poverty index 
gen poverty_index_left=poverty_index-58 if poverty_index<=58 
replace poverty_index_left=0 if poverty_index>58
gen poverty_index_right=poverty_index-58 if poverty_index>58 
replace poverty_index_right=0 if poverty_index<=58

reg health_expenditures poverty_index_left poverty_index_right eligible if round ==1 

predict he_pred1
* Creating a simple graph.
graph7 he_pred1 poverty_index if round ==1

*----------------------------------------------------*
* MATCHING	
* In this context, you compare health expenditures at follow-up between enrolled 
* households and a set of matched nonenrolled households from both treament and comparison villages.

*Select the relevant data
use "evaluation.dta", clear

* reshape the database
reshape wide health_expenditures age_hh age_sp educ_hh educ_sp hospital, i(household_identifier) j(round)
drop age_hh1 age_sp1 educ_hh1 educ_sp1 hospital1
rename age_hh0 age_hh
rename age_sp0 age_sp
rename educ_hh0 educ_hh
rename educ_sp0 educ_sp
rename hospital0 hospital

* Example 13 - Propensity Scores Matching Estimates

probit enrolled age_hh age_sp educ_hh educ_sp female_hh indigenous hhsize dirtfloor bathroom land hospital_distance

predict pscore
kdensity pscore if enrolled ==1, gen(take1 den1)
kdensity pscore if enrolled ==0, gen(take0 den0)
* to replicate the graph with dashed line
* graph twoway (line den0 take0, lpattern(solid)) (line den1 take1, lpattern( dash))
twoway (line den0 take0) (line den1 take1)

* Nearest neighbor propensity score matching on a 0/1 variable requires the observations to be sorted in a random order. 
* Sort the observations in a random order
* generate a random number and sort observations according to that number
set seed 100
generate u=runiform()
sort u

psmatch2 enrolled age_hh age_sp educ_hh educ_sp female_hh indigenous hhsize dirtfloor bathroom land hospital_distance, out(health_expenditures1) 

* Standard error on the impact estimate
* There are different views on the way to estimate the standard error - here are two of them.
* Estimating standard errors using bootstrapping.
set seed 100
bootstrap r(att) : psmatch2 enrolled age_hh age_sp educ_hh educ_sp female_hh indigenous hhsize dirtfloor bathroom land hospital_distance, out(health_expenditures1)
* Estimating standard errors using linear regression. 
reg health_expenditures1 enrolled [fweight=_weight]


xxxx;

*----------------------------------------------------*
* Matched difference-in-differences

* Manually compute the matched difference-in-differences
sort _id
gen health_exp_match0 = health_expenditures0[_n1] /*variable contains health expenditures of nearest neighbor match at baseline */
gen health_exp_match1 = health_expenditures1[_n1] /*variable contains health expenditures of nearest neighbor match at follow-up */
summ health_expenditures0 health_expenditures1 health_exp_match0 health_exp_match1 if enrolled==1
gen matchedDD=(health_expenditures1-health_expenditures0)-(health_exp_match1-health_exp_match0) if enrolled==1
summ matchedDD

* Use regression to compute matched difference-in-differences and standard error on DD
gen diff=health_expenditures1-health_expenditures0
tab _weight, missing
gen matched=_weight>=1 & _weight~=.& enrolled==0
drop if matched==0&enrolled==0
reg diff enrolled [fweight=_weight]

* Note that you might find some small difference in the estimates depending on the software version.

*----------------------------------------------------*
* POWER CALCULATIONS WITHOUT CLUSTERS

use "evaluation.dta", clear

*Note: Focus on the randomized assignment case in chapter 4
drop if eligible==0

* Example 14 - Power Calculations without Clusters (out-of-pciket expenditures)
sum health_expenditures if round==1 & treatment_locality==1 
	local m1 = `r(mean)' /*This saves the mean which will be used as m1 in power calculations below*/
	local sd = `r(sd)' /*This saves the standard deviation which will be used as sd1 and sd2 in power calclulations below*/

local mde_1 = `m1'-1
local mde_2 = `m1'-2
local mde_3 = `m1'-3

/*$1 Minimum Detectable Effect*/ sampsi `m1' `mde_1', p(0.8) r(1) sd1(`sd') sd2(`sd') /*Note: Use the command sampncti when sample sizes are small*/
/*$2 Minimum Detectable Effect*/ sampsi `m1' `mde_2', p(0.8) r(1) sd1(`sd') sd2(`sd')
/*$3 Minimum Detectable Effect*/ sampsi `m1' `mde_3', p(0.8) r(1) sd1(`sd') sd2(`sd')

sampsi `m1' `mde_1', p(0.8) r(1) sd1(`sd') sd2(`sd')

*----------------------------------------------------*
* POWER CALCULATIONS WITH CLUSTERS

use "evaluation.dta", clear

*Note: Focus on the randomized assignment case in chapter 4
drop if eligible==0

* Example 14 - Power Calculations without Clusters (out-of-pciket expenditures)
sum health_expenditures if round==1 & treatment_locality==1 
	local m1 = `r(mean)' /*This saves the mean which will be used as m1 in power calculations below*/
	local sd = `r(sd)' /*This saves the standard deviation which will be used as sd1 and sd2 in power calclulations below*/

local mde_1 = `m1'-1

* Example 15 - Power Calculations without Clusters (out-of-pocket expenditures)

iclassr health_expenditures locality_identifier if round==1 & treatment_locality==1, noisily /*This gives you the intra-cluster correlation, or rho*/
	local rho = $S_1 /*This saves the intra-cluster correlation, or rho, which will be used in clustered power calculations below*/
	display `rho'

sampsi `m1' `mde_1', p(0.8) r(1) sd1(`sd') sd2(`sd')
sampclus, numclus(100) rho(`rho') /*This corrects for clusters, Note: You may need to install the sampclus command*/

* Example 16.Power Calculations with Clusters (trade-off between number of clusters and number of observations per cluster)

use "evaluation.dta", clear

*Note: Focus on the randomized assignment case in chapter 4
drop if eligible==0

* Example 14 - Power Calculations without Clusters (out-of-pciket expenditures)
sum health_expenditures if round==1 & treatment_locality==1 
	local m1 = `r(mean)' /*This saves the mean which will be used as m1 in power calculations below*/
	local sd = `r(sd)' /*This saves the standard deviation which will be used as sd1 and sd2 in power calclulations below*/

local mde_1 = `m1'-1
local mde_2 = `m1'-2

iclassr health_expenditures locality_identifier if round==1 & treatment_locality==1, noisily /*This gives you the intra-cluster correlation, or rho*/
	local rho = $S_1 /*This saves the intra-cluster correlation, or rho, which will be used in clustered power calculations below*/
	display `rho'

/*30 Clusters*/sampsi  `m1' `mde_2', p(0.8) r(1) sd1(`sd') sd2(`sd')
	sampclus, numclus(30) rho(`rho')

*============================*

*End
log close


