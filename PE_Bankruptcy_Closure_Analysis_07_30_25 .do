
/*This analysis file is was used to generate the results for 

Bankruptcy and Closure After Private Equity Acquisition of Health Care Delivery Organizations: A Retrospective Cohort Study

Cai, Christopher L; Rome, Benjamin N  ; Gondi, Suhas;  Kannan, Sneha; Song, Zirui 

This file was updated on July 30, 2025 in response to revisions. We originally calculated standardized mean differences in excel and now add code that calculates them using the STDDIFF package. Results were not changed. 
*/ 

clear
*This loads the study file extracted from _Pitchbook. Each row is a single company, with columns for  covariates and outcomes, including date of acquisition, year of outcome (if relevant) and follow-up. 
use pitchbookfinalcohort

*==================
*TABLE 1
*==================
generate profitable=0
replace profitable=1 if status=="Profitable"

*Create unweighted table 1: covariates at baseline. Treatment is a variable for the assigned PE vs Corporate group based on inclusion criteria documented in the paper above. 
tab companyindustrycode treatment, col chi2 
tab year_cat treatment, col chi2 
tab ownershipstatusbeforeorat1 treatment, col chi2 
tab censusregion1 treatment, col chi2 
tab profitable treatment, col chi2 

*calculate SMDs
tab companyindustrycode, gen(industry_)
tab year_cat, gen(yearcat_)
tab ownershipstatusbeforeorat1, gen(owner_)
tab censusregion1, gen(region_)

local dummyVar "industry_1 industry_2 industry_3 yearcat_1 yearcat_2 yearcat_3 yearcat_4 yearcat_5 owner_1 owner_2 owner_3 region_1 region_2 region_3 region_4 profitable" 

foreach var of local dummyVar {
stddiff `var', by(treatment)	
} 

*by default, the SMDs are calculated as the value of treatment=0 minus the value of treatment=1. The signs were reversed manually for our Table 1.

*descriptive statistics for results section 
summarize followup if treatment==1 , detail
summarize followup if treatment==0 , detail

*composite is a binary of whether the sample experienced the primary compsoite outcome or censoring. Followup is average followup. Censoryear is a variable for the year an entity was censored.  
summarize followup if composite==1, detail
summarize followup if composite==1 & treatment==1, detail
summarize followup if composite==1 & treatment==0, detail

summarize censoryear if treatment==1, detail
summarize censoryear if treatment==0, detail

*summary statistics for sample size of subjects experiencing the outcome, overall and stratified by treatment. Presented in results section 

tab composite, missing
tab bankruptcybinary, missing
tab outofbusinessbinary, missing

tab composite treatment, missing col 
tab bankruptcybinary treatment, missing col 
tab outofbusinessbinary treatment, missing col 
tab bankruptcybinary treatment, missing col 
tab outofbusinessbinary treatment, missing col 

*now generate descriptive statistics only for those subjects which experienced the outcome. This is Table S2

preserve 
keep if composite==1

keep companyname state statusnote company treatment yearofacquisition yearofoutofbusinessclosure bankruptcyyear censoryear companyindustrycode1

*this .dta file was copied into excel and then cleaned manually

restore 

*==================
*PROPENSITY SCORE 
*==================

* create a logistic regression to generate your p score 
logit treatment i.companyindustrycode1 i.year_cat i.ownershipstatusbeforeorat1 i.profitable i.censusregion1, or

*generate summary metrics of score and then graph 
predict pscore 
summarize pscore if treatment==1 , detail
summarize pscore if treatment==0 , detail

twoway (kdensity pscore if treatment==0) (kdensity pscore if treatment==1, lpattern(dash)), ///
    legend(label(1 "Corporate") label(2 "PE")) ///
    xtitle("Propensity Score") ///
    ytitle("Density") ///
	title("Propensity Score by Treatment Group")

*trim outliers, generate inverse probability weights

replace pscore=. if pscore <= 0.1 
replace pscore=. if pscore >= 0.9

gen IPW = 1.treatment/pscore + 0.treatment/(1-pscore)

*now weight table 1, inspect to confirm if the propensity score achieved balance 
*calculate SMDs in excel 
svyset [pweight=IPW]
* IPW 
svy: tab companyindustrycode treatment, col se
svy: tab year_cat treatment , col se 
svy: tab ownershipstatusbeforeorat1 treatment, col se 
svy: tab profitable treatment, col se 
svy: tab censusregion1 treatment, col se 


*==================
*SURVIVAL ANALYSIS
*==================
*Sample sizes. count =1 for each observation

total count,  over(treatment) 

total count if pscore!=.,  over(treatment) 

total count if companyindustrycode1==1,  over(treatment) 

total count if companyindustrycode1==2,  over(treatment) 

total count if companyindustrycode1==3,  over(treatment) 


*followup for composite outcome
total followup2 ,  over(treatment) 

total followup2 if companyindustrycode1==1,  over(treatment) 

total followup2 if companyindustrycode1==2,  over(treatment) 

total followup2 if companyindustrycode1==3,  over(treatment) 


*followup for bankrutpcy
total followup3 ,  over(treatment) 

total followup3 if companyindustrycode1==1,  over(treatment) 

total followup3 if companyindustrycode1==2,  over(treatment) 

total followup3 if companyindustrycode1==3,  over(treatment) 


*followup for closure
total followup4 ,  over(treatment) 

total followup4 if companyindustrycode1==1,  over(treatment) 

total followup4 if companyindustrycode1==2,  over(treatment) 

total followup4 if companyindustrycode1==3,  over(treatment) 


*PRIMARY OUTCOME, UNWEIGHTED AND UNWEIGHTED 
stset followup2 , failure(composite) 

ereturn list 
sts test treatment, cox
stcox i.treatment
est store model1
estat phtest
total _t , over (treatment)


*y axis label 
mylabels 90(1)100, myscale(@/100) local(myla) 

sts graph,  survival by(treatment) risktable(0(1)15, size(tiny) order(1 "Control" 2 "PE")) title("Bankruptcy/Closure Kaplan Meier Curve") t1title("PE-acquired vs Corporate-Acquired") xtitle("Year of Follow-Up") ytitle("Percentage Survival") tmax(15) ylabel(`myla') legend(label(1 "Corporate") label(2 "PE")) 
graph save Fig1, replace

*weighted 
stset followup2   [pweight=IPW], failure(composite) 
sts test treatment, cox
stcox i.treatment 
est store model1a
estat phtest

sts graph,  survival by(treatment) risktable(0(1)15, size(tiny) order(1 "Control" 2 "PE")) title("Bankruptcy/Closure Kaplan Meier Curve") t1title("PE-acquired vs Corporate-Acquired") xtitle("Year of Follow-Up") ytitle("Percentage Survival") tmax(15) ylabel(`myla') legend(label(1 "Corporate") label(2 "PE")) 
graph save Fig1WEIGHTED, replace

*Repeat this for each subgroup. Copy the coefficients for a forest plot. 
