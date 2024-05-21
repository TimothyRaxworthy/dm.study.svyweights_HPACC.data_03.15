************** Mixed effect model **************
************************************************
/*
Generalized Linear
Latent and Mixed Models (GLLAMMs).

From the abstract of the gllamm stata program written by Sophia Rabe-Hesketh, Anders Skrondal, and Andrew Pickles:

GLLAMMs are a class of multilevel latent variable models for (multivariate) responses of mixed type including continuous
responses, counts, duration/survival data, dichotomous, ordered and unordered categorical responses and rankings. The latent variables (common factors or random effects) can be assumed to be discrete or to have a multivariate normal
distribution. Examples of models in this class are multilevel generalized linear models or generalized linear mixed models, multilevel factor or latent trait models, item response models, latent class models and multilevel structural equation
models. The program can be downloaded from http://www.gllamm.org.


*/

***START HERE***
****************
*Settings
clear all
cls
version 16
set more off
capture log close
set showbaselevels

*set directory
cd "C:\Users\timra\Desktop\a"

*Use the macro $S_DATE to save files with current date
global date : di %tdCCYY.NN.DD date("$S_DATE","DMY")

* Start log
log using "log_hpacc03.15_${date}.log", replace

*Load in data
use "dta_analysis.dta", clear



*****Weight Scaling*****
************************

*Create psuedo-weights for level 2
gen pwt2 = 1

*Survey weights for each individual
gen pwt1 = wpopnr_sample_dm_prevalence

*recode sex variable
gen sex2 = sex - 1

*Scale weights
*scaling method 1: Effective sample size
gen sqw = pwt1^2 
egen sumsqw = sum(sqw), by(country_factor) 
egen sumw = sum(pwt1), by (country_factor) 
gen pwtlsl = pwt1 * sumw/sumsqw


*scaling method 2: Actual sample size
gen counter = 1
egen nj = count(counter) , by (country_factor) 
gen pwtls2 = pwt1 * nj/sumw 
gen pweight1 = pwtls2







*Mixed effect model with no weights*
************************************

xi:gllamm dm_prevalence i.age_group i.educat i.bmicat sex2, i(country_factor) l(logit) f(binom) nip(7) trace

*corrected variance estimate due to clustering
gllamm, cluster (psu_factor)

*gather start values
matrix a = e(b)

*extract predicted values (mu)
gllapred mu, mu
gllapred u, u ///random effects

*conduct survey analaysis of model predicted values
svyset psu[pw=wpopnr_sample_dm_prevalence], strata(stratum)
svy, over(country_factor): mean mu

*unweighted
svyset psu, strata(stratum)
svy, over(country_factor): mean mu





***Mixed effect model with weights***
*************************************

*Run Model 
xi:gllamm dm_prevalence i.age_group i.educat i.bmicat sex2, i(country_factor) pweight(pweight) l(logit) f(binom) from(a) nip(7) adapt iterate(10) trace 

gllamm, cluster (psu_factor)
gllapred mu2, mu
gllapred u2, u

svyset psu[pw=wpopnr_sample_dm_prevalence], strata(stratum)
svy, over(country_factor): mean mu

svyset psu, strata(stratum)
svy, over(country_factor): mean mu








