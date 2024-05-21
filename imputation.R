## impute
### Create data object for imputation

library(rstanarm)
library(haven)
library(tidyverse)
library(survey)
library(DataExplorer)
library(data.table)
library(mice)


data_dm_prevalence <- read_dta("analysis_data.dta")
profile_missing(data_dm_prevalence) #check missing

##  Check any differences between distributions of covariates 
##  for those who are missing outcome and those who are not

not_missing <- data_dm_prevalence %>% #36,638
  filter(!is.na(dm_prevalence)) 

missing <- data_dm_prevalence %>% #5,777
  filter(is.na(dm_prevalence))

hist(not_missing$age) # overall not a large difference
hist(missing$age)

hist(not_missing$bmi) #missing codes
hist(missing$bmi) #we see missing codes


not_missing <- not_missing %>%
  filter(bmi < 100) 

missing <- missing %>%
  filter(bmi < 100)

hist(not_missing$bmi)
hist(missing$bmi)

#not a large difference

#total across categories for those not missing outcome
educat_not_missing  <- not_missing %>%
  group_by(educat3) %>%
  filter(!is.na(educat3)) %>%
  summarise(count = n())

#total across categories for those missing outcome
educat_missing <- missing %>%
  group_by(educat3) %>%
  filter(!is.na(educat3)) %>%
  summarise(count = n())

#percentage missing
(educat_not_missing$count)/36464 
educat_missing$count/4922


#create new id
data_dm_prevalence <- data_dm_prevalence %>% # N = 42,415
  mutate(new_id=1:nrow(.))

summary(data_dm_prevalence$dm_prevalence, na.rm = T)


data_new = analysis_data %>% 
  filter(!is.na(age)) %>% #drop missing for both age and sex
  filter(!is.na(sex)) %>%
  # convert variables to factor for regression model
  mutate_at(vars(sex, bmi_cat, countryGDPclass, educat3, dm_prevalence), ~as.factor(.)) %>%
  mutate(country_factor=as.factor(country),
         psu_factor=as.factor(psu)) %>%
  select(c(country_factor, psu_factor, sex, age, bmi_cat, educat3, countryGDPclass, dm_prevalence, new_id)) # n=41,694




# impute missing values for variables bmicat, educat, and dm_prevalence
init = mice(data_new, maxit=0)
meth = init$method
meth
predM = init$predictorMatrix
predM
# set id = 0 because we do not want to use ID as covariate
predM[,"psu_factor"]=0
predM[,"new_id"]=0
predM
meth[c("bmi_cat")]="polr" #Proportional odds model
meth[c("educat3")]="polr"
meth

DataExplorer::plot_missing(data_new) # less than 5% missing on average

#compute average missingness in data
(2.09 + 6.89 + 13.08)/7
#stick with having 5 imputations based on Rubin, Donald B. (1987) Multiple Imputation for Nonresponse in Surveys. New York: Wiley.


# impute data  
## Checks
my_imp_test = mice(data_new, m=5, method= meth, pred = predM, maxit = 20, seed = 1015) # maxit = 20
plot(my_imp_test)
data_no_weights = mice::complete(my_imp_test) 
save(data_no_weights, file="data_no_weights.rds")
load("data_no_weights.rds")

summary(data_no_weights)
md.pattern(data_no_weights)

# look at the impute values and conclude the impute data is similar to the original data
my_imp_test$imp$bmi
my_imp_test$imp$educat


## Merge back in weights, now imputing the weights


#merge data
weights = data_dm_prevalence %>%
  select(wpopnr_sample_dm_prevalence, new_id)

dta_with_weights <- left_join(data_no_weights, weights, by = "new_id", keep=F)



## Impute the weights
init = mice(dta_with_weights, maxit=0)
meth = init$method
meth #the method is Predictive Mean Matching
predM = init$predictorMatrix
predM
# set id = 0 because we do not want to use ID as covariates
predM[,"psu_factor"]=0
predM[,"new_id"]=0
predM
meth

my_imp_weights= mice(dta_with_weights, 
                     m=10, #Ten imputations
                     method= meth, 
                     pred = predM, 
                     maxit = 50, 
                     seed = 1015) 
my_imp_weights$imp$wpopnr_sample_dm_prevalence

data_with_weights = mice::complete(my_imp_weights) 
save(data_with_weights, file="data_w_weights.rds")
md.pattern(data_with_weights)

#merge data
data_merge =  data_dm_prevalence %>%
  select(country, svy, country_id, survey_year, stratum, stratum_num, psu, 
         psu_num, new_id, p_id, population_2020_30_69, population_2020_40_69)


impute_dta.complete <- left_join(data_with_weights, data_merge, by = "new_id", keep=F)
md.pattern(impute_dta.complete)
summary(impute_dta.complete) #N = 39713
save(data_merge, file="data_merge.rds")


save(impute_dta.complete, file="impute_dta.complete.rds")
write_dta(impute_dta.complete, "/Users/timra/Desktop/a/merge_2015_data.dta")
