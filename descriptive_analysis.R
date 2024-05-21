library(tidyverse)
library(kableExtra)

# Load in Data

library(haven)
analysis_data <- read_dta("/Users/timra/Desktop/a/merge_2015_data.dta")

#remove cases beyond age range for analysis
dta <- analysis_data %>% 
  filter(age >= 30 & age <= 69) 

# Recode variables
## Create age group variable

#create age group variable
dta$age <- trunc(dta$age) #some have decimal place in age

#create age group
dta <- dta %>%
  mutate(age_group=case_when(between(age, 30,34) ~1,
                             between(age, 35,39) ~2,
                             between(age, 40,44) ~3,
                             between(age, 45,49) ~4,
                             between(age, 50,54) ~5,
                             between(age, 55,59) ~6,
                             between(age, 60,64) ~7,
                             between(age, 65,69) ~8),
         age_group=factor(age_group, levels=1:8, labels=c("30-34",
                                                          "35-39",
                                                          "40-44",
                                                          "45-49",
                                                          "50-54",
                                                          "55-59",
                                                          "60-64",
                                                          "65-69") ))

#create factor variables
dta$bmicat <- as.factor(dta$bmi_cat)
dta$gdp <- as.factor(dta$countryGDPclass) 
dta$educat <- as.factor(dta$educat3)
dta$sex_chr <- as.character(dta$sex)
dta$sex_chr[dta$sex_chr=="1"]<-"M"
dta$sex_chr[dta$sex_chr=="2"]<-"F"
dta$dm_prevalence <- dta$dm_prevalence

#check missing
na <- dta %>%
  filter(is.na(age_group))

na #no missing

#check total amount across groups
dta %>%
  group_by(age_group,country) %>%
  summarise(sum = n())


dta %>%
  group_by(educat,country) %>%
  summarise(sum = n())


write_dta(dta, "/dta_analysis.dta")


# Check sample pop size and weighted pop size
dta %>%
  group_by(country) %>%
  summarize(sum = n()) #people across all age groups

dta %>%
  group_by(country) %>%
  summarise(sum = sum(wpopnr_sample_dm_prevalence)) #total population using weights



# Check how informative weights are

## Age
dta <- read_dta("/Users/timra/Desktop/a/dta_analysis.dta")
dta$age_group <- as.factor(dta$age_group)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
object = svydesign(data = dta, 
                   strata = ~stratum_num, 
                   ids = ~psu_num, 
                   weights = ~wpopnr_sample_dm_prevalence, 
                   nest = T) 

#weighted
svy_age = svyby(~age_group, 
                ~country, 
                object, 
                svymean, 
                na.rm = T, 
                covmat = TRUE 
)
svy_age
w.age.prop <- svy_age[,2:9]
w.age.prop



#unweighted
object2 = svydesign(data = dta, 
                    strata = ~stratum_num, 
                    ids = ~psu_num, 
                    nest = T) 

svy_age_unweight = svyby(~age_group, ~country, object2, svymean, deff=TRUE, 
                         na.rm = T, covmat = TRUE, 
                         vartype = c("ci","se"), df = degf(design))


age.prop <- svy_age_unweight[,2:9]
age.prop



table_age <- (w.age.prop - age.prop)*100
kable(table,
caption = "Percent difference between weighted and unweighted proportions") %>%
kable_styling(latex_options = "hold_position")
table_age


## bmi
#1. < 25#
#2. >= 25 & < 30
#3. >= 30


dta$bmi_cat <- as.factor(dta$bmi_cat)
table(dta$bmi_cat)
svy_bmi = svyby(~bmicat, ~country, object, svymean, 
                deff=TRUE, na.rm = T, df = degf(design), covmat = TRUE)

w.bmi.prop <- svy_bmi[,2:4]
is.num <- sapply(w.bmi.prop, is.numeric)
w.bmi.prop[is.num] <- lapply(w.bmi.prop[is.num], round, 3) #round
w.bmi.prop

#unweighted
svy_bmi_unweight = svyby(~bmi_cat, ~country, object2, 
                         svymean, na.rm = T, covmat = TRUE)
svy_bmi_unweight

bmi.prop <- data.frame(svy_bmi_unweight[,2:4])
is.num <- sapply(bmi.prop, is.numeric)
bmi.prop[is.num] <- lapply(bmi.prop[is.num], round, 3)
bmi.prop



table_bmi <- (w.bmi.prop - bmi.prop)*100
kable(table2,
caption = "Percent difference between weighted and unweighted proportions") %>%
kable_styling(latex_options = "hold_position")

#bmi_table
table_bmi


## Education Level

# 1. No formal education
# 2. Completed Primary education
# 3. High school and above


svy_educat = svyby(~educat, ~country, object, svymean, na.rm = T, covmat = T)
svy_educat

w.educat.prop <- svy_educat[,2:4]
w.educat.prop

w.educat.pro <- data.frame(w.educat.prop)
is.num <- sapply(w.educat.pro, is.numeric)
w.educat.pro[is.num] <- lapply(w.educat.pro[is.num], round, 3)
w.educat.pro

svy_educat_unweight = svyby(~educat, ~country, object2, svymean, na.rm = T, covmat = T)

educat.prop <- svy_educat_unweight[,2:4]
educat.prop

educat.pro <- data.frame(educat.prop)
is.num <- sapply(educat.pro, is.numeric)
educat.pro[is.num] <- lapply(educat.pro[is.num], round, 3)
educat.pro



table_educat <- 100*(w.educat.prop - educat.prop)
kable(table3,
caption = "Percent difference between weighted and unweighted proportions") %>%
kable_styling(latex_options = "hold_position")
table_educat


## sex
svy_sex = svyby(~sex_chr, ~country, object, svymean, na.rm = T, covmat = T)

w.sex.prop <- svy_sex[,2:3]

w.sex.pro <- data.frame(w.sex.prop)
is.num <- sapply(w.sex.pro, is.numeric)
w.sex.pro[is.num] <- lapply(w.sex.pro[is.num], round, 3)
w.sex.pro


svy_sex_unweight = svyby(~sex_chr, ~country, object2, svymean, na.rm = T, covmat = T)

sex.prop <- svy_sex_unweight[,2:3]
sex.pro <- data.frame(sex.prop)
is.num <- sapply(sex.pro, is.numeric)
sex.pro[is.num] <- lapply(sex.pro[is.num], round, 3)
sex.pro


table_sex <- (w.sex.prop - sex.prop)*100
kable(table4,
caption = "Percent difference between weighted and unweighted proportions") %>%
kable_styling(latex_options = "hold_position")

table_sex


## outcome variable
svy_dm= svyby(~dm_prevalence_binary, ~country, object, svymean, na.rm = T, covmat = TRUE)
svy_dm
w.dm.prop <- svy_dm[,2]


svy_dm_unweight = svyby(~dm_prevalence_binary, ~country, object2, svymean, 
                        na.rm = T, covmat = TRUE)

dm.prop <- svy_dm_unweight[,2]
coef(svy_age_unweight)


table_dm <- (w.dm.prop - dm.prop)*100
kable(table_dm,
caption = "Percent difference between weighted and unweighted proportions") %>%
kable_styling(latex_options = "hold_position")


#See all percent differences
full_diff <- cbind(table_sex, table_age, table_bmi, table_educat, table_dm)
full_diff

kable(full_diff,
      caption = "Percent difference between weighted and unweighted proportions 
      for categorical predictors and binary outcome of diabetes prevalence") %>%
  kable_styling(latex_options = "hold_position")





