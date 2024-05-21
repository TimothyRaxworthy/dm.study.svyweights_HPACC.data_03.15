
library(lme4)
library(tidyverse)
library(survey)
library(ggplot2)

# Load in Data

library(haven)
analysis_data <- read_dta("/Users/timra/Desktop/a/merge_2015_data.dta")

#remove 14 cases with missing weights
dta <- analysis_data %>% 
  filter(age >= 30 & age <= 69) 


# Recode variables
## Create age group variable
#create age group variable
dta$age <- trunc(dta$age)

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
dta$dm_prevalence <- dta$dm_prevalence-1

na <- dta %>%
  filter(is.na(age_group))

na


dta %>%
  group_by(age_group,country) %>%
  summarise(sum = n())


dta %>%
  group_by(educat,country) %>%
  summarise(sum = n())

write_dta(dta, "/dta_analysis.dta")










## Load in data and make graphs
library(haven)
dta <- read_dta("/Users/timra/Desktop/a/dta_analysis.dta")

#replace age categories with direct label
dta$age_chr <- as.character(dta$age_group)
dta$age_chr <-replace(dta$age_chr, dta$age_chr == "1", "30-34")
dta$age_chr <-replace(dta$age_chr, dta$age_chr == "2", "35-39")
dta$age_chr <-replace(dta$age_chr, dta$age_chr == "3", "40-44")
dta$age_chr <-replace(dta$age_chr, dta$age_chr == "4", "45-49")
dta$age_chr <-replace(dta$age_chr, dta$age_chr == "5", "50-54")
dta$age_chr <-replace(dta$age_chr, dta$age_chr == "6", "55-59")
dta$age_chr <-replace(dta$age_chr, dta$age_chr == "7", "60-64")
dta$age_chr <-replace(dta$age_chr, dta$age_chr == "8", "65-69")
table(dta$age_chr)

#group by country and age category
summary_data2 <- dta %>%
  group_by(age_chr, country) %>%
  dplyr::summarise(proportion = mean(dm_prevalence)) %>%
  ungroup()

p <- ggplot(summary_data2, aes(x = age_chr, y = proportion, group = country, 
                               color = country))
p + geom_line() + 
  theme_bw() + 
  labs(x = "Age Group", y = "Proportion",
      title ="Variation between countries in change of diabetes prevalence across different age categories")

ggsave(filename = "variation.pdf", device = "pdf", 
       path = "", width = 8, height = 5.71, 
       units = "in", dpi = "print")
#reasoning for using variable slopes as well

#group by country and bmicat
summary_data2 <- dta %>%
  group_by(bmicat, country) %>%
  dplyr::summarise(proportion = mean(dm_prevalence)) %>%
  ungroup()

p <- ggplot(summary_data2, aes(x = bmicat, y = proportion, 
                               group = country, color = country))
p + geom_point() + geom_line() + theme_bw()

#group by country and educat
summary_data2 <- dta %>%
  group_by(educat, country) %>%
  dplyr::summarise(proportion = mean(dm_prevalence)) %>%
  ungroup()

p <- ggplot(summary_data2, aes(x = educat, y = proportion, 
                               group = country, color = country))
p + geom_line() + theme_bw()

#group by sex and country
summary_data2 <- dta %>%
  group_by(sex_chr, country) %>%
  dplyr::summarise(proportion = mean(dm_prevalence)) %>%
  ungroup()

p <- ggplot(summary_data2, aes(x = sex_chr, y = proportion, 
                               group = country, color = country))
p + geom_point() + geom_line() + theme_bw()



#gdp category
dta$gdp2 <- as.numeric(dta$gdp)
dta$gdp2 <-  replace(dta$gdp2, dta$gdp2 == 1, 0) 
dta$gdp2 <-  replace(dta$gdp2, dta$gdp2 == 2, 0)
dta$gdp2 <-  replace(dta$gdp2, dta$gdp2 == 3, 1)
dta$gdp2 <-  replace(dta$gdp2, dta$gdp2 == 4, 1)

#group by gdp category and country
summary_data <- dta %>%
  group_by(gdp, country) %>%
  summarise(proportion = mean(dm_prevalence)) %>%
  ungroup()

p <- ggplot(summary_data, aes(x = gdp, y = proportion, 
                              group = country, color = country))
p + geom_point() + theme_bw()








# Model Comparison
## Generalized logit model

### Fit model using svyglm()

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

#Survey object WITH WEIGHTS
object = svydesign(data = dta, strata = ~stratum_num, 
                   ids = ~psu_num, weights = ~wpopnr_sample_dm_prevalence, 
                   nest = T)
#Survey object WITHOUT WEIGHTS
object2 = svydesign(data = dta, strata = ~stratum_num, ids = 
                      ~psu_num, nest = T)
### weighted & unweighted glm


weighted_glm <- svyglm(dm_prevalence ~ age_group + sex_chr + educat + bmicat + 
                         country, design = object, 
                       family = binomial(link = "logit"))

unweighted_glm <- svyglm(dm_prevalence ~ age_group + sex_chr + educat + bmicat + 
                           country, design = object2, 
                         family = binomial(link = "logit"))
summary(weighted_glm)
summary(unweighted_glm)



# extract predicted values
phat = predict(weighted_glm, type = "response")
dta$phat <- phat

#weighted analysis
svyby_glm = svyby(~phat, ~country, object, svymean, na.rm = T)
svyby_glm
#unweighted analysis
svyby_glm_unweight = svyby(~phat, ~country, object2, svymean, na.rm = T)
svyby_glm_unweight

# extract predicted values for unweighted model
phat2 = predict(unweighted_glm, type = "response")
dta$phat2 <- phat2



#weighted analysis
unsvyby_glm = svyby(~phat2, ~country, object, svymean, na.rm = T)
unsvyby_glm
#unweighted analysis
svyby_glm_unweight = svyby(~phat2, ~country, object2, svymean, na.rm = T)
svyby_glm_unweight

#Extract coefficients and standard errors
un_glm_coef <- coef(unweighted_glm)
un_glm_se <- SE(unweighted_glm)
un_w_glm_coef <- coef(unweighted_glm)
un_w_glm_se <- SE(unweighted_glm)


coefficient_table <- cbind(un_glm_coef,un_w_glm_coef,un_glm_se,un_w_glm_se)
coefficient_table


#Weighted unweighted comparison of outcome
svyby(~dm_prevalence, ~country, object, svymean, na.rm = T)
svyby(~dm_prevalence, ~country, object2, svymean, na.rm=T)





