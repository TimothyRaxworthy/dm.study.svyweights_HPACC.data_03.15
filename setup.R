#packages 
library(rstanarm)
library(haven)
library(tidyverse)
library(survey)
library(DataExplorer)
library(data.table)

## Load in Data

HPACC_2022 <- read_dta("HPACC_Maindata_appended_0315.dta")

##Subset for a specific year
analysis_data <- subset(HPACC_2022, survey_year == "2015")

#check amount of missing
profile_missing(analysis_data)

#make missing PSU object
missing_psu <- analysis_data %>% 
  select(psu_num, psu, country) %>%
  filter(is.na(psu_num))

num_miss <- nrow(missing_psu)
num_miss #number of missing

#set seed
set.seed(1034)
#there are 86 unique PSU to this country
missing_psu$assign.psu <- sample(seq(1,86), num_miss)  #assign an id to each row 
#                                                        randomly

#All of them are missing codes, they are missing PSU


#make object to attach each PSU randomly selected
solomon <- analysis_data %>%
  filter(country == "Solomon Islands") %>%
  select(psu_num) %>%
  na.omit() %>%
  reframe(psu = unique(psu_num))

solomon$id <- seq(1,86)

##########################################
# Make an atomic vector of key-values
key_values <- c("1" = "827012871",
                "2" = "827012927",
                "3" = "827012873",
                "4" = "827012943",
                "5" = "827012937",
                "6" = "827012940",
                "7" = "827012919",
                "8" = "827012881",
                "9" = "827012908",
                "10" = "827012882",
                "11" = "827012876",
                "12" = "827012875",
                "13" = "827012929",
                "14" = "827012893",
                "15" = "827012877",
                "16" = "827012889",
                "17" = "827012891",
                "18" = "827012902",
                "19" = "827012924",
                "20" = "827012946",
                "21" = "827012931",
                "22" = "827012939",
                "23" = "827012887",
                "24" = "827012892",
                "25" = "827012915",
                "26" = "827012926",
                "27" = "827012912",
                "28" = "827012896",
                "29" = "827012906",
                "30" = "827012886",
                "31" = "827012883",
                "32" = "827012898",
                "33" = "827012928",
                "34" = "827012879",
                "35" = "827012894",
                "36" = "827012942",
                "37" = "827012909",
                "38" = "827012945",
                "39" = "827012921",
                "40" = "827012936",
                "41" = "827012865",
                "42" = "827012911",
                "43" = "827012895",
                "44" = "827012867",
                "45" = "827012874",
                "46" = "827012872",
                "47" = "827012913",
                "48" = "827012861",
                "49" = "827012885",
                "50" = "827012869",
                "51" = "827012922",
                "52" = "827012880",
                "53" = "827012900",
                "54" = "827012938",
                "55" = "827012899",
                "56" = "827012910",
                "57" = "827012917",
                "58" = "827012868",
                "59" = "827012916",
                "60" = "827012901",
                "61" = "827012925",
                "62" = "827012904",
                "63" = "827012907",
                "64" = "827012930",
                "65" = "827012923",
                "66" = "827012934",
                "67" = "827012878",
                "68" = "827012920",
                "69" = "827012884",
                "70" = "827012933",
                "71" = "827012917",
                "72" = "827012868",
                "73" = "827012916",
                "74" = "827012901",
                "75" = "827012925",
                "76" = "827012904",
                "77" = "827012907",
                "78" = "827012930",
                "79" = "827012923",
                "80" = "827012934",
                "81" = "827012890",
                "82" = "827012878",
                "83" = "827012920",
                "84" = "827012903",
                "85" = "827012884",
                "86" = "827012933")






#transform vector 
b <- key_values[missing_psu$assign.psu]
missing_psu$psu <- replace_na(b)
missing_psu$psu_num <- as.numeric(missing_psu$psu)

#append in codes
setorder(analysis_data, psu)
head(analysis_data)
analysis_data$psu[1:70] <- missing_psu$psu
analysis_data$psu_num[1:70] <- missing_psu$psu_num
profile_missing(analysis_data)

#No more missing PSUs


## Save Variables of interest for dm prevalence

## select variables

data_dm_prevalence <- analysis_data %>%
  select(p_id, survey_year, svy, country, country_id, stratum, stratum_num, psu
         , psu_num, wpopnr_sample_dm_prevalence, dm_prevalence, age, 
         population_2020_30_69, population_2020_40_69, sex, bmi, bmi_cat, educat3
         , countryGDPclass)



write_dta(data_dm_prevalence, "analysis_data.dta")



