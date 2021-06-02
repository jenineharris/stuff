'# PROLOG   ################################################################'

'# PROJECT: Transgender Identity and Cardiovascular Disease #'
'# PURPOSE: Examining relationship between CVD and transgender identity using BRFSS data #'
'# AUTHOR:  Isabelle Howerton, Jenine Harris #'
'# CONTACT: harrisj@wustl.edu #'
'# LATEST:  June 1, 2021 #'

'# PROLOG   ###############################################################'


# load packages needed
# if not already installed use Tools -> Install Packages menu
# to install each package first
library(car)
library(tidyverse)
library(ROCR)
library(survey)
library(spatstat)
library(tidyr, warn.conflicts = FALSE)
library(sjstats)

# no scientific notation
options(scipen = 999)

# # TO CREATE ANALYTIC DATA SET, UNCOMMENT AND RUN LINES 26-91
# # bring in files from online BRFSS 2015 to 2018 data
# library(haven)
# # #
# # #
# # # # 2015 data
#   temp <- tempfile(fileext = ".zip")
#   download.file(url  = "https://www.cdc.gov/brfss/annual_data/2015/files/LLCP2015XPT.zip", destfile = temp)
#   BRFSS_2015 <- read_xpt(file = temp)
# # #
# # # 2016 data
#  temp <- tempfile(fileext = ".zip")
#  download.file(url  = "https://www.cdc.gov/brfss/annual_data/2016/files/LLCP2016XPT.zip", destfile = temp)
#  BRFSS_2016 <- read_xpt(file = temp)
# #
# # # 2017 data
#  temp <- tempfile(fileext = ".zip")
#  download.file(url  = "https://www.cdc.gov/brfss/annual_data/2017/files/LLCP2017XPT.zip", destfile = temp)
#  BRFSS_2017 <- read_xpt(file = temp)
# #
# # 2018 data
# temp <- tempfile(fileext = ".zip")
# download.file(url  = "https://www.cdc.gov/brfss/annual_data/2018/files/LLCP2018XPT.zip", destfile = temp)
# BRFSS_2018 <- read_xpt(file = temp)
# 
# # 2019 data
# temp <- tempfile(fileext = ".zip")
# download.file(url  = "https://www.cdc.gov/brfss/annual_data/2019/files/LLCP2019XPT.zip", destfile = temp)
# BRFSS_2019 <- read_xpt(file = temp)
# #
# # # select only the variables to use in the analysis (e.g. changes 300+ variables to only needed for analysis)
# #
#  BRFSS_2015 <- BRFSS_2015 %>%
#    select(`_MICHD`, SEX, EDUCA, `_BMI5CAT`, `_DRNKWEK`, EXERANY2, `_RACE`,
#           `_AGE80`, `_SMOKER3`, TRNSGNDR, `_LLCPWT`)
# #
#  BRFSS_2016 <- BRFSS_2016 %>%
#    select(`_MICHD`, SEX, EDUCA, `_BMI5CAT`, `_DRNKWEK`, EXERANY2, `_RACE`,
#           `_AGE80`, `_SMOKER3`, TRNSGNDR, `_LLCPWT`)
# #
#  BRFSS_2017 <- BRFSS_2017 %>%
#    select(`_MICHD`, SEX, EDUCA, `_BMI5CAT`, `_DRNKWEK`, EXERANY2, `_RACE`,
#           `_AGE80`, `_SMOKER3`, TRNSGNDR, `_LLCPWT`)
# #
#  BRFSS_2018 <- BRFSS_2018 %>%
#   select(`_MICHD`, SEX1, EDUCA, `_BMI5CAT`, `_DRNKWEK`, EXERANY2, `_RACE`,
#           `_AGE80`, `_SMOKER3`, TRNSGNDR, `_LLCPWT`)
# #
#  BRFSS_2019 <- BRFSS_2019 %>%
#    select(`_MICHD`, `_SEX`, EDUCA, `_BMI5CAT`, `_DRNKWK1`, EXERANY2, `_RACE`,
#           `_AGE80`, `_SMOKER3`, TRNSGNDR, `_LLCPWT`)
# #
# # # renamed SEX1 in 2018 to SEX like all the other years
#  BRFSS_2018 <- BRFSS_2018 %>%
#    rename(SEX = SEX1)
#  BRFSS_2019 <- BRFSS_2019 %>%
#    rename(SEX = `_SEX`)
# #
#  # renamed AVEDRNK3 to AVEDRNK2 in 2019 like all the other years
# BRFSS_2019 <- BRFSS_2019 %>%
#   rename(`_DRNKWEK` = `_DRNKWK1`)
# 
# # # combine all years into 1 dataset
# BRFSS_all <- rbind(BRFSS_2015, BRFSS_2016, BRFSS_2017, BRFSS_2018, BRFSS_2019)
# 
# # save BRFSS_all dataset as a .csv file so everytime i want to work with, I just read that file
# # in instead of running all steps above
# write.csv(BRFSS_all, file = "brfss_all_wt.csv")

BRFSS_all <- read.csv("brfss_all_wt.csv")

# Weighting variable divide by number of survey years
BRFSS_all <- BRFSS_all %>%
  mutate(brfssWt = `X_LLCPWT`/5)

# OUTCOME variable - Ever had heart attack or diagnosed with CVD
BRFSS_all <- BRFSS_all %>%
  mutate(heartattack_chd = as.numeric(dplyr::recode(`X_MICHD`, `2` = "0",
                                                              `1` = "1")
         ))

table(BRFSS_all$heartattack_chd)

# MAIN STUDY variable - transgender identification
BRFSS_all$transgender <- car::recode(as.factor(BRFSS_all$TRNSGNDR),
                                     "1 = 'Trans';
                                      2 = 'Trans';
                                      3 = 'NC';
                                      4 = 'Cisgender';
                                      7 = NA;
                                      9 = NA")

# DEMOGRAPHIC variables - sex, marital status, education, BMI, race, age
BRFSS_all$sex <- car::recode(as.factor(BRFSS_all$SEX),
                             "1 = 'Male';
                              2 = 'Female';
                              7 = NA;
                              9 = NA")

# check sex * transgender
sexByTrans <- t(questionr::tabs(df = BRFSS_all,
                                x = 'SEX',
                                y = 'TRNSGNDR',
                                weight = 'brfssWt',
                                normwt = TRUE,
                                type = 'counts',
                                digits = 0,
                                na.show = FALSE))

# change sex to sex assigned at birth for participants who indicated
# male sex or don't know / refused and female-to-male transgender identity and for 
# those who indicated female sex or don't know / refused and male-to-female
# trans identity
BRFSS_all$sex[BRFSS_all$SEX == 2 & BRFSS_all$TRNSGNDR == 1] <- "Male"
BRFSS_all$sex[BRFSS_all$SEX == 1 & BRFSS_all$TRNSGNDR == 2] <- "Female"

# check recoding
table(BRFSS_all$sex, BRFSS_all$SEX)

BRFSS_all$edu_level <- car::recode(as.factor(BRFSS_all$EDUCA),
                                   "1 = 'High school, GED, or less';
                                    2 = 'High school, GED, or less';
                                    3 = 'High school, GED, or less';
                                    4 = 'High school, GED, or less';
                                    5 = 'Some college or college degree';
                                    6 = 'Some college or college degree';
                                    9 = NA")

BRFSS_all$BMI <- car::recode(as.factor(BRFSS_all$`X_BMI5CAT`),
                             "1 = 'Normal or underweight';
                              2 = 'Normal or underweight';
                              3 = 'Overweight or obese';
                              4 = 'Overweight or obese';
                              9 = NA")

# collapsed races:
BRFSS_all$race <- car::recode(as.factor(BRFSS_all$`X_RACE`),
                              "1 = 'White';
                                2 = 'BIPOC';
                                3 = 'BIPOC';
                                4 = 'BIPOC';
                                5 = 'BIPOC';
                                6 = 'BIPOC';
                                7 = 'BIPOC';
                                8 = 'BIPOC';
                                9 = NA")

# collapsed smoke_status: former, never, current
BRFSS_all$smoke_status <- car::recode(as.factor(BRFSS_all$`X_SMOKER3`),
                                      "1 = 'Current or former';
                                       2 = 'Current or former';
                                       3 = 'Current or former';
                                       4 = 'Never smoker';
                                       7 = NA;
                                       9 = NA")

BRFSS_all$exercise <- car::recode(as.factor(BRFSS_all$EXERANY2),
                                  "1 = 'Yes';
                                   2 = 'No';
                                   7 = NA;
                                   9 = NA")

# Unclear whether definitions of drinking level apply to sex recorded at birth
# or gender identity, compare
BRFSS_all %>% 
  group_by(transgender, sex) %>%
  mutate(X_DRNKWEK = na_if(X_DRNKWEK, 99900)) %>% 
  summarize(meanDr = mean(X_DRNKWEK/100, na.rm = TRUE))

# sex recorded at birth as male is higher mean drinks in all
# gender identity categories, use sex at birth for classification
BRFSS_all <- BRFSS_all %>% 
  rename(age = `X_AGE80`) %>% 
  mutate(alcohol = if_else(X_DRNKWEK == 0, "Never drinker",
                             if_else(X_DRNKWEK/100 > 0 & X_DRNKWEK/100 <= 11/52, "Infrequent drinker",
                           if_else(X_DRNKWEK/100 > 11/52 & X_DRNKWEK/100 <= 3, "Light drinker",
                                   if_else(X_DRNKWEK/100 > 3 & X_DRNKWEK/100 <= 7 & sex == "Female", 'Moderate drinker',
                                           if_else(X_DRNKWEK/100 > 3 & X_DRNKWEK/100 <= 14 & sex == "Male", 'Moderate drinker',
                                                   if_else(X_DRNKWEK != 99900, 'Heavier drinker', NA_character_)))))))

# keep variables for models and delete cases with missing values
sample_cc <- BRFSS_all %>% 
  select(age, sex, race, edu_level, BMI,
         smoke_status, alcohol, exercise, transgender, heartattack_chd, brfssWt) %>%
  drop_na()

# check age distribution and transform when linearity fails
hist(sample_cc$age)
# # create object of the variable AGE*log(AGE)
sample_cc$ageStand <- (sample_cc$age - mean(sample_cc$age))/sd(sample_cc$age)
age.times.logAge <- sample_cc$ageStand * log(sample_cc$ageStand)

# Box Tidwell test
boxTidwell_age <- glm(heartattack_chd ~ ageStand + age.times.logAge,
                      data = sample_cc,
                      family = "binomial")
summary(boxTidwell_age)

# CHOOSE REFERENCE GROUPS

# transgender: cisgender
sample_cc$transgender <- factor(sample_cc$transgender, levels = c('Cisgender', 'Trans', 'NC'))
# sex: male
sample_cc$sex <- factor(sample_cc$sex, levels = c('Male', 'Female'))
# edu_level: no high school
sample_cc$edu_level <- factor(sample_cc$edu_level, levels = c('Some college or college degree',
                                                              'High school, GED, or less'))
# BMI: underweight
sample_cc$BMI <- factor(sample_cc$BMI, levels = c('Normal or underweight',
                                                  'Overweight or obese'))
# race: white
sample_cc$race <- factor(sample_cc$race, levels = c('White', 'BIPOC'))
# smoke_status: never
sample_cc$smoke_status <- factor(sample_cc$smoke_status, levels = c('Current or former',
                                                                    'Never smoker'))
# exercise: no
sample_cc$exercise <- factor(sample_cc$exercise, levels = c('No', 'Yes'))

# alcohol: never
sample_cc$alcohol <- factor(sample_cc$alcohol, levels = c('Heavier drinker','Light drinker', 'Never drinker', 
                                                          'Moderate drinker'))
# Build the Complex survey Object
svy_df <- svydesign(data = sample_cc, strata = , ids = ~1, weights = ~brfssWt)

# CREATE TABLE 1

# weighted table frequencies
table1wt <- t(questionr::tabs(df = sample_cc,
                  x = 'heartattack_chd',
                  y = c('sex', 'race', 'edu_level', 'BMI',
                        'smoke_status', 'alcohol', 'exercise',
                        'transgender'),
                  weight = 'brfssWt',
                  normwt = TRUE,
                  type = 'counts',
                  digits = 0))
table1wtpct <- t(questionr::tabs(df = sample_cc,
                  x = 'heartattack_chd',
                  y = c('sex', 'race', 'edu_level', 'BMI',
                        'smoke_status', 'alcohol', 'exercise',
                        'transgender'),
                  weight = 'brfssWt',
                  normwt = TRUE,
                  type = 'percent',
                  digits = 2))
# weighted chi-squared
weights::wtd.chi.sq(sample_cc$sex, sample_cc$heartattack_chd, weight=sample_cc$brfssWt, na.rm=TRUE)
weights::wtd.chi.sq(sample_cc$race, sample_cc$heartattack_chd, weight=sample_cc$brfssWt, na.rm=TRUE)
weights::wtd.chi.sq(sample_cc$edu_level, sample_cc$heartattack_chd, weight=sample_cc$brfssWt, na.rm=TRUE)
weights::wtd.chi.sq(sample_cc$BMI, sample_cc$heartattack_chd, weight=sample_cc$brfssWt, na.rm=TRUE)
weights::wtd.chi.sq(sample_cc$smoke_status, sample_cc$heartattack_chd, weight=sample_cc$brfssWt, na.rm=TRUE)
weights::wtd.chi.sq(sample_cc$alcohol, sample_cc$heartattack_chd, weight=sample_cc$brfssWt, na.rm=TRUE)
weights::wtd.chi.sq(sample_cc$exercise, sample_cc$heartattack_chd, weight=sample_cc$brfssWt, na.rm=TRUE)
weights::wtd.chi.sq(sample_cc$transgender, sample_cc$heartattack_chd, weight=sample_cc$brfssWt, na.rm=TRUE)

# weighted median & IQR
spatstat.geom::weighted.median(x = sample_cc$age[sample_cc$heartattack_chd == 0], w = sample_cc$brfssWt, na.rm = TRUE)
spatstat.geom::weighted.median(x = sample_cc$age[sample_cc$heartattack_chd == 1], w = sample_cc$brfssWt, na.rm = TRUE)
spatstat.geom::weighted.quantile(x = sample_cc$age[sample_cc$heartattack_chd == 0], w = sample_cc$brfssWt, probs=seq(0,1,0.25), na.rm = TRUE)
spatstat.geom::weighted.quantile(x = sample_cc$age[sample_cc$heartattack_chd == 1], w = sample_cc$brfssWt, probs=seq(0,1,0.25), na.rm = TRUE)
spatstat.geom::weighted.median(x = sample_cc$age, w = sample_cc$brfssWt, na.rm = TRUE)
spatstat.geom::weighted.quantile(x = sample_cc$age, w = sample_cc$brfssWt, probs=seq(0,1,0.25), na.rm = TRUE)

# weighted mann-whitney U
weighted_mannwhitney(data = sample_cc, x = age, grp = heartattack_chd,
                              weights = brfssWt)

# Figure 1 gender identity & sex for CVD
ha <- sample_cc %>% 
  filter(heartattack_chd == 1)
nha <- sample_cc %>% 
  filter(heartattack_chd == 0)
tableHA <- t(questionr::tabs(df = ha,
                              x = 'sex',
                              y = 'transgender',
                              weight = 'brfssWt',
                              normwt = TRUE,
                              type = 'counts',
                              digits = 0))
tableNHA <- t(questionr::tabs(df = nha,
                             x = 'sex',
                             y = 'transgender',
                             weight = 'brfssWt',
                             normwt = TRUE,
                             type = 'counts',
                             digits = 0))
tableHA
tableNHA

percCHD <- c(100*tableHA[2,1]/(sum(tableHA[2,1], tableNHA[2,1])),
             100*tableHA[2,2]/(sum(tableHA[2,2], tableNHA[2,2])),
             100*tableHA[3,1]/(sum(tableHA[3,1], tableNHA[3,1])),
             100*tableHA[3,2]/(sum(tableHA[3,2], tableNHA[3,2])),
             100*tableHA[4,1]/(sum(tableHA[4,1], tableNHA[4,1])),
             100*tableHA[4,2]/(sum(tableHA[4,2], tableNHA[4,2])))
Sex <- c('Male sex at birth', 'Female sex at birth', 'Male sex at birth', 
         'Female sex at birth','Male sex at birth', 'Female sex at birth')
identity <- c('Cisgender', 'Cisgender',
             'Transgender', 'Transgender',
             'Gender non-conforming', 'Gender non-conforming')
fig1data <- data.frame(percCHD, Sex, identity)

fig1data %>% 
  ggplot(aes(x = identity, y = percCHD)) +
  geom_col(position = "dodge") +
  theme_minimal(base_size = 16) +
  labs(y = "Percent of participants\nreporting CVD within group", x = "Gender identity") +
  facet_grid(rows = vars(Sex))
  
# BUILD MODEL

# weighted model 1
# Fit the logistic regression demographics only
model1 <- svyglm(heartattack_chd ~ ageStand + sex + race + edu_level + BMI, svy_df, family = "binomial")
# weighted model 2
# Fit the logistic regression
model2 <- svyglm(heartattack_chd ~ ageStand + sex + race + edu_level + BMI +   # model 1 variables
                smoke_status + alcohol + exercise, svy_df, family = "binomial")
# weighted model 3
# Fit the logistic regression
model3 <- svyglm(heartattack_chd ~ ageStand + sex + race + edu_level + BMI +   # model 1 variables
                    smoke_status + alcohol + exercise+                     # model 2 variables
                    transgender, svy_df, family = "binomial")
# weighted model 4
# Fit the logistic regression
model4 <- svyglm(heartattack_chd ~ ageStand + sex + race + edu_level + BMI +   # model 1 variables
                    smoke_status + alcohol + exercise+                     # model 2 variables
                    transgender + transgender*sex, svy_df, family = "binomial")
summary(model4)
dfModel4 <- as.numeric(model4$df.null-model4$df.residual)
chisqModel4 <- as.numeric(model4$null.deviance - model4$deviance)
orModel4 <- exp(coef(model4))
ciModel4 <- exp(confint(model4))

# Stratified by sex at birth
sample_cc_male <- sample_cc %>% 
  filter(sex == "Male") %>% 
  select(-sex)

sample_cc_female <- sample_cc %>% 
  filter(sex == "Female")%>% 
  select(-sex)

# Recheck linearity assumption for stratified
# check age distribution and transform when linearity fails
hist(sample_cc_male$age)
# # create object of the variable AGE*log(AGE)
sample_cc_male$ageStand <- (sample_cc_male$age)^(1/3)
sample_cc_male$age.times.logAge <- sample_cc_male$ageStand * log(sample_cc_male$ageStand)

# Box Tidwell test
boxTidwell_age_male <- glm(heartattack_chd ~ ageStand + age.times.logAge,
                           data = sample_cc_male,
                           family = "binomial")
summary(boxTidwell_age_male)

# Recheck linearity assumption for stratified
# check age distribution and transform when linearity fails
hist(sample_cc_female$age)
# # create object of the variable AGE*log(AGE)
sample_cc_female$ageStand <- sqrt(sample_cc_female$age)
sample_cc_female$age.times.logAge <- sample_cc_female$ageStand * log(sample_cc_female$ageStand)

# Box Tidwell test
boxTidwell_age_female <- glm(heartattack_chd ~ ageStand + age.times.logAge,
                             data = sample_cc_female,
                             family = "binomial")
summary(boxTidwell_age_female)


# MODEL FIT AND MODEL COMPARISONS

# AIC for model 1
AIC(model1)
# AIC for model 2
AIC(model2)
# AIC for model 3
AIC(model3)
# AIC for model 4
AIC(model4)

# create survey weighted objects for stratified models
svy_df_male <- svydesign(data = sample_cc_male, strata = , ids = ~1, weights = ~brfssWt)
svy_df_female <- svydesign(data = sample_cc_female, strata = , ids = ~1, weights = ~brfssWt)

# stratified model estimation
model4male <- svyglm(heartattack_chd ~ ageStand + race + edu_level + BMI +   # model 1 variables
                       smoke_status + alcohol + exercise+                     # model 2 variables
                       transgender, svy_df_male, family = "binomial")

model4female <- svyglm(heartattack_chd ~ ageStand + race + edu_level + BMI +   # model 1 variables
                         smoke_status + alcohol + exercise+                     # model 2 variables
                         transgender, svy_df_female, family = "binomial")

# stratified models output
orModel4male <- exp(coef(model4male))
ciModel4male <- exp(confint(model4male))
dfModel4male <- as.numeric(model4male$df.null-model4male$df.residual)
chisqModel4male <- as.numeric(model4male$null.deviance - model4male$deviance)
orModel4female <- exp(coef(model4female))
ciModel4female <- exp(confint(model4female))
dfModel4female <- as.numeric(model4female$df.null-model4female$df.residual)
chisqModel4female <- as.numeric(model4female$null.deviance - model4female$deviance)

# MODEL FIT
# for female strat
pct_correct_predict_fem <- addmargins(table(predchd = round(predict(model4female,
                                                                type="response")),
                                        chd = model4female$model$heartattack_chd)
)
pct_correct_predict_fem

count.rsquared_fem <- (pct_correct_predict_fem[1,1] + pct_correct_predict_fem[2,2])/
  pct_correct_predict_fem[3,3]

# for male strat
pct_correct_predict_male <- addmargins(table(predchd = round(predict(model4male,
                                                                type="response")),
                                        chd = model4male$model$heartattack_chd)
)
pct_correct_predict_male

count.rsquared_male <- (pct_correct_predict_male[1,1] + pct_correct_predict_male[2,2])/
  pct_correct_predict_male[3,3]

# CHECK MODEL ASSUMPTIONS AND DIAGNOSTICS

vif(model1)
vif(model2)
vif(model3)
vif(model4)
vif(model4female)
vif(model4male)

# plot Cook's distances
plot(model4female, which=4, id.n=5, col="blue", cex.id=0.60)
plot(model4male, which=4, id.n=5, col="blue", cex.id=0.60)

# identify observations with a Cook's Distance > 4/n
# add 2 new variables to the sample_cc dataset called "cooks_dist" and "influential"
# stratified
sample_cc_female <- sample_cc_female %>% 
  mutate(cooks_dist = cooks.distance(model4female)) %>% 
  mutate(influential = case_when(
    cooks_dist > 4/n() ~ 1,
    TRUE ~ 0
  ))
sample_cc_male <- sample_cc_male %>% 
  mutate(cooks_dist = cooks.distance(model4male)) %>% 
  mutate(influential = case_when(
    cooks_dist > 4/n() ~ 1,
    TRUE ~ 0
  ))

# remove influential observations w/o top 5 influential cases
# FEMALE STRATIFIED
# remove influential observations w/o top 5 influential cases
noOut_samplebrfss_female <- sample_cc_female %>%
  top_n(-(n()-5), cooks_dist)    # keep bottom n-5 rows ranked by cooks_dist

# create new Model 4 without outlier cases, called "model4_noOut"
svy_df_noOut_female <- svydesign(data = noOut_samplebrfss_female, ids = ~1, weights = ~brfssWt)
model4_noOut_female <- svyglm(heartattack_chd ~ ageStand + race + edu_level + BMI +   # model 1 variables
                         smoke_status + alcohol + exercise+                     # model 2 variables
                         transgender, svy_df_noOut_female, family = "binomial")
exp(coef(model4_noOut_female))
exp(confint(model4_noOut_female))
exp(confint(model4female))
exp(coef(model4female))
# compare model4female (with outliers) to model4_noOut_female (no outliers)
car::compareCoefs(model4female, model4_noOut_female)

# MALE STRATIFIED
# remove influential observations w/o top 5 influential cases
noOut_samplebrfss_male <- sample_cc_male %>%
  top_n(-(n()-5), cooks_dist)    # keep bottom n-5 rows ranked by cooks_dist

# create new Model 4 without outlier cases, called "model4_noOut"
svy_df_noOut_male <- svydesign(data = noOut_samplebrfss_male, ids = ~1, weights = ~brfssWt)
model4_noOut_male <- svyglm(heartattack_chd ~ ageStand + race + edu_level + BMI +   # model 1 variables
                                smoke_status + alcohol + exercise+                     # model 2 variables
                                transgender, svy_df_noOut_male, family = "binomial")
exp(coef(model4_noOut_male))
exp(confint(model4_noOut_male))
exp(confint(model4male))
exp(coef(model4male))
# compare model4male (with outliers) to model4_noOut_male (no outliers)
car::compareCoefs(model4male, model4_noOut_male)

