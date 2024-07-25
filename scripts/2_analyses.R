### Load packages and import prepared data #####################################
################################################################################

# load packages
library(tidyverse)
library(lavaan)
library(semTools)
library(MASS)
library(pscl)
library(lmtest)
library(mvord)

# import prepared data
df <- readRDS("./data/prepared_data.rds")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Measurement and structural models of latent variables ######################
################################################################################

## measurement model--------
# model
model1 <-
  "
av_usefulness     =~ av_benefit_1 + av_benefit_2 + av_benefit_3 +
                     av_benefit_4 + av_benefit_5 + av_benefit_6 +
                     av_concern_1 + av_concern_4 + av_concern_5
av_concern        =~ av_concern_2 + av_concern_3 + av_concern_6 +
                     av_concern_7
tech_savviness    =~ tech_savvy_1  + tech_savvy_3
driving_enjoyment =~ enjoy_driving_1 + enjoy_driving_3 + enjoy_driving_4
polychronicity    =~ polychronicity_1 + polychronicity_2 + polychronicity_3
envt_concern      =~ envt_concern_1 + envt_concern_2 + envt_concern_3
"

# fit the model and save the outputs
model1 <- cfa(model1, data = df, estimator = "MLM")
sink("./outputs/models/model1.txt")
print(summary(model1, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE))

# convergent and discriminant validity tests
view(reliability(model1))
view(lavInspect(model1, "cor.lv"))
## --------


## structural model (full model)--------
# model
model2 <-
  "
av_usefulness     =~ av_benefit_1 + av_benefit_2 + av_benefit_3 +
                     av_benefit_4 + av_benefit_5 + av_benefit_6 +
                     av_concern_1 + av_concern_4 + av_concern_5
av_concern        =~ av_concern_2 + av_concern_3 + av_concern_6 +
                     av_concern_7
tech_savviness    =~ tech_savvy_1  + tech_savvy_3
driving_enjoyment =~ enjoy_driving_1 + enjoy_driving_3 + enjoy_driving_4
polychronicity    =~ polychronicity_1 + polychronicity_2 + polychronicity_3
envt_concern      =~ envt_concern_1 + envt_concern_2 + envt_concern_3
av_usefulness      ~ age_grp_2 + age_grp_3 + race_1 +
                     gender_1 + education_2 + education_3 +
                     school_2 + school_3 + hh_adult +
                     hh_child + income_grp_2 + income_grp_3 +
                     income_grp_4 + income_grp_5 + employment_2 +
                     employment_3 + driving_exp + citation_1 +
                     crash_exp_1 + hh_vehs + mode_commute_3 +
                     mode_shopping_3 + mode_personal_3 + mode_social_3 +
                     rec_trips
av_concern         ~ age_grp_2 + age_grp_3 + race_1 +
                     gender_1 + education_2 + education_3 +
                     school_2 + school_3 + hh_adult +
                     hh_child + income_grp_2 + income_grp_3 +
                     income_grp_4 + income_grp_5 + employment_2 +
                     employment_3 + driving_exp + citation_1 +
                     crash_exp_1 + hh_vehs + mode_commute_3 +
                     mode_shopping_3 + mode_personal_3 + mode_social_3 +
                     rec_trips
tech_savviness     ~ age_grp_2 + age_grp_3 + race_1 +
                     gender_1 + education_2 + education_3 +
                     school_2 + school_3 + hh_adult +
                     hh_child + income_grp_2 + income_grp_3 +
                     income_grp_4 + income_grp_5 + employment_2 +
                     employment_3 + driving_exp + citation_1 +
                     crash_exp_1 + hh_vehs + mode_commute_3 +
                     mode_shopping_3 + mode_personal_3 + mode_social_3 +
                     rec_trips
driving_enjoyment  ~ age_grp_2 + age_grp_3 + race_1 +
                     gender_1 + education_2 + education_3 +
                     school_2 + school_3 + hh_adult +
                     hh_child + income_grp_2 + income_grp_3 +
                     income_grp_4 + income_grp_5 + employment_2 +
                     employment_3 + driving_exp + citation_1 +
                     crash_exp_1 + hh_vehs + mode_commute_3 +
                     mode_shopping_3 + mode_personal_3 + mode_social_3 +
                     rec_trips
polychronicity     ~ age_grp_2 + age_grp_3 + race_1 +
                     gender_1 + education_2 + education_3 +
                     school_2 + school_3 + hh_adult +
                     hh_child + income_grp_2 + income_grp_3 +
                     income_grp_4 + income_grp_5 + employment_2 +
                     employment_3 + driving_exp + citation_1 +
                     crash_exp_1 + hh_vehs + mode_commute_3 +
                     mode_shopping_3 + mode_personal_3 + mode_social_3 +
                     rec_trips
envt_concern       ~ age_grp_2 + age_grp_3 + race_1 +
                     gender_1 + education_2 + education_3 +
                     school_2 + school_3 + hh_adult +
                     hh_child + income_grp_2 + income_grp_3 +
                     income_grp_4 + income_grp_5 + employment_2 +
                     employment_3 + driving_exp + citation_1 +
                     crash_exp_1 + hh_vehs + mode_commute_3 +
                     mode_shopping_3 + mode_personal_3 + mode_social_3 +
                     rec_trips
"

# fit the model and save the outputs
model2 <- cfa(model2, data = df, estimator = "MLM")
sink("./outputs/models/model2.txt")
print(summary(model2, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE))
rm(model2)
## --------


## structural model (final model)--------
# model
model3 <-
  "
av_usefulness     =~ av_benefit_1 + av_benefit_2 + av_benefit_3 +
                     av_benefit_4 + av_benefit_5 + av_benefit_6 +
                     av_concern_1 + av_concern_4 + av_concern_5
av_concern        =~ av_concern_2 + av_concern_3 + av_concern_6 +
                     av_concern_7
tech_savviness    =~ tech_savvy_1  + tech_savvy_3
driving_enjoyment =~ enjoy_driving_1 + enjoy_driving_3 + enjoy_driving_4
polychronicity    =~ polychronicity_1 + polychronicity_2 + polychronicity_3
envt_concern      =~ envt_concern_1 + envt_concern_2 + envt_concern_3
av_usefulness      ~ gender_1 + education_3 + employment_3 +
                     driving_exp + mode_commute_3 + mode_personal_3
av_concern         ~ race_1 + gender_1 +  hh_adult +
                     hh_child + income_grp_2 + income_grp_3 +
                     driving_exp + citation_1
tech_savviness     ~ age_grp_2 + gender_1 + hh_child +
                     income_grp_4 + employment_3 + driving_exp +
                     crash_exp_1 + mode_personal_3 + mode_social_3
driving_enjoyment  ~ gender_1 + school_2 + driving_exp +
                     citation_1 + mode_personal_3 + rec_trips
polychronicity     ~ education_3 + hh_child + income_grp_2 +
                     employment_3 + driving_exp + citation_1
envt_concern       ~ gender_1 + education_2 + education_3 +
                     employment_2 + crash_exp_1
"

# fit the model and save the outputs
model3 <- cfa(model3, data = df, estimator = "MLM")
sink("./outputs/models/model3.txt")
print(summary(model3, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE))
rm(model3)
## --------


## Join the predicted values of latent variables to main data frame--------
df <- cbind(df, as.data.frame(lavPredict(model1)))
rm(model1)
## --------

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Multivariate binary probit models of TBAs in HV and AV ####################
################################################################################

# make output variables as ordered
df <- df %>%
  mutate_at(
    colnames(df %>%
      dplyr::select(
        tba_g1_hv:tba_g7_hv,
        tba_g1_av:tba_g7_av
      )),
    as.ordered
  )


## model for TBA-g1--------
# null model
model4 <- mvord(
  formula = MMO2(tba_g1_hv, tba_g1_av) ~ 0,
  threshold.constraints = c(1:2),
  coef.constraints = c(1:2),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
options(max.print = 100000000)
sink("./outputs/models/model4.txt")
print(summary(model4))
rm(model4)

# full model
model5 <- mvord(
  formula = MMO2(tba_g1_hv, tba_g1_av) ~ 0 +
    age_grp_2 + age_grp_3 + gender_1 +
    education_2 + education_3 + school_2 +
    school_3 + employment_2 + employment_3 +
    race_1 + hh_adult + hh_child +
    income_grp_2 + income_grp_3 + income_grp_4 +
    income_grp_5 + driving_exp + hh_vehs +
    mode_commute_3 + mode_shopping_3 + mode_personal_3 +
    mode_social_3 + citation_1 + crash_exp_1 +
    rec_trips + time + cost +
    veh_own_1 + veh_type_1 + veh_type_2 +
    veh_type_3 + veh_feature_1 + veh_feature_2 +
    veh_feature_3 + veh_feature_4 + veh_feature_5 +
    veh_feature_6 + veh_feature_7 + companion_tot +
    companion_1 + companion_2 + companion_3 +
    companion_4 + companion_5 + per_drive_3 +
    per_drive_4 + per_drive_5 + trip_exp_1 +
    trip_exp_3 + trip_exp_4 + av_usefulness +
    av_concern + tech_savviness + driving_enjoyment +
    polychronicity + envt_concern,
  threshold.constraints = c(1:2),
  coef.constraints = cbind(
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(NA, 2),
    c(NA, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2)
  ),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model5.txt")
print(summary(model5))
rm(model5)

# final model
model6 <- mvord(
  formula = MMO2(tba_g1_hv, tba_g1_av) ~ 0 + # 1
    age_grp_2 + age_grp_3 + gender_1 + # 2
    education_2 + education_3 + school_2 + # 3
    school_3 + employment_2 + employment_3 + # 4
    race_1 + hh_adult + hh_child + # 5
    income_grp_2 + income_grp_3 + income_grp_4 + # 6
    income_grp_5 + driving_exp + hh_vehs + # 7
    mode_commute_3 + mode_shopping_3 + mode_personal_3 + # 8
    mode_social_3 + citation_1 + crash_exp_1 + # 9
    rec_trips + time + cost + # 10
    veh_own_1 + veh_type_1 + veh_type_2 + # 11
    veh_type_3 + veh_feature_1 + veh_feature_2 + # 12
    veh_feature_3 + veh_feature_4 + veh_feature_5 + # 13
    veh_feature_6 + veh_feature_7 + companion_tot + # 14
    companion_1 + companion_2 + companion_3 + # 15
    companion_4 + companion_5 + per_drive_3 + # 16
    per_drive_4 + per_drive_5 + trip_exp_1 + # 17
    trip_exp_3 + trip_exp_4 + av_usefulness + # 18
    av_concern + tech_savviness + driving_enjoyment + # 19
    polychronicity + envt_concern, # 20
  threshold.constraints = c(1:2), # 1
  coef.constraints = cbind(
    c(1, NA), c(1, NA), c(NA, NA), # 2
    c(NA, 2), c(1, NA), c(NA, NA), # 3
    c(NA, NA), c(NA, NA), c(NA, NA), # 4
    c(NA, NA), c(NA, 2), c(NA, NA), # 5
    c(NA, NA), c(NA, NA), c(NA, NA), # 6
    c(NA, NA), c(NA, 2), c(NA, NA), # 7
    c(NA, NA), c(NA, NA), c(NA, NA), # 8
    c(1, NA), c(NA, NA), c(NA, NA), # 9
    c(NA, NA), c(NA, NA), c(NA, NA), # 10
    c(NA, NA), c(NA, NA), c(NA, NA), # 11
    c(NA, NA), c(NA, NA), c(NA, NA), # 12
    c(1, NA), c(NA, NA), c(NA, NA), # 13
    c(NA, NA), c(1, NA), c(NA, NA), # 14
    c(NA, NA), c(1, 2), c(NA, NA), # 15
    c(NA, 2), c(NA, 2), c(NA, NA), # 16
    c(1, NA), c(1, NA), c(NA, NA), # 17
    c(NA, 2), c(NA, 2), c(NA, 2), # 18
    c(NA, 2), c(NA, NA), c(NA, NA), # 19
    c(1, 2), c(NA, NA)
  ), # 20
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model6.txt")
print(summary(model6))
rm(model6)

# likelihood ratio test
ll_null <- as.numeric(logLik(model4))
ll_full <- as.numeric(logLik(model6))
df_null <- attr(logLik(model4), "df")
df_full <- attr(logLik(model6), "df")
LR_stat <- 2 * (ll_full - ll_null)
df_diff <- df_full - df_null
p_value <- pchisq(LR_stat, df_diff, lower.tail = FALSE)
round(LR_stat, 2)
round(p_value, 3)
round(df_diff, 0)
## --------


## model for TBA-g2--------
# null model
model7 <- mvord(
  formula = MMO2(tba_g2_hv, tba_g2_av) ~ 0,
  threshold.constraints = c(1:2),
  coef.constraints = c(1:2),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model7.txt")
print(summary(model7))
rm(model7)

# full model
model8 <- mvord(
  formula = MMO2(tba_g2_hv, tba_g2_av) ~ 0 +
    age_grp_2 + age_grp_3 + gender_1 +
    education_2 + education_3 + school_2 +
    school_3 + employment_2 + employment_3 +
    race_1 + hh_adult + hh_child +
    income_grp_2 + income_grp_3 + income_grp_4 +
    income_grp_5 + driving_exp + hh_vehs +
    mode_commute_3 + mode_shopping_3 + mode_personal_3 +
    mode_social_3 + citation_1 + crash_exp_1 +
    rec_trips + time + cost +
    veh_own_1 + veh_type_1 + veh_type_2 +
    veh_type_3 + veh_feature_1 + veh_feature_2 +
    veh_feature_3 + veh_feature_4 + veh_feature_5 +
    veh_feature_6 + veh_feature_7 + companion_tot +
    companion_1 + companion_2 + companion_3 +
    companion_4 + companion_5 + per_drive_3 +
    per_drive_4 + per_drive_5 + trip_exp_1 +
    trip_exp_3 + trip_exp_4 + av_usefulness +
    av_concern + tech_savviness + driving_enjoyment +
    polychronicity + envt_concern,
  threshold.constraints = c(1:2),
  coef.constraints = cbind(
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(NA, 2),
    c(NA, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2)
  ),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model8.txt")
print(summary(model8))
rm(model8)

# final model
model9 <- mvord(
  formula = MMO2(tba_g2_hv, tba_g2_av) ~ 0 + # 1
    age_grp_2 + age_grp_3 + gender_1 + # 2
    education_2 + education_3 + school_2 + # 3
    school_3 + employment_2 + employment_3 + # 4
    race_1 + hh_adult + hh_child + # 5
    income_grp_2 + income_grp_3 + income_grp_4 + # 6
    income_grp_5 + driving_exp + hh_vehs + # 7
    mode_commute_3 + mode_shopping_3 + mode_personal_3 + # 8
    mode_social_3 + citation_1 + crash_exp_1 + # 9
    rec_trips + time + cost + # 10
    veh_own_1 + veh_type_1 + veh_type_2 + # 11
    veh_type_3 + veh_feature_1 + veh_feature_2 + # 12
    veh_feature_3 + veh_feature_4 + veh_feature_5 + # 13
    veh_feature_6 + veh_feature_7 + companion_tot + # 14
    companion_1 + companion_2 + companion_3 + # 15
    companion_4 + companion_5 + per_drive_3 + # 16
    per_drive_4 + per_drive_5 + trip_exp_1 + # 17
    trip_exp_3 + trip_exp_4 + av_usefulness + # 18
    av_concern + tech_savviness + driving_enjoyment + # 19
    polychronicity + envt_concern, # 20
  threshold.constraints = c(1:2), # 1
  coef.constraints = cbind(
    c(NA, NA), c(NA, NA), c(NA, NA), # 2
    c(NA, NA), c(NA, NA), c(NA, NA), # 3
    c(NA, NA), c(NA, NA), c(NA, NA), # 4
    c(NA, 2), c(NA, NA), c(NA, NA), # 5
    c(NA, NA), c(NA, NA), c(NA, NA), # 6
    c(NA, 2), c(1, 2), c(NA, NA), # 7
    c(NA, 2), c(NA, NA), c(NA, NA), # 8
    c(NA, NA), c(NA, NA), c(NA, NA), # 9
    c(NA, 2), c(NA, NA), c(NA, NA), # 10
    c(NA, NA), c(NA, NA), c(NA, NA), # 11
    c(NA, NA), c(NA, NA), c(1, NA), # 12
    c(1, NA), c(NA, NA), c(NA, NA), # 13
    c(NA, NA), c(1, NA), c(NA, NA), # 14
    c(NA, NA), c(NA, NA), c(NA, NA), # 15
    c(NA, NA), c(NA, NA), c(NA, NA), # 16
    c(1, NA), c(1, NA), c(NA, NA), # 17
    c(1, 2), c(NA, 2), c(NA, 2), # 18
    c(NA, 2), c(NA, 2), c(NA, NA), # 19
    c(1, 2), c(NA, 2)
  ), # 20
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model9.txt")
print(summary(model9))
rm(model9)

# likelihood ratio test
ll_null <- as.numeric(logLik(model7))
ll_full <- as.numeric(logLik(model9))
df_null <- attr(logLik(model7), "df")
df_full <- attr(logLik(model9), "df")
LR_stat <- 2 * (ll_full - ll_null)
df_diff <- df_full - df_null
p_value <- pchisq(LR_stat, df_diff, lower.tail = FALSE)
round(LR_stat, 2)
round(p_value, 3)
round(df_diff, 0)
## --------


## model for TBA-g3--------
# null model
model10 <- mvord(
  formula = MMO2(tba_g3_hv, tba_g3_av) ~ 0,
  threshold.constraints = c(1:2),
  coef.constraints = c(1:2),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model10.txt")
print(summary(model10))
rm(model10)

# full model
model11 <- mvord(
  formula = MMO2(tba_g3_hv, tba_g3_av) ~ 0 +
    age_grp_2 + age_grp_3 + gender_1 +
    education_2 + education_3 + school_2 +
    school_3 + employment_2 + employment_3 +
    race_1 + hh_adult + hh_child +
    income_grp_2 + income_grp_3 + income_grp_4 +
    income_grp_5 + driving_exp + hh_vehs +
    mode_commute_3 + mode_shopping_3 + mode_personal_3 +
    mode_social_3 + citation_1 + crash_exp_1 +
    rec_trips + time + cost +
    veh_own_1 + veh_type_1 + veh_type_2 +
    veh_type_3 + veh_feature_1 + veh_feature_2 +
    veh_feature_3 + veh_feature_4 + veh_feature_5 +
    veh_feature_6 + veh_feature_7 + companion_tot +
    companion_1 + companion_2 + companion_3 +
    companion_4 + companion_5 + per_drive_3 +
    per_drive_4 + per_drive_5 + trip_exp_1 +
    trip_exp_3 + trip_exp_4 + av_usefulness +
    av_concern + tech_savviness + driving_enjoyment +
    polychronicity + envt_concern,
  threshold.constraints = c(1:2),
  coef.constraints = cbind(
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(NA, 2),
    c(NA, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2)
  ),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model11.txt")
print(summary(model11))
rm(model11)

# final model
model12 <- mvord(
  formula = MMO2(tba_g3_hv, tba_g3_av) ~ 0 + # 1
    age_grp_2 + age_grp_3 + gender_1 + # 2
    education_2 + education_3 + school_2 + # 3
    school_3 + employment_2 + employment_3 + # 4
    race_1 + hh_adult + hh_child + # 5
    income_grp_2 + income_grp_3 + income_grp_4 + # 6
    income_grp_5 + driving_exp + hh_vehs + # 7
    mode_commute_3 + mode_shopping_3 + mode_personal_3 + # 8
    mode_social_3 + citation_1 + crash_exp_1 + # 9
    rec_trips + time + cost + # 10
    veh_own_1 + veh_type_1 + veh_type_2 + # 11
    veh_type_3 + veh_feature_1 + veh_feature_2 + # 12
    veh_feature_3 + veh_feature_4 + veh_feature_5 + # 13
    veh_feature_6 + veh_feature_7 + companion_tot + # 14
    companion_1 + companion_2 + companion_3 + # 15
    companion_4 + companion_5 + per_drive_3 + # 16
    per_drive_4 + per_drive_5 + trip_exp_1 + # 17
    trip_exp_3 + trip_exp_4 + av_usefulness + # 18
    av_concern + tech_savviness + driving_enjoyment + # 19
    polychronicity + envt_concern, # 20
  threshold.constraints = c(1:2), # 1
  coef.constraints = cbind(
    c(NA, NA), c(NA, NA), c(1, NA), # 2
    c(NA, NA), c(1, NA), c(NA, NA), # 3
    c(NA, NA), c(NA, NA), c(NA, NA), # 4
    c(NA, 2), c(1, NA), c(NA, NA), # 5
    c(NA, NA), c(NA, NA), c(NA, NA), # 6
    c(NA, 2), c(NA, NA), c(NA, NA), # 7
    c(NA, NA), c(NA, NA), c(NA, NA), # 8
    c(1, NA), c(1, NA), c(NA, NA), # 9
    c(1, 2), c(NA, NA), c(NA, NA), # 10
    c(NA, NA), c(1, NA), c(1, NA), # 11
    c(NA, NA), c(NA, NA), c(NA, NA), # 12
    c(NA, NA), c(1, NA), c(NA, NA), # 13
    c(NA, NA), c(NA, NA), c(NA, NA), # 14
    c(NA, NA), c(NA, 2), c(1, 2), # 15
    c(1, NA), c(NA, 2), c(NA, NA), # 16
    c(NA, NA), c(NA, NA), c(1, NA), # 17
    c(1, NA), c(NA, NA), c(NA, 2), # 18
    c(NA, 2), c(NA, NA), c(NA, NA), # 19
    c(NA, NA), c(1, 2)
  ), # 20
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model12.txt")
print(summary(model12))
rm(model12)

# likelihood ratio test
ll_null <- as.numeric(logLik(model10))
ll_full <- as.numeric(logLik(model12))
df_null <- attr(logLik(model10), "df")
df_full <- attr(logLik(model12), "df")
LR_stat <- 2 * (ll_full - ll_null)
df_diff <- df_full - df_null
p_value <- pchisq(LR_stat, df_diff, lower.tail = FALSE)
round(LR_stat, 2)
round(p_value, 3)
round(df_diff, 0)
## --------


## model for TBA-g4--------
# null model
model13 <- mvord(
  formula = MMO2(tba_g4_hv, tba_g4_av) ~ 0,
  threshold.constraints = c(1:2),
  coef.constraints = c(1:2),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model13.txt")
print(summary(model13))
rm(model13)

# full model
model14 <- mvord(
  formula = MMO2(tba_g4_hv, tba_g4_av) ~ 0 +
    age_grp_2 + age_grp_3 + gender_1 +
    education_2 + education_3 + school_2 +
    school_3 + employment_2 + employment_3 +
    race_1 + hh_adult + hh_child +
    income_grp_2 + income_grp_3 + income_grp_4 +
    income_grp_5 + driving_exp + hh_vehs +
    mode_commute_3 + mode_shopping_3 + mode_personal_3 +
    mode_social_3 + citation_1 + crash_exp_1 +
    rec_trips + time + cost +
    veh_own_1 + veh_type_1 + veh_type_2 +
    veh_type_3 + veh_feature_1 + veh_feature_2 +
    veh_feature_3 + veh_feature_4 + veh_feature_5 +
    veh_feature_6 + veh_feature_7 + companion_tot +
    companion_1 + companion_2 + companion_3 +
    companion_4 + companion_5 + per_drive_3 +
    per_drive_4 + per_drive_5 + trip_exp_1 +
    trip_exp_3 + trip_exp_4 + av_usefulness +
    av_concern + tech_savviness + driving_enjoyment +
    polychronicity + envt_concern,
  threshold.constraints = c(1:2),
  coef.constraints = cbind(
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(NA, 2),
    c(NA, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2)
  ),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model14.txt")
print(summary(model14))
rm(model14)

# final model
model15 <- mvord(
  formula = MMO2(tba_g4_hv, tba_g4_av) ~ 0 + # 1
    age_grp_2 + age_grp_3 + gender_1 + # 2
    education_2 + education_3 + school_2 + # 3
    school_3 + employment_2 + employment_3 + # 4
    race_1 + hh_adult + hh_child + # 5
    income_grp_2 + income_grp_3 + income_grp_4 + # 6
    income_grp_5 + driving_exp + hh_vehs + # 7
    mode_commute_3 + mode_shopping_3 + mode_personal_3 + # 8
    mode_social_3 + citation_1 + crash_exp_1 + # 9
    rec_trips + time + cost + # 10
    veh_own_1 + veh_type_1 + veh_type_2 + # 11
    veh_type_3 + veh_feature_1 + veh_feature_2 + # 12
    veh_feature_3 + veh_feature_4 + veh_feature_5 + # 13
    veh_feature_6 + veh_feature_7 + companion_tot + # 14
    companion_1 + companion_2 + companion_3 + # 15
    companion_4 + companion_5 + per_drive_3 + # 16
    per_drive_4 + per_drive_5 + trip_exp_1 + # 17
    trip_exp_3 + trip_exp_4 + av_usefulness + # 18
    av_concern + tech_savviness + driving_enjoyment + # 19
    polychronicity + envt_concern, # 20
  threshold.constraints = c(1:2), # 1
  coef.constraints = cbind(
    c(1, 2), c(1, 2), c(NA, NA), # 2
    c(NA, 2), c(1, NA), c(1, NA), # 3
    c(NA, NA), c(NA, NA), c(NA, NA), # 4
    c(NA, 2), c(NA, NA), c(1, 2), # 5
    c(1, NA), c(NA, NA), c(NA, NA), # 6
    c(NA, 2), c(NA, 2), c(NA, 2), # 7
    c(1, NA), c(NA, NA), c(1, NA), # 8
    c(NA, NA), c(NA, 2), c(NA, NA), # 9
    c(NA, NA), c(NA, NA), c(NA, NA), # 10
    c(NA, NA), c(NA, NA), c(NA, 2), # 11
    c(NA, NA), c(NA, NA), c(NA, NA), # 12
    c(NA, NA), c(1, NA), c(NA, NA), # 13
    c(NA, NA), c(NA, NA), c(1, NA), # 14
    c(NA, NA), c(NA, NA), c(NA, NA), # 15
    c(NA, NA), c(NA, NA), c(NA, NA), # 16
    c(1, NA), c(1, NA), c(NA, NA), # 17
    c(1, 2), c(1, 2), c(NA, 2), # 18
    c(NA, NA), c(NA, NA), c(NA, NA), # 19
    c(1, NA), c(NA, NA)
  ), # 20
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model15.txt")
print(summary(model15))
rm(model15)

# likelihood ratio test
ll_null <- as.numeric(logLik(model13))
ll_full <- as.numeric(logLik(model15))
df_null <- attr(logLik(model13), "df")
df_full <- attr(logLik(model15), "df")
LR_stat <- 2 * (ll_full - ll_null)
df_diff <- df_full - df_null
p_value <- pchisq(LR_stat, df_diff, lower.tail = FALSE)
round(LR_stat, 2)
round(p_value, 3)
round(df_diff, 0)
## --------


## model for TBA-g5--------
# null model
model16 <- mvord(
  formula = MMO2(tba_g5_hv, tba_g5_av) ~ 0,
  threshold.constraints = c(1:2),
  coef.constraints = c(1:2),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model16.txt")
print(summary(model16))
rm(model16)

# full model
model17 <- mvord(
  formula = MMO2(tba_g5_hv, tba_g5_av) ~ 0 +
    age_grp_2 + age_grp_3 + gender_1 +
    education_2 + education_3 + school_2 +
    school_3 + employment_2 + employment_3 +
    race_1 + hh_adult + hh_child +
    income_grp_2 + income_grp_3 + income_grp_4 +
    income_grp_5 + driving_exp + hh_vehs +
    mode_commute_3 + mode_shopping_3 + mode_personal_3 +
    mode_social_3 + citation_1 + crash_exp_1 +
    rec_trips + time + cost +
    veh_own_1 + veh_type_1 + veh_type_2 +
    veh_type_3 + veh_feature_1 + veh_feature_2 +
    veh_feature_3 + veh_feature_4 + veh_feature_5 +
    veh_feature_6 + veh_feature_7 + companion_tot +
    companion_1 + companion_2 + companion_3 +
    companion_4 + companion_5 + per_drive_3 +
    per_drive_4 + per_drive_5 + trip_exp_1 +
    trip_exp_3 + trip_exp_4 + av_usefulness +
    av_concern + tech_savviness + driving_enjoyment +
    polychronicity + envt_concern,
  threshold.constraints = c(1:2),
  coef.constraints = cbind(
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(NA, 2),
    c(NA, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2)
  ),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model17.txt")
print(summary(model17))
rm(model17)

# final model
model18 <- mvord(
  formula = MMO2(tba_g5_hv, tba_g5_av) ~ 0 + # 1
    age_grp_2 + age_grp_3 + gender_1 + # 2
    education_2 + education_3 + school_2 + # 3
    school_3 + employment_2 + employment_3 + # 4
    race_1 + hh_adult + hh_child + # 5
    income_grp_2 + income_grp_3 + income_grp_4 + # 6
    income_grp_5 + driving_exp + hh_vehs + # 7
    mode_commute_3 + mode_shopping_3 + mode_personal_3 + # 8
    mode_social_3 + citation_1 + crash_exp_1 + # 9
    rec_trips + time + cost + # 10
    veh_own_1 + veh_type_1 + veh_type_2 + # 11
    veh_type_3 + veh_feature_1 + veh_feature_2 + # 12
    veh_feature_3 + veh_feature_4 + veh_feature_5 + # 13
    veh_feature_6 + veh_feature_7 + companion_tot + # 14
    companion_1 + companion_2 + companion_3 + # 15
    companion_4 + companion_5 + per_drive_3 + # 16
    per_drive_4 + per_drive_5 + trip_exp_1 + # 17
    trip_exp_3 + trip_exp_4 + av_usefulness + # 18
    av_concern + tech_savviness + driving_enjoyment + # 19
    polychronicity + envt_concern, # 20
  threshold.constraints = c(1:2), # 1
  coef.constraints = cbind(
    c(NA, 2), c(NA, 2), c(1, NA), # 2
    c(NA, NA), c(1, NA), c(NA, NA), # 3
    c(NA, NA), c(NA, NA), c(NA, NA), # 4
    c(NA, 2), c(NA, NA), c(NA, 2), # 5
    c(NA, NA), c(NA, NA), c(NA, NA), # 6
    c(NA, NA), c(NA, NA), c(NA, NA), # 7
    c(NA, NA), c(NA, 2), c(NA, NA), # 8
    c(NA, NA), c(NA, NA), c(1, 2), # 9
    c(NA, 2), c(NA, NA), c(NA, NA), # 10
    c(NA, NA), c(NA, NA), c(1, 2), # 11
    c(1, NA), c(NA, NA), c(NA, NA), # 12
    c(NA, NA), c(NA, NA), c(NA, NA), # 13
    c(NA, NA), c(NA, NA), c(NA, NA), # 14
    c(NA, NA), c(1, NA), c(NA, NA), # 15
    c(NA, NA), c(NA, NA), c(NA, NA), # 16
    c(NA, NA), c(NA, NA), c(NA, NA), # 17
    c(1, 2), c(1, 2), c(NA, 2), # 18
    c(NA, 2), c(NA, NA), c(1, NA), # 19
    c(1, NA), c(NA, 2)
  ), # 20
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model18.txt")
print(summary(model18))
rm(model18)
closeAllConnections()

# likelihood ratio test
ll_null <- as.numeric(logLik(model16))
ll_full <- as.numeric(logLik(model18))
df_null <- attr(logLik(model16), "df")
df_full <- attr(logLik(model18), "df")
LR_stat <- 2 * (ll_full - ll_null)
df_diff <- df_full - df_null
p_value <- pchisq(LR_stat, df_diff, lower.tail = FALSE)
round(LR_stat, 2)
round(p_value, 3)
round(df_diff, 0)
## --------


## model for TBA-g6--------
# null model
model19 <- mvord(
  formula = MMO2(tba_g6_hv, tba_g6_av) ~ 0,
  threshold.constraints = c(1:2),
  coef.constraints = c(1:2),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model19.txt")
print(summary(model19))
rm(model19)

# full model
model20 <- mvord(
  formula = MMO2(tba_g6_hv, tba_g6_av) ~ 0 +
    age_grp_2 + age_grp_3 + gender_1 +
    education_2 + education_3 + school_2 +
    school_3 + employment_2 + employment_3 +
    race_1 + hh_adult + hh_child +
    income_grp_2 + income_grp_3 + income_grp_4 +
    income_grp_5 + driving_exp + hh_vehs +
    mode_commute_3 + mode_shopping_3 + mode_personal_3 +
    mode_social_3 + citation_1 + crash_exp_1 +
    rec_trips + time + cost +
    veh_own_1 + veh_type_1 + veh_type_2 +
    veh_type_3 + veh_feature_1 + veh_feature_2 +
    veh_feature_3 + veh_feature_4 + veh_feature_5 +
    veh_feature_6 + veh_feature_7 + companion_tot +
    companion_1 + companion_2 + companion_3 +
    companion_4 + companion_5 + per_drive_3 +
    per_drive_4 + per_drive_5 + trip_exp_1 +
    trip_exp_3 + trip_exp_4 + av_usefulness +
    av_concern + tech_savviness + driving_enjoyment +
    polychronicity + envt_concern,
  threshold.constraints = c(1:2),
  coef.constraints = cbind(
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(NA, 2),
    c(NA, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2)
  ),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model20.txt")
print(summary(model20))
rm(model20)

# final model
model21 <- mvord(
  formula = MMO2(tba_g6_hv, tba_g6_av) ~ 0 + # 1
    age_grp_2 + age_grp_3 + gender_1 + # 2
    education_2 + education_3 + school_2 + # 3
    school_3 + employment_2 + employment_3 + # 4
    race_1 + hh_adult + hh_child + # 5
    income_grp_2 + income_grp_3 + income_grp_4 + # 6
    income_grp_5 + driving_exp + hh_vehs + # 7
    mode_commute_3 + mode_shopping_3 + mode_personal_3 + # 8
    mode_social_3 + citation_1 + crash_exp_1 + # 9
    rec_trips + time + cost + # 10
    veh_own_1 + veh_type_1 + veh_type_2 + # 11
    veh_type_3 + veh_feature_1 + veh_feature_2 + # 12
    veh_feature_3 + veh_feature_4 + veh_feature_5 + # 13
    veh_feature_6 + veh_feature_7 + companion_tot + # 14
    companion_1 + companion_2 + companion_3 + # 15
    companion_4 + companion_5 + per_drive_3 + # 16
    per_drive_4 + per_drive_5 + trip_exp_1 + # 17
    trip_exp_3 + trip_exp_4 + av_usefulness + # 18
    av_concern + tech_savviness + driving_enjoyment + # 19
    polychronicity + envt_concern, # 20
  threshold.constraints = c(1:2), # 1
  coef.constraints = cbind(
    c(1, 2), c(1, NA), c(NA, NA), # 2
    c(NA, NA), c(1, NA), c(1, NA), # 3
    c(NA, 2), c(NA, NA), c(NA, NA), # 4
    c(1, NA), c(NA, NA), c(NA, NA), # 5
    c(NA, NA), c(NA, NA), c(NA, NA), # 6
    c(NA, NA), c(1, NA), c(NA, 2), # 7
    c(NA, 2), c(NA, NA), c(NA, NA), # 8
    c(NA, 2), c(NA, NA), c(NA, 2), # 9
    c(NA, NA), c(1, NA), c(1, NA), # 10
    c(NA, NA), c(1, NA), c(1, NA), # 11
    c(1, NA), c(NA, NA), c(NA, NA), # 12
    c(NA, NA), c(NA, NA), c(NA, NA), # 13
    c(1, NA), c(NA, NA), c(NA, NA), # 14
    c(NA, NA), c(NA, NA), c(NA, NA), # 15
    c(NA, 2), c(1, NA), c(NA, NA), # 16
    c(1, NA), c(1, NA), c(NA, NA), # 17
    c(NA, NA), c(1, 2), c(NA, 2), # 18
    c(NA, 2), c(NA, NA), c(NA, NA), # 19
    c(1, NA), c(NA, NA)
  ), # 20
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model21.txt")
print(summary(model21))
rm(model21)

# likelihood ratio test
ll_null <- as.numeric(logLik(model19))
ll_full <- as.numeric(logLik(model21))
df_null <- attr(logLik(model19), "df")
df_full <- attr(logLik(model21), "df")
LR_stat <- 2 * (ll_full - ll_null)
df_diff <- df_full - df_null
p_value <- pchisq(LR_stat, df_diff, lower.tail = FALSE)
round(LR_stat, 2)
round(p_value, 3)
round(df_diff, 0)
## --------


## model for TBA-g7--------
# null model
model22 <- mvord(
  formula = MMO2(tba_g7_hv, tba_g7_av) ~ 0,
  threshold.constraints = c(1:2),
  coef.constraints = c(1:2),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model22.txt")
print(summary(model22))
rm(model22)

# full model
model23 <- mvord(
  formula = MMO2(tba_g7_hv, tba_g7_av) ~ 0 +
    age_grp_2 + age_grp_3 + gender_1 +
    education_2 + education_3 + school_2 +
    school_3 + employment_2 + employment_3 +
    race_1 + hh_adult + hh_child +
    income_grp_2 + income_grp_3 + income_grp_4 +
    income_grp_5 + driving_exp + hh_vehs +
    mode_commute_3 + mode_shopping_3 + mode_personal_3 +
    mode_social_3 + citation_1 + crash_exp_1 +
    rec_trips + time + cost +
    veh_own_1 + veh_type_1 + veh_type_2 +
    veh_type_3 + veh_feature_1 + veh_feature_2 +
    veh_feature_3 + veh_feature_4 + veh_feature_5 +
    veh_feature_6 + veh_feature_7 + companion_tot +
    companion_1 + companion_2 + companion_3 +
    companion_4 + companion_5 + per_drive_3 +
    per_drive_4 + per_drive_5 + trip_exp_1 +
    trip_exp_3 + trip_exp_4 + av_usefulness +
    av_concern + tech_savviness + driving_enjoyment +
    polychronicity + envt_concern,
  threshold.constraints = c(1:2),
  coef.constraints = cbind(
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(NA, 2),
    c(NA, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2)
  ),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model23.txt")
print(summary(model23))
rm(model23)

# final model
model24 <- mvord(
  formula = MMO2(tba_g7_hv, tba_g7_av) ~ 0 + # 1
    age_grp_2 + age_grp_3 + gender_1 + # 2
    education_2 + education_3 + school_2 + # 3
    school_3 + employment_2 + employment_3 + # 4
    race_1 + hh_adult + hh_child + # 5
    income_grp_2 + income_grp_3 + income_grp_4 + # 6
    income_grp_5 + driving_exp + hh_vehs + # 7
    mode_commute_3 + mode_shopping_3 + mode_personal_3 + # 8
    mode_social_3 + citation_1 + crash_exp_1 + # 9
    rec_trips + time + cost + # 10
    veh_own_1 + veh_type_1 + veh_type_2 + # 11
    veh_type_3 + veh_feature_1 + veh_feature_2 + # 12
    veh_feature_3 + veh_feature_4 + veh_feature_5 + # 13
    veh_feature_6 + veh_feature_7 + companion_tot + # 14
    companion_1 + companion_2 + companion_3 + # 15
    companion_4 + companion_5 + per_drive_3 + # 16
    per_drive_4 + per_drive_5 + trip_exp_1 + # 17
    trip_exp_3 + trip_exp_4 + av_usefulness + # 18
    av_concern + tech_savviness + driving_enjoyment + # 19
    polychronicity + envt_concern, # 20
  threshold.constraints = c(1:2), # 1
  coef.constraints = cbind(
    c(1, 2), c(1, 2), c(NA, NA), # 2
    c(NA, NA), c(NA, NA), c(NA, NA), # 3
    c(1, NA), c(NA, NA), c(NA, NA), # 4
    c(NA, NA), c(NA, NA), c(NA, NA), # 5
    c(NA, NA), c(NA, NA), c(NA, NA), # 6
    c(NA, NA), c(1, 2), c(NA, 2), # 7
    c(NA, 2), c(1, NA), c(NA, NA), # 8
    c(NA, NA), c(NA, NA), c(1, NA), # 9
    c(1, NA), c(NA, NA), c(NA, NA), # 10
    c(NA, NA), c(NA, NA), c(NA, NA), # 11
    c(NA, NA), c(NA, NA), c(NA, NA), # 12
    c(NA, NA), c(NA, NA), c(NA, NA), # 13
    c(NA, NA), c(NA, NA), c(NA, NA), # 14
    c(NA, NA), c(NA, NA), c(1, NA), # 15
    c(1, NA), c(NA, NA), c(NA, NA), # 16
    c(NA, NA), c(NA, NA), c(NA, NA), # 17
    c(NA, NA), c(1, NA), c(NA, NA), # 18
    c(NA, NA), c(NA, NA), c(NA, 2), # 19
    c(NA, NA), c(NA, NA)
  ), # 20
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model24.txt")
print(summary(model24))
rm(model24)

# likelihood ratio test
ll_null <- as.numeric(logLik(model22))
ll_full <- as.numeric(logLik(model24))
df_null <- attr(logLik(model22), "df")
df_full <- attr(logLik(model24), "df")
LR_stat <- 2 * (ll_full - ll_null)
df_diff <- df_full - df_null
p_value <- pchisq(LR_stat, df_diff, lower.tail = FALSE)
round(LR_stat, 2)
round(p_value, 3)
round(df_diff, 0)
## --------

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Multivariate ordered probit models of TU or TTU in HV and AV ###############
################################################################################

# make outcome variables as ordered
df <- df %>%
  mutate_at(
    colnames(df %>%
      dplyr::select(tu_hv, tu_av)),
    as.ordered
  )

# null model
model25 <- mvord(
  formula = MMO2(tu_hv, tu_av) ~ 0,
  threshold.constraints = c(1:2),
  coef.constraints = c(1:2),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model25.txt")
print(summary(model25))
rm(model25)

# full model (couldn't solve)
model26 <- mvord(
  formula = MMO2(tu_hv, tu_av) ~ 0 +
    age_grp_2 + age_grp_3 + gender_1 +
    education_2 + education_3 + school_2 +
    school_3 + employment_2 + employment_3 +
    race_1 + hh_adult + hh_child +
    income_grp_2 + income_grp_3 + income_grp_4 +
    income_grp_5 + driving_exp + hh_vehs +
    mode_commute_3 + mode_shopping_3 + mode_personal_3 +
    mode_social_3 + citation_1 + crash_exp_1 +
    rec_trips + time + cost +
    veh_own_1 + veh_type_1 + veh_type_2 +
    veh_type_3 + veh_feature_1 + veh_feature_2 +
    veh_feature_3 + veh_feature_4 + veh_feature_5 +
    veh_feature_6 + veh_feature_7 + companion_tot +
    companion_1 + companion_2 + companion_3 +
    companion_4 + companion_5 + per_drive_3 +
    per_drive_4 + per_drive_5 + trip_exp_1 +
    trip_exp_3 + trip_exp_4 + av_usefulness +
    av_concern + tech_savviness + driving_enjoyment +
    polychronicity + envt_concern + tba_g1_hv +
    tba_g2_hv + tba_g3_hv + tba_g4_hv +
    tba_g5_hv + tba_g6_hv + tba_g7_hv +
    tba_g1_av + tba_g2_av + tba_g3_av +
    tba_g4_av + tba_g5_av + tba_g6_av +
    tba_g7_av,
  threshold.constraints = c(1:2),
  coef.constraints = cbind(
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, NA),
    c(1, NA), c(1, NA), c(1, 2),
    c(1, 2), c(1, 2), c(NA, 2),
    c(NA, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, NA),
    c(1, NA), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, NA),
    c(NA, 1), c(NA, 1), c(NA, 1),
    c(NA, 1), c(NA, 1), c(NA, 1),
    c(NA, 1)
  ),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)

# full model - part I
model26_1 <- mvord(
  formula = MMO2(tu_hv, tu_av) ~ 0 +
    age_grp_2 + age_grp_3 + gender_1 +
    education_2 + education_3 + school_2 +
    school_3 + employment_2 + employment_3 +
    race_1 + hh_adult + hh_child +
    income_grp_2 + income_grp_3 + income_grp_4 +
    income_grp_5 + driving_exp + hh_vehs +
    mode_commute_3 + mode_shopping_3 + mode_personal_3 +
    mode_social_3 + citation_1 + crash_exp_1 +
    rec_trips + time + cost,
  threshold.constraints = c(1:2),
  coef.constraints = cbind(
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2)
  ),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)

sink("./outputs/models/model26_1.txt")
print(summary(model26_1))
rm(model26_1)

# full model - part II
model26_2 <- mvord(
  formula = MMO2(tu_hv, tu_av) ~ 0 +
    veh_own_1 + veh_type_1 + veh_type_2 +
    veh_type_3 + veh_feature_1 + veh_feature_2 +
    veh_feature_3 + veh_feature_4 + veh_feature_5 +
    veh_feature_6 + veh_feature_7 + companion_tot +
    companion_1 + companion_2 + companion_3 +
    companion_4 + companion_5 + per_drive_3 +
    per_drive_4 + per_drive_5 + trip_exp_1 +
    trip_exp_3 + trip_exp_4 + av_usefulness +
    av_concern + tech_savviness + driving_enjoyment +
    polychronicity + envt_concern + tba_g1_hv +
    tba_g2_hv + tba_g3_hv + tba_g4_hv +
    tba_g5_hv + tba_g6_hv + tba_g7_hv +
    tba_g1_av + tba_g2_av + tba_g3_av +
    tba_g4_av + tba_g5_av + tba_g6_av +
    tba_g7_av,
  threshold.constraints = c(1:2),
  coef.constraints = cbind(
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(NA, 2),
    c(NA, 2), c(1, 2), c(1, 2),
    c(1, 2), c(1, 2), c(1, NA),
    c(1, NA), c(1, NA), c(1, NA),
    c(1, NA), c(1, NA), c(1, NA),
    c(NA, 1), c(NA, 1), c(NA, 1),
    c(NA, 1), c(NA, 1), c(NA, 1),
    c(NA, 1)
  ),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model26_2.txt")
print(summary(model26_2))
rm(model26_2)

# final model
model27 <- mvord(
  formula = MMO2(tu_hv, tu_av) ~ 0 +
    race_1 + driving_exp + mode_shopping_3 +
    citation_1 + cost + companion_3 +
    companion_4 + trip_exp_1 + trip_exp_3 +
    av_usefulness + tech_savviness + driving_enjoyment +
    tba_g5_hv + tba_g3_av + tba_g5_av,
  threshold.constraints = c(1:2),
  coef.constraints = cbind(
    c(1, NA), c(NA, 2), c(1, 2),
    c(1, NA), c(1, 2), c(NA, 2),
    c(NA, 2), c(1, NA), c(NA, 2),
    c(NA, 2), c(1, 2), c(1, 2),
    c(1, NA), c(NA, 2), c(NA, 2)
  ),
  link = mvprobit(),
  control = mvord.control(solver = "nlminb"),
  data = df
)
sink("./outputs/models/model27.txt")
print(summary(model27))
rm(model27)

# likelihood ratio test
ll_null <- as.numeric(logLik(model25))
ll_full <- as.numeric(logLik(model27))
df_null <- attr(logLik(model25), "df")
df_full <- attr(logLik(model27), "df")
LR_stat <- 2 * (ll_full - ll_null)
df_diff <- df_full - df_null
p_value <- pchisq(LR_stat, df_diff, lower.tail = FALSE)
round(LR_stat, 2)
round(p_value, 3)
round(df_diff, 3)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Ordered probit model of increase in TU or TTU between AV and HV (AV - HV) ##
################################################################################

# null model
model28 <- polr(
  diff_tu ~
    1,
  data = df, Hess = TRUE
)
sink("./outputs/models/model28.txt")
print(summary(model28))
closeAllConnections()
pR2(model28)

# full model
model29 <- polr(
  diff_tu ~
    age_grp_2 + age_grp_3 + gender_1 +
    education_2 + education_3 + school_2 +
    school_3 + employment_2 + employment_3 +
    race_1 + hh_adult + hh_child +
    income_grp_2 + income_grp_3 + income_grp_4 +
    income_grp_5 + driving_exp + hh_vehs +
    mode_commute_3 + mode_shopping_3 + mode_personal_3 +
    mode_social_3 + citation_1 + crash_exp_1 +
    rec_trips + time + cost +
    veh_own_1 + veh_type_1 + veh_type_2 +
    veh_type_3 + veh_feature_1 + veh_feature_2 +
    veh_feature_3 + veh_feature_4 + veh_feature_5 +
    veh_feature_6 + veh_feature_7 + companion_tot +
    companion_1 + companion_2 + companion_3 +
    companion_4 + companion_5 + per_drive_3 +
    per_drive_4 + per_drive_5 + trip_exp_1 +
    trip_exp_3 + trip_exp_4 + av_usefulness +
    av_concern + tech_savviness + driving_enjoyment +
    polychronicity + envt_concern + tba_g1_diff +
    tba_g2_diff + tba_g3_diff + tba_g4_diff +
    tba_g5_diff + tba_g6_diff + tba_g7_diff,
  data = df, Hess = TRUE, method = "probit"
)
sink("./outputs/models/model29.txt")
print(summary(model29))

# final model
model30 <- polr(
  diff_tu ~
    race_1 + citation_1 + companion_3 +
    companion_4 + trip_exp_1 + trip_exp_3 +
    av_usefulness + driving_enjoyment + tba_g2_diff,
  data = df, Hess = TRUE
)
sink("./outputs/models/model30.txt")
print(summary(model30))
closeAllConnections()
pR2(model30)

# likelihood ratio test
ll_null <- as.numeric(logLik(model28))
ll_full <- as.numeric(logLik(model30))
df_null <- attr(logLik(model28), "df")
df_full <- attr(logLik(model30), "df")
LR_stat <- 2 * (ll_full - ll_null)
df_diff <- df_full - df_null
p_value <- pchisq(LR_stat, df_diff, lower.tail = FALSE)
round(LR_stat, 2)
round(p_value, 3)
round(df_diff, 3)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
