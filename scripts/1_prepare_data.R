### Load packages ##############################################################
################################################################################

library(fastDummies)
library(tidyverse)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Prepare the data for analysis ##############################################
################################################################################

# import the data
df <- readRDS("./data/data.rds")

# dummy coding of some variables
df <- dummy_cols(df,
  select_columns =
    c(
      "veh_own",
      "per_drive",
      "av_fam",
      "age_grp",
      "gender",
      "education",
      "school",
      "income_grp",
      "employment",
      "citation",
      "crash_exp",
      "mode_commute",
      "mode_shopping",
      "mode_personal",
      "mode_social"
    ),
  remove_selected_columns = TRUE
)

# difference in travel time usefulness (TU or TTU)
df$diff_tu <- factor(df$tu_av - df$tu_hv, levels = c(-4:4), labels = c(-4:4))

# grouped travel-based activities (TBAs) for HV - continuous variables
df$tba_g1_hv_c <- df$tba_hv_7
df$tba_g2_hv_c <- df$tba_hv_5 + df$tba_hv_6 + df$tba_hv_10
df$tba_g3_hv_c <- df$tba_hv_3 + df$tba_hv_4
df$tba_g4_hv_c <- df$tba_hv_2 + df$tba_hv_8 + df$tba_hv_9
df$tba_g5_hv_c <- df$tba_hv_11 + df$tba_hv_12
df$tba_g6_hv_c <- df$tba_hv_13 + df$tba_hv_14 + df$tba_hv_15
df$tba_g7_hv_c <- df$tba_hv_16

# grouped TBAs for AV - continuous variables
df$tba_g1_av_c <- df$tba_av_7
df$tba_g2_av_c <- df$tba_av_5 + df$tba_av_6 + df$tba_av_10
df$tba_g3_av_c <- df$tba_av_3 + df$tba_av_4
df$tba_g4_av_c <- df$tba_av_2 + df$tba_av_8 + df$tba_av_9
df$tba_g5_av_c <- df$tba_av_11 + df$tba_av_12
df$tba_g6_av_c <- df$tba_av_13 + df$tba_av_14 + df$tba_av_15
df$tba_g7_av_c <- df$tba_av_16

# grouped TBAs for HV - dichotomous variables
for (i in 1:7) {
  df[paste0("tba_g", i, "_hv")] <-
    ifelse(df[paste0("tba_g", i, "_hv_c")] > 0, 1, 0)
}
rm(i)

# grouped TBAs for AV - dichotomous variables
for (i in 1:7) {
  df[paste0("tba_g", i, "_av")] <-
    ifelse(df[paste0("tba_g", i, "_av_c")] > 0, 1, 0)
}
rm(i)

# difference in TBAs (AV - HV)
for (i in 1:7) {
  df[paste0("tba_g", i, "_diff")] <-
    df[paste0("tba_g", i, "_av_c")] - df[paste0("tba_g", i, "_hv_c")]
}
rm(i)

# make other variables as factor
df <- df %>%
  mutate_at(colnames(df %>%
    dplyr::select(
      veh_type_1:veh_type_6,
      veh_feature_1:veh_feature_9,
      companion_alone:companion_6,
      trip_exp_1:trip_exp_8,
      tba_hv_1:tba_hv_17,
      tba_av_1:tba_av_17,
      veh_own_1:veh_own_3,
      per_drive_1:per_drive_5,
      av_fam_1:av_fam_5,
      age_grp_1:age_grp_3,
      race_1:race_8,
      gender_1:gender_4,
      education_1:education_3,
      school_1:school_3,
      income_grp_1:income_grp_5,
      employment_1:employment_3,
      citation_1:citation_5,
      crash_exp_1:crash_exp_5,
      mode_commute_1:mode_commute_5,
      mode_shopping_1:mode_shopping_5,
      mode_personal_1:mode_personal_5,
      mode_social_1:mode_social_5,
      tu_av,
      tu_hv,
      tba_g1_hv:tba_g7_hv,
      tba_g1_av:tba_g7_av
    )), as.factor)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Export the prepared data ###################################################
################################################################################

write_rds(df, "./data/prepared_data.rds")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
