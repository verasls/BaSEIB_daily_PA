# Load packages and required files ----------------------------------------

library(tidyverse)
library(car)
library(effects)
source("code/per_protocol/01_tidy_data.R")
source("code/per_protocol/02_explore_data.R")

# Read and tidy PA data ---------------------------------------------------

# Read and tidy whole day PA data
whole_day_3rd_total_PA <- whole_day %>% 
  filter(eval == "3rd") %>% 
  mutate(
    SED_total = avg_SED * n_valid_days,
    LPA_total = avg_LPA * n_valid_days,
    MVPA_total = avg_MVPA * n_valid_days,
    steps_total = avg_steps * n_valid_days
  ) %>% 
  dplyr::select(
    ID, eval, group, 
    SED_total, LPA_total, MVPA_total, steps_total, n_valid_days
    )

# Read and tidy training sessions PA data
training_sessions_3rd_total_PA <- read_csv("data/troiano2008_training_sessions_filter.csv") %>% 
  dplyr::select(
    ID,
    SED_training = Sedentary,
    LPA_training = Light,
    MPA_training = Moderate,
    VPA_training = Vigorous,
    steps_training = `Steps Counts`
  ) %>% 
  mutate(MVPA_training = MPA_training + VPA_training) %>% 
  dplyr::select(ID, SED_training, LPA_training, MVPA_training, steps_training)

# Exclude training sessions PA from whole day
exclude_training_time_3rd_all_IDs <- whole_day_3rd_total_PA %>% 
  full_join(training_sessions_3rd_total_PA, by = "ID") %>% 
  mutate(
    avg_SED   = (SED_total - SED_training) / n_valid_days,
    avg_LPA   = (LPA_total - LPA_training) / n_valid_days,
    avg_MVPA  = (MVPA_total - MVPA_training) / n_valid_days,
    avg_steps = (steps_total - steps_training) / n_valid_days
  ) %>% 
  dplyr::select(
    ID, eval, group, avg_SED, avg_LPA, avg_MVPA, avg_steps
  )

# Filter IDs with selected attendance rate
exclude_training_time_3rd_control <- exclude_training_time_3rd_all_IDs %>% 
  filter(group == "control")

exclude_training_time_3rd_exercise <- exclude_training_time_3rd_all_IDs %>% 
  filter(group == "exercise" & ID %in% selected_IDs)

exclude_training_time_3rd <- rbind(
  exclude_training_time_3rd_control, exclude_training_time_3rd_exercise
  ) %>% 
  arrange(ID)

# Normality tests ---------------------------------------------------------

# Log-transformation
exclude_training_time_3rd <- exclude_training_time_3rd %>% 
  mutate(
    log10_avg_MVPA = log10(avg_MVPA)
  )

exclude_training_time_3rd_control <- exclude_training_time_3rd_control %>% 
  mutate(
    log10_avg_MVPA = log10(avg_MVPA)
  )

exclude_training_time_3rd_exercise <- exclude_training_time_3rd_exercise %>% 
  mutate(
    log10_avg_MVPA = log10(avg_MVPA)
  )

# Control group
shapiro.test(exclude_training_time_3rd_control$avg_SED) # normal
shapiro.test(exclude_training_time_3rd_control$avg_LPA) # normal
shapiro.test(exclude_training_time_3rd_control$avg_MVPA) # normal
shapiro.test(exclude_training_time_3rd_control$avg_steps) # normal

# Exercise group
shapiro.test(exclude_training_time_3rd_exercise$avg_SED) # normal
shapiro.test(exclude_training_time_3rd_exercise$avg_LPA) # normal
shapiro.test(exclude_training_time_3rd_exercise$avg_MVPA) # not normal
shapiro.test(exclude_training_time_3rd_exercise$avg_steps) # normal
# Normality test with log10 transformed variable where data is not normal
shapiro.test(exclude_training_time_3rd_exercise$log10_avg_MVPA) # normal

# Descriptives ------------------------------------------------------------

# Control group
PA_desc_exclude_training_time_3rd_ctr <- exclude_training_time_3rd_control %>% 
  summarise(
    SED_mean     = round(mean(avg_SED), 1),
    SED_sd       = round(sd(avg_SED), 1),
    LPA_mean     = round(mean(avg_LPA), 1),
    LPA_sd       = round(sd(avg_LPA), 1),
    MVPA_median  = round(median(avg_MVPA), 1),
    MVPA_iqr     = round(IQR(avg_MVPA), 1),
    steps_median = round(median(avg_steps), 1),
    steps_iqr    = round(IQR(avg_steps), 1)
  )

# Exercise group
PA_desc_exclude_training_time_3rd_exe <- exclude_training_time_3rd_exercise %>% 
  summarise(
    SED_mean     = round(mean(avg_SED), 1),
    SED_sd       = round(sd(avg_SED), 1),
    LPA_mean     = round(mean(avg_LPA), 1),
    LPA_sd       = round(sd(avg_LPA), 1),
    MVPA_median  = round(median(avg_MVPA), 1),
    MVPA_iqr     = round(IQR(avg_MVPA), 1),
    steps_median = round(median(avg_steps), 1),
    steps_iqr    = round(IQR(avg_steps), 1)
  )

# ANCOVA ------------------------------------------------------------------

# Prepare data frames
whole_day_2nd_50_attend <- whole_day_50_attend %>% 
  filter(eval == "2nd") %>% 
  dplyr::select(-c(body_mass:sex, n_valid_days, n_days))

exclude_training_time_2nd_3rd <- rbind(
  whole_day_2nd_50_attend,  exclude_training_time_3rd
) %>% 
  as_tibble()

# Build ANCOVA data frames
ANCOVA_SED_df <- exclude_training_time_2nd_3rd %>% 
  dplyr::select(ID, eval, group, avg_SED) %>% 
  spread(key = eval, value = avg_SED) %>% 
  dplyr::select(
    ID, group,
    SED_2nd = '2nd',
    SED_3rd = '3rd'
  )

ANCOVA_LPA_df <- exclude_training_time_2nd_3rd %>% 
  dplyr::select(ID, eval, group, avg_LPA) %>% 
  spread(key = eval, value = avg_LPA) %>% 
  dplyr::select(
    ID, group,
    LPA_2nd = '2nd',
    LPA_3rd = '3rd'
  )

ANCOVA_MVPA_df <- exclude_training_time_2nd_3rd %>% 
  dplyr::select(ID, eval, group, avg_MVPA) %>% 
  spread(key = eval, value = avg_MVPA) %>% 
  dplyr::select(
    ID, group,
    MVPA_2nd = '2nd',
    MVPA_3rd = '3rd'
  )

ANCOVA_steps_df <- exclude_training_time_2nd_3rd %>% 
  dplyr::select(ID, eval, group, avg_steps) %>% 
  spread(key = eval, value = avg_steps) %>% 
  dplyr::select(
    ID, group,
    steps_2nd = '2nd',
    steps_3rd = '3rd'
  )

# Compare control and exercise at 2nd and 3rd evals
# SED
contrasts(ANCOVA_SED_df$group) <- contr.helmert(2)
ANCOVA_SED <- aov(SED_3rd ~ SED_2nd + group, data = ANCOVA_SED_df)
Anova(ANCOVA_SED, type = "III")

SED_adjusted_means <- effect("group", ANCOVA_SED, se = TRUE)
summary(SED_adjusted_means)

# LPA
contrasts(ANCOVA_LPA_df$group) <- contr.helmert(2)
ANCOVA_LPA <- aov(LPA_3rd ~ LPA_2nd + group, data = ANCOVA_LPA_df)
Anova(ANCOVA_LPA, type = "III")

LPA_ajusted_means <- effect("group", ANCOVA_LPA, se = TRUE)
summary(LPA_ajusted_means)

# MVPA
contrasts(ANCOVA_MVPA_df$group) <- contr.helmert(2)
ANCOVA_MVPA <- aov(MVPA_3rd ~ MVPA_2nd + group, data = ANCOVA_MVPA_df)
Anova(ANCOVA_MVPA, type = "III")

MVPA_ajusted_means <- effect("group", ANCOVA_MVPA, se = TRUE)
summary(MVPA_ajusted_means)

# Steps
contrasts(ANCOVA_steps_df$group) <- contr.helmert(2)
ANCOVA_steps <- aov(steps_3rd ~ steps_2nd + group, data = ANCOVA_steps_df)
Anova(ANCOVA_steps, type = "III")

steps_ajusted_means <- effect("group", ANCOVA_steps, se = TRUE)
summary(steps_ajusted_means)