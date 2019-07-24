# Load packages and required files ----------------------------------------

library(tidyverse)
library(car)
library(effects)
source("code/intention_to_treat/02_explore_data.R")

# Prepare data frames -----------------------------------------------------

whole_day_1st_2nd <- whole_day %>% 
  filter(eval %in% c("1st", "2nd"))

whole_day_2nd <- whole_day %>% 
  filter(eval == "2nd")

whole_day_2nd_3rd <- whole_day %>% 
  filter(eval %in% c("2nd", "3rd"))

# Build ANCOVA data frames ------------------------------------------------

ANCOVA_SED_df <- whole_day_2nd_3rd %>% 
  dplyr::select(ID, eval, group, avg_SED) %>% 
  spread(key = eval, value = avg_SED) %>% 
  dplyr::select(
    ID, group,
    SED_2nd = '2nd',
    SED_3rd = '3rd'
  )

ANCOVA_LPA_df <- whole_day_2nd_3rd %>% 
  dplyr::select(ID, eval, group, avg_LPA) %>% 
  spread(key = eval, value = avg_LPA) %>% 
  dplyr::select(
    ID, group,
    LPA_2nd = '2nd',
    LPA_3rd = '3rd'
  )

ANCOVA_MVPA_df <- whole_day_2nd_3rd %>% 
  dplyr::select(ID, eval, group, log10_avg_MVPA) %>% 
  spread(key = eval, value = log10_avg_MVPA) %>% 
  dplyr::select(
    ID, group,
    MVPA_2nd = '2nd',
    MVPA_3rd = '3rd'
  )

ANCOVA_steps_df <- whole_day_2nd_3rd %>% 
  dplyr::select(ID, eval, group, log10_avg_steps) %>% 
  spread(key = eval, value = log10_avg_steps) %>% 
  dplyr::select(
    ID, group,
    steps_2nd = '2nd',
    steps_3rd = '3rd'
  )

# Compare 1st and 2nd evals -----------------------------------------------

t.test(avg_SED ~ eval, data = whole_day_1st_2nd, paired = TRUE)
t.test(avg_LPA ~ eval, data = whole_day_1st_2nd, paired = TRUE)
t.test(log10_avg_MVPA ~ eval, data = whole_day_1st_2nd, paired = TRUE)
t.test(log10_avg_steps ~ eval, data = whole_day_1st_2nd, paired = TRUE)

# Compare control and exercise at 2nd eval --------------------------------

t.test(avg_SED ~ group, data = whole_day_2nd)
t.test(avg_LPA ~ group, data = whole_day_2nd)
t.test(log10_avg_MVPA ~ group, data = whole_day_2nd)
t.test(log10_avg_steps ~ group, data = whole_day_2nd)

# Compare control and exercise at 2nd and 3rd evals -----------------------

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

# Calculate treatment effects ---------------------------------------------

# Build data frames with non-log transformed MVPA and steps

MVPA_2nd_3rd <- whole_day_2nd_3rd %>% 
  dplyr::select(ID, eval, group, avg_MVPA) %>% 
  spread(key = eval, value = avg_MVPA) %>% 
  dplyr::select(
    ID, group,
    MVPA_2nd = '2nd', MVPA_3rd = '3rd'
  )

steps_2nd_3rd <- whole_day_2nd_3rd %>% 
  dplyr::select(ID, eval, group, avg_steps) %>% 
  spread(key = eval, value = avg_steps) %>% 
  dplyr::select(
    ID, group,
    steps_2nd = '2nd', steps_3rd = '3rd'
  )

# Calculate treatment effects

PA_treatment_effect <- plyr::join_all(
  list(ANCOVA_SED_df, ANCOVA_LPA_df, MVPA_2nd_3rd, steps_2nd_3rd),
  by = c("ID", "group")
) %>% 
  as_tibble() %>% 
  mutate(
    SED_TE   = SED_3rd - SED_2nd,
    LPA_TE   = LPA_3rd - LPA_2nd,
    MVPA_TE  = MVPA_3rd - MVPA_2nd,
    steps_TE = steps_3rd - steps_2nd
  ) %>% 
  group_by(group) %>% 
  summarise(
    mean_TE_SED     = mean(SED_TE),
    sd_TE_SED       = sd(SED_TE),
    mean_TE_LPA     = mean(LPA_TE),
    sd_TE_LPA       = sd(LPA_TE),
    median_TE_MVPA  = median(MVPA_TE),
    iqr_TE_MVPA     = IQR(MVPA_TE),
    median_TE_steps = median(steps_TE),
    iqr_TE_steps    = IQR(steps_TE)
  )