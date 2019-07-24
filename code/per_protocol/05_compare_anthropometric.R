# Load packages and required files ----------------------------------------

library(tidyverse)
library(car)
library(effects)
source("code/per_protocol/01_tidy_data.R")

# Prepare data frames -----------------------------------------------------

ant_2nd_3rd_50_attend <- ant_50_attend %>% 
  filter(eval %in% c("2nd", "3rd"))

# Build ANCOVA data frames ------------------------------------------------

ANCOVA_body_mass_df <- ant_2nd_3rd_50_attend %>% 
  dplyr::select(ID, eval, group, body_mass) %>% 
  spread(key = eval, value = body_mass) %>% 
  dplyr::select(
    ID, group,
    body_mass_2nd = '2nd',
    body_mass_3rd = '3rd'
  )

ANCOVA_BMI_df <- ant_2nd_3rd_50_attend %>% 
  dplyr::select(ID, eval, group, BMI) %>% 
  spread(key = eval, value = BMI) %>% 
  dplyr::select(
    ID, group,
    BMI_2nd = '2nd',
    BMI_3rd = '3rd'
  )

ANCOVA_waist_circ_df <- ant_2nd_3rd_50_attend %>% 
  dplyr::select(ID, eval, group, waist_circ) %>% 
  spread(key = eval, value = waist_circ) %>% 
  dplyr::select(
    ID, group,
    waist_circ_2nd = '2nd',
    waist_circ_3rd = '3rd'
  )

ANCOVA_hip_circ_df <- ant_2nd_3rd_50_attend %>% 
  dplyr::select(ID, eval, group, hip_circ) %>% 
  spread(key = eval, value = hip_circ) %>% 
  dplyr::select(
    ID, group,
    hip_circ_2nd = '2nd',
    hip_circ_3rd = '3rd'
  )

# Compare control and exercise at 2nd eval --------------------------------

t.test(body_mass ~ group, data = ant_2nd_50_attend)
t.test(BMI ~ group, data = ant_2nd_50_attend)
t.test(waist_circ ~ group, data = ant_2nd_50_attend)
t.test(hip_circ ~ group, data = ant_2nd_50_attend)

# Compare control and exercise at 2nd and 3rd evals -----------------------

# Body mass
contrasts(ANCOVA_body_mass_df$group) <- contr.helmert(2)
ANCOVA_body_mass <- aov(body_mass_3rd ~ body_mass_2nd + group, data = ANCOVA_body_mass_df)
Anova(ANCOVA_body_mass, type = "III")

body_mass_adjusted_means <- effect("group", ANCOVA_body_mass, se = TRUE)
summary(body_mass_adjusted_means)

# BMI
contrasts(ANCOVA_BMI_df$group) <- contr.helmert(2)
ANCOVA_BMI <- aov(BMI_3rd ~ BMI_2nd + group, data = ANCOVA_BMI_df)
Anova(ANCOVA_BMI, type = "III")

BMI_adjusted_means <- effect("group", ANCOVA_BMI, se = TRUE)
summary(BMI_adjusted_means)

# Waist circunference
contrasts(ANCOVA_waist_circ_df$group) <- contr.helmert(2)
ANCOVA_waist_circ <- aov(waist_circ_3rd ~ waist_circ_2nd + group, data = ANCOVA_waist_circ_df)
Anova(ANCOVA_waist_circ, type = "III")

waist_circ_adjusted_means <- effect("group", ANCOVA_waist_circ, se = TRUE)
summary(waist_circ_adjusted_means)

# Hip circunference
contrasts(ANCOVA_hip_circ_df$group) <- contr.helmert(2)
ANCOVA_hip_circ <- aov(hip_circ_3rd ~ hip_circ_2nd + group, data = ANCOVA_hip_circ_df)
Anova(ANCOVA_hip_circ, type = "III")

hip_circ_adjusted_means <- effect("group", ANCOVA_hip_circ, se = TRUE)
summary(hip_circ_adjusted_means)

# Calculate treatment effects ---------------------------------------------

ant_treatment_effect <- plyr::join_all(
  list(ANCOVA_body_mass_df, ANCOVA_BMI_df, 
       ANCOVA_waist_circ_df, ANCOVA_hip_circ_df),
  by = c("ID", "group")
) %>% 
  as_tibble() %>% 
  mutate(
    body_mass_TE  = body_mass_3rd - body_mass_2nd,
    BMI_TE        = BMI_3rd - BMI_2nd,
    waist_circ_TE = waist_circ_3rd - waist_circ_2nd,
    hip_circ_TE   = hip_circ_3rd - hip_circ_2nd
  ) %>% 
  group_by(group) %>% 
  summarise(
    mean_TE_body_mass  = mean(body_mass_TE),
    sd_TE_body_mass    = sd(body_mass_TE),
    mean_TE_BMI        = mean(BMI_TE),
    sd_TE_BMI          = sd(BMI_TE),
    mean_TE_waist_circ = mean(waist_circ_TE, na.rm = TRUE),
    sd_TE_waist_circ   = sd(waist_circ_TE, na.rm = TRUE),
    mean_TE_hip_circ   = mean(hip_circ_TE, na.rm = TRUE),
    sd_TE_hip_circ     = sd(hip_circ_TE, na.rm = TRUE)
  )