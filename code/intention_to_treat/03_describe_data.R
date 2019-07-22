# Load packages and required files ----------------------------------------

library(tidyverse)
source("code/intention_to_treat/01_tidy_data.R")

# Physical activity descriptives ------------------------------------------

# ** 1st eval, both groups ------------------------------------------------

PA_desc_1st <- whole_day %>% 
  filter(eval == "1st") %>% 
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

# ** 2nd eval, both groups ------------------------------------------------

PA_desc_2nd <- whole_day %>% 
  filter(eval == "2nd") %>% 
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

# ** 2nd eval, control group ----------------------------------------------

PA_desc_2nd_ctr <- whole_day %>% 
  filter(eval == "2nd" & group == "control") %>% 
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

# ** 2nd eval, exercise group ---------------------------------------------

PA_desc_2nd_exe <- whole_day %>% 
  filter(eval == "2nd" & group == "exercise") %>% 
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


# ** 3rd eval, control group ----------------------------------------------

PA_desc_3rd_ctr <- whole_day %>% 
  filter(eval == "3rd" & group == "control") %>% 
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

# ** 3rd eval, exercise group ---------------------------------------------

PA_desc_3rd_exe <- whole_day %>% 
  filter(eval == "3rd" & group == "exercise") %>% 
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

# Anthropometric descriptives ---------------------------------------------

# ** 1st eval, both groups ------------------------------------------------

ant_desc_1st <- ant_1st %>% 
  summarise(
    age_mean = round(mean(age), 1),
    age_sd = round(sd(age), 1),
    height_mean = round(mean(height), 1),
    height_sd = round(sd(height), 1),
    body_mass_mean = round(mean(body_mass), 1),
    body_mass_sd = round(sd(body_mass), 1),
    BMI_mean = round(mean(BMI), 1),
    BMI_sd = round(sd(BMI), 1),
    waist_circ_mean = round(mean(waist_circ), 1),
    waist_circ_sd = round(sd(waist_circ), 1),
    hip_circ_mean = round(mean(hip_circ), 1),
    hip_circ_sd = round(sd(hip_circ), 1)
  )

# ** 2nd eval, both groups ------------------------------------------------

ant_desc_2nd <- ant_2nd %>% 
  summarise(
    age_mean = round(mean(age), 1),
    age_sd = round(sd(age), 1),
    height_mean = round(mean(height), 1),
    height_sd = round(sd(height), 1),
    body_mass_mean = round(mean(body_mass), 1),
    body_mass_sd = round(sd(body_mass), 1),
    BMI_mean = round(mean(BMI), 1),
    BMI_sd = round(sd(BMI), 1),
    waist_circ_mean = round(mean(waist_circ), 1),
    waist_circ_sd = round(sd(waist_circ), 1),
    hip_circ_mean = round(mean(hip_circ), 1),
    hip_circ_sd = round(sd(hip_circ), 1)
  )

# ** 2nd eval, control group ----------------------------------------------

ant_desc_2nd_ctr <- ant_2nd %>% 
  filter(group == "control") %>% 
  summarise(
    age_mean = round(mean(age), 1),
    age_sd = round(sd(age), 1),
    height_mean = round(mean(height), 1),
    height_sd = round(sd(height), 1),
    body_mass_mean = round(mean(body_mass), 1),
    body_mass_sd = round(sd(body_mass), 1),
    BMI_mean = round(mean(BMI), 1),
    BMI_sd = round(sd(BMI), 1),
    waist_circ_mean = round(mean(waist_circ), 1),
    waist_circ_sd = round(sd(waist_circ), 1),
    hip_circ_mean = round(mean(hip_circ), 1),
    hip_circ_sd = round(sd(hip_circ), 1)
  )

# ** 2nd eval, exercise group ---------------------------------------------

ant_desc_2nd_exe <- ant_2nd %>% 
  filter(group == "exercise") %>% 
  summarise(
    age_mean = round(mean(age), 1),
    age_sd = round(sd(age), 1),
    height_mean = round(mean(height), 1),
    height_sd = round(sd(height), 1),
    body_mass_mean = round(mean(body_mass), 1),
    body_mass_sd = round(sd(body_mass), 1),
    BMI_mean = round(mean(BMI), 1),
    BMI_sd = round(sd(BMI), 1),
    waist_circ_mean = round(mean(waist_circ), 1),
    waist_circ_sd = round(sd(waist_circ), 1),
    hip_circ_mean = round(mean(hip_circ), 1),
    hip_circ_sd = round(sd(hip_circ), 1)
  )

# ** 3rd eval, control group ----------------------------------------------

ant_desc_3rd_ctr <- ant_3rd %>% 
  filter(group == "control") %>% 
  summarise(
    age_mean = round(mean(age), 1),
    age_sd = round(sd(age), 1),
    height_mean = round(mean(height), 1),
    height_sd = round(sd(height), 1),
    body_mass_mean = round(mean(body_mass), 1),
    body_mass_sd = round(sd(body_mass), 1),
    BMI_mean = round(mean(BMI), 1),
    BMI_sd = round(sd(BMI), 1),
    waist_circ_mean = round(mean(waist_circ, na.rm = TRUE), 1),
    waist_circ_sd = round(sd(waist_circ, na.rm = TRUE), 1),
    hip_circ_mean = round(mean(hip_circ, na.rm = TRUE), 1),
    hip_circ_sd = round(sd(hip_circ, na.rm = TRUE), 1)
  )

# ** 3rd eval, exercise group ---------------------------------------------

ant_desc_3rd_exe <- ant_3rd %>% 
  filter(group == "exercise") %>% 
  summarise(
    age_mean = round(mean(age), 1),
    age_sd = round(sd(age), 1),
    height_mean = round(mean(height), 1),
    height_sd = round(sd(height), 1),
    body_mass_mean = round(mean(body_mass), 1),
    body_mass_sd = round(sd(body_mass), 1),
    BMI_mean = round(mean(BMI), 1),
    BMI_sd = round(sd(BMI), 1),
    waist_circ_mean = round(mean(waist_circ), 1),
    waist_circ_sd = round(sd(waist_circ), 1),
    hip_circ_mean = round(mean(hip_circ), 1),
    hip_circ_sd = round(sd(hip_circ), 1)
  )