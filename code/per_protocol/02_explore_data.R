# Load packages and required files ----------------------------------------

library(tidyverse)
source("code/per_protocol/01_tidy_data.R")

# Normality tests (PA data) -----------------------------------------------

# * Log transformation ----------------------------------------------------

whole_day_50_attend <- whole_day_50_attend %>% 
  mutate(
    log10_avg_MPA   = log10(avg_MPA),
    log10_avg_VPA   = log10(avg_VPA + 1),
    log10_avg_MVPA  = log10(avg_MVPA),
    log10_avg_steps = log10(avg_steps)
  ) %>% 
  dplyr::select(
    ID, eval, group, body_mass, age, sex,
    SED, LPA, MPA, VPA, steps,
    avg_SED, avg_LPA, avg_MPA, avg_VPA, avg_MVPA, avg_steps,
    log10_avg_MPA, log10_avg_VPA, log10_avg_MVPA, log10_avg_steps,
    n_valid_days, n_days
  )

# ** 1st eval, both groups ------------------------------------------------

shapiro.test(whole_day_50_attend$avg_SED[which(whole_day_50_attend$eval == "1st")]) # normal
shapiro.test(whole_day_50_attend$avg_LPA[which(whole_day_50_attend$eval == "1st")]) # normal
shapiro.test(whole_day_50_attend$avg_MPA[which(whole_day_50_attend$eval == "1st")]) # not normal
shapiro.test(whole_day_50_attend$avg_VPA[which(whole_day_50_attend$eval == "1st")]) # not normal
shapiro.test(whole_day_50_attend$avg_MVPA[which(whole_day_50_attend$eval == "1st")]) # not normal
shapiro.test(whole_day_50_attend$avg_steps[which(whole_day_50_attend$eval == "1st")]) # normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(whole_day_50_attend$log10_avg_MPA[which(whole_day_50_attend$eval == "1st")]) # normal
shapiro.test(whole_day_50_attend$log10_avg_VPA[which(whole_day_50_attend$eval == "1st")]) # not normal
shapiro.test(whole_day_50_attend$log10_avg_MVPA[which(whole_day_50_attend$eval == "1st")]) # normal

# ** 2nd eval, both groups ------------------------------------------------

shapiro.test(whole_day_50_attend$avg_SED[which(whole_day_50_attend$eval == "2nd")]) # normal
shapiro.test(whole_day_50_attend$avg_LPA[which(whole_day_50_attend$eval == "2nd")]) # normal
shapiro.test(whole_day_50_attend$avg_MPA[which(whole_day_50_attend$eval == "2nd")]) # not normal
shapiro.test(whole_day_50_attend$avg_VPA[which(whole_day_50_attend$eval == "2nd")]) # not normal
shapiro.test(whole_day_50_attend$avg_MVPA[which(whole_day_50_attend$eval == "2nd")]) # not normal
shapiro.test(whole_day_50_attend$avg_steps[which(whole_day_50_attend$eval == "2nd")]) # not normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(whole_day_50_attend$log10_avg_MPA[which(whole_day_50_attend$eval == "2nd")]) # normal
shapiro.test(whole_day_50_attend$log10_avg_VPA[which(whole_day_50_attend$eval == "2nd")]) # not normal
shapiro.test(whole_day_50_attend$log10_avg_MVPA[which(whole_day_50_attend$eval == "2nd")]) # normal
shapiro.test(whole_day_50_attend$log10_avg_steps[which(whole_day_50_attend$eval == "2nd")]) # normal

# ** 2nd eval, control group ----------------------------------------------

shapiro.test(
  whole_day_50_attend$avg_SED[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # normal
shapiro.test(
  whole_day_50_attend$avg_LPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # normal
shapiro.test(
  whole_day_50_attend$avg_MPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # not normal
shapiro.test(
  whole_day_50_attend$avg_VPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # not normal
shapiro.test(
  whole_day_50_attend$avg_MVPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # not normal
shapiro.test(
  whole_day_50_attend$avg_steps[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(
  whole_day_50_attend$log10_avg_MPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # normal
shapiro.test(
  whole_day_50_attend$log10_avg_VPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # not normal
shapiro.test(
  whole_day_50_attend$log10_avg_MVPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # normal

# ** 2nd eval, exercise group ---------------------------------------------

shapiro.test(
  whole_day_50_attend$avg_SED[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_LPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_MPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_VPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "exercise")]) # not normal
shapiro.test(
  whole_day_50_attend$avg_MVPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_steps[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "exercise")]) # normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(
  whole_day_50_attend$log10_avg_VPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "exercise")]) # not normal

# ** 3rd eval, control group ----------------------------------------------

shapiro.test(
  whole_day_50_attend$avg_SED[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "control")]) # normal
shapiro.test(
  whole_day_50_attend$avg_LPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "control")]) # normal
shapiro.test(
  whole_day_50_attend$avg_MPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "control")]) # normal
shapiro.test(
  whole_day_50_attend$avg_VPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "control")]) # not normal
shapiro.test(
  whole_day_50_attend$avg_MVPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "control")]) # normal
shapiro.test(
  whole_day_50_attend$avg_steps[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "control")]) # normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(
  whole_day_50_attend$log10_avg_VPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "control")]) # not normal

# ** 3rd eval, exercise group ---------------------------------------------

shapiro.test(
  whole_day_50_attend$avg_SED[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_LPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_MPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_VPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "exercise")]) # not normal
shapiro.test(
  whole_day_50_attend$avg_MVPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_steps[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "exercise")]) # normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(
  whole_day_50_attend$log10_avg_VPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "control")]) # not normal


# Normality tests (anthropometric data) -----------------------------------

# ** 1st eval, both groups ------------------------------------------------

shapiro.test(ant_1st_50_attend$age) # normal
shapiro.test(ant_1st_50_attend$height) # normal
shapiro.test(ant_1st_50_attend$body_mass) # normal
shapiro.test(ant_1st_50_attend$BMI) # normal
shapiro.test(ant_1st_50_attend$waist_circ) # normal
shapiro.test(ant_1st_50_attend$hip_circ) # normal

# ** 2nd eval, both groups ------------------------------------------------

shapiro.test(ant_2nd_50_attend$age) # normal
shapiro.test(ant_2nd_50_attend$height) # normal
shapiro.test(ant_2nd_50_attend$body_mass) # normal
shapiro.test(ant_2nd_50_attend$BMI) # normal
shapiro.test(ant_2nd_50_attend$waist_circ) # normal
shapiro.test(ant_2nd_50_attend$hip_circ) # normal

# ** 2nd eval, control group ----------------------------------------------

shapiro.test(ant_2nd_50_attend$age[which(ant_2nd_50_attend$group == "control")]) # normal
shapiro.test(ant_2nd_50_attend$height[which(ant_2nd_50_attend$group == "control")]) # normal
shapiro.test(ant_2nd_50_attend$body_mass[which(ant_2nd_50_attend$group == "control")]) # normal
shapiro.test(ant_2nd_50_attend$BMI[which(ant_2nd_50_attend$group == "control")]) # normal
shapiro.test(ant_2nd_50_attend$waist_circ[which(ant_2nd_50_attend$group == "control")]) # normal
shapiro.test(ant_2nd_50_attend$hip_circ[which(ant_2nd_50_attend$group == "control")]) # normal

# ** 2nd eval, exercise group ---------------------------------------------

shapiro.test(ant_2nd_50_attend$age[which(ant_2nd_50_attend$group == "exercise")]) # normal
shapiro.test(ant_2nd_50_attend$height[which(ant_2nd_50_attend$group == "exercise")]) # normal
shapiro.test(ant_2nd_50_attend$body_mass[which(ant_2nd_50_attend$group == "exercise")]) # normal
shapiro.test(ant_2nd_50_attend$BMI[which(ant_2nd_50_attend$group == "exercise")]) # normal
shapiro.test(ant_2nd_50_attend$waist_circ[which(ant_2nd_50_attend$group == "exercise")]) # normal
shapiro.test(ant_2nd_50_attend$hip_circ[which(ant_2nd_50_attend$group == "exercise")]) # normal

# ** 3rd eval, control group ----------------------------------------------

shapiro.test(ant_3rd_50_attend$age[which(ant_3rd_50_attend$group == "control")]) # normal
shapiro.test(ant_3rd_50_attend$height[which(ant_3rd_50_attend$group == "control")]) # normal
shapiro.test(ant_3rd_50_attend$body_mass[which(ant_3rd_50_attend$group == "control")]) # normal
shapiro.test(ant_3rd_50_attend$BMI[which(ant_3rd_50_attend$group == "control")]) # normal
shapiro.test(ant_3rd_50_attend$waist_circ[which(ant_3rd_50_attend$group == "control")]) # normal
shapiro.test(ant_3rd_50_attend$hip_circ[which(ant_3rd_50_attend$group == "control")]) # normal

# ** 3rd eval, exercise group ---------------------------------------------

shapiro.test(ant_3rd_50_attend$age[which(ant_3rd_50_attend$group == "exercise")]) # normal
shapiro.test(ant_3rd_50_attend$height[which(ant_3rd_50_attend$group == "exercise")]) # normal
shapiro.test(ant_3rd_50_attend$body_mass[which(ant_3rd_50_attend$group == "exercise")]) # normal
shapiro.test(ant_3rd_50_attend$BMI[which(ant_3rd_50_attend$group == "exercise")]) # normal
shapiro.test(ant_3rd_50_attend$waist_circ[which(ant_3rd_50_attend$group == "exercise")]) # normal
shapiro.test(ant_3rd_50_attend$hip_circ[which(ant_3rd_50_attend$group == "exercise")]) # normal