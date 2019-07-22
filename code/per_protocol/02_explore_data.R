# Load packages and required files ----------------------------------------

library(tidyverse)
source("code/per_protocol/01_tidy_data.R")

# Normality tests (PA data) -----------------------------------------------

# * Log transformation ----------------------------------------------------

whole_day_50_attend <- whole_day_50_attend %>% 
  mutate(
    log10_avg_MVPA  = log10(avg_MVPA),
    log10_avg_steps = log10(avg_steps)
  )

# ** 2nd eval, control group ----------------------------------------------

shapiro.test(
  whole_day_50_attend$avg_SED[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # normal
shapiro.test(
  whole_day_50_attend$avg_LPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # normal
shapiro.test(
  whole_day_50_attend$avg_MVPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # not normal
shapiro.test(
  whole_day_50_attend$avg_steps[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(
  whole_day_50_attend$log10_avg_MVPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "control")]) # normal

# ** 2nd eval, exercise group ---------------------------------------------

shapiro.test(
  whole_day_50_attend$avg_SED[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_LPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_MVPA[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_steps[which(whole_day_50_attend$eval == "2nd" & whole_day_50_attend$group == "exercise")]) # normal

# ** 3rd eval, control group ----------------------------------------------

shapiro.test(
  whole_day_50_attend$avg_SED[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "control")]) # normal
shapiro.test(
  whole_day_50_attend$avg_LPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "control")]) # normal
shapiro.test(
  whole_day_50_attend$avg_MVPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "control")]) # normal
shapiro.test(
  whole_day_50_attend$avg_steps[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "control")]) # normal

# ** 3rd eval, exercise group ---------------------------------------------

shapiro.test(
  whole_day_50_attend$avg_SED[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_LPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_MVPA[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "exercise")]) # normal
shapiro.test(
  whole_day_50_attend$avg_steps[which(whole_day_50_attend$eval == "3rd" & whole_day_50_attend$group == "exercise")]) # normal

# Normality tests (anthropometric data) -----------------------------------

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