# Load packages and required files ----------------------------------------

library(tidyverse)
source("code/01_tidy_data.R")

# Physical activity descriptives ------------------------------------------

# ** 1st eval, both groups ------------------------------------------------

PA_desc_1st <- whole_day %>% 
  filter(eval == "1st") %>% 
  summarise(
    SED_mean     = round(mean(avg_SED), 1),
    SED_sd       = round(sd(avg_SED), 1),
    LPA_mean     = round(mean(avg_LPA), 1),
    LPA_sd       = round(sd(avg_LPA), 1),
    MPA_median   = round(median(avg_MPA), 1),
    MPA_iqr      = round(IQR(avg_MPA), 1),
    VPA_median   = round(median(avg_VPA), 1),
    VPA_iqr      = round(IQR(avg_VPA), 1),
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
    MPA_median   = round(median(avg_MPA), 1),
    MPA_iqr      = round(IQR(avg_MPA), 1),
    VPA_median   = round(median(avg_VPA), 1),
    VPA_iqr      = round(IQR(avg_VPA), 1),
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
    MPA_median   = round(median(avg_MPA), 1),
    MPA_iqr      = round(IQR(avg_MPA), 1),
    VPA_median   = round(median(avg_VPA), 1),
    VPA_iqr      = round(IQR(avg_VPA), 1),
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
    MPA_median   = round(median(avg_MPA), 1),
    MPA_iqr      = round(IQR(avg_MPA), 1),
    VPA_median   = round(median(avg_VPA), 1),
    VPA_iqr      = round(IQR(avg_VPA), 1),
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
    MPA_median   = round(median(avg_MPA), 1),
    MPA_iqr      = round(IQR(avg_MPA), 1),
    VPA_median   = round(median(avg_VPA), 1),
    VPA_iqr      = round(IQR(avg_VPA), 1),
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
    MPA_median   = round(median(avg_MPA), 1),
    MPA_iqr      = round(IQR(avg_MPA), 1),
    VPA_median   = round(median(avg_VPA), 1),
    VPA_iqr      = round(IQR(avg_VPA), 1),
    MVPA_median  = round(median(avg_MVPA), 1),
    MVPA_iqr     = round(IQR(avg_MVPA), 1),
    steps_median = round(median(avg_steps), 1),
    steps_iqr    = round(IQR(avg_steps), 1)
  )