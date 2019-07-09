# Load packages and required files ----------------------------------------

library(tidyverse)
source("code/01_tidy_data.R")

# Plots -------------------------------------------------------------------

# ** 1st eval, both groups ------------------------------------------------

hist_SED_1st <- ggplot(data = filter(whole_day, eval == "1st"), aes(avg_SED)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 10)

hist_LPA_1st <- ggplot(data = filter(whole_day, eval == "1st"), aes(avg_LPA)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 10)

hist_MPA_1st <- ggplot(data = filter(whole_day, eval == "1st"), aes(avg_MPA)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_VPA_1st <- ggplot(data = filter(whole_day, eval == "1st"), aes(avg_VPA)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 0.1)

hist_MVPA_1st <- ggplot(data = filter(whole_day, eval == "1st"), aes(avg_MVPA)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_steps_1st <- ggplot(data = filter(whole_day, eval == "1st"), aes(avg_steps)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 500)

# ** 2nd eval, both groups ------------------------------------------------

hist_SED_2nd <- ggplot(data = filter(whole_day, eval == "2nd"), aes(avg_SED)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 10)

hist_LPA_2nd <- ggplot(data = filter(whole_day, eval == "2nd"), aes(avg_LPA)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 10)

hist_MPA_2nd <- ggplot(data = filter(whole_day, eval == "2nd"), aes(avg_MPA)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_VPA_2nd <- ggplot(data = filter(whole_day, eval == "2nd"), aes(avg_VPA)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 0.1)

hist_MVPA_2nd <- ggplot(data = filter(whole_day, eval == "2nd"), aes(avg_MVPA)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_steps_2nd <- ggplot(data = filter(whole_day, eval == "2nd"), aes(avg_steps)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 500)

# ** 2nd eval, control group ----------------------------------------------

hist_SED_2nd_ctr <- ggplot(
  data = filter(whole_day, eval == "2nd" & group == "control"), aes(avg_SED)
  ) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_LPA_2nd_ctr <- ggplot(
  data = filter(whole_day, eval == "2nd" & group == "control"), aes(avg_LPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_MPA_2nd_ctr <- ggplot(
  data = filter(whole_day, eval == "2nd" & group == "control"), aes(avg_MPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_VPA_2nd_ctr <- ggplot(
  data = filter(whole_day, eval == "2nd" & group == "control"), aes(avg_VPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 1)

hist_MVPA_2nd_ctr <- ggplot(
  data = filter(whole_day, eval == "2nd" & group == "control"), aes(avg_MVPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 10)

hist_steps_2nd_ctr <- ggplot(
  data = filter(whole_day, eval == "2nd" & group == "control"), aes(avg_steps)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 500)

# ** 2nd eval, exercise group ---------------------------------------------

hist_SED_2nd_exe <- ggplot(
  data = filter(whole_day, eval == "2nd" & group == "exercise"), aes(avg_SED)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_LPA_2nd_exe <- ggplot(
  data = filter(whole_day, eval == "2nd" & group == "exercise"), aes(avg_LPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_MPA_2nd_exe <- ggplot(
  data = filter(whole_day, eval == "2nd" & group == "exercise"), aes(avg_MPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_VPA_2nd_exe <- ggplot(
  data = filter(whole_day, eval == "2nd" & group == "exercise"), aes(avg_VPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 1)

hist_MVPA_2nd_exe <- ggplot(
  data = filter(whole_day, eval == "2nd" & group == "exercise"), aes(avg_MVPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 10)

hist_steps_2nd_exe <- ggplot(
  data = filter(whole_day, eval == "2nd" & group == "exercise"), aes(avg_steps)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 500)

# ** 3rd eval, control group ----------------------------------------------

hist_SED_3rd_ctr <- ggplot(
  data = filter(whole_day, eval == "3rd" & group == "control"), aes(avg_SED)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_LPA_3rd_ctr <- ggplot(
  data = filter(whole_day, eval == "3rd" & group == "control"), aes(avg_LPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_MPA_3rd_ctr <- ggplot(
  data = filter(whole_day, eval == "3rd" & group == "control"), aes(avg_MPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_VPA_3rd_ctr <- ggplot(
  data = filter(whole_day, eval == "3rd" & group == "control"), aes(avg_VPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 1)

hist_MVPA_3rd_ctr <- ggplot(
  data = filter(whole_day, eval == "3rd" & group == "control"), aes(avg_MVPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_steps_3rd_ctr <- ggplot(
  data = filter(whole_day, eval == "3rd" & group == "control"), aes(avg_steps)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 500)

# ** 3rd eval, exercise group ---------------------------------------------

hist_SED_3rd_exe <- ggplot(
  data = filter(whole_day, eval == "3rd" & group == "exercise"), aes(avg_SED)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_LPA_3rd_exe <- ggplot(
  data = filter(whole_day, eval == "3rd" & group == "exercise"), aes(avg_LPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_MPA_3rd_exe <- ggplot(
  data = filter(whole_day, eval == "3rd" & group == "exercise"), aes(avg_MPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 5)

hist_VPA_3rd_exe <- ggplot(
  data = filter(whole_day, eval == "3rd" & group == "exercise"), aes(avg_VPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 1)

hist_MVPA_3rd_exe <- ggplot(
  data = filter(whole_day, eval == "3rd" & group == "exercise"), aes(avg_MVPA)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 10)

hist_steps_3rd_exe <- ggplot(
  data = filter(whole_day, eval == "3rd" & group == "exercise"), aes(avg_steps)
) +
  geom_histogram(colour = "black", fill = "white", binwidth = 500)

# Normality tests ---------------------------------------------------------

# * Log transformation ----------------------------------------------------

whole_day <- whole_day %>% 
  mutate(
    log10_avg_MPA   = log10(avg_MPA),
    log10_avg_VPA   = log10(avg_VPA + 1),
    log10_avg_MVPA  = log10(avg_MVPA),
    log10_avg_steps = log10(avg_steps)
  ) %>% 
  select(
    ID, eval, group, body_mass, age, sex,
    SED, LPA, MPA, VPA, steps,
    avg_SED, avg_LPA, avg_MPA, avg_VPA, avg_MVPA, avg_steps,
    log10_avg_MPA, log10_avg_VPA, log10_avg_MVPA, log10_avg_steps,
    n_valid_days, n_days
  )

# ** 1st eval, both groups ------------------------------------------------

shapiro.test(whole_day$avg_SED[which(whole_day$eval == "1st")]) # normal
shapiro.test(whole_day$avg_LPA[which(whole_day$eval == "1st")]) # normal
shapiro.test(whole_day$avg_MPA[which(whole_day$eval == "1st")]) # not normal
shapiro.test(whole_day$avg_VPA[which(whole_day$eval == "1st")]) # not normal
shapiro.test(whole_day$avg_MVPA[which(whole_day$eval == "1st")]) # not normal
shapiro.test(whole_day$avg_steps[which(whole_day$eval == "1st")]) # normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(whole_day$log10_avg_MPA[which(whole_day$eval == "1st")]) # normal
shapiro.test(whole_day$log10_avg_VPA[which(whole_day$eval == "1st")]) # not normal
shapiro.test(whole_day$log10_avg_MVPA[which(whole_day$eval == "1st")]) # normal

# ** 2nd eval, both groups ------------------------------------------------

shapiro.test(whole_day$avg_SED[which(whole_day$eval == "2nd")]) # normal
shapiro.test(whole_day$avg_LPA[which(whole_day$eval == "2nd")]) # normal
shapiro.test(whole_day$avg_MPA[which(whole_day$eval == "2nd")]) # not normal
shapiro.test(whole_day$avg_VPA[which(whole_day$eval == "2nd")]) # not normal
shapiro.test(whole_day$avg_MVPA[which(whole_day$eval == "2nd")]) # not normal
shapiro.test(whole_day$avg_steps[which(whole_day$eval == "2nd")]) # not normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(whole_day$log10_avg_MPA[which(whole_day$eval == "2nd")]) # normal
shapiro.test(whole_day$log10_avg_VPA[which(whole_day$eval == "2nd")]) # not normal
shapiro.test(whole_day$log10_avg_MVPA[which(whole_day$eval == "2nd")]) # normal
shapiro.test(whole_day$log10_avg_steps[which(whole_day$eval == "2nd")]) # normal

# ** 2nd eval, control group ----------------------------------------------

shapiro.test(
  whole_day$avg_SED[which(whole_day$eval == "2nd" & whole_day$group == "control")]) # normal
shapiro.test(
  whole_day$avg_LPA[which(whole_day$eval == "2nd" & whole_day$group == "control")]) # normal
shapiro.test(
  whole_day$avg_MPA[which(whole_day$eval == "2nd" & whole_day$group == "control")]) # not normal
shapiro.test(
  whole_day$avg_VPA[which(whole_day$eval == "2nd" & whole_day$group == "control")]) # not normal
shapiro.test(
  whole_day$avg_MVPA[which(whole_day$eval == "2nd" & whole_day$group == "control")]) # not normal
shapiro.test(
  whole_day$avg_steps[which(whole_day$eval == "2nd" & whole_day$group == "control")]) # normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(
  whole_day$log10_avg_MPA[which(whole_day$eval == "2nd" & whole_day$group == "control")]) # normal
shapiro.test(
  whole_day$log10_avg_VPA[which(whole_day$eval == "2nd" & whole_day$group == "control")]) # not normal
shapiro.test(
  whole_day$log10_avg_MVPA[which(whole_day$eval == "2nd" & whole_day$group == "control")]) # normal

# ** 2nd eval, exercise group ---------------------------------------------

shapiro.test(
  whole_day$avg_SED[which(whole_day$eval == "2nd" & whole_day$group == "exercise")]) # normal
shapiro.test(
  whole_day$avg_LPA[which(whole_day$eval == "2nd" & whole_day$group == "exercise")]) # normal
shapiro.test(
  whole_day$avg_MPA[which(whole_day$eval == "2nd" & whole_day$group == "exercise")]) # not normal
shapiro.test(
  whole_day$avg_VPA[which(whole_day$eval == "2nd" & whole_day$group == "exercise")]) # not normal
shapiro.test(
  whole_day$avg_MVPA[which(whole_day$eval == "2nd" & whole_day$group == "exercise")]) # not normal
shapiro.test(
  whole_day$avg_steps[which(whole_day$eval == "2nd" & whole_day$group == "exercise")]) # not normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(
  whole_day$log10_avg_MPA[which(whole_day$eval == "2nd" & whole_day$group == "exercise")]) # normal
shapiro.test(
  whole_day$log10_avg_VPA[which(whole_day$eval == "2nd" & whole_day$group == "exercise")]) # not normal
shapiro.test(
  whole_day$log10_avg_MVPA[which(whole_day$eval == "2nd" & whole_day$group == "exercise")]) # normal
shapiro.test(
  whole_day$log10_avg_steps[which(whole_day$eval == "2nd" & whole_day$group == "exercise")]) # normal

# ** 3rd eval, control group ----------------------------------------------

shapiro.test(
  whole_day$avg_SED[which(whole_day$eval == "3rd" & whole_day$group == "control")]) # normal
shapiro.test(
  whole_day$avg_LPA[which(whole_day$eval == "3rd" & whole_day$group == "control")]) # normal
shapiro.test(
  whole_day$avg_MPA[which(whole_day$eval == "3rd" & whole_day$group == "control")]) # normal
shapiro.test(
  whole_day$avg_VPA[which(whole_day$eval == "3rd" & whole_day$group == "control")]) # not normal
shapiro.test(
  whole_day$avg_MVPA[which(whole_day$eval == "3rd" & whole_day$group == "control")]) # normal
shapiro.test(
  whole_day$avg_steps[which(whole_day$eval == "3rd" & whole_day$group == "control")]) # normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(
  whole_day$log10_avg_VPA[which(whole_day$eval == "3rd" & whole_day$group == "control")]) # not normal

# ** 3rd eval, exercise group ---------------------------------------------

shapiro.test(
  whole_day$avg_SED[which(whole_day$eval == "3rd" & whole_day$group == "exercise")]) # normal
shapiro.test(
  whole_day$avg_LPA[which(whole_day$eval == "3rd" & whole_day$group == "exercise")]) # normal
shapiro.test(
  whole_day$avg_MPA[which(whole_day$eval == "3rd" & whole_day$group == "exercise")]) # not normal
shapiro.test(
  whole_day$avg_VPA[which(whole_day$eval == "3rd" & whole_day$group == "exercise")]) # not normal
shapiro.test(
  whole_day$avg_MVPA[which(whole_day$eval == "3rd" & whole_day$group == "exercise")]) # not normal
shapiro.test(
  whole_day$avg_steps[which(whole_day$eval == "3rd" & whole_day$group == "exercise")]) # normal

# Normality test with log10 transformed variable where data is not normal
shapiro.test(
  whole_day$log10_avg_MPA[which(whole_day$eval == "3rd" & whole_day$group == "control")]) # normal
shapiro.test(
  whole_day$log10_avg_VPA[which(whole_day$eval == "3rd" & whole_day$group == "control")]) # not normal
shapiro.test(
  whole_day$log10_avg_MVPA[which(whole_day$eval == "3rd" & whole_day$group == "control")]) # normal