# Load packages and required files ----------------------------------------

library(tidyverse)
library(ez)
library(multcomp)
source("code/intention_to_treat/02_explore_data.R")

# Prepare data frames -----------------------------------------------------

whole_day_1st_2nd <- whole_day %>% 
  filter(eval %in% c("1st", "2nd"))

whole_day_2nd <- whole_day %>% 
  filter(eval == "2nd")

whole_day_2nd_3rd <- whole_day %>% 
  filter(eval %in% c("2nd", "3rd"))

whole_day_2nd_3rd_control <- whole_day_2nd_3rd %>% 
  filter(group == "control")

whole_day_2nd_3rd_exercise <- whole_day_2nd_3rd %>% 
  filter(group == "exercise")

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
ANOVA_SED <- ezANOVA(
  data = whole_day_2nd_3rd,
  dv = .(avg_SED),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_SED <- pairwise.t.test(
  whole_day_2nd_3rd_control$avg_SED, whole_day_2nd_3rd_control$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_SED <- pairwise.t.test(
  whole_day_2nd_3rd_exercise$avg_SED, whole_day_2nd_3rd_exercise$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

# LPA
ANOVA_LPA <- ezANOVA(
  data = whole_day_2nd_3rd,
  dv = .(avg_LPA),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_LPA <- pairwise.t.test(
  whole_day_2nd_3rd_control$avg_LPA, whole_day_2nd_3rd_control$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_LPA <- pairwise.t.test(
  whole_day_2nd_3rd_exercise$avg_LPA, whole_day_2nd_3rd_exercise$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

# MVPA
ANOVA_MVPA <- ezANOVA(
  data = whole_day_2nd_3rd,
  dv = .(log10_avg_MVPA),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_MVPA <- pairwise.t.test(
  whole_day_2nd_3rd_control$log10_avg_MVPA, whole_day_2nd_3rd_control$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_MVPA <- pairwise.t.test(
  whole_day_2nd_3rd_exercise$log10_avg_MVPA, whole_day_2nd_3rd_exercise$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

# Steps
ANOVA_steps <- ezANOVA(
  data = whole_day_2nd_3rd,
  dv = .(log10_avg_steps),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_steps <- pairwise.t.test(
  whole_day_2nd_3rd_control$log10_avg_steps, whole_day_2nd_3rd_control$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_steps <- pairwise.t.test(
  whole_day_2nd_3rd_exercise$log10_avg_steps, whole_day_2nd_3rd_exercise$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)