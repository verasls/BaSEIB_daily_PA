# Load packages and required files ----------------------------------------

library(tidyverse)
library(ez)
library(multcomp)
source("code/per_protocol/02_explore_data.R")

# Prepare data frames -----------------------------------------------------

whole_day_2nd_50_attend <- whole_day_50_attend %>% 
  filter(eval == "2nd")

whole_day_2nd_3rd_50_attend <- whole_day_50_attend %>% 
  filter(eval %in% c("2nd", "3rd"))

whole_day_2nd_3rd_control_50_attend <- whole_day_2nd_3rd_50_attend %>% 
  filter(group == "control")

whole_day_2nd_3rd_exercise_50_attend <- whole_day_2nd_3rd_50_attend %>% 
  filter(group == "exercise")

# Compare control and exercise at 2nd eval --------------------------------

t.test(avg_SED ~ group, data = whole_day_2nd_50_attend)
t.test(avg_LPA ~ group, data = whole_day_2nd_50_attend)
t.test(log10_avg_MVPA ~ group, data = whole_day_2nd_50_attend)
t.test(avg_steps ~ group, data = whole_day_2nd_50_attend)

# Compare control and exercise at 2nd and 3rd evals -----------------------

# SED
ANOVA_SED <- ezANOVA(
  data = whole_day_2nd_3rd_50_attend,
  dv = .(avg_SED),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_SED <- pairwise.t.test(
  whole_day_2nd_3rd_control_50_attend$avg_SED, whole_day_2nd_3rd_control_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_SED <- pairwise.t.test(
  whole_day_2nd_3rd_exercise_50_attend$avg_SED, whole_day_2nd_3rd_exercise_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

# LPA
ANOVA_LPA <- ezANOVA(
  data = whole_day_2nd_3rd_50_attend,
  dv = .(avg_LPA),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_LPA <- pairwise.t.test(
  whole_day_2nd_3rd_control_50_attend$avg_LPA, whole_day_2nd_3rd_control_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_LPA <- pairwise.t.test(
  whole_day_2nd_3rd_exercise_50_attend$avg_LPA, whole_day_2nd_3rd_exercise_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

# MVPA
ANOVA_MVPA <- ezANOVA(
  data = whole_day_2nd_3rd_50_attend,
  dv = .(log10_avg_MVPA),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_MVPA <- pairwise.t.test(
  whole_day_2nd_3rd_control_50_attend$log10_avg_MVPA, whole_day_2nd_3rd_control_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_MVPA <- pairwise.t.test(
  whole_day_2nd_3rd_exercise_50_attend$log10_avg_MVPA, whole_day_2nd_3rd_exercise_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

# Steps
ANOVA_steps <- ezANOVA(
  data = whole_day_2nd_3rd_50_attend,
  dv = .(avg_steps),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_steps <- pairwise.t.test(
  whole_day_2nd_3rd_control_50_attend$avg_steps, whole_day_2nd_3rd_control_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_steps <- pairwise.t.test(
  whole_day_2nd_3rd_exercise_50_attend$avg_steps, whole_day_2nd_3rd_exercise_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)