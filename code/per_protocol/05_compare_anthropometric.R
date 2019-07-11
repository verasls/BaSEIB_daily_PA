# Load packages and required files ----------------------------------------

library(tidyverse)
library(ez)
library(multcomp)
source("code/per_protocol/01_tidy_data.R")

# Prepare data frames -----------------------------------------------------

ant_2nd_3rd_50_attend <- ant_50_attend %>% 
  filter(eval %in% c("2nd", "3rd"))

ant_2nd_3rd_control_50_attend <- ant_2nd_3rd_50_attend %>% 
  filter(group == "control")

ant_2nd_3rd_exercise_50_attend <- ant_2nd_3rd_50_attend %>% 
  filter(group == "exercise")

# Compare control and exercise at 2nd eval --------------------------------

t.test(body_mass ~ group, data = ant_2nd_50_attend)
t.test(BMI ~ group, data = ant_2nd_50_attend)
t.test(waist_circ ~ group, data = ant_2nd_50_attend)
t.test(hip_circ ~ group, data = ant_2nd_50_attend)

# Compare control and exercise at 2nd and 3rd evals -----------------------

# Body mass
ANOVA_body_mass <- ezANOVA(
  data = ant_2nd_3rd_50_attend,
  dv = .(body_mass),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_body_mass <- pairwise.t.test(
  ant_2nd_3rd_control_50_attend$body_mass, ant_2nd_3rd_control_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_body_mass <- pairwise.t.test(
  ant_2nd_3rd_exercise_50_attend$body_mass, ant_2nd_3rd_exercise_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

# BMI
ANOVA_BMI <- ezANOVA(
  data = ant_2nd_3rd_50_attend,
  dv = .(BMI),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_BMI <- pairwise.t.test(
  ant_2nd_3rd_control_50_attend$BMI, ant_2nd_3rd_control_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_BMI <- pairwise.t.test(
  ant_2nd_3rd_exercise_50_attend$BMI, ant_2nd_3rd_exercise_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

# Waist circunference
ant_2nd_3rd_no_NA_50_attend <- ant_2nd_3rd_50_attend %>% filter(ID != 41)
ant_2nd_3rd_no_NA_control_50_attend <- ant_2nd_3rd_no_NA_50_attend %>% filter(group == "control")
ant_2nd_3rd_no_NA_exercise_50_attend <- ant_2nd_3rd_no_NA_50_attend %>% filter(group == "exercise")
ANOVA_waist_circ <- ezANOVA(
  data = ant_2nd_3rd_no_NA_50_attend,
  dv = .(waist_circ),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3,
)

control_posthoc_waist_circ <- pairwise.t.test(
  ant_2nd_3rd_no_NA_control_50_attend$waist_circ, ant_2nd_3rd_no_NA_control_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_waist_circ <- pairwise.t.test(
  ant_2nd_3rd_no_NA_exercise_50_attend$waist_circ, ant_2nd_3rd_no_NA_exercise_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

# Hip circunference
ANOVA_hip_circ <- ezANOVA(
  data = ant_2nd_3rd_50_attend,
  dv = .(hip_circ),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_hip_circ <- pairwise.t.test(
  ant_2nd_3rd_control_50_attend$hip_circ, ant_2nd_3rd_control_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_hip_circ <- pairwise.t.test(
  ant_2nd_3rd_exercise_50_attend$hip_circ, ant_2nd_3rd_exercise_50_attend$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)