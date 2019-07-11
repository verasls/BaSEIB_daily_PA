# Load packages and required files ----------------------------------------

library(tidyverse)
library(ez)
library(multcomp)
source("code/intention_to_treat/01_tidy_data.R")

# Prepare data frames -----------------------------------------------------

ant_1st_2nd <- ant %>% 
  filter(eval %in% c("1st", "2nd"))

ant_2nd_3rd <- ant %>% 
  filter(eval %in% c("2nd", "3rd"))

ant_2nd_3rd_control <- ant_2nd_3rd %>% 
  filter(group == "control")

ant_2nd_3rd_exercise <- ant_2nd_3rd %>% 
  filter(group == "exercise")

# Compare 1st and 2nd evals -----------------------------------------------

t.test(body_mass ~ eval, data = ant_1st_2nd, paired = TRUE)
t.test(BMI ~ eval, data = ant_1st_2nd, paired = TRUE)
t.test(waist_circ ~ eval, data = ant_1st_2nd, paired = TRUE)
t.test(hip_circ ~ eval, data = ant_1st_2nd, paired = TRUE)

# Compare control and exercise at 2nd eval --------------------------------

t.test(body_mass ~ group, data = ant_2nd)
t.test(BMI ~ group, data = ant_2nd)
t.test(waist_circ ~ group, data = ant_2nd)
t.test(hip_circ ~ group, data = ant_2nd)

# Compare control and exercise at 2nd and 3rd evals -----------------------

# Body mass
ANOVA_body_mass <- ezANOVA(
  data = ant_2nd_3rd,
  dv = .(body_mass),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_body_mass <- pairwise.t.test(
  ant_2nd_3rd_control$body_mass, ant_2nd_3rd_control$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_body_mass <- pairwise.t.test(
  ant_2nd_3rd_exercise$body_mass, ant_2nd_3rd_exercise$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

# BMI
ANOVA_BMI <- ezANOVA(
  data = ant_2nd_3rd,
  dv = .(BMI),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_BMI <- pairwise.t.test(
  ant_2nd_3rd_control$BMI, ant_2nd_3rd_control$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_BMI <- pairwise.t.test(
  ant_2nd_3rd_exercise$BMI, ant_2nd_3rd_exercise$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

# Waist circunference
ant_2nd_3rd_no_NA <- ant_2nd_3rd %>% filter(ID != 41)
ant_2nd_3rd_no_NA_control <- ant_2nd_3rd_no_NA %>% filter(group == "control")
ant_2nd_3rd_no_NA_exercise <- ant_2nd_3rd_no_NA %>% filter(group == "exercise")
ANOVA_waist_circ <- ezANOVA(
  data = ant_2nd_3rd_no_NA,
  dv = .(waist_circ),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3,
)

control_posthoc_waist_circ <- pairwise.t.test(
  ant_2nd_3rd_no_NA_control$waist_circ, ant_2nd_3rd_no_NA_control$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_waist_circ <- pairwise.t.test(
  ant_2nd_3rd_no_NA_exercise$waist_circ, ant_2nd_3rd_no_NA_exercise$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

# Hip circunference
ANOVA_hip_circ <- ezANOVA(
  data = ant_2nd_3rd,
  dv = .(hip_circ),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

control_posthoc_hip_circ <- pairwise.t.test(
  ant_2nd_3rd_control$hip_circ, ant_2nd_3rd_control$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)

exercise_posthoc_hip_circ <- pairwise.t.test(
  ant_2nd_3rd_exercise$hip_circ, ant_2nd_3rd_exercise$eval,
  paired = TRUE, p.adjust.method = "bonferroni"
)