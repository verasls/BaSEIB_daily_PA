# Load packages and required files ----------------------------------------

library(tidyverse)
library(ez)
library(multcomp)
source("code/02_explore_data.R")

# Prepare data frames -----------------------------------------------------

whole_day_1st_2nd <- whole_day %>% 
  filter(eval %in% c("1st", "2nd"))

whole_day_2nd <- whole_day %>% 
  filter(eval == "2nd")

whole_day_2nd_3rd <- whole_day %>% 
  filter(eval %in% c("2nd", "3rd"))

# Compare 1st and 2nd evals -----------------------------------------------

t.test(avg_SED ~ eval, data = whole_day_1st_2nd, paired = TRUE)
t.test(avg_LPA ~ eval, data = whole_day_1st_2nd, paired = TRUE)
t.test(log10_avg_MPA ~ eval, data = whole_day_1st_2nd, paired = TRUE)
t.test(log10_avg_VPA ~ eval, data = whole_day_1st_2nd, paired = TRUE)
t.test(log10_avg_MVPA ~ eval, data = whole_day_1st_2nd, paired = TRUE)
t.test(log10_avg_steps ~ eval, data = whole_day_1st_2nd, paired = TRUE)

# Compare control and exercise at 2nd eval

t.test(avg_SED ~ group, data = whole_day_2nd)
t.test(avg_LPA ~ group, data = whole_day_2nd)
t.test(log10_avg_MPA ~ group, data = whole_day_2nd)
t.test(log10_avg_VPA ~ group, data = whole_day_2nd)
t.test(log10_avg_MVPA ~ group, data = whole_day_2nd)
t.test(log10_avg_steps ~ group, data = whole_day_2nd)

# Compare control and exercise at 2nd and 3rd evals

ANOVA_SED <- ezANOVA(
  data = whole_day_2nd_3rd,
  dv = .(avg_SED),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

ANOVA_LPA <- ezANOVA(
  data = whole_day_2nd_3rd,
  dv = .(avg_LPA),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

ANOVA_MPA <- ezANOVA(
  data = whole_day_2nd_3rd,
  dv = .(log10_avg_MPA),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

ANOVA_VPA <- ezANOVA(
  data = whole_day_2nd_3rd,
  dv = .(log10_avg_VPA),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

ANOVA_MVPA <- ezANOVA(
  data = whole_day_2nd_3rd,
  dv = .(log10_avg_MVPA),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

ANOVA_steps <- ezANOVA(
  data = whole_day_2nd_3rd,
  dv = .(log10_avg_steps),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)