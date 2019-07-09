# Load packages and required files ----------------------------------------

library(tidyverse)
library(ez)
library(multcomp)
source("code/02_explore_data.R")

# Compare 1st and 2nd evals -----------------------------------------------

t.test(avg_SED ~ eval, data = filter(whole_day, eval %in% c("1st", "2nd")), paired = TRUE)
t.test(avg_LPA ~ eval, data = filter(whole_day, eval %in% c("1st", "2nd")), paired = TRUE)
t.test(log10_avg_MPA ~ eval, data = filter(whole_day, eval %in% c("1st", "2nd")), paired = TRUE)
t.test(log10_avg_VPA ~ eval, data = filter(whole_day, eval %in% c("1st", "2nd")), paired = TRUE)
t.test(log10_avg_MVPA ~ eval, data = filter(whole_day, eval %in% c("1st", "2nd")), paired = TRUE)
t.test(log10_avg_steps ~ eval, data = filter(whole_day, eval %in% c("1st", "2nd")), paired = TRUE)

# Compare control and exercise at 2nd eval

t.test(avg_SED ~ group, data = filter(whole_day, eval == "2nd"))
t.test(avg_LPA ~ group, data = filter(whole_day, eval == "2nd"))
t.test(log10_avg_MPA ~ group, data = filter(whole_day, eval == "2nd"))
t.test(log10_avg_VPA ~ group, data = filter(whole_day, eval == "2nd"))
t.test(log10_avg_MVPA ~ group, data = filter(whole_day, eval == "2nd"))
t.test(log10_avg_steps ~ group, data = filter(whole_day, eval == "2nd"))

# Compare control and exercise at 2nd and 3rd evals

ANOVA_SED <- ezANOVA(
  data = filter(whole_day, eval %in% c("2nd", "3rd")),
  dv = .(avg_SED),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

ANOVA_LPA <- ezANOVA(
  data = filter(whole_day, eval %in% c("2nd", "3rd")),
  dv = .(avg_LPA),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

ANOVA_MPA <- ezANOVA(
  data = filter(whole_day, eval %in% c("2nd", "3rd")),
  dv = .(log10_avg_MPA),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

ANOVA_VPA <- ezANOVA(
  data = filter(whole_day, eval %in% c("2nd", "3rd")),
  dv = .(log10_avg_VPA),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

ANOVA_MVPA <- ezANOVA(
  data = filter(whole_day, eval %in% c("2nd", "3rd")),
  dv = .(log10_avg_MVPA),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)

ANOVA_steps <- ezANOVA(
  data = filter(whole_day, eval %in% c("2nd", "3rd")),
  dv = .(log10_avg_steps),
  wid = .(ID),
  within = .(eval),
  between = .(group),
  detailed = TRUE,
  type = 3
)