# Load packages -----------------------------------------------------------

library(tidyverse)

# Read and tidy data ------------------------------------------------------

whole_day <- read_csv("data/troiano2008_whole_day.csv") %>% 
  dplyr::select(
    eval = Eval,
    ID,
    body_mass = `Weight (kg)`,
    age = Age,
    sex = Gender,
    SED = Sedentary,
    LPA = Light,
    MPA = Moderate,
    VPA = Vigorous,
    steps = `Steps Counts`,
    n_valid_days = `Calendar Days`,
    n_days = `Calendar Days Unfiltered`
  ) %>% 
  filter(n_valid_days >= 4) %>% 
  mutate(
    MVPA = MPA + VPA,
    avg_SED = SED / n_valid_days,
    avg_LPA = LPA / n_valid_days,
    avg_MPA = MPA / n_valid_days,
    avg_VPA = VPA / n_valid_days,
    avg_MVPA = MVPA / n_valid_days,
    avg_steps = steps / n_valid_days
  ) %>% 
  dplyr::select(
    ID, eval, body_mass, age, sex,
    SED, LPA, MPA, VPA, steps,
    avg_SED, avg_LPA, avg_MPA, avg_VPA, avg_MVPA, avg_steps,
    n_valid_days, n_days
    )

# Rename eval values
whole_day$eval <- as.character(whole_day$eval)
for (i in 1:nrow(whole_day)) {
  if (whole_day$eval[i] == "1") {
    whole_day$eval[i] <- str_c(whole_day$eval[i], "st")
  } else {
    if (whole_day$eval[i] == "2") {
      whole_day$eval[i] <- str_c(whole_day$eval[i], "nd")
    } else {
      if (whole_day$eval[i] == "3") {
        whole_day$eval[i] <- str_c(whole_day$eval[i], "rd")
      }
    }
  }
}

# Exclude subjects who did not undergo bariatric surgery
to_exclude <- c(77, 79, 81, 88, 89, 90)
whole_day <- whole_day %>% 
  filter(!(ID %in% to_exclude))

# Separate control and exercise groups
control <- c(
  5, 6, 8, 9, 10, 14, 19, 20, 21, 23, 24, 26, 30, 31, 32, 
  33, 36, 38, 40, 41, 45, 53, 64, 65, 70, 76, 83, 85
  )

whole_day$group <- NA
for (i in 1:nrow(whole_day)) {
  if (whole_day$ID[i] %in% control) {
    whole_day$group[i] <- "control"
  } else {
    whole_day$group[i] <- "exercise"
  }
}
whole_day <- dplyr::select(whole_day, ID, eval, group, everything())

# Change type of some variables
whole_day$eval  <- as_factor(whole_day$eval)
whole_day$group <- as_factor(whole_day$group)

# Keep only subjects who completed the 3 evaluations with a minimum of 4 valid
# days
eval_1 <- whole_day$ID[which(whole_day$eval == "1st")]
eval_2 <- whole_day$ID[which(whole_day$eval == "2nd")]
eval_3 <- whole_day$ID[which(whole_day$eval == "3rd")]

all_eval <- intersect(intersect(eval_1, eval_2), eval_3)

whole_day <- whole_day %>% 
  filter(ID %in% all_eval)