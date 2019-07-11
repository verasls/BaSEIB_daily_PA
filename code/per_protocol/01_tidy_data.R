# Load packages and required files ----------------------------------------

library(tidyverse)
source("code/intention_to_treat/01_tidy_data.R")

# Select IDs by attendance rate -------------------------------------------

# Read training attendance data
attendance <- read_csv("data/BaSEIB_attendance_rate.csv") %>% 
  na.omit()

IDs_attendance_50 <- attendance$ID[which(attendance$attend_rate_3rd_eval >= 50)]

selected_IDs <- intersect(unique(whole_day$ID), IDs_attendance_50)

# Read and tidy PA data ------------------------------------------------------

# Filter IDs with selected attendance rate
whole_day_50_attend_exercise <- whole_day %>% 
  filter(ID %in% selected_IDs)

whole_day_50_attend_control <- whole_day %>% 
  filter(group == "control")

whole_day_50_attend <- rbind(whole_day_50_attend_exercise, whole_day_50_attend_control) %>% 
  arrange(eval, ID)

# Read and tidy anthropometric data ---------------------------------------

# Filter IDs with selected attendance rate
ant_1st_50_attend_exercise <- ant_1st %>% 
  filter(ID %in% selected_IDs)

ant_1st_50_attend_control <- ant_1st %>% 
  filter(group == "control")

ant_1st_50_attend <- rbind(ant_1st_50_attend_exercise, ant_1st_50_attend_control) %>% 
  arrange(ID)

ant_2nd_50_attend_exercise <- ant_2nd %>% 
  filter(ID %in% selected_IDs)

ant_2nd_50_attend_control <- ant_2nd %>% 
  filter(group == "control")

ant_2nd_50_attend <- rbind(ant_2nd_50_attend_exercise, ant_2nd_50_attend_control) %>% 
  arrange(ID)

ant_3rd_50_attend_exercise <- ant_3rd %>% 
  filter(ID %in% selected_IDs)

ant_3rd_50_attend_control <- ant_3rd %>% 
  filter(group == "control")

ant_3rd_50_attend <- rbind(ant_3rd_50_attend_exercise, ant_3rd_50_attend_control) %>% 
  arrange(ID)

ant_50_attend_exercise <- ant %>%
  filter(ID %in% selected_IDs)

ant_50_attend_control <- ant %>% 
  filter(group == "control")

ant_50_attend <- rbind(ant_50_attend_exercise, ant_50_attend_control) %>% 
  arrange(eval, ID)