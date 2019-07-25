# Load packages and required files ----------------------------------------

library(tidyverse)
library(cowplot)
source("code/intention_to_treat/04_compare_PA.R")
source("code/per_protocol/04_compare_PA.R")
source("code/exclude_training_time/01_exclude_training_time_analysis.R")

# Prepare data ------------------------------------------------------------

# Get control group and exercise group (intention-to-treat) data
CG_EG_ITT <- whole_day_2nd_3rd %>% 
  dplyr::select(ID, eval, group, avg_SED, avg_LPA, avg_MVPA)
# Recode groups
CG_EG_ITT$group <- as.character(CG_EG_ITT$group)
for (i in 1:nrow(CG_EG_ITT)) {
  CG_EG_ITT$group[which(CG_EG_ITT$group == "control")]  <- "CG"
  CG_EG_ITT$group[which(CG_EG_ITT$group == "exercise")] <- "EG (ITT)"
}

# Get exercise group (per-protocol) data
EG_PP <- whole_day_2nd_3rd_50_attend %>% 
  filter(group == "exercise") %>% 
  dplyr::select(ID, eval, group, avg_SED, avg_LPA, avg_MVPA)
# Recode group
EG_PP$group <- as.character(EG_PP$group)
EG_PP$group <- "EG (PP)"

# Get exercise group (per-protocol, excluding training sessions time)
EG_NT <- rbind(
  whole_day_2nd_3rd_50_attend %>% 
    filter(eval == "2nd" & group == "exercise") %>% 
    dplyr::select(ID, eval, group, avg_SED, avg_LPA, avg_MVPA),
  exclude_training_time_3rd %>% 
    filter(group == "exercise") %>% 
    dplyr::select(ID, eval, group, avg_SED, avg_LPA, avg_MVPA)
)
# Recode group
EG_NT$group <- as.character(EG_NT$group)
EG_NT$group <- "EG (ETT)"

# Put all groups together
all_groups <- rbind(CG_EG_ITT, EG_PP, EG_NT)

# Build plot data frame
time <- all_groups %>% 
  group_by(eval, group) %>% 
  summarise(
    SB = mean(avg_SED), LPA = mean(avg_LPA), MVPA = median(avg_MVPA)
  ) %>% 
  gather(
    SB, LPA, MVPA,
    key = "intensity",
    value = "time"
  )

error <- all_groups %>% 
  group_by(eval, group) %>% 
  summarise(
    SB = sd(avg_SED), LPA = sd(avg_LPA), MVPA = IQR(avg_MVPA)
  ) %>% 
  gather(
    SB, LPA, MVPA,
    key = "intensity",
    value = "error"
  )

plot_data <- time %>% 
  full_join(error, by = c("eval", "group", "intensity")) %>% 
  dplyr::select(group, eval, intensity, time, error)
  
plot_data$group     <- as_factor(plot_data$group)
plot_data$intensity <- as_factor(plot_data$intensity)

# Plots -------------------------------------------------------------------

# ** SB plot --------------------------------------------------------------

SB_plot <- ggplot(
  data = filter(plot_data, intensity == "SB"),
  aes(x = group, y = time, fill = eval)
  ) +
  geom_bar(
    position = position_dodge(0.7),
    colour = "black",
    stat = "identity",
    width = 0.5
  ) +
  geom_errorbar(
    mapping = aes(ymin = time, ymax = time + error),
    position = position_dodge(0.7),
    width = 0.25
  ) +
  scale_fill_manual(
    name = element_blank(),
    labels = c("1 month after BS", "6 months after BS"),
    values = c("#CCCCCC", "#FFFFFF")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 750),
    breaks = seq(0, 700, 100)
  ) +
  theme(axis.ticks = element_blank(), legend.position = "bottom") +
  labs(
    title = "SB",
    x = element_blank(),
    y = "Minutes/day"
  )

# ** LPA plot -------------------------------------------------------------

LPA_plot <- ggplot(
  data = filter(plot_data, intensity == "LPA"),
  aes(x = group, y = time, fill = eval)
) +
  geom_bar(
    position = position_dodge(0.7),
    colour = "black",
    stat = "identity",
    width = 0.5
  ) +
  geom_errorbar(
    mapping = aes(ymin = time, ymax = time + error),
    position = position_dodge(0.7),
    width = 0.25
  ) +
  scale_fill_manual(
    name = element_blank(),
    labels = c("1 month after BS", "6 months after BS"),
    values = c("#CCCCCC", "#FFFFFF")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 450),
    breaks = seq(0, 400, 100)
  ) +
  theme(axis.ticks = element_blank()) +
  labs(
    title = "LPA",
    x = element_blank(),
    y = "Minutes/day"
  )

# ** MVPA plot ------------------------------------------------------------

MVPA_plot <- ggplot(
  data = filter(plot_data, intensity == "MVPA"),
  aes(x = group, y = time, fill = eval)
) +
  geom_bar(
    position = position_dodge(0.7),
    colour = "black",
    stat = "identity",
    width = 0.5
  ) +
  geom_errorbar(
    mapping = aes(ymin = time, ymax = time + error),
    position = position_dodge(0.7),
    width = 0.25
  ) +
  scale_fill_manual(
    name = element_blank(),
    labels = c("1 month after BS", "6 months after BS"),
    values = c("#CCCCCC", "#FFFFFF")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 65),
    breaks = seq(0, 60, 10)
  ) +
  theme(axis.ticks = element_blank()) +
  labs(
    title = "MVPA",
    x = element_blank(),
    y = "Minutes/day"
  )

# **** Plot grid ----------------------------------------------------------

plot_grid_no_legend <- plot_grid(
  SB_plot + theme(legend.position = "none"),
  LPA_plot + theme(legend.position = "none"), 
  MVPA_plot + theme(legend.position = "none"),
  ncol = 3, nrow = 1
)

legend <- get_legend(SB_plot)

plot_grid <- plot_grid(plot_grid_no_legend, legend, ncol = 1, rel_heights = c(1, 0.1))

# Uncomment lines below to save plot
# ggsave(
#   filename = "figs/fig2.pdf",
#   plot = plot_grid, width = 60, height = 20, dpi = 200, units = "cm"
# )