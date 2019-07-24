# Load packages and required files ----------------------------------------

library(tidyverse)
source("code/intention_to_treat/04_compare_PA.R")

# Prepare data ------------------------------------------------------------

time <- whole_day_1st_2nd %>% 
  group_by(eval) %>% 
  summarise(
    SB = mean(avg_SED), LPA = mean(avg_LPA), MVPA = median(avg_MVPA)
    ) %>% 
  gather(
    SB, LPA, MVPA,
    key = "intensity",
    value = "time"
  )

error <- whole_day_1st_2nd %>% 
  group_by(eval) %>% 
  summarise(
    SB = sd(avg_SED), LPA = sd(avg_LPA), MVPA = IQR(avg_MVPA)
  ) %>% 
  gather(
    SB, LPA, MVPA,
    key = "intensity",
    value = "error"
  )

plot_data <- time %>% 
  full_join(error, by = c("eval", "intensity")) %>% 
  dplyr::select(intensity, eval, time, error)

plot_data$intensity <- as_factor(plot_data$intensity)

# Plot --------------------------------------------------------------------

plot_1st_2nd <- ggplot(data = plot_data, aes(x = intensity, y = time, fill = eval)) +
  geom_bar(
    position = position_dodge(0.9),
    colour = "black",
    stat = "identity"
    ) +
  geom_errorbar(
    mapping = aes(ymin = time, ymax = time + error),
    position = position_dodge(0.9),
    width = 0.25
  ) +
  scale_fill_manual(
    name = element_blank(), 
    labels = c("1 month before BS", "1 month after BS"),
    values = c("#CCCCCC", "#FFFFFF")
    ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 750)) +
  labs(
    x = element_blank(),
    y = "Minutes/day"
  ) +
  annotate("segment", x = 1.6, xend = 2.4, y = 370, yend = 370) +
  annotate("segment", x = 1.6, xend = 1.6, y = 370, yend = 350) +
  annotate("segment", x = 2.4, xend = 2.4, y = 370, yend = 350) +
  annotate("text", x = 2, y = 380, label = "*") +
  annotate("segment", x = 2.6, xend = 3.4, y = 60, yend = 60) +
  annotate("segment", x = 2.6, xend = 2.6, y = 60, yend = 40) +
  annotate("segment", x = 3.4, xend = 3.4, y = 60, yend = 40) +
  annotate("text", x = 3, y = 70, label = "*")

# Uncomment lines below to save plot
# ggsave(
#   filename = "figs/fig1.pdf",
#   plot = plot_1st_2nd, width = 20, height = 15, dpi = 200, units = "cm"
# )