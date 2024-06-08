################################################################################
# Description:  MCTS calibration for an Agent-Based version of the Acemoglu et
#               al. (2008) model of autocratic coalition-formation.
# Date:         May 21, 2024
# Author:       Siebren Kooistra
################################################################################

# Preliminaries -----------------------------------------------------------

# Prepare workspace
rm(list = ls())
graphics.off()

# Load relevant packages
library(tidyverse)

# Create labelling function
label_initial_coalition_size <- 
  function(string_vec) {
    lapply(string_vec, FUN = \(string) paste("Initial coalition size:", string))
    }

# Loading and shaping the data --------------------------------------------

# Load the data
MCTS_calibration_data <- read.csv("30_04_2024_parameter_exploration.csv",
                                  skip = 6)

MCTS_calibration_data |>
  group_by(X.run.number.) |>
  summarise(timer = last(timer)
  ) |> summarise(timer_sum = sum(timer))

# Focus on the final entries
MCTS_calibration_lastentries <- MCTS_calibration_data |>
  group_by(X.run.number.) |>
  summarise(initial_coalition_size = last(initial.coalition.size),
            UCT_constant = last(UCT.constant),
            MCTS_trials = last(MCTS.trials),
            URC_turtles = last(sort..who..of.turtles.with..in.coalition..)) |>
  mutate(URC_rational = if_else((initial_coalition_size == 3 & URC_turtles == "[0 1 2]")|(initial_coalition_size == 4 & URC_turtles == "[0 1 2]")|(initial_coalition_size == 5 & URC_turtles == "[0 1 2 3 4]"), true = 1, false = 0))

# Summarise the last entries across parameter combinations
MCTS_calibration_summary <- MCTS_calibration_lastentries |>
  group_by(UCT_constant, MCTS_trials, initial_coalition_size) |>
  summarise(URC_rational = mean(URC_rational))

# Tabulation and visualisation --------------------------------------------

# Heatmaps
ggplot(MCTS_calibration_summary, 
       mapping = aes(x = as.factor(MCTS_trials), 
                     y = as.factor(UCT_constant), 
                     fill = 100 * URC_rational)
       ) +
  geom_bin2d() +
  geom_point(mapping = aes(x = as.factor(MCTS_trials),
                           y = as.factor(UCT_constant),
                           shape = URC_rational >= 0.95
                           ),
             colour = "black",
             na.rm = TRUE
             ) +
  #levels(as.factor(parameter_space_exploration_summary$UCT_constant))[c(1, 5, 10, 15, 20, 25, 30)]
  scale_shape_manual(values = c("TRUE" = 8), labels = c("≥95%"), 
                     name = "% of simulations\nwhose URC is the\nrational outcome at\nleast 95%?") +
  facet_wrap(facets = "initial_coalition_size",
             scales = "free_x",
             labeller = label_initial_coalition_size) +
  labs(x = "Number of iterations for the MCTS algorithm", 
       y = "UCT constant", 
       fill = "% of simulations\nwhose URC is the\nrational outcome") +
  theme_bw(base_family = "serif")
ggsave("MCTS_calibration_heatmap.png", 
       width = 20,
       height = 15,
       unit = "cm",
       path = "../Figures")

# Line plots
ggplot(MCTS_calibration_summary, 
       mapping = aes(x = MCTS_trials, 
                     y = 100 * URC_rational, 
                     colour = as.factor(UCT_constant))
       ) +
  geom_line() +
  geom_point() +
  facet_wrap(facets = "initial_coalition_size",
             scales = "free_x",
             labeller = label_initial_coalition_size) +
  labs(x = "Number of iterations for the MCTS algorithm", 
       y = "% of simulations whose URC is the rational outcome", 
       colour = "URC constant")
ggsave("MCTS_calibration_along_MCTS_trials.png", 
       path = "../Figures")

ggplot(MCTS_calibration_summary, 
       mapping = aes(x = UCT_constant, 
                     y = 100 * URC_rational, 
                     colour = as.factor(MCTS_trials))
       ) +
  geom_line() +
  geom_point() +
  facet_wrap(facets = "initial_coalition_size",
             labeller = label_initial_coalition_size) +
  labs(x = "URC constant", 
       y = "% of simulations whose URC is the rational outcome", 
       colour = "Number of iterations for\nthe MCTS algorithm")
ggsave("MCTS_calibration_along_UCT_constant.png", 
       path = "../Figures")

# Tabulation of URCs in the four-agent setting
table(MCTS_calibration_lastentries[
  MCTS_calibration_lastentries$initial_coalition_size == 4, ][[
    "URC_turtles"
    ]],
      MCTS_calibration_lastentries[
        MCTS_calibration_lastentries$initial_coalition_size == 4, ][[
          "MCTS_trials"
          ]]) |>
  prop.table(margin = 2)

# Highest performance in the five-agent example
MCTS_calibration_summary$URC_rational[
  MCTS_calibration_summary$MCTS_trials == 600 & 
    MCTS_calibration_summary$UCT_constant == 0.5
  ]
