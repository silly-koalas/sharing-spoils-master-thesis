################################################################################
# Description:  Analysis of the experiment on a three-person coalition.
# Date:         May 21, 2024
# Author:       Siebren Kooistra
################################################################################

# Preliminaries -----------------------------------------------------------

# Prepare workspace
rm(list = ls())
graphics.off()

# Load relevant packages
library(tidyverse)

# Read in all functions that might be necessary for running the script
source("functions.R", echo = FALSE)

# Loading and shaping the data --------------------------------------------

# Data from a small three-agent system
triad_data <- read.csv("28_04_2024_triad_experiment.csv",
                       skip = 6) |>
  mutate(position_assignment = case_match(
    map..a.....positional.power..of.a..sort.on..who..turtles,
    "[3 0 0]" ~ "A",
    "[0 3 0]" ~ "B",
    "[0 0 3]" ~ "C",
    "[0 0 0]" ~ "No positional power"
  ))

# Focus on the final entries
triad_lastentries <- triad_data |>
  group_by(X.run.number.) |>
  summarise(seed = last(current.random.seed.1),
            payoff_transfer = last(patron.to.client.payoff.transfer),
            power_transfer = last(patronage.power.transfer),
            max_power_transferred = last(max.patronage.power.transfer),
            initial_coalition_size = last(initial.coalition.size),
            residual_power_levels = last(manually.specified.power),
            positions_power = last(positions.power.string),
            URC_turtles = last(sort..who..of.turtles.with..in.coalition..),
            URC_size = URC_turtles |> str_split_1(pattern = " ") |> length(),
            URC_thesis_notation = reformat_coalition(URC_turtles),
            network_power = 
              last(map..a.....network.power..of.a..sort.on..who..turtles),
            positional_power = 
              last(map..a.....positional.power..of.a..sort.on..who..turtles),
            position_assignment_final = last(position_assignment)
  )

# Summarise across runs with the same parameter combination by counting URCs
triad_summary <- triad_lastentries |>
  group_by(residual_power_levels, 
           payoff_transfer, 
           power_transfer, 
           max_power_transferred, 
           URC_size,
           URC_thesis_notation) |>
  summarise(count = n()) |>
  mutate(
    positions_marker = ifelse(residual_power_levels == "[3 4 2]",
                              yes = "Part of C's power positional",
                              no = "No positions")
  )

# Add coalitions that never occur to the summary, to appropriately show
# which URCs are rare

# List all possible URCs
triad_potential_coalitions <- sapply(
  X = 1:3,
  \(m) {
    combn(LETTERS[1:3], 
          m = m, 
          FUN = \(o) {paste0("{", paste(o, collapse = " "), "}")}
    )
  }
)

# Create a data frame with all possible URCs at all possible parameter
# combinations
triad_expanded_summary <- cross_join(
  unique(triad_summary[,c("residual_power_levels",
                          "payoff_transfer",
                          "power_transfer",
                          "positions_marker")]),
  data.frame(URC_thesis_notation = unlist(triad_potential_coalitions),
             URC_size = str_count(
               string = unlist(triad_potential_coalitions), 
               pattern = "[A-Z]"),
             count = 0)
)

# Filter out the URCs that are already in the data
triad_expanded_summary <- anti_join(triad_expanded_summary,
                                    triad_summary, 
                                    by = c("residual_power_levels",
                                           "payoff_transfer",
                                           "power_transfer",
                                           "positions_marker", 
                                           "URC_thesis_notation", 
                                           "URC_size")
)

# Add the URCs that never appear
triad_summary <- rbind(triad_summary,
                       triad_expanded_summary)

# Create a data frame for the relative frequency of position assignments at
# various combinations of payoff transfer and power transfer
positions_summary <- triad_lastentries |>
  group_by(power_transfer, payoff_transfer, positions_power, position_assignment_final) |>
  summarise(count = n()) |>
  ungroup() |>
  filter(positions_power == "[3]") |>
  group_by(power_transfer, payoff_transfer, positions_power) |>
  mutate(
    relative_frequency = count / sum(count)
  )

# Tabulation and visualisation --------------------------------------------

# Plot the URC distribution as a function of payoff transfer conditional on
# power transfer (function in functions.R)
(payoff_plot_0 <- payoff_transfer_line_plots(0))

(payoff_plot_3 <- payoff_transfer_line_plots(0.3))

(payoff_plot_6 <- payoff_transfer_line_plots(0.6))

(payoff_plot_9 <- payoff_transfer_line_plots(0.9))

ggsave(filename = "triad_results_payoff_conditional_power.png", 
       plot = payoff_plot_3,
       width = 200,
       height = 2/3 * 200,
       units = "mm",
       path = "../Figures")

# Plot the URC distribution as a function of power transfer conditional on
# payoff transfer
(power_plot_0 <- power_transfer_line_plots(0))

(power_plot_3 <- power_transfer_line_plots(0.3))

(power_plot_6 <- power_transfer_line_plots(0.6))

(power_plot_9 <- power_transfer_line_plots(0.9))

ggsave("triad_results_power_conditional_payoff.png", 
       plot = power_plot_3,
       width = 200,
       height = 2/3 * 200,
       units = "mm",
       path = "../Figures")

# Plot the distribution of the final occupant of the position as a function
# of payoff transfer, conditional on power transfer
(positions_plot_0 <- payoff_transfer_position_line_plots(0))

(positions_plot_3 <- payoff_transfer_position_line_plots(0.3))

(positions_plot_6 <- payoff_transfer_position_line_plots(0.6))

(positions_plot_9 <- payoff_transfer_position_line_plots(0.9))

ggsave("triad_results_position_assignment.png", 
       plot = positions_plot_3,
       width = 200,
       height = 2/3 * 200,
       units = "mm",
       path = "../Figures")
