################################################################################
# Description:  Robustness check using the three-person coalition.
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
triad_robustness_data <- read.csv("05_05_2024_triad_robustness_test.csv",
                                  skip = 6) |>
  mutate(position_assignment = case_match(
    map..a.....positional.power..of.a..sort.on..who..turtles,
    "[3 0 0]" ~ "A",
    "[0 3 0]" ~ "B",
    "[0 0 3]" ~ "C",
    "[0 0 0]" ~ "No positional power"
  ))

# Focus on the final entries
triad_robustness_lastentries <- triad_robustness_data |>
  group_by(X.run.number.) |>
  summarise(seed = last(current.random.seed.1),
            payoff_transfer = last(patron.to.client.payoff.transfer),
            power_transfer = last(patronage.power.transfer),
            robustness_payoff = case_match(
              last(constant.sum.payoff.transfers.),
              "true" ~ "Payoff transferred like power",
              "false" ~ "Direct, positive-sum payoff transfers"),
            robustness_power = case_match(
              last(one.step.power.transfers.),
              "true" ~ "Power transferred one step",
              "false" ~ "Power transferred iteratively"),
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
triad_robustness_summary <- triad_robustness_lastentries |>
  group_by(residual_power_levels, 
           payoff_transfer, 
           power_transfer, 
           robustness_payoff,
           robustness_power,
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
triad_robustness_potential_coalitions <- sapply(
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
triad_expanded_robustness_summary <- cross_join(
  unique(triad_robustness_summary[,c("residual_power_levels",
                                     "payoff_transfer",
                                     "power_transfer",
                                     "robustness_payoff",
                                     "robustness_power",
                                     "positions_marker")]),
  data.frame(URC_thesis_notation = unlist(triad_robustness_potential_coalitions),
             URC_size = str_count(
               string = unlist(triad_robustness_potential_coalitions), 
               pattern = "[A-Z]"),
             count = 0)
)

# Filter out the URCs that are already in the data
triad_expanded_robustness_summary <- anti_join(triad_expanded_robustness_summary,
                                               triad_robustness_summary, 
                                               by = c("residual_power_levels",
                                                      "payoff_transfer",
                                                      "power_transfer",
                                                      "robustness_payoff",
                                                      "robustness_power",
                                                      "positions_marker", 
                                                      "URC_thesis_notation", 
                                                      "URC_size")
)

# Add the URCs that never appear
triad_robustness_summary <- rbind(triad_robustness_summary,
                                  triad_expanded_robustness_summary)

# Create a data frame for the relative frequency of position assignments at
# various combinations of payoff transfer and power transfer
positions_robustness_summary <- triad_robustness_lastentries |>
  group_by(power_transfer, 
           payoff_transfer, 
           robustness_payoff,
           robustness_power,
           positions_power, 
           position_assignment_final) |>
  summarise(count = n()) |>
  ungroup() |>
  filter(positions_power == "[3]") |>
  group_by(power_transfer, payoff_transfer, positions_power) |>
  mutate(
    relative_frequency = count / sum(count)
  )

# Visualisation -----------------------------------------------------------

# Show how results for payoff transfers vary with robustness checks
ggplot(triad_robustness_summary[
  triad_robustness_summary$power_transfer == 0.3 & 
    triad_robustness_summary$positions_marker == "Part of C's power positional",
], 
mapping = aes(x = payoff_transfer, 
              y = count,
              shape = URC_thesis_notation,
              linetype = URC_thesis_notation
)) +
  geom_line() +
  geom_point(size = 3) +
  facet_grid(rows = vars(robustness_payoff),
             cols = vars(robustness_power),
             labeller = "label_value") +
  labs(x = expression(
    paste("Share of payoff transferred between allies (", delta, ")")
  ), 
  y = "% of simulations with URC", 
  shape = "URC",
  linetype = "URC",
  subtitle = bquote(paste("Position present. Power transfers: ", zeta == 0.3, 
                          " per tie ", 
                          tau == 0.5, 
                          " max total")
  )
  ) +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
  scale_shape_manual(values = c("{A B C}" = "square", 
                                "{A B}" = "circle", 
                                "{A C}" = "triangle", 
                                "{A}" = "cross", 
                                "{B C}" = "diamond plus", 
                                "{B}" = "square cross", 
                                "{C}" = "triangle down open")) +
  theme_bw(base_family = "serif")

ggsave(filename = "triad_results_payoff_robustness.png", 
       width = 200,
       height = 2/3 * 200,
       units = "mm",
       path = "../Figures")


# Show how results for power transfers vary with robustness checks
ggplot(triad_robustness_summary[
  triad_robustness_summary$payoff_transfer == 0.6 & 
    triad_robustness_summary$positions_marker == "Part of C's power positional",
], 
mapping = aes(x = power_transfer, 
              y = count,
              shape = URC_thesis_notation,
              linetype = URC_thesis_notation
)) +
  geom_line() +
  geom_point(size = 3) +
  facet_grid(rows = vars(robustness_payoff),
             cols = vars(robustness_power),
             labeller = "label_value") +
  labs(
    x = bquote(paste("Share of power transferred between allies (", zeta, ")")), 
    y = "% of simulations with URC", 
    shape = "URC",
    linetype = "URC",
    subtitle = bquote(
      list(paste("Position present. Payoff transfers: ", delta == 0.6), 
           paste("Maximal share of power transferred: ", 
                 tau == bgroup("{",atop(0.5 ~ textstyle("if") ~ zeta <= 0.5,
                                        zeta ~ textstyle("if") ~ zeta > 0.5), 
                               ".")
           )
      )
    )
  ) +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
  scale_shape_manual(values = c("{A B C}" = "square", 
                                "{A B}" = "circle", 
                                "{A C}" = "triangle", 
                                "{A}" = "cross", 
                                "{B C}" = "diamond plus", 
                                "{B}" = "square cross", 
                                "{C}" = "triangle down open")) +
  theme_bw(base_family = "serif")

ggsave(filename = "triad_results_power_robustness.png", 
       width = 200,
       height = 2/3 * 200,
       units = "mm",
       path = "../Figures")
