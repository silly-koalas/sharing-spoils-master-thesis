################################################################################
# Description:  Analysis of the experiment varying the cohesion of a four-person
#               coalition.
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

# Load the data
small_coalition_data <- 
  read.csv("29_04_2024_variable_cohesion_small_experiment.csv",
           skip = 6) |>
  filter(!(out.network.matrix != "[[0 1 1 1] [1 0 1 1] [1 1 0 1] [1 1 1 0]]" & 
             patronage.power.transfer == 0 & 
             client.to.patron.payoff.transfer == 0))

# Focus on the final entries
small_coalition_lastentries <- small_coalition_data |>
  group_by(X.run.number.) |>
  summarise(seed = last(current.random.seed.1),
            initial_coalition_size = last(initial.coalition.size),
            residual_power_levels = last(manually.specified.power),
            positions_power = last(positions.power.string),
            percentage_rewired = last(percentage.rewired),
            URC_turtles = last(sort..who..of.turtles.with..in.coalition..),
            URC_thesis_notation = reformat_coalition(URC_turtles),
            URC_size = URC_turtles |> str_split_1(pattern = " ") |> length(),
            residual_power = 
              last(map..a.....residual.power..of.a..sort.on..who..turtles),
            network_power = 
              last(map..a.....network.power..of.a..sort.on..who..turtles),
            payoff_transfer = last(client.to.patron.payoff.transfer),
            power_transfer = last(patronage.power.transfer),
            network_structure = 
              case_when(
                last(out.network.matrix) == 
                  "[[0 1 1 1] [1 0 1 1] [1 1 0 1] [1 1 1 0]]" & 
                  power_transfer == 0 & 
                  payoff_transfer == 0 ~ 
                  "No network",
                last(out.network.matrix) == 
                  "[[0 1 1 1] [1 0 1 1] [1 1 0 1] [1 1 1 0]]" ~ 
                  "High cohesion",
                last(out.network.matrix) == 
                  "[[0 1 0 1] [1 0 1 0] [0 1 0 1] [1 0 1 0]]" ~ 
                  "Medium cohesion",
                last(out.network.matrix) == 
                  "[[0 1 0 0] [1 0 0 0] [0 0 0 1] [0 0 1 0]]" ~ 
                  "Low cohesion"
              ) |>
              factor(levels = c("High cohesion", 
                                "Medium cohesion", 
                                "Low cohesion",
                                "No network")
              ),
  )

# Summarise the last entries across parameter combinations
small_coalition_summary <- small_coalition_lastentries |>
  group_by(network_structure, payoff_transfer, power_transfer, 
           URC_thesis_notation, URC_size) |>
  summarise(count = n())

# Add coalitions that never occur to the summary, to appropriately show
# which URCs are rare

# List all possible URCs
small_coalition_potential_coalitions <- sapply(
  X = 1:4,
  \(m) {
    combn(LETTERS[1:4], 
          m = m, 
          FUN = \(o) {paste0("{", paste(o, collapse = " "), "}")}
    )
  }
)

# Create a data frame with all possible URCs at all possible parameter
# combinations
small_coalition_expanded_summary <- cross_join(
  unique(small_coalition_summary[,c("network_structure", 
                                    "payoff_transfer", 
                                    "power_transfer")]),
  data.frame(URC_thesis_notation = unlist(small_coalition_potential_coalitions),
             URC_size = str_count(
               string = unlist(small_coalition_potential_coalitions), 
               pattern = "[A-Z]"),
             count = 0)
)

# Filter out the URCs that are already in the data
small_coalition_expanded_summary <- anti_join(small_coalition_expanded_summary,
                                              small_coalition_summary, 
                                              by = c("network_structure", 
                                                     "payoff_transfer", 
                                                     "power_transfer", 
                                                     "URC_thesis_notation", 
                                                     "URC_size")
)

# Add the URCs that never appear
small_coalition_summary <- rbind(small_coalition_summary,
                                 small_coalition_expanded_summary)

# Tabulation and visualisation --------------------------------------------

# Plot the distribution of URCs per parameter combination for the high, medium
# and low cohesion networks in one plot
(variable_cohesion_small_results_urc_barplot <- 
   ggplot(small_coalition_summary, 
          mapping = aes(y = reorder(URC_thesis_notation, 
                                    URC_size, 
                                    decreasing = FALSE), 
                        x = count,
                        fill = network_structure),
   ) +
   geom_col(position = "dodge", colour = "grey20") +
   facet_grid(rows = vars(payoff_transfer),
              cols = vars(power_transfer),
              labeller = 
                label_bquote(rows = paste("Payoff transfer:  ", 
                                          delta == .(payoff_transfer)
                ),
                cols = atop(
                  textstyle("Power transfer: "),
                  atop(
                    textstyle(paste(zeta == .(power_transfer), " per tie,")),
                    textstyle(paste(tau == .(ifelse(power_transfer > 0.5, 
                                                    yes = power_transfer, 
                                                    no = 0.5)), 
                                    " total.")
                    )
                  )
                )
                )#,
              #axes = "all_y"
   ) +
   ggokabeito::scale_fill_okabe_ito() +
   labs(x = "URC as % of URCs at parameter combination",
        y = "URC (sorted by size from large above to small below)",
        fill = "Network cohesion") +
   guides(fill = guide_legend(position = "bottom")) +
   theme_bw(base_family = "serif", base_size = 11)
)
ggsave(filename = "variable_cohesion_small_results_urc_barplot.png", 
       plot = variable_cohesion_small_results_urc_barplot,
       width = 200,
       height = 300,
       units = "mm",
       path = "../Figures")

# Plot the distribution of URCs per parameter combination for the high, medium
# and low cohesion networks seperately
(variable_cohesion_small_results_high_cohesion_urc_barplot <- 
    ggplot(small_coalition_summary[
      small_coalition_summary$network_structure == "High cohesion",
    ], 
    mapping = aes(y = reorder(URC_thesis_notation, 
                              URC_size, 
                              decreasing = FALSE), 
                  x = count),
    ) +
    geom_col(position = "dodge") +
    facet_grid(rows = vars(payoff_transfer),
               cols = vars(power_transfer),
               labeller = 
                 label_bquote(rows = paste("Payoff transfer:  ", 
                                           delta == .(payoff_transfer)
                 ),
                 cols = atop(
                   textstyle("Power transfer: "),
                   atop(
                     textstyle(paste(zeta == .(power_transfer), " per tie,")),
                     textstyle(paste(tau == .(ifelse(power_transfer > 0.5, 
                                                     yes = power_transfer, 
                                                     no = 0.5)), 
                                     " total.")
                     )
                   )
                 )
                 ),
               axes = "all_y"
    ) +
    labs(x = "URC as % of URCs at parameter combination",
         y = "URC (sorted by size from large above to small below)",
         fill = "Network cohesion") +
    theme_bw(base_family = "serif", base_size = 11)
)
(variable_cohesion_small_results_medium_cohesion_urc_barplot <- 
    ggplot(small_coalition_summary[
      small_coalition_summary$network_structure == "Medium cohesion",
    ],
    mapping = aes(y = reorder(URC_thesis_notation, 
                              URC_size, 
                              decreasing = FALSE), 
                  x = count),
    ) +
    geom_col() +
    facet_grid(rows = vars(payoff_transfer),
               cols = vars(power_transfer),
               labeller = 
                 label_bquote(rows = paste("Payoff transfer:  ", 
                                           delta == .(payoff_transfer)
                 ),
                 cols = atop(
                   textstyle("Power transfer: "),
                   atop(
                     textstyle(paste(zeta == .(power_transfer), " per tie,")),
                     textstyle(paste(tau == .(ifelse(power_transfer > 0.5, 
                                                     yes = power_transfer, 
                                                     no = 0.5)), 
                                     " total.")
                     )
                   )
                 )
                 ),
               axes = "all_y"
    ) +
    labs(x = "URC as % of URCs at parameter combination",
         y = "URC (sorted by size from large above to small below)") +
    theme_bw(base_family = "serif", base_size = 11)
)
(variable_cohesion_small_results_low_cohesion_urc_barplot <- 
    ggplot(small_coalition_summary[
      small_coalition_summary$network_structure == "Low cohesion",
    ],
    mapping = aes(y = reorder(URC_thesis_notation, 
                              URC_size, 
                              decreasing = FALSE), 
                  x = count),
    ) +
    geom_col() +
    facet_grid(rows = vars(payoff_transfer),
               cols = vars(power_transfer),
               labeller = 
                 label_bquote(rows = paste("Payoff transfer:  ", 
                                           delta == .(payoff_transfer)
                 ),
                 cols = atop(
                   textstyle("Power transfer: "),
                   atop(
                     textstyle(paste(zeta == .(power_transfer), " per tie,")),
                     textstyle(paste(tau == .(ifelse(power_transfer > 0.5, 
                                                     yes = power_transfer, 
                                                     no = 0.5)), 
                                     " total.")
                     )
                   )
                 )
                 ),
               axes = "all_y"
    ) +
    labs(x = "URC as % of URCs at parameter combination",
         y = "URC (sorted by size from large above to small below)") +
    theme_bw(base_family = "serif", base_size = 11)
)

# Post-transfer power plot ------------------------------------------------

# Extract the adjacency matrix's contents at each level of network cohesion
network_data_high_dens <- unique(small_coalition_data$out.network.matrix[
  small_coalition_data$client.to.patron.payoff.transfer == 0 &
    small_coalition_data$patronage.power.transfer])[1] |>
  str_remove_all(pattern = "\\]|\\[") |>
  str_split_1(pattern = " ") |>
  as.numeric()

network_data_medium_dens <- unique(small_coalition_data$out.network.matrix[
  small_coalition_data$client.to.patron.payoff.transfer == 0 &
    small_coalition_data$patronage.power.transfer])[2] |>
  str_remove_all(pattern = "\\]|\\[") |>
  str_split_1(pattern = " ") |>
  as.numeric()

network_data_low_dens <- unique(small_coalition_data$out.network.matrix[
  small_coalition_data$client.to.patron.payoff.transfer == 0 &
    small_coalition_data$patronage.power.transfer])[3] |>
  str_remove_all(pattern = "\\]|\\[") |>
  str_split_1(pattern = " ") |>
  as.numeric()

# Patronage network matrix
P_H <- matrix(network_data_high_dens, 
              nrow = 4, ncol = 4,
              byrow = TRUE)
P_M <- matrix(network_data_medium_dens, 
              nrow = 4, ncol = 4,
              byrow = TRUE)
P_L <- matrix(network_data_low_dens, 
              nrow = 4, ncol = 4,
              byrow = TRUE)
# Coalition membership matrix
N <- matrix(1,
            nrow = 4, ncol = 1,
            byrow = TRUE)

M <- N %*% t(N)

# Non-network power vector
gamma <- matrix(c(3, 4, 5, 10), nrow = 4, ncol = 1)


# Plot after-transfer power as a function of zeta with a symmetric network
#   Create an empty data frame to put the data into
power_df <- expand.grid(
  network_cohesion = c("High cohesion", "Medium cohesion", "Low cohesion"),
  zeta = seq(from = 0, to = 0.99, by = 0.01),
  power_A = NA,
  power_B = NA,
  power_C = NA,
  power_D = NA
)

# Fill the data frame
for (z in seq(from = 0, to = 0.99, by = 0.01)) {
  # Calculate after-transfer power levels for each level of cohesion
  member_power_temp_H <- power_transfer_itercomp(gamma, z, 
                                                 max(0.5, z), 
                                                 P_H, 
                                                 M, 
                                                 0.00001)
  member_power_temp_M <- power_transfer_itercomp(gamma, z, 
                                                 max(0.5, z), 
                                                 P_M, 
                                                 M, 
                                                 0.00001)
  member_power_temp_L <- power_transfer_itercomp(gamma, z, 
                                                 max(0.5, z), 
                                                 P_L, 
                                                 M, 
                                                 0.00001)
  
  # Add the after-transfer power levels to the data frame
  power_df[power_df$network_cohesion == "High cohesion" & 
             power_df$zeta == z, 3:6] <- member_power_temp_H
  power_df[power_df$network_cohesion == "Medium cohesion" & 
             power_df$zeta == z, 3:6] <- member_power_temp_M
  power_df[power_df$network_cohesion == "Low cohesion" & 
             power_df$zeta == z, 3:6] <- member_power_temp_L
}

plot_df <- power_df |> pivot_longer(cols = !network_cohesion:zeta, 
                                    names_to = "player", 
                                    names_pattern = "(Sum|[A-Z])",
                                    values_to = "power")

(initial_posttransfer_power_lineplot <- 
    ggplot(plot_df, 
           mapping = aes(x = zeta,
                         y = power,
                         linetype = player,
                         colour = network_cohesion)
    ) +
    geom_line() +
    labs(x = bquote(
      paste("Share of power transferred between allies ", 
            bgroup("(", 
                   list(zeta, 
                        tau == bgroup("{",
                                      atop(0.5 ~ textstyle("if") ~ zeta <= 0.5,
                                           zeta ~ textstyle("if") ~ zeta > 0.5), 
                                      ".")),
                   ")")
      )
    ),
    y = "Power of player",
    colour = "Network cohesion",
    linetype = "Player") +
    scale_y_continuous(breaks = 1:100) +
    scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
    ggokabeito::scale_colour_okabe_ito() +
    theme_bw(base_family = "serif", base_size = 12))
ggsave(filename = "variable_cohesion_initial_posttransfer_power_lineplot.png", 
       plot = initial_posttransfer_power_lineplot,
       width = 200,
       height = 150,
       units = "mm",
       path = "../Figures")