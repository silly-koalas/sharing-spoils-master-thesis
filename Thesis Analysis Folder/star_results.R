################################################################################
# Description:  Analysis of star network experiment.
# Date:         May 21, 2024
# Author:       Siebren Kooistra
################################################################################

# Preliminaries -----------------------------------------------------------

# Prepare workspace
rm(list = ls())
graphics.off()

# Load relevant packages
library(tidyverse)

# Load functions
source("functions.R")

# Loading and shaping the data --------------------------------------------

# Load the data
star_data <- read.csv("28_04_2024_star_experiment.csv",
                      skip = 6)

# Focus on the final entries
star_lastentries <- star_data |>
  group_by(X.run.number.) |>
  summarise(seed = last(current.random.seed.1),
            initial_coalition_size = last(initial.coalition.size),
            residual_power_levels = last(manually.specified.power),
            positions_power = last(positions.power.string),
            URC_turtles = last(sort..who..of.turtles.with..in.coalition..),
            URC_thesis_notation = reformat_coalition(URC_turtles),
            URC_size = URC_turtles |> str_split_1(pattern = " ") |> length(),
            residual_power = last(map..a.....residual.power..of.a..sort.on..who..turtles),
            network_power = last(map..a.....network.power..of.a..sort.on..who..turtles),
            initial_network_power = first(map..a.....network.power..of.a..sort.on..who..turtles),
            payoff_transfer = last(client.to.patron.payoff.transfer),
            power_transfer = last(patronage.power.transfer),
            max_power_transfer = last(max.patronage.power.transfer)
  )

# Also create a dataframe with each agent's after-transfer power level
agent_power_frame <- star_lastentries |> 
  group_by(X.run.number., 
           seed, 
           URC_size, 
           URC_thesis_notation, 
           payoff_transfer, 
           power_transfer,
           max_power_transfer) |>
  reframe(residual_power = netlogo_list_to_numeric_vector(residual_power),
          initial_network_power = netlogo_list_to_numeric_vector(initial_network_power),
          final_network_power = netlogo_list_to_numeric_vector(network_power),
          agent_ids = LETTERS[1:5],
          in_URC = str_detect(string = URC_thesis_notation, pattern = agent_ids)
  )

# Summarise the last entries across parameter combinations
star_summary <- star_lastentries |>
  group_by(payoff_transfer, 
           power_transfer,
           max_power_transfer,
           URC_thesis_notation, 
           URC_size) |>
  summarise(count = n()) |>
  ungroup() |>
  group_by(payoff_transfer, power_transfer) |>
  mutate(
    relative_frequency = count / sum(count)
  )

# Add coalitions that never occur to the summary, to appropriately show
# which URCs are rare

# List all possible URCs
star_potential_coalitions <- sapply(
  X = 1:5,
  \(m) {
    combn(LETTERS[1:5], 
          m = m, 
          FUN = \(o) {paste0("{", paste(o, collapse = " "), "}")}
    )
  }
)

# Create a data frame with all possible URCs at all possible parameter
# combinations
star_expanded_summary <- cross_join(
  unique(star_summary[,c("payoff_transfer", 
                         "power_transfer")]),
  data.frame(URC_thesis_notation = unlist(star_potential_coalitions),
             URC_size = str_count(
               string = unlist(star_potential_coalitions), 
               pattern = "[A-Z]"),
             count = 0)
)

# Filter out the URCs that are already in the data
star_expanded_summary <- anti_join(star_expanded_summary,
                                   star_summary, 
                                   by = c("payoff_transfer", 
                                          "power_transfer", 
                                          "URC_thesis_notation", 
                                          "URC_size")
)

# Add the URCs that never appear
star_summary <- rbind(star_summary,
                      star_expanded_summary)


# Exploratory tabulation --------------------------------------------------

# Tabulate the prevalence of each URC by payoff transfer and
# power transfer
ftable(star_lastentries$URC_thesis_notation, 
       star_lastentries$payoff_transfer, 
       star_lastentries$power_transfer)


# Analysis of URC compositions ---------------------------------

# Create a bar plot of the URC distribution per parameter combination
(star_results_urc_barplot <- ggplot(star_summary, 
                                    mapping = aes(
                                      y = reorder(URC_thesis_notation, 
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
                                   " max total.")
                   )
                 )
               )
               )#,
             #axes = "all_y"
  ) +
  labs(x = "URC as % of URCs at parameter combination",
       y = "URC (sorted by size from large above to small below)") +
  theme_bw(base_family = "serif", base_size = 11)
)
ggsave(filename = "star_results_URCs_barplot.png", 
       plot = star_results_urc_barplot,
       width = 200,
       height = 300,
       units = "mm",
       path = "C:/Users/Siebren Kooistra/Documents/Opleiding/Linköpings Universitet/Master Thesis/Figures")

# Post-transfer power plot ------------------------------------------------

# Patronage network matrix
P <- matrix(c(0, 1, 1, 1, 1,
              0, 0, 0, 0, 0,
              0, 0, 0, 0, 0,
              0, 0, 0, 0, 0,
              0, 0, 0, 0, 0), 
            nrow = 5, ncol = 5,
            byrow = TRUE)
P_sym <- P + t(P)

# Coalition membership matrix
N <- matrix(1,
            nrow = 5, ncol = 1,
            byrow = TRUE)

M <- N %*% t(N)

# Non-network power vector
gamma <- matrix(c(4.6, 4.9, 4.2, 5.3, 5.4), nrow = 5, ncol = 1)

# Plot after-transfer power as a function of zeta with a symmetric network
power_df <- data.frame(
  zeta = seq(from = 0, to = 0.99, by = 0.01),
  power_A = NA,
  power_B = NA,
  power_C = NA,
  power_D = NA,
  power_E = NA
)

for (z in seq(from = 0, to = 0.99, by = 0.01)) {
  member_power_temp <- power_transfer_itercomp(gamma, z, 
                                               max(0.5, z), 
                                               P_sym, 
                                               M, 
                                               0.00001)
  
  power_df[power_df$zeta == z, 2:6] <- member_power_temp
}

plot_df <- power_df |> pivot_longer(cols = !zeta, 
                                    names_to = "player", 
                                    names_pattern = "(Sum|[A-Z])",
                                    values_to = "power")

(initial_posttransfer_power_lineplot <- 
    ggplot(plot_df, 
           mapping = aes(x = zeta,
                         y = power,
                         linetype = player)
    ) +
    geom_line() +
    labs(x = 
           bquote(
             paste("Share of power transferred between allies ", 
                   bgroup("(", 
                          list(
                            paste(zeta, " per tie"), 
                            paste(
                              tau == bgroup("{",
                                            atop(
                                              0.5 ~ textstyle("if") ~ 
                                                zeta <= 0.5,
                                              zeta ~ textstyle("if") ~ 
                                                zeta > 0.5), 
                                            "."), 
                              " max total")
                          ),
                          ")")
             )
           ),
         y = "Power of player",
         linetype = "Player") +
    scale_y_continuous(breaks = 1:100) +
    scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
    theme_bw(base_family = "serif", base_size = 12))
ggsave(filename = "star_initial_posttransfer_power_lineplot.png", 
       plot = initial_posttransfer_power_lineplot,
       width = 200,
       height = 150,
       units = "mm",
       path = "C:/Users/Siebren Kooistra/Documents/Opleiding/Linköpings Universitet/Master Thesis/Figures")

# Plot obtained from the simulations as a sanity and implementation check
ggplot(agent_power_frame, mapping = aes(x = power_transfer, 
                                        y = initial_network_power, 
                                        linetype = agent_ids)) +
  geom_line() +
  labs(x = 
         bquote(
           paste("Share of power transferred between allies ", 
                 bgroup("(", 
                        list(paste(zeta, " per tie"), 
                             paste(
                               tau == bgroup("{",
                                             atop(0.5 ~ textstyle("if") ~ 
                                                    zeta <= 0.5,
                                                  zeta ~ textstyle("if") ~ 
                                                    zeta > 0.5), 
                                             "."), " max total")
                        ),
                        ")")
           )
         ),
       y = "Power of player",
       linetype = "Player") +
  scale_y_continuous(breaks = 1:100) +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
  theme_bw(base_family = "serif")
