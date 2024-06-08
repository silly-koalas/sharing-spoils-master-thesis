################################################################################
# Description:  Analysis of the 'Old Guard' experiment.
# Date:         May 21, 2024
# Author:       Siebren Kooistra
################################################################################

# Preliminaries -----------------------------------------------------------

# Prepare workspace
rm(list = ls())
graphics.off()

# Load relevant packages
library(tidyverse)
library(igraph)
library(ggraph)
library(kableExtra)
library(ggrepel)
library(patchwork)

# Load functions
source("functions.R")

# Loading and shaping the data --------------------------------------------

# Load the data
old_guard_data <- read.csv("11_05_2024_old_guard_experiment.csv",
                           skip = 6)

# Focus on the final entries
old_guard_lastentries <- old_guard_data |>
  filter(manual.agent.type.input. != "true") |>
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
            payoff_transfer = last(client.to.patron.payoff.transfer),
            power_transfer = last(patronage.power.transfer),
            network_structure = last(manual.patronage.network.input),
            final_position_assignment = last(map..a.....positional.power..of.a..sort.on..who..turtles) |>
              str_remove_all(pattern = "\\[|\\]|\\s") |>
              regexpr(pattern = "5") |>
              as.numeric(),
            final_position_assignment = LETTERS[final_position_assignment],
            satisficers_present = ifelse(
              last(manual.agent.type.input.) == "true", 
              yes = "Yes", 
              no = "No")
  )

# Summarise the last entries across parameter combinations
old_guard_summary <- old_guard_lastentries |>
  group_by(network_structure, 
           payoff_transfer, 
           power_transfer, 
           URC_size, 
           URC_thesis_notation,
           satisficers_present) |>
  summarise(count = n()) |>
  ungroup() |>
  group_by(network_structure, payoff_transfer, power_transfer)

# Add coalitions that never occur to the summary, to appropriately show
# which URCs are rare

# List all possible URCs
old_guard_potential_coalitions <- sapply(
  X = 1:9,
  \(m) {
    combn(LETTERS[1:9], 
          m = m, 
          FUN = \(o) {paste0("{", paste(o, collapse = " "), "}")}
    )
  }
)

# Create a data frame with all possible URCs at all possible parameter
# combinations
old_guard_expanded_summary <- cross_join(
  unique(old_guard_summary[,c("network_structure", 
                              "payoff_transfer", 
                              "power_transfer",
                              "satisficers_present")]),
  data.frame(URC_thesis_notation = unlist(old_guard_potential_coalitions),
             URC_size = str_count(
               string = unlist(old_guard_potential_coalitions), 
               pattern = "[A-Z]"),
             count = 0)
)

# Filter out the URCs that are already in the data
old_guard_expanded_summary <- anti_join(old_guard_expanded_summary,
                                        old_guard_summary, 
                                        by = c("network_structure", 
                                               "payoff_transfer", 
                                               "power_transfer", 
                                               "satisficers_present",
                                               "URC_thesis_notation", 
                                               "URC_size")
)

# Add the URCs that never appear
old_guard_summary <- rbind(old_guard_summary,
                           old_guard_expanded_summary)


# URC frequencies ---------------------------------------------------------

# Plot the frequency of all possible URCs (if there are no satisficing agents)
(
  URC_frequency_plot <- 
    ggplot(old_guard_summary[old_guard_summary$satisficers_present == "No",], 
           mapping = aes(y = reorder(URC_thesis_notation, URC_size), 
                         x = count
           )
    ) + 
    geom_point(alpha = 0.25) +
    geom_linerange(aes(xmin = 0, xmax = count), linetype = "dotted",
                   alpha = 0.25) +
    geom_text_repel(mapping = aes(y = reorder(URC_thesis_notation, URC_size),
                                  x = count, 
                                  label = ifelse(count > quantile(count, 
                                                                  probs = 0.98), 
                                                 yes = URC_thesis_notation,
                                                 no = NA)),
                    min.segment.length = 0,
                    segment.size = 0.1,
                    point.size = 2,
                    point.padding = 1e-01,
                    size = 4,
                    force = 0.5,
                    #nudge_x = 4,
                    hjust = 0.5,
                    max.overlaps = Inf,
                    verbose = TRUE,
                    inherit.aes = FALSE) +
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
                 ),
               axes = "all_y"
    ) +
    scale_x_continuous(breaks = 0:100) +
    scale_y_discrete(label = NULL, 
                     breaks = NULL,
                     expand = expansion(mult = 0.05)
    ) +
    theme_bw(base_family = "serif") +
    theme(panel.grid.major.x = element_line(colour = "white"),
          panel.grid.minor.x = element_line(colour = "white"),
          legend.position = "none") +
    labs(x = "URC as % of URCs at parameter combination", 
         y = "URC (sorted by size from large above to small below)") + 
    #  URC size marking lines
    #    URC Size 1
    annotate(geom = "text", label = "1", y = -5, 
             x = 11, alpha = 0.2) +
    #    URC Size 2
    geom_hline(yintercept = 9.5, 
               linetype = "dashed", alpha = 0.2) +
    annotate(geom = "text", label = "2", 
             y = choose(n = 9, k = 2)/2 + sum(choose(n = 9, k = 1)), 
             x = 11, alpha = 0.2) +
    #    URC Size 3
    geom_hline(yintercept = choose(n = 9, k = 2) + 9.5, 
               linetype = "dashed", alpha = 0.2) +
    annotate(geom = "text", label = "3", 
             y = choose(n = 9, k = 3)/2 + 
               sum(choose(n = 9, k = 1:2)), 
             x = 11, alpha = 0.2) +
    #    URC Size 4
    geom_hline(yintercept = sum(choose(n = 9, k = 2:3)) + 9.5, 
               linetype = "dashed", alpha = 0.2) +
    annotate(geom = "text", label = "4", 
             y = choose(n = 9, k = 4)/2 + 
               sum(choose(n = 9, k = 1:3)), 
             x = 11, alpha = 0.2) +
    #    URC Size 5
    geom_hline(yintercept = sum(choose(n = 9, k = 2:4)) + 9.5, 
               linetype = "dashed", alpha = 0.2) +
    annotate(geom = "text", label = "URC Size = 5",
             y = choose(n = 9, k = 5)/2 +
               sum(choose(n = 9, k = 1:4)),
             x = 7.8, alpha = 0.2) +
    #    URC Size 6
    geom_hline(yintercept = sum(choose(n = 9, k = 2:5)) + 9.5, 
               linetype = "dashed", alpha = 0.2) +
    annotate(geom = "text", label = "6", 
             y = choose(n = 9, k = 6)/2 +
               sum(choose(n = 9, k = 1:5)), 
             x = 11, alpha = 0.2) +
    #    URC Size 7
    geom_hline(yintercept = sum(choose(n = 9, k = 2:6)) + 9.5, 
               linetype = "dashed", alpha = 0.2) +
    annotate(geom = "text", label = "7", 
             y = choose(n = 9, k = 7)/2 +
               sum(choose(n = 9, k = 1:6)), 
             x = 11, alpha = 0.2) +
    #    URC Size 8
    geom_hline(yintercept = sum(choose(n = 9, k = 2:7)) + 9.5, 
               linetype = "dashed", alpha = 0.2) +
    annotate(geom = "text", label = "8", 
             y = choose(n = 9, k = 8)/2 +
               sum(choose(n = 9, k = 1:7)), 
             x = 11, alpha = 0.2,
             size = 1) +
    #    URC Size 9
    geom_hline(yintercept = sum(choose(n = 9, k = 2:8)) + 9.5, 
               linetype = "dashed", alpha = 0.2) +
    annotate(geom = "text", label = "9", 
             y = choose(n = 9, k = 9)/2 +
               sum(choose(n = 9, k = 1:8)) + 10,
             x = 11, alpha = 0.2,
             size = 2)
)
ggsave(filename = "old_guard_results_URCs_frequency_plot.png", 
       plot = URC_frequency_plot,
       width = 200,
       height = 300,
       units = "mm",
       path = "../Figures")

# Summarise the data further to show the frequency of URCs of various sizes
URC_size_summary <- old_guard_summary |>
  group_by(network_structure,
           payoff_transfer, 
           power_transfer, 
           URC_size, 
           satisficers_present) |>
  summarise(count = sum(count)) |>
  filter(satisficers_present == "No")

# Plot the frequency of various URC sizes
(
  URC_size_plot <- ggplot(URC_size_summary, mapping = aes(y = count, x = URC_size)) +
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
                 )
    ) +
    geom_col() +
    scale_x_continuous(breaks = unique(URC_size_summary$URC_size)) +
    coord_flip() +
    theme_bw() +
    labs(x = "URC of given size as % of URCs at parameter combination", 
         y = "URC size")
)
ggsave(filename = "old_guard_results_URC_size_plot.png", 
       plot = URC_size_plot,
       width = 200,
       height = 300,
       units = "mm",
       path = "../Figures")

# Singleton analysis ------------------------------------------------------

# Tabulate the probability of A forming a singleton coalition by
# network structure
singleton_table <- xtabs(count ~ payoff_transfer + 
                           power_transfer, 
                         data = old_guard_summary, 
                         subset = URC_thesis_notation == "{A}" & 
                           satisficers_present == "No")

# Save the current global options
opts <- options()

# Change the way NAs are displayed
options(knitr.kable.NA = "")

# Create a LaTeX table
singleton_table |>
  kable(format = "latex", 
        digits = 0, 
        booktabs = TRUE, 
        row.names = TRUE,
        col.names = c("Payoff transfer ($\\delta$)", "0", "0.3", "0.6", "0.9"),
        escape = FALSE,
        caption = "Frequencies with which $A$ forms a singleton coalition in an `old guard' scenario at various degrees of payoff transfer and power transfer. \\\\ \n\\label{tab:A_sin_fre}"
  ) |>
  add_header_above(header = c(" " = 1, "Power transfer ($\\\\zeta$)" = 4),
                   escape = FALSE) |>
  footnote(general = "Number of simulations per cell is 100. The transition penalty $\\\\epsilon$ is $\\\\frac{1}{100}$ in all simulations. MCTS parameters are 100 trials per decision and a UCT constant of 1.5.",
           threeparttable = TRUE,
           escape = FALSE) |>
  save_kable(file = "../Thesis Writing Folder/old_guard_table.tex")

# Restore the original global options
options(opts)


# Positions ---------------------------------------------------------------

# Plot the frequency with which each agents ends up holding the position by the
# end of a simulation
(URC_positions_plot <- 
   ggplot(old_guard_lastentries[old_guard_lastentries$satisficers_present == "No",], 
          mapping = aes(y = forcats:::fct_rev(final_position_assignment)
          )
   ) + 
   geom_bar() +
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
              # axes = "all_y"
   ) +
   geom_vline(xintercept = 100/9) +
   annotate(geom = "text", x = 110/9, y = 3.5, label = "Uniform expectation", angle = 90, size = 2.5) +
   theme_bw(base_family = "serif") +
   labs(x = "% of URCs at parameter combination with position assignment", 
        y = "Agent with position at URC")
)
ggsave(filename = "old_guard_results_URCs_positions_plot.png", 
       plot = URC_positions_plot,
       width = 200,
       height = 200,
       units = "mm",
       path = "../Figures")


# Power levels ------------------------------------------------------------

# Patronage network matrix
P <- matrix(c(0, 0, 0, 0, 1, 1, 0, 0, 1,
              0, 0, 1, 1, 1, 0, 0, 0, 0,
              0, 0, 0, 1, 0, 0, 0, 1, 0,
              0, 0, 0, 0, 0, 1, 1, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0), 
            nrow = 9, ncol = 9,
            byrow = TRUE)
P_sym <- P + t(P)

# Coalition membership matrix
N <- matrix(1,
            nrow = 9, ncol = 1,
            byrow = TRUE)

M <- N %*% t(N)

# Non-network power vector
gamma <- matrix(c(10, 4, 5.33333, 5.2, 4.6, 4.7, 4.2, 4.5, 4.1), nrow = 9, ncol = 1)

# Plot after-transfer power as a function of zeta with a symmetric network
power_df <- data.frame(
  zeta = seq(from = 0, to = 0.99, by = 0.01),
  power_A = NA,
  power_B = NA,
  power_C = NA,
  power_D = NA,
  power_E = NA,
  power_F = NA,
  power_G = NA,
  power_H = NA,
  power_I = NA
)

for (z in seq(from = 0, to = 0.99, by = 0.01)) {
  member_power_temp <- power_transfer_itercomp(gamma, z, 
                                               max(0.5, z), 
                                               P_sym, 
                                               M, 
                                               0.00001)
  
  power_df[power_df$zeta == z, 2:10] <- member_power_temp
}

# Pivot the data frame for plotting
plot_df <- power_df |> pivot_longer(cols = !zeta, 
                                    names_to = "player", 
                                    names_pattern = "(Sum|[A-Z])",
                                    values_to = "power")

# Add a small data frame to display identification letters at the end of
# the lines
end_letters <- plot_df |> 
  filter(zeta == 0.99)

# Create the plot
(initial_posttransfer_power_lineplot <- 
    ggplot(plot_df, 
           mapping = aes(x = zeta,
                         y = power,
                         group = player)
    ) +
    geom_line() +
    geom_text_repel(data = end_letters, 
                    mapping = aes(label = player),
                    min.segment.length = 0,
                    segment.size = 0.1,
                    segment.linetype = "dashed",
                    hjust = 1,
                    direction = "y",
                    nudge_x = 0.025) +
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
ggsave(filename = "old_guard_initial_posttransfer_power_lineplot.png", 
       plot = initial_posttransfer_power_lineplot,
       width = 200,
       height = 150,
       units = "mm",
       path = "C:/Users/Siebren Kooistra/Documents/Opleiding/Linköpings Universitet/Master Thesis/Figures")


# Survival analysis -------------------------------------------------------

# List all parameter and agent combinations
old_guard_survival_frame <- expand.grid(
  agent = 0:8, 
  payoff_transfer = c(0, 0.3, 0.6, 0.9), 
  power_transfer = c(0, 0.3, 0.6, 0.9)
)

# Create a vector of survival frequencies per agent and parameter combination
old_guard_survival <- 
  sapply(
    1:nrow(old_guard_survival_frame),
    FUN = \(i) {
      sum(
        str_detect(
          string = old_guard_lastentries[
            old_guard_lastentries$payoff_transfer == 
              old_guard_survival_frame[i, "payoff_transfer"] & 
              old_guard_lastentries$power_transfer == 
              old_guard_survival_frame[i, "power_transfer"],
          ]$URC_turtles, 
          pattern = as.character(old_guard_survival_frame[i, "agent"])))
    })

# Add the survival frequencies to the data frame
old_guard_survival_frame$survival <- old_guard_survival

# Turn the agent identifiers from numbers to capital letters
old_guard_survival_frame$agent <- sapply(old_guard_survival_frame$agent, 
                                         FUN = \(x) {LETTERS[x+1]})

# Tabulate each agent's survival frequencies by parameter combinations
xtabs(survival ~ agent + power_transfer + payoff_transfer, 
      data = old_guard_survival_frame)

xtabs(survival ~ agent + payoff_transfer + power_transfer, 
      data = old_guard_survival_frame)

# Also tabulate each the distribution of survivals
old_guard_survival_frame <- old_guard_survival_frame |>
  mutate(relative_survival = survival/sum(survival),
         .by = c("power_transfer", "payoff_transfer"))

xtabs(relative_survival ~ agent + power_transfer + payoff_transfer, 
      data = old_guard_survival_frame)

xtabs(relative_survival ~ agent + payoff_transfer + power_transfer, 
      data = old_guard_survival_frame)

# Graph visualisation -----------------------------------------------------

# Count the number of unique URCs that appear in the simulation results
length(unique(old_guard_summary$URC_thesis_notation))

length(
  unique(
    old_guard_summary[old_guard_summary$satisficers_present == "No",]$
      URC_thesis_notation
  )
)

# Create network and cosurvival graphs for various parameter combinations
# Power transfer 0, payoff transfer 0
old_guard_both_graphs_0_0 <- 
  create_conditioned_graph(power_transfer_cond = 0, 
                           payoff_transfer_cond = 0, 
                           "No")
old_guard_graph_0_0 <- old_guard_both_graphs_0_0[[1]]
old_guard_cosurvival_graph_0_0 <- old_guard_both_graphs_0_0[[2]]

# Power transfer 0.3, payoff transfer 0
old_guard_both_graphs_3_0 <- 
  create_conditioned_graph(power_transfer_cond = 0.3, 
                           payoff_transfer_cond = 0.0, 
                           "No")
old_guard_graph_3_0 <- old_guard_both_graphs_3_0[[1]]
old_guard_cosurvival_graph_3_0 <- old_guard_both_graphs_3_0[[2]]

# Power transfer 0.6, payoff transfer 0
old_guard_both_graphs_6_0 <- 
  create_conditioned_graph(power_transfer_cond = 0.6, 
                           payoff_transfer_cond = 0.0, 
                           "No")
old_guard_graph_6_0 <- old_guard_both_graphs_6_0[[1]]
old_guard_cosurvival_graph_6_0 <- old_guard_both_graphs_6_0[[2]]

# Power transfer 0.9, payoff transfer 0
old_guard_both_graphs_9_0 <- 
  create_conditioned_graph(power_transfer_cond = 0.9, 
                           payoff_transfer_cond = 0.0, 
                           "No")
old_guard_graph_9_0 <- old_guard_both_graphs_9_0[[1]]
old_guard_cosurvival_graph_9_0 <- old_guard_both_graphs_9_0[[2]]

# Power transfer 0, payoff transfer 0.3
old_guard_both_graphs_0_3 <- 
  create_conditioned_graph(power_transfer_cond = 0, 
                           payoff_transfer_cond = 0.3, 
                           "No")
old_guard_graph_0_3 <- old_guard_both_graphs_0_3[[1]]
old_guard_cosurvival_graph_0_3 <- old_guard_both_graphs_0_3[[2]]

# Power transfer 0.3, payoff transfer 0.3
old_guard_both_graphs_3_3 <- 
  create_conditioned_graph(power_transfer_cond = 0.3, 
                           payoff_transfer_cond = 0.3, 
                           "No")
old_guard_graph_3_3 <- old_guard_both_graphs_3_3[[1]]
old_guard_cosurvival_graph_3_3 <- old_guard_both_graphs_3_3[[2]]

# Power transfer 0.6, payoff transfer 0.3
old_guard_both_graphs_6_3 <- 
  create_conditioned_graph(power_transfer_cond = 0.6, 
                           payoff_transfer_cond = 0.3, 
                           "No")
old_guard_graph_6_3 <- old_guard_both_graphs_6_3[[1]]
old_guard_cosurvival_graph_6_3 <- old_guard_both_graphs_6_3[[2]]

# Power transfer 0.9, payoff transfer 0.3
old_guard_both_graphs_9_3 <- 
  create_conditioned_graph(power_transfer_cond = 0.9, 
                           payoff_transfer_cond = 0.3, 
                           "No")
old_guard_graph_9_3 <- old_guard_both_graphs_9_3[[1]]
old_guard_cosurvival_graph_9_3 <- old_guard_both_graphs_9_3[[2]]

# Power transfer 0, payoff transfer 0.6
old_guard_both_graphs_0_6 <- 
  create_conditioned_graph(power_transfer_cond = 0, 
                           payoff_transfer_cond = 0.6, 
                           "No")
old_guard_graph_0_6 <- old_guard_both_graphs_0_6[[1]]
old_guard_cosurvival_graph_0_6 <- old_guard_both_graphs_0_6[[2]]

# Power transfer 0.3, payoff transfer 0.6
old_guard_both_graphs_3_6 <- 
  create_conditioned_graph(power_transfer_cond = 0.3, 
                           payoff_transfer_cond = 0.6, 
                           "No")
old_guard_graph_3_6 <- old_guard_both_graphs_3_6[[1]]
old_guard_cosurvival_graph_3_6 <- old_guard_both_graphs_3_6[[2]]

# Power transfer 0.6, payoff transfer 0.6
old_guard_both_graphs_6_6 <- 
  create_conditioned_graph(power_transfer_cond = 0.6, 
                           payoff_transfer_cond = 0.6, 
                           "No")
old_guard_graph_6_6 <- old_guard_both_graphs_6_6[[1]]
old_guard_cosurvival_graph_6_6 <- old_guard_both_graphs_6_6[[2]]

# Power transfer 0.9, payoff transfer 0.6
old_guard_both_graphs_9_6 <- 
  create_conditioned_graph(power_transfer_cond = 0.9, 
                           payoff_transfer_cond = 0.6, 
                           "No")
old_guard_graph_9_6 <- old_guard_both_graphs_9_6[[1]]
old_guard_cosurvival_graph_9_6 <- old_guard_both_graphs_9_6[[2]]

# Power transfer 0, payoff transfer 0.9
old_guard_both_graphs_0_9 <- 
  create_conditioned_graph(power_transfer_cond = 0, 
                           payoff_transfer_cond = 0.9, 
                           "No")
old_guard_graph_0_9 <- old_guard_both_graphs_0_9[[1]]
old_guard_cosurvival_graph_0_9 <- old_guard_both_graphs_0_9[[2]]

# Power transfer 0.3, payoff transfer 0.9
old_guard_both_graphs_3_9 <- 
  create_conditioned_graph(power_transfer_cond = 0.3, 
                           payoff_transfer_cond = 0.9, 
                           "No")
old_guard_graph_3_9 <- old_guard_both_graphs_3_9[[1]]
old_guard_cosurvival_graph_3_9 <- old_guard_both_graphs_3_9[[2]]

# Power transfer 0.6, payoff transfer 0.9
old_guard_both_graphs_6_9 <- 
  create_conditioned_graph(power_transfer_cond = 0.6, 
                           payoff_transfer_cond = 0.9, 
                           "No")
old_guard_graph_6_9 <- old_guard_both_graphs_6_9[[1]]
old_guard_cosurvival_graph_6_9 <- old_guard_both_graphs_6_9[[2]]

# Power transfer 0.9, payoff transfer 0.9
old_guard_both_graphs_9_9 <- 
  create_conditioned_graph(power_transfer_cond = 0.9, 
                           payoff_transfer_cond = 0.9, 
                           "No")
old_guard_graph_9_9 <- old_guard_both_graphs_9_6[[1]]
old_guard_cosurvival_graph_9_9 <- old_guard_both_graphs_9_9[[2]]

# Set a default layout
default_layout <- data.frame(
  x = c(-2, 0.5, 1.5, 0, -0.5, -1.3, -1.5, 2.8, -3), 
  y = c(2.5, 1.5, 0.5, 0, 2.75, 0.8, -0.8, 1.5, 1.5)
)

# Visualise network and cosurvival graphs

# Power transfer 0, payoff transfer 0
clusters_0_0 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_0_0, 
                                            payoff_transfer = 0,
                                            power_transfer = 0)

# Power transfer 0.3, payoff transfer 0
clusters_3_0 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_3_0, 
                                            payoff_transfer = 0,
                                            power_transfer = 0.3)

# Power transfer 0.6, payoff transfer 0
clusters_6_0 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_6_0, 
                                            payoff_transfer = 0,
                                            power_transfer = 0.6)

# Power transfer 0.9, payoff transfer 0
clusters_9_0 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_9_0, 
                                            payoff_transfer = 0,
                                            power_transfer = 0.9)

# Power transfer 0, payoff transfer 0.3
clusters_0_3 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_0_3, 
                                            payoff_transfer = 0.3,
                                            power_transfer = 0)

# Power transfer 0.3, payoff transfer 0.3
clusters_3_3 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_3_3, 
                                            payoff_transfer = 0.3,
                                            power_transfer = 0.3)

# Power transfer 0.6, payoff transfer 0.3
clusters_6_3 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_6_3, 
                                            payoff_transfer = 0.3,
                                            power_transfer = 0.6)

# Power transfer 0.9, payoff transfer 0.3
clusters_9_3 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_9_3, 
                                            payoff_transfer = 0.3,
                                            power_transfer = 0.9)

# Power transfer 0, payoff transfer 0.6
clusters_0_6 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_0_6, 
                                            payoff_transfer = 0.6,
                                            power_transfer = 0)

# Power transfer 0.3, payoff transfer 0.6
clusters_3_6 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_3_6, 
                                            payoff_transfer = 0.6,
                                            power_transfer = 0.3)

# Power transfer 0.6, payoff transfer 0.6
clusters_6_6 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_6_6, 
                                            payoff_transfer = 0.6,
                                            power_transfer = 0.6)

# Power transfer 0.9, payoff transfer 0.6
clusters_9_6 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_9_6, 
                                            payoff_transfer = 0.6,
                                            power_transfer = 0.9)

# Power transfer 0, payoff transfer 0.9
clusters_0_9 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_0_9, 
                                            payoff_transfer = 0.9,
                                            power_transfer = 0)

# Power transfer 0.3, payoff transfer 0.9
clusters_3_9 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_3_9, 
                                            payoff_transfer = 0.9,
                                            power_transfer = 0.3)

# Power transfer 0.6, payoff transfer 0.9
clusters_6_9 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_6_9, 
                                            payoff_transfer = 0.9,
                                            power_transfer = 0.6)

# Power transfer 0.9, payoff transfer 0.9
clusters_9_9 <- cosurvival_cluster_analysis(old_guard_cosurvival_graph_9_9, 
                                            payoff_transfer = 0.9, 
                                            power_transfer = 0.9)

# Plot the cosurvival clustering at all parameter combination
co_survival_panelled_graphs <-
  (clusters_0_0|clusters_3_0|clusters_6_0|clusters_9_0)/
  (clusters_0_3|clusters_3_3|clusters_6_3|clusters_9_3)/
  (clusters_0_6|clusters_3_6|clusters_6_6|clusters_9_6)/
  (clusters_0_9|clusters_3_9|clusters_6_9|clusters_9_9)/
  guide_area() +
  plot_layout(guides = "collect", heights = c(1, 1, 1, 1, 0.1))
ggsave(filename = "old_guard_co_survival_panelled_graphs.png", 
       plot = co_survival_panelled_graphs,
       width = 400,
       height = 325,
       units = "mm",
       path = "../Figures")
