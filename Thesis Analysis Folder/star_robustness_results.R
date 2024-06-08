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
star_robustness_data <- read.csv("18_05_2024_star_robustness_test.csv",
                                 skip = 6)

# Focus on the final entries
star_robustness_lastentries <- star_robustness_data |>
  group_by(X.run.number.) |>
  summarise(seed = last(current.random.seed.1),
            initial_coalition_size = last(initial.coalition.size),
            payoff_transfer = last(client.to.patron.payoff.transfer),
            power_transfer = last(patronage.power.transfer),
            max_power_transfer = last(max.patronage.power.transfer),
            robustness_payoff = case_match(
              last(constant.sum.payoff.transfers.),
              "true" ~ "Payoff transferred like power",
              "false" ~ "Direct, positive-sum payoff transfers"),
            robustness_power = case_match(
              last(one.step.power.transfers.),
              "true" ~ "Power transferred one step",
              "false" ~ "Power transferred iteratively"),
            residual_power_levels = last(manually.specified.power),
            positions_power = last(positions.power.string),
            URC_turtles = last(sort..who..of.turtles.with..in.coalition..),
            URC_thesis_notation = reformat_coalition(URC_turtles),
            URC_size = URC_turtles |> str_split_1(pattern = " ") |> length(),
            residual_power = 
              last(map..a.....residual.power..of.a..sort.on..who..turtles),
            network_power = 
              last(map..a.....network.power..of.a..sort.on..who..turtles),
            initial_network_power = 
              first(map..a.....network.power..of.a..sort.on..who..turtles)
  )

# Summarise the last entries across parameter combinations
star_robustness_summary <- star_robustness_lastentries |>
  group_by(payoff_transfer, 
           power_transfer,
           max_power_transfer,
           URC_thesis_notation, 
           URC_size, 
           robustness_payoff,
           robustness_power) |>
  summarise(count = n()) |>
  ungroup() |>
  group_by(payoff_transfer, power_transfer) |>
  mutate(
    relative_frequency = count / sum(count)
  )

# Add coalitions that never occur to the summary, to appropriately show
# which URCs are rare

# List all possible URCs
star_robustness_potential_coalitions <- sapply(
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
star_robustness_expanded_summary <- cross_join(
  unique(star_robustness_summary[,c("payoff_transfer", 
                                    "power_transfer",
                                    "robustness_payoff",
                                    "robustness_power")]),
  data.frame(URC_thesis_notation = unlist(star_robustness_potential_coalitions),
             URC_size = str_count(
               string = unlist(star_robustness_potential_coalitions), 
               pattern = "[A-Z]"),
             count = 0)
)

# Filter out the URCs that are already in the data
star_robustness_expanded_summary <- anti_join(star_robustness_expanded_summary,
                                              star_robustness_summary, 
                                              by = c("payoff_transfer", 
                                                     "power_transfer",
                                                     "robustness_payoff",
                                                     "robustness_power", 
                                                     "URC_thesis_notation", 
                                                     "URC_size")
)

# Add the URCs that never appear
star_robustness_summary <- rbind(star_robustness_summary,
                                 star_robustness_expanded_summary)

# Analysis of URC compositions ---------------------------------

# Create bar plots of the URC distribution per robustness specification
# combination (constant sum payoff transfers and/or one-step power transfers)
# to compare with the barplot in the main analysis file

# Constant sum payoff transfers, no one-step power transfers
(star_results_urc_barplot_yes_robust_payoff_no_robust_power <- 
   ggplot(
     star_robustness_summary[
       star_robustness_summary$robustness_payoff == 
         "Payoff transferred like power" &
         star_robustness_summary$robustness_power == 
         "Power transferred iteratively",], 
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
                label_bquote(rows = atop(
                  textstyle("Payoff\ntransfer: "),
                  atop(
                    textstyle(delta == .(payoff_transfer)),
                    textstyle(tau[delta] == .(
                      ifelse(payoff_transfer > 0.5, 
                             yes = payoff_transfer, 
                             no = 0.5)
                    )
                    )
                  )
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

# One-step power transfers, no constant sum payoff transfers
(star_results_urc_barplot_no_robust_payoff_yes_robust_power <- 
    ggplot(
      star_robustness_summary[
        star_robustness_summary$robustness_payoff == 
          "Direct, positive-sum payoff transfers" &
          star_robustness_summary$robustness_power == 
          "Power transferred one step",], 
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

# Constant sum payoff transfers, one-step power transfers
(star_results_urc_barplot_yes_robust_payoff_yes_robust_power <- 
    ggplot(
      star_robustness_summary[
        star_robustness_summary$robustness_payoff == 
          "Payoff transferred like power" &
          star_robustness_summary$robustness_power == 
          "Power transferred one step",], 
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
                 label_bquote(rows = atop(
                   textstyle("Payoff\ntransfer: "),
                   atop(
                     textstyle(delta == .(payoff_transfer)),
                     textstyle(tau[delta] == .(
                       ifelse(payoff_transfer > 0.5, 
                              yes = payoff_transfer, 
                              no = 0.5)
                     )
                     )
                   )
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
    labs(x = "URC as % of URCs at parameter combination",
         y = "URC (sorted by size from large above to small below)",
         subtitle = "Robustness check with constant-sum payoff transfers and one-step power transfers") +
    theme_bw(base_family = "serif", base_size = 11) +
    theme(strip.text.y = element_text(angle = 0))
)

# Save the plot for the case where both robustness checks are active
ggsave(filename = "star_robustness_results_URCs_barplot.png",
       plot = star_results_urc_barplot_yes_robust_payoff_yes_robust_power,
       width = 200,
       height = 250,
       units = "mm",
       path = "C:/Users/Siebren Kooistra/Documents/Opleiding/Linköpings Universitet/Master Thesis/Figures")

# Payoff comparison -------------------------------------------------------

# Set up the payoff exchange network matrix
P <- matrix(c(0, 1, 1, 1, 1,
              0, 0, 0, 0, 0,
              0, 0, 0, 0, 0,
              0, 0, 0, 0, 0,
              0, 0, 0, 0, 0), 
            nrow = 5, ncol = 5,
            byrow = TRUE)
P_sym <- P + t(P)

# Coalition membership matrix
N <- matrix(c(1, 1, 1, 1, 1),
            nrow = 5, ncol = 1,
            byrow = TRUE)

M <- N %*% t(N)

# Pre-transfer payoffs if the URC is {A, B, C, D, E}
gamma <- matrix(c(4.6/24.4, 4.9/24.4, 4.2/24.4, 5.3/24.4, 5.4/24.4), 
                nrow = 5, ncol = 1)

# Pre-transfer payoffs if the URC is {A}
gamma_alt <- matrix(c(0.98, 0, 0, 0, 0), 
                    nrow = 5, ncol = 1)

# Plot after-transfer payoffs as a function of delta

#   Set up a data frame to store the results
power_df <- data.frame(
  URC = rep(c("{A}", "{A B C D E}"), each = 100),
  zeta = rep(seq(from = 0, to = 0.99, by = 0.01), times = 2),
  power_A = NA,
  power_B = NA,
  power_C = NA,
  power_D = NA,
  power_E = NA
)

#   Fill the data frame
for (i in 1:nrow(power_df)) {
  if(power_df[i,"URC"] == "{A}") 
  {gamma_use <- gamma_alt} else 
  {gamma_use <- gamma}
  
  member_power_temp <- power_transfer_itercomp(gamma_use, power_df[i,"zeta"], 
                                               max(0.5, power_df[i,"zeta"]), 
                                               P_sym, M, 0.00001)
  
  power_df[i, 3:7] <- member_power_temp
}


#   Create the plot
plot_df <- power_df |> pivot_longer(cols = !URC:zeta, 
                                    names_to = "player", 
                                    names_pattern = "([A-Z])",
                                    values_to = "power")

(star_robustness_compared_posttransfer_payoffs_lineplot <- 
    ggplot(plot_df, mapping = aes(x = zeta,
                                  y = power,
                                  linetype = player)
    ) +
    geom_line() +
    facet_grid(cols = vars(URC),
               labeller = "label_both") +
    labs(x = bquote(
      paste("Share of payoff transferred between allies ", 
            bgroup("(", 
                   list(paste(delta, " per tie"), 
                        paste(tau[delta] == bgroup("{",
                                            atop(0.5 ~ textstyle("if") ~ 
                                                   delta <= 0.5,
                                                 delta ~ textstyle("if") ~ 
                                                   delta > 0.5), 
                                            "."), " max total")),
                   ")")
      )
    ),
    y = "After-transfer payoff of player",
    linetype = "Player") +
    scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
    theme_bw(base_family = "serif"))
ggsave(filename = "star_robustness_compared_posttransfer_payoffs_lineplot.png", 
       plot = star_robustness_compared_posttransfer_payoffs_lineplot,
       width = 200,
       height = 150,
       units = "mm",
       path = "C:/Users/Siebren Kooistra/Documents/Opleiding/Linköpings Universitet/Master Thesis/Figures")

