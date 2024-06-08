################################################################################
# Description:  File with all custom functions used in other scripts.
# Date:         May 19, 2024
# Author:       Siebren Kooistra
################################################################################

# Labelling function
label_initial_coalition_size <- 
  function(string_vec) {
    lapply(string_vec, FUN = \(string) paste("Initial coalition size:", string))
  }

# Function to reformat a list of coalition whos into a string with curly
# brackets and capital actor names
reformat_coalition <- function (coalition_string) {
  # Replace square brackets with curly brackets
  coalition_string <- gsub(pattern = "\\[", 
                           replacement = "\\{", 
                           x = coalition_string, 
                           perl = TRUE)
  coalition_string <- gsub(pattern = "\\]", 
                           replacement = "\\}", 
                           x = coalition_string, 
                           perl = TRUE)
  
  # Replace integers with the corresponding letters in the alphabet
  for (i in 0:9) {
    if (grepl(pattern = i, x = coalition_string, perl = TRUE)) {
      coalition_string <- gsub(pattern = i, 
                               replacement = LETTERS[i+1], 
                               x = coalition_string, 
                               perl = TRUE)
    }
  }
  
  return(coalition_string)
}

# Function to turn a NetLogo list with square brackets and numeric
# entries into a numeric vector
netlogo_list_to_numeric_vector <- function(netlogo_list) {
  stand_dev <- netlogo_list |> str_remove_all(pattern = "\\[|\\]") |> 
    str_split_1(pattern = " ") |> 
    as.numeric()
  return(stand_dev)
}

# Line plots of URCs as a function of power transfer and the presence of
# positions, conditional on payoff transfer
power_transfer_line_plots <- function(delta_par) {
  plot <- ggplot(triad_summary[triad_summary$payoff_transfer == delta_par,], 
                 mapping = aes(x = power_transfer, 
                               y = count,
                               shape = URC_thesis_notation,
                               linetype = URC_thesis_notation)
  ) +
    geom_line() +
    geom_point(size = 3) +
    facet_wrap(facets = vars(positions_marker)) +
    labs(x = bquote(paste("Share of power transferred between allies (", zeta, ")")),
         y = "% of simulations with URC", 
         shape = "URC",
         linetype = "URC",
         subtitle = bquote(
           list(paste("Payoff transfers: ", delta == .(delta_par)), 
                paste("Maximal share of power transferred: ", 
                      tau == bgroup("{",atop(0.5 ~ textstyle("if") ~ zeta <= 0.5,
                                             zeta ~ textstyle("if") ~ zeta > 0.5), 
                                    ".")
                )
           )
         )
    ) +
    scale_x_continuous(breaks = seq(from = 0, to = 0.9, by = 0.1)) +
    scale_shape_manual(values = c("{A B C}" = "square", 
                                  "{A B}" = "circle", 
                                  "{A C}" = "triangle", 
                                  "{A}" = "cross", 
                                  "{B C}" = "diamond plus", 
                                  "{B}" = "square cross", 
                                  "{C}" = "triangle down open")) +
    theme_bw(base_family = "serif")
  return(plot)
}

# Line plots of URCs as a function of payoff transfer and the presence of
# positions, conditional on power transfer
payoff_transfer_line_plots <- function(zeta_par) {
  plot <- ggplot(triad_summary[triad_summary$power_transfer == zeta_par,], 
                 mapping = aes(x = payoff_transfer, 
                               y = count,
                               shape = URC_thesis_notation,
                               linetype = URC_thesis_notation)
  ) +
    geom_line() +
    geom_point(size = 3) +
    facet_wrap(facets = vars(positions_marker)) +
    labs(x = expression(paste("Share of payoff transferred between allies (", delta, ")")), 
         y = "% of simulations with URC", 
         shape = "URC",
         linetype = "URC",
         subtitle = bquote(
           paste("Share of power transferred between allies: ", 
                 list(
                   paste(zeta == .(zeta_par), " per tie"),
                   paste(tau == .(ifelse(zeta_par > 0.5, 
                                         yes = zeta_par, 
                                         no = 0.5)), 
                         " maximum total")
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
  return(plot)
}

# Line plots of position assignments as a function of payoff, conditional
# on power transfer
payoff_transfer_position_line_plots <- function(zeta_par) {
  plot <- ggplot(positions_summary[positions_summary$power_transfer == zeta_par, ],
                 mapping = aes(x = payoff_transfer,
                               y = 100 * relative_frequency,
                               shape = position_assignment_final,
                               linetype = position_assignment_final)
  ) +
    geom_line() +
    geom_point(size = 3) +
    labs(x = expression(paste("Share of payoff transferred between allies (", delta, ")")),
         y = "% of simulations where X obtains the position",
         shape = "Position assignment",
         linetype = "Position assignment",
         subtitle = bquote(
           paste("Share of power transferred between allies: ", 
                 list(
                   paste(zeta == .(zeta_par), " per tie"),
                   paste(tau == .(ifelse(zeta_par > 0.5, 
                                         yes = zeta_par, 
                                         no = 0.5)), 
                         " maximum total")
                 )
           )
         )
    ) +
    scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
    theme_bw(base_family = "serif")
  return(plot)
}

# Function to turn a MCTS tree as represented in NetLogo and written into the
# log file into a igraph graph
process_tree <- function (tree) {
  # Transform the string containing the tree so that it can be turned into a
  # list of lists
  tree <- tree |> 
    str_trim() |>
    gsub(pattern = "\\[", replacement = "list\\(", perl = TRUE) |>
    gsub(pattern = "\\]", replacement = "\\)", perl = TRUE) |>
    gsub(pattern = " ", replacement = ", ", perl = TRUE) |>
    gsub(pattern = '(?:(\\"\\w+), )|(?:, (\\w+\\"))', replacement = "\\1 \\2", perl = TRUE)
  
  # Read in the string as a list of lists
  tree <- eval(str2lang(tree))
  
  # Create an appropriately-sized data frame for the edgelist
  edgelist_frame <- data.frame(
    from = vector(mode = "character", length = sum(sapply(X = tree, FUN = \(x) {length(x[[6]])}))), 
    to = vector(mode = "character", length = sum(sapply(X = tree, FUN = \(x) {length(x[[6]])})))
  )
  
  # Fill the edgelist
  for (i in 1:length(tree)) {
    # Look for the first empty entry in the data frame
    first_empty_index <- which.max(edgelist_frame[,"to"] == "")
    
    # Count the number of child nodes of the current node and subtract one
    # to get a range from the first empty entry to the last empty entry needed
    # to add links to all children and not overshoot by one
    to_length_adjusted <- length(tree[[i]][[6]]) - 1
    
    # If there are any child nodes, add an edge to each one
    if (to_length_adjusted >= 0) {
      # Fill the starting points with the same value, the current parent
      edgelist_frame[
        first_empty_index:(first_empty_index + to_length_adjusted), "from"
      ] <- tree[[i]][[1]]
      # Fill the endpoints with the list of children
      edgelist_frame[
        first_empty_index:(first_empty_index + to_length_adjusted), "to"
      ] <- unlist(tree[[i]][[6]])
    }
  }
  
  # Remove stopping conditions' STOP in their list of children being
  # interpreted as a child node
  edgelist_frame <- edgelist_frame[edgelist_frame$to != "STOP", ]
  
  # Create an empty data frame for the node attributes
  node_attributes <- data.frame(
    id = vector(mode = "character", length = length(tree)),
    type = vector(mode = "character", length = length(tree)),
    value = vector(mode = "character", length = length(tree)),
    payoffs = vector(mode = "character", length = length(tree)),
    visits = vector(mode = "character", length = length(tree))
  )
  
  # Fill the node attributes by going over them one by one and extracting that
  # attribute for each node. Take into account that lists have to be unlisted
  # to be added in the appropriate way (that they're lists is not relevant any
  # more either way)
  for (n in 1:length(node_attributes)) {
    node_attributes[, n] <- sapply(
      tree, FUN = \(x) {
        ifelse(is.list(x[[n]]), 
               yes = unlist(x[[n]]) |> paste(collapse = " "), 
               no = x[[n]])
      }
    )
  }
  
  # Use an igraph function to 
  tree_graph <- graph_from_data_frame(edgelist_frame, 
                                      directed = TRUE, 
                                      vertices = node_attributes)
  
  # Return the completed tree
  return(tree_graph)
} 

# Function to iteratively compute the after-transfer power vector
power_transfer_itercomp <- function(gamma_nonnet, zeta, tau, P, M, epsilon, debug = FALSE) {
  # Determine what share of transferable power each agent will transfer
  power_transfer_shares <- matrix(pmin(rowSums(M * t(P))^-1, 1), nrow = length(gamma_nonnet), ncol = 1)
  
  # Determine what share of each agent's power is non-transferable
  power_residuals <- (1 - matrix(pmin(tau, zeta * rowSums(M * t(P))), nrow = length(gamma_nonnet), ncol = 1))
  
  # Compartmentalise non-transferable and transferable power
  gamma_total <- diag(M) * power_residuals * gamma_nonnet
  gamma_transfers <- (1 - power_residuals) * gamma_nonnet
  
  # Create an empty data frame to plot the iterative calculation of power levels
  if (debug == TRUE) {
    debug_df <- as.data.frame(matrix(nrow = 1, ncol = 1 + length(gamma)))
    colnames(debug_df) <- c("iteration", paste0("power_", LETTERS[1:length(gamma)]))
    counter <- 0
    debug_df$iteration <- counter
    for (i in 1:length(gamma)) {
      debug_df[counter + 1,paste0("power_", LETTERS[i])] <- gamma[i]
    }
  }
  
  # As long as the power that has not been definitively assigned is more than
  # a share epsilon of the total power around...
  while (sum(gamma_transfers) >= epsilon * sum(gamma_nonnet)) {
    # Transfer power trough the network
    gamma_transfers <-  (M * P) %*% (power_transfer_shares * gamma_transfers)
    
    # Store some of the transferred power as non-transferable power
    gamma_total <- gamma_total + power_residuals * gamma_transfers
    
    # Transfer the rest of the transferred power
    gamma_transfers <- (1 - power_residuals) * gamma_transfers
    
    # For a row of the debug plotting data frame
    if (debug == TRUE) {
      counter <- counter + 1
      debug_df[counter + 1, "iteration"] <- counter
      for (i in 1:length(gamma_total)) {
        debug_df[counter + 1, paste0("power_", LETTERS[i])] <- gamma_total[i]
      }
    }
  }
  
  # Assign the residuals of transferable power to the agent that last received
  # it
  gamma_total <- gamma_total + gamma_transfers
  
  
  # Plot the iterative calculation of power levels
  if (debug == TRUE) {
    debug_plot <- debug_df |> pivot_longer(cols = !iteration, 
                                           names_to = "player", 
                                           names_pattern = "([A-Z])",
                                           values_to = "power") |>
      ggplot(mapping = aes(x = iteration,
                           y = power,
                           linetype = player)
      ) +
      geom_line() +
      scale_x_continuous(breaks = unique(debug_df$iteration)) +
      labs(x = "Iteration",
           y = "Power of player",
           linetype = "Player") +
      theme_classic()
    print(debug_plot)
    print(debug_df)
  }
  
  # Return the final power vector
  return(gamma_total)
}

# Function to create a graph based on runs with a given network structure,
# power transfer parameter and payoff transfer parameter
create_conditioned_graph <- function(power_transfer_cond, 
                                     payoff_transfer_cond,
                                     satisficers_cond) {
  network_structure_cond <- "[\n[0 \"--\" 4]\n[0 \"--\" 5]\n[0 \"--\" 8]\n[1 \"--\" 2]\n[1 \"--\" 3]\n[1 \"--\" 4]\n[2 \"--\" 3]\n[2 \"--\" 7]\n[3 \"--\" 5]\n[3 \"--\" 6]\n]"
  
  old_guard_subset <- old_guard_data |> 
    filter(manual.patronage.network.input == network_structure_cond,
           patronage.power.transfer == power_transfer_cond,
           client.to.patron.payoff.transfer == payoff_transfer_cond
    )
  
  old_guard_lastentries_subset <- old_guard_lastentries |> 
    filter(network_structure == network_structure_cond,
           power_transfer == power_transfer_cond,
           payoff_transfer == payoff_transfer_cond,
           satisficers_present == satisficers_cond
    )
  
  # Count the number of URCs that each agent ended up in over the 100 runs
  old_guard_survival <- sapply(0:8, FUN = \(x) {
    sum(str_detect(string = old_guard_lastentries_subset$URC_turtles, 
                   pattern = as.character(x)))
  })
  
  old_guard_cosurvival <- matrix(nrow = 9, ncol = 9)
  
  combinations <- combn(1:9, m = 2) |> t() |> as.data.frame()
  
  for (i in 1:nrow(combinations)) {
    old_guard_cosurvival[combinations[i, 1], combinations[i, 2]] <- 
      sum(
        str_detect(
          string = old_guard_lastentries_subset$URC_turtles, 
          pattern = paste0(combinations[i, 1], ".*", combinations[i, 2], 
                           "|", 
                           combinations[i, 2], ".*", combinations[i, 1]
          )
        )
      )
  }
  
  old_guard_cosurvival[is.na(old_guard_cosurvival)] <- 0
  old_guard_cosurvival <- old_guard_cosurvival + t(old_guard_cosurvival)
  diag(old_guard_cosurvival) <- old_guard_survival
  
  # Calculate the mean power of each agent over the 100 runs
  old_guard_power <- old_guard_lastentries_subset$residual_power |>
    sapply(FUN = str_remove_all, pattern = "\\[|\\]") |>
    str_split(pattern = " ") |>
    sapply(FUN = as.numeric) |>
    rowSums() / 100
  
  # Extract an edge list from the data frame
  old_guard_network_structure <- 
    last(old_guard_subset$manual.patronage.network.input) |>
    str_remove_all(pattern = "(\\[\\n\\[)|(\\]\\n\\]$)") |>
    str_split_1(pattern = "\\]\\n\\[") |>
    str_remove_all(pattern = '\"--\"') |>
    sapply(FUN = str_split, pattern = "  ") |>
    sapply(FUN = as.numeric) |>
    t()
  
  if (power_transfer_cond == 0 & payoff_transfer_cond == 0) {
    conditioned_graph <- make_empty_graph(n = 9)
    cosurvival_graph <- graph_from_adjacency_matrix(old_guard_cosurvival,
                                                    mode = "undirected",
                                                    weighted = "cosurvival")
    
    # Add percentage survival and mean power as vertex attributes, and calculate
    # eigenvector centrality
    V(conditioned_graph)$letter_name <- LETTERS[1:9]
    V(conditioned_graph)$survival <- old_guard_survival
    V(conditioned_graph)$power <- old_guard_power
    V(conditioned_graph)$centrality <- centr_eigen(conditioned_graph)$vector
    
    V(cosurvival_graph)$letter_name <- LETTERS[1:9]
    V(cosurvival_graph)$survival <- old_guard_survival
    V(cosurvival_graph)$power <- old_guard_power
    V(cosurvival_graph)$centrality <- centr_eigen(conditioned_graph)$vector
    
  } else {
    # Create an appropriately-sized data frame for the edgelist
    old_guard_network_frame <- data.frame(
      from = vector(mode = "character", 
                    length = nrow(old_guard_network_structure)), 
      to = vector(mode = "character", 
                  length = nrow(old_guard_network_structure))
    )
    
    # Fill the data frame
    for (i in 1:nrow(old_guard_network_structure)) {
      old_guard_network_frame[i, "from"] <- 
        old_guard_network_structure[i, 1] + 1
      old_guard_network_frame[i, "to"] <- 
        old_guard_network_structure[i, 2] + 1
    }
    
    old_guard_network_frame <- unique(old_guard_network_frame)
    
    vertex_attributes <- 
      data.frame(name = 1:9,
                 letter_name = LETTERS[1:9],
                 survival = old_guard_survival, 
                 power = old_guard_power)
    
    # Create a graph based on the edge list
    conditioned_graph <- graph_from_data_frame(d = old_guard_network_frame, 
                                               directed = FALSE, 
                                               vertices = vertex_attributes)
    
    cosurvival_graph <- graph_from_adjacency_matrix(old_guard_cosurvival,
                                                    mode = "undirected",
                                                    weighted = "cosurvival")
    V(cosurvival_graph)$letter_name <- LETTERS[1:9]
    V(cosurvival_graph)$survival <- old_guard_survival
    V(cosurvival_graph)$power <- old_guard_power
    V(cosurvival_graph)$centrality <- centr_eigen(conditioned_graph)$vector
    
    # Add percentage survival and mean power as vertex attributes, and calculate
    # eigenvector centrality
    V(conditioned_graph)$centrality <- centr_eigen(conditioned_graph)$vector
  }
  
  return(list(conditioned_graph, cosurvival_graph))
}

# Function to run a cosurvival analysis on a cosurvival graph
cosurvival_cluster_analysis <- function(cosurvival_graph, 
                                        payoff_transfer, 
                                        power_transfer) {
  # Experiment with clustering one cosurvival graph in various ways
  #   Extract an adjacency matrix from the cosurvival graph
  closeness_matrix <- as_adjacency_matrix(cosurvival_graph,
                                          attr = "cosurvival")
  
  # Turn the adjacency matrix into a distance matrix
  distance_matrix <- as.dist(100 - closeness_matrix)
  
  # Carry out hierarchical clustering using the distance matrix
  cosurvival_HC_1 <- hclust(d = distance_matrix, method = "complete")
  
  # Single linkage optimisation clustering
  plot(cosurvival_HC_1)
  fuse_heights_1 <- cosurvival_HC_1$height
  partition_1 <- cutree(cosurvival_HC_1, h = 90)
  partition_1_auto <- cutree(cosurvival_HC_1, 
                             h = fuse_heights_1[which.max(diff(fuse_heights_1[-8]))])
  if (length(unique(partition_1)) %in% 2:8) {
    rect.hclust(cosurvival_HC_1,length(unique(partition_1)), 
              border = "purple")
  }
  
  V(cosurvival_graph)$partition_1 <- partition_1
  
  cluster_plot_1 <- ggraph(graph = cosurvival_graph, 
                           layout = "manual", 
                           x = default_layout$x, 
                           y = default_layout$y) +
    geom_edge_link(mapping = aes(edge_width = cosurvival, alpha = cosurvival)) +
    geom_node_circle(mapping = aes(r = 0.2, 
                                   fill = as.factor(partition_1))) +
    geom_node_text(mapping = aes(label = survival), nudge_y = -0.5) +
    geom_node_text(mapping = aes(label = letter_name)) +
    coord_fixed() +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                                 "#0072B2","#D55E00", "#CC79A7", "#999999",
                                 "ghostwhite")) +
    labs(caption = "Number under node name label denotes of how many URCs over 100 simulations an agent is part.",
         fill = "Clustering via single linkage hierarchical clustering") +
    theme_void(base_size = 20) +
    theme(plot.background = element_rect(fill = NA))
  
  return(
    cluster_plot_1 + 
      labs(caption = bquote(
        list(delta == .(payoff_transfer),
             zeta == .(power_transfer)#,
            # Decrepated code for showing automated thresholds
             #"Cosurvival grouping threshold" == 
             #.(100 - fuse_heights_1[which.max(diff(fuse_heights_1[-8]))])
        )
      )
      ) +
      scale_edge_width_continuous(name = "Co-survival", limits = c(1, 50)) + 
      scale_edge_alpha_continuous(name = "Co-survival", limits = c(1, 50)) +
      guides(fill = "none") +
      theme(legend.position = "bottom")
  )
}
