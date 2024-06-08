# sharing-spoils-master-thesis
Repository for my MSc Thesis project "Sharing the spoils of rule" (2024) expanding the model of Acemoglu et al. (2008) from "Coalition formation in non-democracies"

As further supplementary information to this thesis, there is the NetLogo 6.4.0 (Wilensky, 1999) implementation of the model I present and there is a set of R 4.3.3 (R Core Team, 2024) files where I analyse the experimental results using `ggplot2` (Wickham, 2016) extended by `ggraph` (Pedersen, 2024a) and `patchwork` (Pedersen, 2024b) for visualisation, `kableExtra` (Zhu, 2024) for Table 3.1 of the thesis and other `tidyverse` packages for data manipulation (Wickham et al., 2019). These files are provided as a folder with the following structure:
* `MAIN\_AcemogluEtal2008\_ABM.nlogo`, a NetLogo 6.4.0 model that contains the model code, user interface and model description.
* A folder "Figures" to which all figures created by the analysis scripts will be added and contains all figures needed to properly display the documentation in the `MAIN\_AcemogluEtal2008\_ABM.nlogo` file:
  - `flowchart_game_scheduling.png`
  - `comprehensive_tree_growing_algorithm.png`
  - `comprehensive_tree_evaluation_algorithm.png`
  - `MCTS_algorithm.png`
  - `MCTS_example_thirty_trials.png`
  - `MCTS_example_hundred_trials.png`
  - `flowchart_MCTS_expansion_algorithm.png`
  - `flowchart_MCTS_playout_algorithm.png`
  - `flowchart_MCTS_back_propagation_algorithm.png`
  - `flowchart_combinatorics_algorithm.png`
* A folder "Thesis Analysis Folder" containing the data and scripts needed to reproduce my analyses of the experimental results:
  - `28_04_2024_triad_experiment.csv` containing data for the main results for the three-agent toy scenario.
  - `29_04_2024_variable_cohesion_small_experiment.csv` containing data from the main results for the four-agent variable network cohesion scenario.
  - `28_04_2024_star_experiment.csv` containing data for the main results for the five-agent star scenario.
  - `11_05_2024_old_guard_experiment.csv` containing data from the main results for the nine-agent 'old guard' scenario.
  - `05_05_2024_triad_robustness_test.csv` containing data for the robustness test in the triad scenario.
  - `18_05_2024_star_robustness_test.csv` containing data for the robustness test in the star scenario.
  - `30_04_2024_parameter_exploration.csv` containing data for the latest parameter exploration.
  - `functions.R` an `R` script containing all custom functions used by the other `R` scripts.
  - `triad_results.R` an `R` script analysing the three-agent toy scenario.
  - `variable_cohesion_small_results.R` an `R` script analysing the four-agent scenario with a variable-cohesion network.
  - `star_results.R` an `R` script analysing the five-agent star scenario.
  - `old_guard_results.R` an `R` script analysing the nine-agent 'old guard' scenario.
  - `triad_robustness_results.R` an `R` script creating the robustness check results for the triad scenario.
  - `star_robustness_results.R` an `R` script creating the robustness check results for the star scenario.
  - `parameter_space_exploration_analysis.R` an `R` script for the MCTS calibration.
 
**References**

Acemoglu, D., Egorov, G., & Sonin, K. (2008). Coalition formation in non-democracies. _Review of Economic Studies_, _75_(4), 987–1009. https://doi.org/10.1111/j.1467-937X.2008.00503.x

Pedersen, T. L. (2024a). _Ggraph: An implementation of grammar of graphics for graphs and networks_ [R package version 2.2.1]. https://CRAN.R-project.org/package=ggraph

Pedersen, T. L. (2024b). _Patchwork: The composer of plots_ [R package version 1.2.0]. https://CRAN.R-project.org/package=patchwork

R Core Team. (2024). _R: A language and environment for statistical computing_. R Foundation for Statistical Computing. Vienna, Austria. https://www.R-project.org/

Wickham, H. (2016). _Ggplot2: Elegant graphics for data analysis._ Springer-Verlag New York. https://ggplot2.tidyverse.org

Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D., François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Pedersen, T. L., Miller, E., Bache, S. M., Müller, K., Ooms, J., Robinson, D., Seidel, D. P., Spinu, V., . . . Yutani, H. (2019). _Welcome to the tidyverse_. Journal of Open Source Software, _4_(43), 1686. https://doi.org/10.21105/joss.01686

Wilensky, U. (1999). _NetLogo_ (Version 6.4.0). Evanston, IL, Center for Connected Learning; Computer-Based Modeling, Northwestern University. http://ccl.northwestern.edu/netlogo/

Zhu, H. (2024). _Kableextra: Construct complex table with 'kable' and pipe syntax_ [R package version 1.4.0]. https://CRAN.R-project.org/package=kableExtra
