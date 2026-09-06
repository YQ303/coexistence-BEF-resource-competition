library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "cowplot",
    "dplyr",
    "ggnewscale",
    "ggplot2",
    "ggpubr",
    "readr",
    "tidyr"
  ),
  seed = 20260801
)

# Source function definitions and small global objects only.
tar_source()

list(
  tar_target(
    model_function,
    CR_model
  ),

  # Transgressive-overyielding and positive-NBE regions -------------------
  tar_target(
    parameters_bef_trans,
    prepare_para_resource_trans(n = 25, lb = 1, ub = 1)
  ),
  tar_target(
    parameters_bef_notrans,
    prepare_para_resource_notrans(n = 25, lb = 1, ub = 1)
  ),
  tar_target(
    parameters_bef_nbe_upper,
    prepare_para_resource_nbe(n = 25, lb = 1, ub = 1)
  ),
  tar_target(
    simulation_bef_trans,
    simulation_CR(parameters_bef_trans)
  ),
  tar_target(
    simulation_bef_notrans,
    simulation_CR(parameters_bef_notrans)
  ),
  tar_target(
    simulation_bef_nbe_upper,
    simulation_CR(parameters_bef_nbe_upper)
  ),
  tar_target(
    plot_bef_trans,
    bef_region(simulation_bef_trans, var = "trans")
  ),
  tar_target(
    plot_bef_nbe_trans,
    bef_region(
      simulation_bef_trans,
      var = "NBE",
      col = scales::alpha("#D53E4F", 0.075)
    )
  ),
  tar_target(
    plot_bef_notrans,
    bef_region(simulation_bef_notrans, var = "trans")
  ),
  tar_target(
    plot_bef_nbe_lower,
    bef_region(
      simulation_bef_notrans,
      var = "NBE",
      col = scales::alpha("#D53E4F", 0.075)
    )
  ),
  tar_target(
    plot_bef_nbe_upper,
    bef_region(
      simulation_bef_nbe_upper,
      var = "NBE",
      col = scales::alpha("#D53E4F", 0.075)
    )
  ),
  tar_target(
    figure_bef,
    compose_bef_figure(
      plot_bef_trans,
      plot_bef_nbe_trans,
      plot_bef_notrans,
      plot_bef_nbe_lower
    )
  ),
  tar_target(
    figure_bef_file,
    write_plot_file(
      figure_bef,
      "output/figures/trans_nbe.png",
      width = 6,
      height = 4
    ),
    format = "file"
  ),
  tar_target(
    figure_bef_nbe_upper_file,
    write_plot_file(
      plot_bef_nbe_upper,
      "output/figures/nbe-upper-region.png",
      width = 3,
      height = 2.25
    ),
    format = "file"
  ),

  # Resource-supply gradient ----------------------------------------------
  tar_target(
    parameters_resource_main,
    prepare_para_resource_main(n = 10, lb = 1.01, ub = 0.99)
  ),
  tar_target(
    parameters_resource_supplement,
    prepare_para_resource_sup(n = 5, lb = 1.01, ub = 0.99)
  ),
  tar_target(
    simulation_resource_main,
    simulation_CR(parameters_resource_main)
  ),
  tar_target(
    simulation_resource_supplement,
    simulation_CR(parameters_resource_supplement)
  ),
  tar_target(
    plot_resource_main,
    add_connector_lines(
      RC_graph(
        simulation_resource_main,
        exa = c(2, 5, 9),
        fig_labs = letters[1:6],
        x_var = "r_v",
        x_label = expression("Resource l's supply"),
        dir = "inc"
      ),
      x = matrix(
        c(0.215, 0.215, 0.46, 0.49, 0.785, 0.785),
        ncol = 2,
        byrow = TRUE
      )
    )
  ),
  tar_target(
    plot_resource_supplement,
    add_connector_lines(
      RC_graph(
        simulation_resource_supplement,
        exa = c(1, 3, 5),
        fig_labs = letters[1:6],
        x_var = "r_v",
        x_label = expression("Resource l's supply"),
        dir = "inc"
      ),
      x = matrix(
        c(0.13, 0.215, 0.495, 0.495, 0.86, 0.785),
        ncol = 2,
        byrow = TRUE
      )
    )
  ),
  tar_target(
    figure_resource_main_file,
    write_plot_file(
      plot_resource_main,
      "output/figures/resource-supply-main.png",
      width = 9,
      height = 6.5
    ),
    format = "file"
  ),
  tar_target(
    figure_resource_supplement_file,
    write_plot_file(
      plot_resource_supplement,
      "output/figures/resource-supply-supplement.png",
      width = 9,
      height = 6.5
    ),
    format = "file"
  ),

  # Non-limiting-resource consumption gradient ---------------------------
  tar_target(
    parameters_nonlimiting_main,
    prepare_para_nonlim_main(n = 10)
  ),
  tar_target(
    parameters_nonlimiting_supplement,
    prepare_para_nonlim_sup(n = 10)
  ),
  tar_target(
    simulation_nonlimiting_main,
    simulation_CR(parameters_nonlimiting_main)
  ),
  tar_target(
    simulation_nonlimiting_supplement,
    simulation_CR(parameters_nonlimiting_supplement)
  ),
  tar_target(
    plot_nonlimiting_main,
    add_connector_lines(
      RC_graph(
        simulation_nonlimiting_main,
        exa = c(2, 5, 9),
        fig_labs = letters[1:6],
        x_var = "qil",
        x_label = expression(
          paste("Species i's consumption of resource l (", q[il]^minute, ")")
        ),
        dir = "desc"
      ),
      x = matrix(
        c(0.215, 0.215, 0.46, 0.49, 0.785, 0.785),
        ncol = 2,
        byrow = TRUE
      )
    )
  ),
  tar_target(
    plot_nonlimiting_supplement,
    add_connector_lines(
      RC_graph(
        simulation_nonlimiting_supplement,
        exa = c(2, 5, 9),
        fig_labs = letters[1:6],
        x_var = "qil",
        x_label = expression(
          paste("Species i's consumption of resource l (", q[il]^minute, ")")
        ),
        dir = "desc"
      ),
      x = matrix(
        c(0.215, 0.215, 0.46, 0.49, 0.785, 0.785),
        ncol = 2,
        byrow = TRUE
      )
    )
  ),
  tar_target(
    figure_nonlimiting_main_file,
    write_plot_file(
      plot_nonlimiting_main,
      "output/figures/nonlimiting-consumption-main.png",
      width = 9,
      height = 6.5
    ),
    format = "file"
  ),
  tar_target(
    figure_nonlimiting_supplement_file,
    write_plot_file(
      plot_nonlimiting_supplement,
      "output/figures/nonlimiting-consumption-supplement.png",
      width = 9,
      height = 6.5
    ),
    format = "file"
  ),

  # Limiting-resource consumption gradient -------------------------------
  tar_target(
    parameters_limiting_main,
    prepare_para_lim_main(n = 10)
  ),
  tar_target(
    parameters_limiting_supplement,
    prepare_para_lim_sup(n = 10)
  ),
  tar_target(
    simulation_limiting_main,
    simulation_CR(parameters_limiting_main)
  ),
  tar_target(
    simulation_limiting_supplement,
    simulation_CR(parameters_limiting_supplement)
  ),
  tar_target(
    plot_limiting_main,
    add_connector_lines(
      RC_graph(
        simulation_limiting_main,
        exa = c(2, 5, 9),
        fig_labs = letters[1:6],
        x_var = "qik",
        x_label = expression(
          paste("Species i's consumption of resource k (", q[ik]^minute, ")")
        ),
        dir = "inc"
      ),
      x = matrix(
        c(0.2, 0.215, 0.445, 0.49, 0.775, 0.785),
        ncol = 2,
        byrow = TRUE
      )
    )
  ),
  tar_target(
    plot_limiting_supplement,
    add_connector_lines(
      RC_graph(
        simulation_limiting_supplement,
        exa = c(2, 5, 9),
        fig_labs = letters[1:6],
        x_var = "qik",
        x_label = expression(
          paste("Species i's consumption of resource k (", q[ik]^minute, ")")
        ),
        dir = "inc"
      ),
      x = matrix(
        c(0.2, 0.215, 0.455, 0.49, 0.79, 0.785),
        ncol = 2,
        byrow = TRUE
      )
    )
  ),
  tar_target(
    figure_limiting_main_file,
    write_plot_file(
      plot_limiting_main,
      "output/figures/limiting-consumption-main.png",
      width = 9,
      height = 6.5
    ),
    format = "file"
  ),
  tar_target(
    figure_limiting_supplement_file,
    write_plot_file(
      plot_limiting_supplement,
      "output/figures/limiting-consumption-supplement.png",
      width = 9,
      height = 6.5
    ),
    format = "file"
  ),

  # Parameter tables ------------------------------------------------------
  tar_target(
    parameter_table_bef_trans,
    simulation_parameter_table(
      parameters_bef_trans,
      "Transgressive overyielding and positive NBE guaranteed"
    )
  ),
  tar_target(
    parameter_table_bef_notrans,
    simulation_parameter_table(
      parameters_bef_notrans,
      "Transgressive overyielding absent"
    )
  ),
  tar_target(
    parameter_table_bef_nbe_upper,
    simulation_parameter_table(
      parameters_bef_nbe_upper,
      "Positive-NBE region above the coexistence region"
    )
  ),
  tar_target(
    parameter_table_resource,
    dplyr::bind_rows(
      simulation_parameter_table(
        parameters_resource_main,
        "Varying resource-$l$ supply with $T_{ij}<1$"
      ),
      simulation_parameter_table(
        parameters_resource_supplement,
        "Varying resource-$l$ supply with $T_{ij}>1$"
      )
    )
  ),
  tar_target(
    parameter_table_nonlimiting,
    dplyr::bind_rows(
      simulation_parameter_table(
        parameters_nonlimiting_main,
        "Varying $q_{il}^{\\prime}$ with $q_{ik}>q_{jk}$"
      ),
      simulation_parameter_table(
        parameters_nonlimiting_supplement,
        "Varying $q_{il}^{\\prime}$ with $q_{ik}<q_{jk}$"
      )
    )
  ),
  tar_target(
    parameter_table_limiting,
    dplyr::bind_rows(
      simulation_parameter_table(
        parameters_limiting_main,
        "Varying $q_{ik}^{\\prime}$: ND decreases CE"
      ),
      simulation_parameter_table(
        parameters_limiting_supplement,
        "Varying $q_{ik}^{\\prime}$: ND increases CE"
      )
    )
  ),
  tar_target(
    parameter_table_all,
    dplyr::bind_rows(
      parameter_table_bef_trans,
      parameter_table_bef_notrans,
      parameter_table_bef_nbe_upper,
      parameter_table_resource,
      parameter_table_nonlimiting,
      parameter_table_limiting
    )
  ),
  tar_target(
    parameter_table_file,
    write_csv_file(
      parameter_table_all,
      "output/tables/simulation-parameters.csv"
    ),
    format = "file"
  ),

  # Reusable outputs ------------------------------------------------------
  tar_target(
    all_simulations,
    list(
      bef_trans = simulation_bef_trans,
      bef_notrans = simulation_bef_notrans,
      bef_nbe_upper = simulation_bef_nbe_upper,
      resource_main = simulation_resource_main,
      resource_supplement = simulation_resource_supplement,
      nonlimiting_main = simulation_nonlimiting_main,
      nonlimiting_supplement = simulation_nonlimiting_supplement,
      limiting_main = simulation_limiting_main,
      limiting_supplement = simulation_limiting_supplement
    )
  ),
  tar_target(
    all_plots,
    list(
      bef_trans = plot_bef_trans,
      bef_nbe_trans = plot_bef_nbe_trans,
      bef_notrans = plot_bef_notrans,
      bef_nbe_lower = plot_bef_nbe_lower,
      bef_nbe_upper = plot_bef_nbe_upper,
      resource_main = plot_resource_main,
      resource_supplement = plot_resource_supplement,
      nonlimiting_main = plot_nonlimiting_main,
      nonlimiting_supplement = plot_nonlimiting_supplement,
      limiting_main = plot_limiting_main,
      limiting_supplement = plot_limiting_supplement
    )
  ),
  tar_target(
    simulation_summary,
    collect_simulation_results(all_simulations)
  ),
  tar_target(
    simulation_results_file,
    write_rds_file(
      all_simulations,
      "output/simulation-results/simulations.rds"
    ),
    format = "file"
  ),
  tar_target(
    plot_objects_file,
    write_rds_file(
      all_plots,
      "output/simulation-results/plots.rds"
    ),
    format = "file"
  ),
  tar_target(
    simulation_summary_file,
    write_csv_file(
      simulation_summary,
      "output/tables/simulation-summary.csv"
    ),
    format = "file"
  ),

  tar_quarto(
    report,
    path = "report.qmd",
    quiet = FALSE
  )
)
