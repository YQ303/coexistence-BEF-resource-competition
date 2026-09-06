# Analysis assembly and export helpers -------------------------------------

# This file intentionally defines functions only. The computational workflow
# is declared in _targets.R so that targets can track every dependency.

collect_simulation_results <- function(simulations) {
  if (is.null(names(simulations)) || any(names(simulations) == "")) {
    stop("`simulations` must be a named list.", call. = FALSE)
  }

  results <- lapply(simulations, function(simulation) {
    dplyr::bind_rows(lapply(simulation, function(x) x[[3]]))
  })

  dplyr::bind_rows(results, .id = "scenario")
}

format_parameter_range <- function(x, digits = 3) {
  x <- as.numeric(x)
  if (length(x) == 0L || any(!is.finite(x))) {
    stop("Parameter values must be finite numeric values.", call. = FALSE)
  }

  format_one <- function(value) {
    formatted <- formatC(value, format = "f", digits = digits)
    sub("\\.?0+$", "", formatted)
  }

  unique_values <- sort(unique(signif(x, 12)))
  if (length(unique_values) == 1L) {
    return(format_one(unique_values))
  }

  paste0(
    format_one(min(unique_values)),
    "–",
    format_one(max(unique_values)),
    " (",
    length(unique_values),
    " values)"
  )
}

simulation_parameter_table <- function(parameter_sets, scenario, digits = 3) {
  if (!is.list(parameter_sets) || length(parameter_sets) == 0L) {
    stop("`parameter_sets` must be a non-empty list.", call. = FALSE)
  }

  extract <- function(fun) {
    vapply(parameter_sets, fun, numeric(1))
  }

  q_values <- list(
    ik = extract(function(x) x$Q[1, 1]),
    il = extract(function(x) x$Q[1, 2]),
    jk = extract(function(x) x$Q[2, 1]),
    jl = extract(function(x) x$Q[2, 2])
  )
  k_values <- list(
    ik = extract(function(x) x$K[1, 1]),
    il = extract(function(x) x$K[1, 2]),
    jk = extract(function(x) x$K[2, 1]),
    jl = extract(function(x) x$K[2, 2])
  )
  mu_values <- list(
    i = extract(function(x) x$mu[1]),
    j = extract(function(x) x$mu[2])
  )

  resource_supply <- vapply(
    parameter_sets,
    function(x) {
      dilution <- rep(1, nrow(x$Q))
      a <- x$Q * matrix(
        rep(x$mu, ncol(x$Q)),
        nrow = nrow(x$Q),
        byrow = FALSE
      ) / dilution
      r_star <- x$K * a / (1 - a)
      diag(r_star) + x$r_v * x$f
    },
    numeric(2)
  )

  format_indexed <- function(symbol, values) {
    paste(
      vapply(
        names(values),
        function(index) {
          paste0(
            "$",
            symbol,
            "_{",
            index,
            "}$ = ",
            format_parameter_range(values[[index]], digits)
          )
        },
        character(1)
      ),
      collapse = ", "
    )
  }

  data.frame(
    Scenario = rep(scenario, 5),
    Parameter = c(
      "Resource consumption, $q$",
      "Mortality rates, $\\mu$",
      "Half-saturation constant, $K$",
      "Resource-$k$ supply, $R_{k0}$",
      "Resource-$l$ supply, $R_{l0}$"
    ),
    `Value(s)` = c(
      format_indexed("q", q_values),
      format_indexed("\\mu", mu_values),
      format_indexed("K", k_values),
      format_parameter_range(resource_supply[1, ], digits),
      format_parameter_range(resource_supply[2, ], digits)
    ),
    check.names = FALSE
  )
}

compose_bef_figure <- function(
  plot_trans,
  plot_nbe_trans,
  plot_notrans,
  plot_nbe_notrans
) {
  ggpubr::ggarrange(
    plot_trans,
    plot_nbe_trans,
    plot_notrans,
    plot_nbe_notrans,
    nrow = 2,
    ncol = 2,
    labels = paste0("(", letters[1:4], ")"),
    label.x = rep(0.01, 4),
    label.y = rep(1, 4),
    hjust = 0.1,
    vjust = 1,
    font.label = list(size = 16, face = "bold")
  )
}

add_connector_lines <- function(plot, x, y = c(0.33, 0.36), linewidth = 0.7) {
  if (!is.matrix(x) || ncol(x) != 2L) {
    stop("`x` must be a two-column matrix of segment endpoints.", call. = FALSE)
  }

  result <- cowplot::ggdraw(plot)
  for (i in seq_len(nrow(x))) {
    result <- result + cowplot::draw_line(
      x = x[i, ],
      y = y,
      linewidth = linewidth
    )
  }
  result
}

write_plot_file <- function(plot, path, width, height, dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  path
}

write_rds_file <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(object, path)
  path
}

write_csv_file <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path)
  path
}
