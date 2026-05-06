#' Plot posterior predictive check from Stan model
#'
#' @param data Tibble of D-PLACE data
#' @param fit_draws_model Draws from the fitted model
#'
#' @returns A ggplot object
#'
plot_posterior_predictive_check <- function(data, fit_draws_model) {

  # plotting function
  plot_pp_check <- function(variable, ndraws = 20, transform = "identity",
                            limits = NULL, breaks = waiver()) {
    # data variable
    y_raw <- data[[variable]]
    if (is.ordered(y_raw)) {
      y <- as.numeric(y_raw)
    } else if (is.factor(y_raw)) {
      y <- as.numeric(y_raw) - 1
    } else {
      y <- y_raw
    }
    # ids for observed data
    obs_ids <- which(!is.na(y))
    y <- y[obs_ids]
    # posterior predictive distribution
    yrep <-
      fit_draws_model |>
      dplyr::select(starts_with(paste0(variable, "_rep"))) |>
      as.matrix()
    # get random draws
    draw_ids <- sample(1:nrow(yrep), ndraws)
    # plot
    if (is.factor(y_raw)) {
      ppc_bars(y, yrep[draw_ids, obs_ids]) +
        xlab(variable)
    } else {
      ppc_dens_overlay(y, yrep[draw_ids, ]) +
        scale_x_continuous(
          name = variable,
          transform = transform,
          limits = limits,
          breaks = breaks
        )
    }
  }

  # get list of plots
  plot_list <-
    list(
      plot_pp_check("temperature_variance", transform = "log",
                    breaks = c(0.01, 1, 100)),
      plot_pp_check("temperature_predict"),
      plot_pp_check("precipitation_predict"),
      plot_pp_check("percent_hunting"),
      plot_pp_check("large_game_hunting"),
      plot_pp_check("food_sharing"),
      plot_pp_check("starvation_occurrence"),
      plot_pp_check("famine_occurrence"),
      plot_pp_check("resource_problems"),
      plot_pp_check("gossip_government"),
      plot_pp_check("gossip_politics"),
      plot_pp_check("gossip_family"),
      plot_pp_check("checks_power"),
      plot_pp_check("remove_leaders"),
      plot_pp_check("political_fission")
    )

  # put together
  p <-
    wrap_plots(plot_list) +
    plot_layout(guides = "collect")

  # save
  ggsave(
    filename = "plots/pp_check.pdf",
    plot = p,
    width = 8.5,
    height = 6
  )

  # return
  p

}

