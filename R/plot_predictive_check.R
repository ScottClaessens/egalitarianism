#' Plot predictive check from Stan model
#'
#' @param data Tibble of D-PLACE data
#' @param draws_model Draws from the fitted model
#' @param prior Logical. If \code{FALSE} (default), plot is treated a posterior
#'   predictive check. If \code{TRUE}, plot is treated as a prior predictive
#'   check.
#'
#' @returns A ggplot object
#'
plot_predictive_check <- function(data, draws_model, prior = FALSE) {

  # plotting function
  plot_pred_check <- function(variable, ndraws = 20, transform = "identity",
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
      draws_model |>
      dplyr::select(starts_with(paste0(variable, "_rep"))) |>
      as.matrix()
    # get random draws
    draw_ids <- sample(1:nrow(yrep), ndraws)
    # plot
    if (is.factor(y_raw)) {
      p <- ppc_bars(y, yrep[draw_ids, obs_ids], size = 0.5)
    } else {
      p <- ppc_dens_overlay(y, yrep[draw_ids, ])
    }
    # add x axis scale
    p +
      scale_x_continuous(
        name = variable,
        transform = transform,
        limits = limits,
        breaks = breaks,
        labels = scales::label_number()
      )
  }

  # get list of plots
  plot_list <-
    suppressMessages(
      list(
        plot_pred_check("temperature_variance", transform = "log",
                        breaks = c(0.01, 1, 100)),
        plot_pred_check("temperature_predict", limits = c(0.1, 0.9),
                        breaks = c(0.2, 0.5, 0.8)),
        plot_pred_check("precipitation_predict", limits = c(0.1, 0.9),
                        breaks = c(0.2, 0.5, 0.8)),
        plot_pred_check("egalitarianism", breaks = 0:1),
        plot_pred_check("percent_hunting", breaks = seq(1, 9, by = 2)),
        plot_pred_check("large_game_hunting", breaks = 0:1),
        plot_pred_check("food_sharing", breaks = seq(2, 6, by = 2)),
        plot_pred_check("starvation_occurrence", breaks = 1:3),
        plot_pred_check("famine_occurrence", breaks = 1:4),
        plot_pred_check("resource_problems", breaks = 1:4),
        plot_pred_check("gossip_government", breaks = 0:1),
        plot_pred_check("gossip_politics", breaks = 0:1),
        plot_pred_check("gossip_family", breaks = 0:1),
        plot_pred_check("checks_power", breaks = 1:4),
        plot_pred_check("remove_leaders", breaks = 1:4),
        plot_pred_check("political_fission", breaks = 1:3),
        plot_pred_check("political_violence", breaks = 1:3)
      )
    )

  # put together
  p <-
    wrap_plots(plot_list) +
    plot_layout(guides = "collect")

  # save
  ggsave(
    filename = paste0(
      "plots/", ifelse(prior, "prior", "posterior"), "_check.pdf"
    ),
    plot = p,
    width = 8.5,
    height = 6
  )

  # return
  p

}

