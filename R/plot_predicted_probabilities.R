#' Plot predicted probabilities of egalitarianism
#'
#' Plot predicted probabilities of egalitarianism under interventions on latent
#' variables in the model
#'
#' @param draws Draws from the fitted model
#'
#' @returns A ggplot object
#'
plot_predicted_probabilities <- function(draws) {

  # get sequence of predictor values
  predictor_seq <- seq(-3, 3, length.out = 100)

  # function to get matrix of predicted probabilities
  get_predicted_probabilities <- function(variable) {
    sapply(
      predictor_seq,
      function(x) {
        # get arguments
        args <- list()
        args[["draws"]] <- draws
        args[[variable]] <- x
        # run predict intervention function
        do.call(predict_intervention, args)
      }
    )
  }

  # calculate for every latent variable
  p <-
    tibble(
      type = rep(c("Exogenous", "Endogenous"), each = 3),
      variable = c("climate_variation", "subsistence", "scarcity",
                   "public_opinion", "violence", "sanctions")
    ) |>
    mutate(
      pred = map(variable, get_predicted_probabilities),
      median = map(pred, function(x) apply(x, 2, median)),
      lower50 = map(pred, function(x) apply(x, 2, quantile, prob = 0.25)),
      upper50 = map(pred, function(x) apply(x, 2, quantile, prob = 0.75)),
      lower95 = map(pred, function(x) apply(x, 2, quantile, prob = 0.025)),
      upper95 = map(pred, function(x) apply(x, 2, quantile, prob = 0.975))
    ) |>
    dplyr::select(!pred) |>
    unnest(c(median, lower50, upper50, lower95, upper95)) |>
    mutate(
      variable = get_variable_names()[variable],
      variable = factor(
        variable, levels = c("Public opinion", "Political violence",
                             "Sanctions", "Climate variation", "Subsistence",
                             "Resource scarcity")
      ),
      value = rep(predictor_seq, times = 6)
    ) |>

    # plot
    ggplot(
      aes(
        x = value,
        y = median
      )
    ) +
    geom_ribbon(
      aes(
        ymin = lower95,
        ymax = upper95,
        fill = type
      )
    ) +
    scale_fill_manual(values = c("#fff1ea", "#f0ffec")) +
    ggnewscale::new_scale_fill() +
    geom_ribbon(
      aes(
        ymin = lower50,
        ymax = upper50,
        fill = type
      )
    ) +
    scale_fill_manual(values = c("#f5c6ac", "#c1f1c8")) +
    geom_line() +
    facet_wrap(
      . ~ variable,
      scales = "free_x",
      strip.position = "bottom"
    ) +
    labs(
      x = NULL,
      y = "Predicted probability\nof egalitarianism"
    ) +
    theme_classic() +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      legend.title = element_blank()
    )

  # save
  ggsave(
    filename = "plots/predict_probs.pdf",
    plot = p,
    width = 5.5,
    height = 3.5
  )

  # return
  p

}
