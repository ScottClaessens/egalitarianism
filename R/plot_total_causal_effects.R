#' Plot total causal effects of variables in the model
#'
#' @param draws Draws from the fitted model
#'
#' @returns A ggplot object
#'
plot_total_causal_effects <- function(draws) {

  # get total causal effects
  p <-
    tibble(

      climate_variation =
        predict_intervention(draws, climate_variation = 1) -
        predict_intervention(draws, climate_variation = 0),

      public_opinion =
        predict_intervention(draws, public_opinion = 1) -
        predict_intervention(draws, public_opinion = 0),

      sanctions =
        predict_intervention(draws, sanctions = 1) -
        predict_intervention(draws, sanctions = 0),

      subsistence =
        predict_intervention(draws, subsistence = 1) -
        predict_intervention(draws, subsistence = 0),

      scarcity =
        predict_intervention(draws, scarcity = 1) -
        predict_intervention(draws, scarcity = 0),

      political_violence =
        predict_intervention(draws, violence = 1) -
        predict_intervention(draws, violence = 0)

    ) |>
    pivot_longer(everything()) |>

    # plot
    ggplot(
      aes(
        x = value,
        y = name
      )
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed"
    ) +
    tidybayes::stat_halfeye() +
    scale_y_discrete(
      name = "Variable",
      labels = function(x) get_variable_names()[x]
    ) +
    scale_x_continuous(
      name = paste0(
        "Percentage point change in the probability of egalitarianism\n",
        "from a 1 standard deviation increase in the predictor variable"
      ),
      labels = scales::label_percent()
    ) +
    ggtitle("Total causal effects") +
    theme_classic() +
    theme(axis.title.x = element_text(size = 9))

  # save
  ggsave(
    filename = "plots/total_causal_effects.pdf",
    plot = p,
    width = 5,
    height = 3
  )

  # return
  p

}
