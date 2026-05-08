#' Plot total causal effects of variables in the model
#'
#' @param draws Draws from the fitted model
#'
#' @returns A ggplot object
#'
plot_total_causal_effects <- function(draws) {

  # number of draws
  n <- nrow(draws)

  # function to simulate probability of egalitarianism
  # with potential intervention values for latent variables
  sim <- function(climate_variation = NULL,
                  public_opinion = NULL,
                  sanctions = NULL,
                  subsistence = NULL,
                  scarcity = NULL,
                  violence = NULL) {

    with(draws, {

      withr::with_seed(1, {

        # if latent variable is not set to a fixed value, simulate
        if (is.null(climate_variation)) {
          climate_variation <- rnorm(n)
        }
        if (is.null(public_opinion)) {
          public_opinion <- rnorm(n)
        }
        if (is.null(sanctions)) {
          sanctions <- rnorm(n)
        }
        if (is.null(subsistence)) {
          subsistence <- rnorm(n, `beta[1]` * climate_variation)
        }
        if (is.null(scarcity)) {
          scarcity <- rnorm(n, `beta[2]` * climate_variation +
                              `beta[3]` * subsistence)
        }
        if (is.null(violence)) {
          violence <- `beta[4]` * sanctions + `beta[5]` * public_opinion
        }

        # return probability
        plogis(
          `alpha[1]` +
            `beta[6]` * climate_variation +
            `beta[7]` * subsistence +
            `beta[8]` * scarcity +
            `beta[9]` * sanctions +
            `beta[10]` * public_opinion +
            `beta[11]` * violence
        )

      })

    })
  }

  # simulate total causal effects
  p <-
    tibble(

      climate_variation =
        sim(climate_variation = 1) - sim(climate_variation = 0),

      public_opinion =
        sim(public_opinion = 1) - sim(public_opinion = 0),

      sanctions =
        sim(sanctions = 1) - sim(sanctions = 0),

      subsistence =
        sim(subsistence = 1) - sim(subsistence = 0),

      scarcity =
        sim(scarcity = 1) - sim(scarcity = 0),

      political_violence =
        sim(violence = 1) - sim(violence = 0)

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
