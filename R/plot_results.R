#' Plot results of fitted model
#'
#' @param draws_model Draws from the fitted model
#' @param simulation Logical. If \code{TRUE}, the resulting plot contains known
#'   reference values for parameters.
#'
#' @returns A ggplot object
#'
plot_results <- function(draws_model, simulation = FALSE) {

  # readable beta parameters
  betas <- c(
    "Subsistence ~ climate variation",
    "Resource scarcity ~ climate variation",
    "Resource scarcity ~ subsistence"
  )

  # plot
  p <-
    draws_model |>
    dplyr::select(starts_with("beta")) |>
    pivot_longer(everything()) |>
    mutate(name = betas[parse_number(name)]) |>
    ggplot(aes(x = value, y = name)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    tidybayes::stat_halfeye() +
    labs(
      x = "Parameter value",
      y = NULL
    ) +
    theme_classic()

  # if simulation, add known values
  if (simulation) {
    p <-
      p +
      geom_point(
        data = tibble(name = betas, value = 1),
        colour = "red",
        size = 2
      ) +
      ggtitle("Results of simulation (N = 200)")
  } else {
    p <-
      p +
      ggtitle("Results of fitted model")
  }

  # save
  ggsave(
    filename = paste0(
      "plots/",
      ifelse(simulation, "simulation", "results"),
      ".pdf"
    ),
    plot = p,
    width = 6,
    height = 4
  )

  # return
  p

}
