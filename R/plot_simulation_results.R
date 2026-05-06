#' Plot model results fitted to simulated data
#'
#' @param sim_draws_model Draws from the model fitted to simulated data
#'
#' @returns A ggplot object
#'
plot_simulation_results <- function(sim_draws_model) {

  # readable beta parameters
  betas <- c(
    "Subsistence ~ climate variation",
    "Resource scarcity ~ climate variation",
    "Resource scarcity ~ subsistence"
  )

  # plot
  p <-
    sim_draws_model |>
    dplyr::select(starts_with("beta")) |>
    pivot_longer(everything()) |>
    mutate(name = betas[parse_number(name)]) |>
    ggplot(aes(x = value, y = name)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    tidybayes::stat_halfeye() +
    geom_point(
      data = tibble(name = betas, value = 1),
      colour = "red",
      size = 2
    ) +
    labs(
      title = "Results of simulation (N = 200)",
      x = "Parameter value",
      y = NULL
    ) +
    theme_classic()

  # save
  ggsave(
    filename = "plots/simulation.pdf",
    plot = p,
    width = 6,
    height = 4
  )

  # return
  p

}
