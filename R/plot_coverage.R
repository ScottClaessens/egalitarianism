#' Plot variable coverage
#'
#' @param data Tibble of D-PLACE data
#'
#' @returns A ggplot object
#'
plot_coverage <- function(data) {

  # plot
  p <-
    data |>
    summarise(
      across(
        temperature_variance:political_violence,
        function(x) sum(!is.na(x))
      )
    ) |>
    pivot_longer(everything()) |>
    mutate(name = factor(name, levels = unique(name))) |>

    ggplot(
      aes(
        x = name,
        y = value
      )
    ) +
    geom_col(fill = "seagreen") +
    geom_hline(
      yintercept = nrow(data),
      linetype = "dashed"
    ) +
    scale_y_continuous(
      name = " \nNumber of societies\nwith observed data",
      expand = c(0, 0)
    ) +
    scale_x_discrete(
      name = NULL,
      labels = function(x) get_variable_names()[x]
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(
        angle = 30,
        hjust = 1,
        vjust = 1
      )
    )

  # save
  ggsave(
    filename = "plots/coverage.pdf",
    plot = p,
    height = 4,
    width = 6
  )

  # return
  p

}
