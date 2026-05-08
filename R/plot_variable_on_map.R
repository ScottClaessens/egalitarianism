#' Plot variable on a world map
#'
#' @param data Tibble of D-PLACE data
#' @param variable Name of the variable to plot on the map
#'
#' @returns A ggplot object
#'
plot_variable_on_map <- function(data, variable = "egalitarianism") {

  # shorten some variable levels for plot
  levels(data$food_sharing) <- c(
    "Nuclear family", "Local kin", "External kin",
    "Local non-kin", "All local", "Wider", "Other"
  )
  levels(data$resource_problems) <- c(
    "Low or rare", "Some hunger", "Some members", "Most members"
  )
  levels(data$remove_leaders) <- c(
    "None", "Occasional", "Lose influence", "Loss of power"
  )
  levels(data$political_violence) <- c(
    "Violent acts absent", "Acts of violence", "Active resistance"
  )
  levels(data$political_fission) <- c(
    "Rarely", "Sometimes", "Often"
  )

  # plot
  p <-
    ne_countries(
      scale = "small",
      returnclass = "sf"
    ) |>
    filter(continent != "Antarctica") |>
    ggplot() +
    geom_sf(
      fill = "grey80",
      colour = NA
    ) +
    geom_point(
      data = data,
      mapping = aes(
        x = longitude,
        y = latitude,
        colour = !!sym(variable)
      )
    ) +
    guides(
      colour = guide_legend(
        title = get_variable_names()[variable]
      )
    ) +
    theme_void()

  # choose colour palette
  if (is.ordered(data[[variable]])) {

    p <- p + scale_colour_ordinal(na.value = "grey50")

  } else if (is.factor(data[[variable]])) {

    p <-
      p +
      scale_colour_manual(
        values = c("#377EB8", "#FF7F00")
      )

  }

  # save
  ggsave(
    filename = paste0("plots/maps/", variable, ".pdf"),
    plot = p,
    height = 5,
    width = 8
  )

  # return
  p

}
