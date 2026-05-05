#' Wrangle data list for Stan
#'
#' @param data Tibble of D-PLACE data
#'
#' @returns Named list
#'
wrangle_data_list <- function(data) {

  # function to convert binary variables to numeric vector
  convert_binary <- function(x) {
    x <- as.numeric(x) - 1
    ifelse(is.na(x), -9999, x)
  }

  # list for stan
  list(
    N = nrow(data),
    gossip_government = convert_binary(data$gossip_government),
    gossip_politics   = convert_binary(data$gossip_politics),
    gossip_family     = convert_binary(data$gossip_family)
  )

}
