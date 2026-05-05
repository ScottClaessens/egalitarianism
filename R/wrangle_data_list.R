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

  # function to convert ordinal variables to numeric vector
  convert_ordinal <- function(x) {
    x <- as.numeric(x)
    ifelse(is.na(x), -9999, x)
  }

  # list for stan
  list(
    N = nrow(data),
    # climate variables are transformed inside the model
    temperature_variance  = data$temperature_variance,
    temperature_predict   = data$temperature_predict,
    precipitation_predict = data$precipitation_predict,
    # convert binary and ordinal variables to numeric
    percent_hunting       = convert_ordinal(data$percent_hunting),
    large_game_hunting    = convert_binary(data$large_game_hunting),
    food_sharing          = convert_ordinal(data$food_sharing),
    starvation_occurrence = convert_ordinal(data$starvation_occurrence),
    famine_occurrence     = convert_ordinal(data$famine_occurrence),
    resource_problems     = convert_ordinal(data$resource_problems),
    gossip_government     = convert_binary(data$gossip_government),
    gossip_politics       = convert_binary(data$gossip_politics),
    gossip_family         = convert_binary(data$gossip_family),
    checks_power          = convert_ordinal(data$checks_power),
    remove_leaders        = convert_ordinal(data$remove_leaders),
    political_fission     = convert_ordinal(data$political_fission),
    prior_only            = 0 # ignore the likelihood?
  )

}
