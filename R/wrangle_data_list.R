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

    # observed data
    N                     = nrow(data),
    temperature_variance  = data$temperature_variance,
    temperature_predict   = data$temperature_predict,
    precipitation_predict = data$precipitation_predict,
    egalitarianism        = convert_binary(data$egalitarianism),
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

    # counts of observed data
    N_obs_large_game      = sum(!is.na(data$large_game_hunting)),
    N_obs_food_sharing    = sum(!is.na(data$food_sharing)),
    N_obs_starvation      = sum(!is.na(data$starvation_occurrence)),
    N_obs_famine          = sum(!is.na(data$famine_occurrence)),
    N_obs_resource        = sum(!is.na(data$resource_problems)),
    N_obs_gossip          = sum(!is.na(data$gossip_government)),
    N_obs_checks          = sum(!is.na(data$checks_power)),
    N_obs_remove          = sum(!is.na(data$remove_leaders)),
    N_obs_fission         = sum(!is.na(data$political_fission)),

    # observed data indicators
    idx_large_game        = which(!is.na(data$large_game_hunting)),
    idx_food_sharing      = which(!is.na(data$food_sharing)),
    idx_starvation        = which(!is.na(data$starvation_occurrence)),
    idx_famine            = which(!is.na(data$famine_occurrence)),
    idx_resource          = which(!is.na(data$resource_problems)),
    idx_gossip            = which(!is.na(data$gossip_government)),
    idx_checks            = which(!is.na(data$checks_power)),
    idx_remove            = which(!is.na(data$remove_leaders)),
    idx_fission           = which(!is.na(data$political_fission)),

    # ignore likelihood?
    prior_only            = 0

  )

}
