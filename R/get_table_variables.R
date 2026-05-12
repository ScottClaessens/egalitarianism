#' Get table of variables used in the analysis
#'
#' @returns A tibble
#'
get_table_variables <- function() {

  tibble(

    `Latent variable` = c(
      "Climate variability",
      "",
      "",
      "Large game hunting",
      "",
      "",
      "Resource scarcity",
      "",
      "",
      "Public opinion",
      "",
      "",
      "Sanctions",
      "",
      "",
      "Political violence"
    ),

    Code = c(
      "AnnualTemperatureVariance",
      "TemperaturePredictability",
      "PrecipitationPredictability",
      "SCCS204",
      "SCCS10",
      "SCCS1718",
      "SCCS1262",
      "SCCS1265",
      "SCCS1685",
      "SCCS1789",
      "SCCS1796",
      "SCCS1799",
      "SCCS761",
      "SCCS762",
      "SCCS785",
      "SCCS1739"
    ),

    Type = c(
      "Positive real",
      "0 - 1",
      "0 - 1",
      "Ordinal",
      "Binary",
      "Ordinal",
      "Ordinal",
      "Ordinal",
      "Ordinal",
      "Binary",
      "Binary",
      "Binary",
      "Ordinal",
      "Ordinal",
      "Ordinal",
      "Ordinal"
    ),

    Description = c(
      "Variance in monthly temperature means",
      "Extent to which temperature is predictable",
      "Extent to which precipitation is predictable",
      "Percentage dependence on hunting",
      "Presence/absence of large game hunting",
      "Extent of food sharing",
      "Occurrence of short-term starvation",
      "Occurrence of famine",
      "Extent of chronic resource problems",
      "Presence/absence of gossip on government",
      "Presence/absence of gossip on politics",
      "Presence/absence of gossip on family",
      "Extent of checks on leaders' power",
      "Extent of institutions for removing unliked leaders",
      "Extent of political fission of dissatisfied persons",
      "Extent of violence against political institution"
    )

  )

}
