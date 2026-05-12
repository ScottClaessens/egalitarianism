#' Predict consequences of an intervention on probability of egalitarianism
#'
#' Function to calculate the probability of egalitarianism with potential
#' intervention values for latent variables
#'
#' @param draws Draws from the fitted model
#' @param climate_variation Numeric. If \code{NULL} (default), variable is not
#'   intervened on. Otherwise, variable is held at specified value.
#' @param public_opinion Numeric. If \code{NULL} (default), variable is not
#'   intervened on. Otherwise, variable is held at specified value.
#' @param sanctions Numeric. If \code{NULL} (default), variable is not
#'   intervened on. Otherwise, variable is held at specified value.
#' @param subsistence Numeric. If \code{NULL} (default), variable is not
#'   intervened on. Otherwise, variable is held at specified value.
#' @param scarcity Numeric. If \code{NULL} (default), variable is not
#'   intervened on. Otherwise, variable is held at specified value.
#' @param violence Numeric. If \code{NULL} (default), variable is not
#'   intervened on. Otherwise, variable is held at specified value.
#'
#' @returns Vector of length draws with probabilities of egalitarianism
#'
predict_intervention <- function(draws, climate_variation = NULL,
                                 public_opinion = NULL, sanctions = NULL,
                                 subsistence = NULL, scarcity = NULL,
                                 violence = NULL) {

  with(draws, {

    if (is.null(climate_variation)) {
      climate_variation <- 0
    }
    if (is.null(public_opinion)) {
      public_opinion <- 0
    }
    if (is.null(sanctions)) {
      sanctions <- 0
    }
    if (is.null(subsistence)) {
      subsistence <-
        `beta[1]` * climate_variation
    }
    if (is.null(scarcity)) {
      scarcity <-
        `beta[2]` * climate_variation +
        `beta[3]` * subsistence
    }
    if (is.null(violence)) {
      violence <-
        `beta[4]` * sanctions +
        `beta[5]` * public_opinion
    }

    # return probability of egalitarianism
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

}
