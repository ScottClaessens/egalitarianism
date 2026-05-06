#' Simulate data for model testing
#'
#' @returns A tibble
#'
simulate_data <- function(n = 1000) {

  # ──────────────────────────────────────────────────
  # Simulate latent variables
  # ──────────────────────────────────────────────────

  climate_variation <- rnorm(n)
  public_opinion <- rnorm(n)
  sanctions <- rnorm(n)
  subsistence <- rnorm(n, climate_variation)
  scarcity <- rnorm(n, climate_variation + subsistence)

  # ──────────────────────────────────────────────────
  # Simulate political violence
  # ──────────────────────────────────────────────────

  violence <- sanctions + public_opinion
  political_violence <- ordered(rordlogit(n, violence, a = c(-0.1, 0.9)))

  # ──────────────────────────────────────────────────
  # Simulate egalitarianism
  # ──────────────────────────────────────────────────

  egalitarianism <-
    factor(
      rbinom(
        n, 1,
        plogis(
          climate_variation + subsistence + scarcity + sanctions +
            public_opinion + violence
          )
        )
    )

  # ──────────────────────────────────────────────────
  # Simulate "climate variation" indicator variables
  # ──────────────────────────────────────────────────

  temperature_variance <- exp(rnorm(n, climate_variation, 2))
  temperature_predict <- rbeta(
    n = n,
    shape1 = plogis(-1 * climate_variation) * 10,
    shape2 = (1 - plogis(-1 * climate_variation)) * 10
  )
  precipitation_predict <- rbeta(
    n = n,
    shape1 = plogis(-1 * climate_variation) * 10,
    shape2 = (1 - plogis(-1 * climate_variation)) * 10
  )

  # ──────────────────────────────────────────────────
  # Simulate "subsistence" indicator variables
  # ──────────────────────────────────────────────────

  percent_hunting <- ordered(
    rordlogit(n, subsistence, a = c(-1, 0.4, 1.4, 2, 3, 3.5, 4, 4.5, 5))
  )
  large_game_hunting <- factor(rbinom(n, 1, plogis(subsistence)))
  food_sharing <- ordered(
    rordlogit(n, subsistence, a = c(-2.8, -1.4, -0.7, -0.5, 0.6, 2.3))
  )

  # ──────────────────────────────────────────────────
  # Simulate "resource scarcity" indicator variables
  # ──────────────────────────────────────────────────

  starvation_occurrence <- ordered(rordlogit(n, scarcity, a = c(-2.4, 2.8)))
  famine_occurrence <- ordered(rordlogit(n, scarcity, a = c(-2.8, -1.2, -0.8)))
  resource_problems <- ordered(rordlogit(n, scarcity, a = c(0.2, 2.2, 3.6)))

  # ──────────────────────────────────────────────────
  # Simulate "public opinion" indicator variables
  # ──────────────────────────────────────────────────

  gossip_government <- factor(rbinom(n, 1, plogis(public_opinion)))
  gossip_politics <- factor(rbinom(n, 1, plogis(public_opinion)))
  gossip_family <- factor(rbinom(n, 1, plogis(public_opinion)))

  # ──────────────────────────────────────────────────
  # Simulate "sanctions" indicator variables
  # ──────────────────────────────────────────────────

  checks_power <- ordered(rordlogit(n, sanctions, a = c(-3.2, -0.8, 1.7)))
  remove_leaders <- ordered(rordlogit(n, sanctions, a = c(-3, -0.8, 2.2)))
  political_fission <- ordered(rordlogit(n, sanctions, a = c(-1.6, 0.2)))

  # ──────────────────────────────────────────────────
  # Compile and return dataset
  # ──────────────────────────────────────────────────

  tibble(
    temperature_variance,
    temperature_predict,
    precipitation_predict,
    egalitarianism,
    percent_hunting,
    large_game_hunting,
    food_sharing,
    starvation_occurrence,
    famine_occurrence,
    resource_problems,
    gossip_government,
    gossip_politics,
    gossip_family,
    checks_power,
    remove_leaders,
    political_fission,
    political_violence
  )

}
