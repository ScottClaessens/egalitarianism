# get human readable variable names
get_variable_names <- function() {
  c(
    "temperature_variance"   = "Temperature variance",
    "temperature_predict"    = "Temperature predictability",
    "precipitation_predict"  = "Precipitation predictability",
    "egalitarianism"         = "Egalitarianism",
    "percent_hunting"        = "Percent hunting",
    "large_game_hunting"     = "Large game hunting",
    "food_sharing"           = "Food sharing",
    "starvation_occurrence"  = "Starvation occurrence",
    "famine_occurrence"      = "Famine occurrence",
    "resource_problems"      = "Resource problems",
    "gossip_government"      = "Gossip about government",
    "gossip_politics"        = "Gossip about politics",
    "gossip_family"          = "Gossip about family",
    "checks_power"           = "Checks on power",
    "remove_leaders"         = "Removal of leaders",
    "political_fission"      = "Political fission",
    "political_violence"     = "Political violence"
  )
}

# helper functions from rethinking package
rordlogit <- function (n, phi = 0, a) {
  a <- c(as.numeric(a), Inf)
  k <- 1:length(a)
  if (length(phi) == 1) {
    p <- dordlogit(k, a = a, phi = phi, log = FALSE)
    y <- sample(k, size = n, replace = TRUE, prob = p)
  }
  else {
    y <- rep(NA, n)
    if (n > length(phi)) {
      phi <- rep(phi, ceiling(n/length(phi)))
    }
    for (i in 1:n) {
      p <- dordlogit(k, a = a, phi = phi[i], log = FALSE)
      y[i] <- sample(k, size = 1, replace = TRUE, prob = p)
    }
  }
  y
}

dordlogit <- function (x, phi, a, log = FALSE) {
  a <- c(as.numeric(a), Inf)
  p <- logistic(a[x] - phi)
  na <- c(-Inf, a)
  np <- logistic(na[x] - phi)
  p <- p - np
  if (log == TRUE)
    p <- log(p)
  p
}

logistic <- function (x) {
  p <- 1/(1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  p
}
