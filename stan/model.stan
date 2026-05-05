data {

  // ─────────────────────────────────────────────────────
  // Observed data
  // ─────────────────────────────────────────────────────

  int<lower=0> N; // number of societies
  vector[N] temperature_variance;
  vector[N] temperature_predict;
  vector[N] precipitation_predict;
  array[N] int percent_hunting;
  array[N] int large_game_hunting;
  array[N] int food_sharing;
  array[N] int starvation_occurrence;
  array[N] int famine_occurrence;
  array[N] int resource_problems;
  array[N] int gossip_government;
  array[N] int gossip_politics;
  array[N] int gossip_family;
  array[N] int checks_power;
  array[N] int remove_leaders;
  array[N] int political_fission;

  // ─────────────────────────────────────────────────────
  // Missing data indicators
  // ─────────────────────────────────────────────────────

  // numbers of societies with observed data
  // other variables have complete data
  int<lower=0> N_obs_large_game;
  int<lower=0> N_obs_food_sharing;
  int<lower=0> N_obs_starvation;
  int<lower=0> N_obs_famine;
  int<lower=0> N_obs_resource;
  int<lower=0> N_obs_gossip;
  int<lower=0> N_obs_checks;
  int<lower=0> N_obs_remove;
  int<lower=0> N_obs_fission;

  // indicators for societies with observed data
  array[N_obs_large_game]   int idx_large_game;
  array[N_obs_food_sharing] int idx_food_sharing;
  array[N_obs_starvation]   int idx_starvation;
  array[N_obs_famine]       int idx_famine;
  array[N_obs_resource]     int idx_resource;
  array[N_obs_gossip]       int idx_gossip;
  array[N_obs_checks]       int idx_checks;
  array[N_obs_remove]       int idx_remove;
  array[N_obs_fission]      int idx_fission;

  // ─────────────────────────────────────────────────────
  // Ignore likelihood?
  // ─────────────────────────────────────────────────────

  int<lower=0, upper=1> prior_only;

}

transformed data {

  // ─────────────────────────────────────────────────────
  // Log and standardise temperature variance
  // ─────────────────────────────────────────────────────

  vector[N] temperature_variance_log;
  vector[N] temperature_variance_log_std;

  temperature_variance_log = log(temperature_variance);

  temperature_variance_log_std =
    (temperature_variance_log - mean(temperature_variance_log)) /
    sd(temperature_variance_log);

  // ─────────────────────────────────────────────────────
  // Reverse climate predictability variables
  // ─────────────────────────────────────────────────────

  vector[N] temperature_unpredict;
  vector[N] precipitation_unpredict;

  temperature_unpredict = 1 - temperature_predict;
  precipitation_unpredict = 1 - precipitation_predict;

}

parameters {

  // ─────────────────────────────────────────────────────
  // Factor loadings (lambda)
  // ─────────────────────────────────────────────────────

  array[10] real lambda;

  // ─────────────────────────────────────────────────────
  // Variances (sigma) and beta precision (phi) parameters
  // ─────────────────────────────────────────────────────

  real<lower=0> sigma;
  array[2] real<lower=0> phi;

  // ─────────────────────────────────────────────────────
  // Regression coefficients (beta)
  // ─────────────────────────────────────────────────────

  array[3] real beta;

  // ─────────────────────────────────────────────────────
  // Ordered cutpoint parameters
  // ─────────────────────────────────────────────────────

  ordered[9] c1;
  ordered[6] c2;
  ordered[2] c3;
  ordered[3] c4;
  ordered[3] c5;
  ordered[3] c6;
  ordered[3] c7;
  ordered[2] c8;

  // ─────────────────────────────────────────────────────
  // Latent variables
  // ─────────────────────────────────────────────────────

  vector[N] climate_variation;
  vector[N] subsistence;
  vector[N] scarcity;
  vector[N] public_opinion;
  vector[N] sanctions;

}

model {

  // ─────────────────────────────────────────────────────
  // Initialise vectors
  // ─────────────────────────────────────────────────────

  vector[N] mu1;
  vector[N] mu2;

  // ─────────────────────────────────────────────────────
  // Priors
  // ─────────────────────────────────────────────────────

  lambda ~ normal(1, 0.5);
  sigma  ~ exponential(1);
  phi ~ exponential(1);
  c1 ~ normal(0, 2);
  c2 ~ normal(0, 2);
  c3 ~ normal(0, 2);
  c4 ~ normal(0, 2);
  c5 ~ normal(0, 2);
  c6 ~ normal(0, 2);
  c7 ~ normal(0, 2);
  c8 ~ normal(0, 2);
  beta ~ normal(0, 1);

  // ─────────────────────────────────────────────────────
  // Structural model
  // ─────────────────────────────────────────────────────

  climate_variation ~ normal(0, 1);
  public_opinion ~ normal(0, 1);
  sanctions ~ normal(0, 1);

  subsistence ~ normal(beta[1] * climate_variation, 1);
  scarcity ~ normal(beta[2] * climate_variation + beta[3] * subsistence, 1);

  if (!prior_only) {

    // ─────────────────────────────────────────────────────
    // Climate variability measurement model
    // ─────────────────────────────────────────────────────

    temperature_variance_log_std ~ normal(climate_variation, sigma);
    mu1 = inv_logit(lambda[1] * climate_variation);
    mu2 = inv_logit(lambda[2] * climate_variation);
    temperature_unpredict ~ beta(mu1 * phi[1], (1.0 - mu1) * phi[1]);
    precipitation_unpredict ~ beta(mu2 * phi[2], (1.0 - mu2) * phi[2]);

    // ─────────────────────────────────────────────────────
    // Subsistence measurement model
    // ─────────────────────────────────────────────────────

    percent_hunting ~ ordered_logistic(
      1.0 * subsistence, c1
    );
    large_game_hunting[idx_large_game] ~ bernoulli_logit(
      lambda[3] * subsistence[idx_large_game]
    );
    food_sharing[idx_food_sharing] ~ ordered_logistic(
      lambda[4] * subsistence[idx_food_sharing], c2
    );

    // ─────────────────────────────────────────────────────
    // Scarcity measurement model
    // ─────────────────────────────────────────────────────

    starvation_occurrence[idx_starvation] ~ ordered_logistic(
      1.0 * scarcity[idx_starvation], c3
    );
    famine_occurrence[idx_famine] ~ ordered_logistic(
      lambda[5] * scarcity[idx_famine], c4
    );
    resource_problems[idx_resource] ~ ordered_logistic(
      lambda[6] * scarcity[idx_resource], c5
    );

    // ─────────────────────────────────────────────────────
    // Public opinion measurement model
    // ─────────────────────────────────────────────────────

    gossip_government[idx_gossip] ~ bernoulli_logit(
      1.0 * public_opinion[idx_gossip]
    );
    gossip_politics[idx_gossip] ~ bernoulli_logit(
      lambda[7] * public_opinion[idx_gossip]
    );
    gossip_family[idx_gossip] ~ bernoulli_logit(
      lambda[8] * public_opinion[idx_gossip]
    );

    // ─────────────────────────────────────────────────────
    // Sanctions measurement model
    // ─────────────────────────────────────────────────────

    checks_power[idx_checks] ~ ordered_logistic(
      1.0 * sanctions[idx_checks], c6
    );
    remove_leaders[idx_remove] ~ ordered_logistic(
      lambda[9] * sanctions[idx_remove], c7
    );
    political_fission[idx_fission] ~ ordered_logistic(
      lambda[10] * sanctions[idx_fission], c8
    );

  }

}

