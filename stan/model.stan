data {

  // ─────────────────────────────────────────────────────
  // Observed data
  // ─────────────────────────────────────────────────────

  int<lower=0> N; // total number of societies
  vector[N] temperature_variance;
  vector[N] temperature_predict;
  vector[N] precipitation_predict;
  array[N] int egalitarianism;
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
  // Numbers of societies with observed data
  // ─────────────────────────────────────────────────────

  int<lower=0> N_obs_large_game;
  int<lower=0> N_obs_food_sharing;
  int<lower=0> N_obs_starvation;
  int<lower=0> N_obs_famine;
  int<lower=0> N_obs_resource;
  int<lower=0> N_obs_gossip;
  int<lower=0> N_obs_checks;
  int<lower=0> N_obs_remove;
  int<lower=0> N_obs_fission;

  // ─────────────────────────────────────────────────────
  // Indicators for societies with observed data
  // ─────────────────────────────────────────────────────

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
  // Log and center temperature variance
  // ─────────────────────────────────────────────────────

  vector[N] temperature_variance_log;
  vector[N] temperature_variance_log_centered;

  temperature_variance_log = log(temperature_variance);

  temperature_variance_log_centered =
    temperature_variance_log - mean(temperature_variance_log);

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
  // Regression slope parameters
  // ─────────────────────────────────────────────────────

  array[6] real beta;

  // ─────────────────────────────────────────────────────
  // Intercepts for non-ordinal variables
  // ─────────────────────────────────────────────────────

  array[8] real alpha;

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

  lambda ~ normal(0, 1);
  sigma  ~ exponential(1);
  phi ~ exponential(1);
  beta ~ normal(0, 1);
  alpha ~ normal(0, 1);
  c1 ~ normal(0, 2);
  c2 ~ normal(0, 2);
  c3 ~ normal(0, 2);
  c4 ~ normal(0, 2);
  c5 ~ normal(0, 2);
  c6 ~ normal(0, 2);
  c7 ~ normal(0, 2);
  c8 ~ normal(0, 2);

  // ─────────────────────────────────────────────────────
  // Structural model
  // ─────────────────────────────────────────────────────

  // exogenous variables
  climate_variation ~ normal(0, 1);
  public_opinion ~ normal(0, 1);
  sanctions ~ normal(0, 1);

  // endogenous variables
  subsistence ~ normal(beta[1] * climate_variation, 1);
  scarcity ~ normal(beta[2] * climate_variation + beta[3] * subsistence, 1);
  egalitarianism ~ bernoulli_logit(
    alpha[1] + beta[4] * climate_variation + beta[5] * subsistence +
      beta[6] * scarcity
  );


  if (!prior_only) {

    // ─────────────────────────────────────────────────────
    // Climate variability measurement model
    // ─────────────────────────────────────────────────────

    temperature_variance_log_centered ~
      normal(alpha[2] + 1.0 * climate_variation, sigma);

    mu1 = inv_logit(alpha[3] + lambda[1] * climate_variation);
    mu2 = inv_logit(alpha[4] + lambda[2] * climate_variation);

    temperature_unpredict ~ beta(mu1 * phi[1], (1.0 - mu1) * phi[1]);

    precipitation_unpredict ~ beta(mu2 * phi[2], (1.0 - mu2) * phi[2]);

    // ─────────────────────────────────────────────────────
    // Subsistence measurement model
    // ─────────────────────────────────────────────────────

    percent_hunting ~ ordered_logistic(
      1.0 * subsistence, c1
    );

    large_game_hunting[idx_large_game] ~ bernoulli_logit(
      alpha[5] + lambda[3] * subsistence[idx_large_game]
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
      alpha[6] + 1.0 * public_opinion[idx_gossip]
    );

    gossip_politics[idx_gossip] ~ bernoulli_logit(
      alpha[7] + lambda[7] * public_opinion[idx_gossip]
    );

    gossip_family[idx_gossip] ~ bernoulli_logit(
      alpha[8] + lambda[8] * public_opinion[idx_gossip]
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

generated quantities {

  // ─────────────────────────────────────────────────────
  // Initialise linear predictors and yrep variables
  // ─────────────────────────────────────────────────────

  array[N] real mu1;
  array[N] real mu2;
  array[N] real temperature_variance_log_centered_rep;
  array[N] real temperature_variance_rep;
  array[N] real temperature_predict_rep;
  array[N] real precipitation_predict_rep;
  array[N] int egalitarianism_rep;
  array[N] int percent_hunting_rep;
  array[N] int large_game_hunting_rep;
  array[N] int food_sharing_rep;
  array[N] int starvation_occurrence_rep;
  array[N] int famine_occurrence_rep;
  array[N] int resource_problems_rep;
  array[N] int gossip_government_rep;
  array[N] int gossip_politics_rep;
  array[N] int gossip_family_rep;
  array[N] int checks_power_rep;
  array[N] int remove_leaders_rep;
  array[N] int political_fission_rep;

  for (i in 1:N) {

    // ─────────────────────────────────────────────────────
    // Egalitarianism yrep
    // ─────────────────────────────────────────────────────

    egalitarianism_rep[i] =
      bernoulli_logit_rng(
        alpha[1] + beta[4] * climate_variation[i] + beta[5] * subsistence[i] +
          beta[6] * scarcity[i]
      );

    // ─────────────────────────────────────────────────────
    // Climate variability yrep
    // ─────────────────────────────────────────────────────

    temperature_variance_log_centered_rep[i] =
      normal_rng(alpha[2] + climate_variation[i], sigma);

    temperature_variance_rep[i] =
      exp(
        temperature_variance_log_centered_rep[i] +
        mean(temperature_variance_log)
      );

    mu1[i] = inv_logit(alpha[3] + lambda[1] * climate_variation[i]);
    mu2[i] = inv_logit(alpha[4] + lambda[2] * climate_variation[i]);

    temperature_predict_rep[i] =
      1.0 - beta_rng(mu1[i] * phi[1], (1 - mu1[i]) * phi[1]);

    precipitation_predict_rep[i] =
      1.0 - beta_rng(mu2[i] * phi[2], (1 - mu2[i]) * phi[2]);

    // ─────────────────────────────────────────────────────
    // Subsistence yrep
    // ─────────────────────────────────────────────────────

    percent_hunting_rep[i] =
      ordered_logistic_rng(subsistence[i], c1);

    large_game_hunting_rep[i] =
      bernoulli_logit_rng(alpha[5] + lambda[3] * subsistence[i]);

    food_sharing_rep[i] =
      ordered_logistic_rng(lambda[4] * subsistence[i], c2);

    // ─────────────────────────────────────────────────────
    // Scarcity yrep
    // ─────────────────────────────────────────────────────

    starvation_occurrence_rep[i] =
      ordered_logistic_rng(scarcity[i], c3);

    famine_occurrence_rep[i] =
      ordered_logistic_rng(lambda[5] * scarcity[i], c4);

    resource_problems_rep[i] =
      ordered_logistic_rng(lambda[6] * scarcity[i], c5);

    // ─────────────────────────────────────────────────────
    // Public opinion yrep
    // ─────────────────────────────────────────────────────

    gossip_government_rep[i] =
      bernoulli_logit_rng(alpha[6] + public_opinion[i]);

    gossip_politics_rep[i] =
      bernoulli_logit_rng(alpha[7] + lambda[7] * public_opinion[i]);

    gossip_family_rep[i] =
      bernoulli_logit_rng(alpha[8] + lambda[8] * public_opinion[i]);

    // ─────────────────────────────────────────────────────
    // Sanctions yrep
    // ─────────────────────────────────────────────────────

    checks_power_rep[i] =
      ordered_logistic_rng(sanctions[i], c6);

    remove_leaders_rep[i] =
      ordered_logistic_rng(lambda[9] * sanctions[i], c7);

    political_fission_rep[i] =
      ordered_logistic_rng(lambda[10] * sanctions[i], c8);

  }

}
