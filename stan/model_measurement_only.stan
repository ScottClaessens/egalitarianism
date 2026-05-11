functions {

  // ───────────────────────────────────────────────────────
  // Function to test if integer is present in integer array
  // ───────────────────────────────────────────────────────
  int in_array(int x, array[] int y) {
    for (i in 1:size(y)) {
      if (x == y[i]) {
        return 1;
      }
    }
    return 0;
  }

}

data {

  // ─────────────────────────────────────────────────────
  // Observed data
  // ─────────────────────────────────────────────────────

  int<lower=0> N; // total number of societies
  vector<lower=0>[N] temperature_variance;
  vector<lower=0, upper=1>[N] temperature_predict;
  vector<lower=0, upper=1>[N] precipitation_predict;
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
  array[N] int political_violence;

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
  int<lower=0> N_obs_violence;

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
  array[N_obs_violence]     int idx_violence;

  // ─────────────────────────────────────────────────────
  // Cholesky factor of phylogenetic correlation matrix
  // ─────────────────────────────────────────────────────

  matrix[N, N] Lcov_phylo;

  // ─────────────────────────────────────────────────────
  // Longitude and latitude coordinates converted to unit
  // sphere (x,y,z) and normalised (maximum distance = 1)
  // ─────────────────────────────────────────────────────

  array[N] vector[3] coords;

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

  vector<lower=0>[10] lambda;

  // ─────────────────────────────────────────────────────
  // Variances (sigma) and beta precision (phi) parameters
  // ─────────────────────────────────────────────────────

  real<lower=0> sigma;
  vector<lower=0>[2] phi;

  // ─────────────────────────────────────────────────────
  // Intercepts for non-ordinal variables
  // ─────────────────────────────────────────────────────

  vector[8] alpha;

  // ─────────────────────────────────────────────────────
  // Ordered cutpoint parameters
  // ─────────────────────────────────────────────────────

  ordered[2] c1;
  ordered[9] c2;
  ordered[6] c3;
  ordered[2] c4;
  ordered[3] c5;
  ordered[3] c6;
  ordered[3] c7;
  ordered[3] c8;
  ordered[2] c9;

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
  phi ~ normal(10, 1);
  alpha ~ normal(0, 1);
  c1 ~ normal(0, 3);
  c2 ~ normal(0, 3);
  c3 ~ normal(0, 3);
  c4 ~ normal(0, 3);
  c5 ~ normal(0, 3);
  c6 ~ normal(0, 3);
  c7 ~ normal(0, 3);
  c8 ~ normal(0, 3);
  c9 ~ normal(0, 3);
  climate_variation ~ normal(0, 1);
  public_opinion ~ normal(0, 1);
  sanctions ~ normal(0, 1);
  subsistence ~ normal(0, 1);
  scarcity ~ normal(0, 1);

  if (!prior_only) {

    political_violence[idx_violence] ~ ordered_logistic(
      rep_vector(0.0, N_obs_violence), c1
    );

    egalitarianism ~ bernoulli_logit(alpha[1]);

    // ─────────────────────────────────────────────────────
    // Climate variability measurement model
    // ─────────────────────────────────────────────────────

    temperature_variance_log_centered ~
      normal(alpha[2] + 1.0 * climate_variation, sigma);

    mu1 = inv_logit(alpha[3] + lambda[1] * climate_variation);
    mu2 = inv_logit(alpha[4] + lambda[2] * climate_variation);

    temperature_unpredict ~ beta(
      mu1 * phi[1] + 1e-06, (1.0 - mu1) * phi[1] + 1e-06
    );

    precipitation_unpredict ~ beta(
      mu2 * phi[2] + 1e-06, (1.0 - mu2) * phi[2] + 1e-06
    );

    // ─────────────────────────────────────────────────────
    // Subsistence measurement model
    // ─────────────────────────────────────────────────────

    percent_hunting ~ ordered_logistic(
      1.0 * subsistence, c2
    );

    large_game_hunting[idx_large_game] ~ bernoulli_logit(
      alpha[5] + lambda[3] * subsistence[idx_large_game]
    );

    food_sharing[idx_food_sharing] ~ ordered_logistic(
      lambda[4] * subsistence[idx_food_sharing], c3
    );

    // ─────────────────────────────────────────────────────
    // Scarcity measurement model
    // ─────────────────────────────────────────────────────

    starvation_occurrence[idx_starvation] ~ ordered_logistic(
      1.0 * scarcity[idx_starvation], c4
    );

    famine_occurrence[idx_famine] ~ ordered_logistic(
      lambda[5] * scarcity[idx_famine], c5
    );

    resource_problems[idx_resource] ~ ordered_logistic(
      lambda[6] * scarcity[idx_resource], c6
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
      1.0 * sanctions[idx_checks], c7
    );

    remove_leaders[idx_remove] ~ ordered_logistic(
      lambda[9] * sanctions[idx_remove], c8
    );

    political_fission[idx_fission] ~ ordered_logistic(
      lambda[10] * sanctions[idx_fission], c9
    );

  }

}

generated quantities {

  // ──────────────────────────────────────────────────────
  // Initialise linear predictors, yrep, and log likelihood
  // ──────────────────────────────────────────────────────

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
  array[N] int political_violence_rep;

  matrix[N, 17] lp = rep_matrix(0.0, N, 17);
  vector[N] log_lik;

  for (i in 1:N) {

    // ─────────────────────────────────────────────────────
    // Political violence
    // ─────────────────────────────────────────────────────

    political_violence_rep[i] = ordered_logistic_rng(0.0, c1);

    if (in_array(i, idx_violence)) {
      lp[i, 1] = ordered_logistic_lpmf(political_violence[i] | 0.0, c1);
    }

    // ─────────────────────────────────────────────────────
    // Egalitarianism
    // ─────────────────────────────────────────────────────

    egalitarianism_rep[i] = bernoulli_logit_rng(alpha[1]);

    lp[i, 2] = bernoulli_logit_lpmf(egalitarianism[i] | alpha[1]);

    // ─────────────────────────────────────────────────────
    // Climate variability
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

    temperature_predict_rep[i] = 1.0 - beta_rng(
      mu1[i] * phi[1] + 1e-06,
      (1 - mu1[i]) * phi[1] + 1e-06
    );

    precipitation_predict_rep[i] = 1.0 - beta_rng(
      mu2[i] * phi[2] + 1e-06,
      (1 - mu2[i]) * phi[2] + 1e-06
    );

    lp[i, 3] = normal_lpdf(
      temperature_variance_log_centered[i] |
      alpha[2] + climate_variation[i],
      sigma
    );

    lp[i, 4] = beta_lpdf(
      temperature_unpredict[i] |
      mu1[i] * phi[1] + 1e-06,
      (1 - mu1[i]) * phi[1] + 1e-06
    );

    lp[i, 5] = beta_lpdf(
      precipitation_unpredict[i] |
      mu2[i] * phi[2] + 1e-06,
      (1 - mu2[i]) * phi[2] + 1e-06
    );

    // ─────────────────────────────────────────────────────
    // Subsistence
    // ─────────────────────────────────────────────────────

    percent_hunting_rep[i] = ordered_logistic_rng(subsistence[i], c2);

    large_game_hunting_rep[i] =
      bernoulli_logit_rng(alpha[5] + lambda[3] * subsistence[i]);

    food_sharing_rep[i] =
      ordered_logistic_rng(lambda[4] * subsistence[i], c3);

    lp[i, 6] = ordered_logistic_lpmf(percent_hunting[i] | subsistence[i], c2);

    if (in_array(i, idx_large_game)) {
      lp[i, 7] = bernoulli_logit_lpmf(
        large_game_hunting[i] | alpha[5] + lambda[3] * subsistence[i]
      );
    }

    if (in_array(i, idx_food_sharing)) {
      lp[i, 8] = ordered_logistic_lpmf(
        food_sharing[i] | lambda[4] * subsistence[i], c3
      );
    }

    // ─────────────────────────────────────────────────────
    // Scarcity yrep
    // ─────────────────────────────────────────────────────

    starvation_occurrence_rep[i] =
      ordered_logistic_rng(scarcity[i], c4);

    famine_occurrence_rep[i] =
      ordered_logistic_rng(lambda[5] * scarcity[i], c5);

    resource_problems_rep[i] =
      ordered_logistic_rng(lambda[6] * scarcity[i], c6);

    if (in_array(i, idx_starvation)) {
      lp[i, 9] = ordered_logistic_lpmf(
        starvation_occurrence[i] | scarcity[i], c4
      );
    }

    if (in_array(i, idx_famine)) {
      lp[i, 10] = ordered_logistic_lpmf(
        famine_occurrence[i] | lambda[5] * scarcity[i], c5
      );
    }

    if (in_array(i, idx_resource)) {
      lp[i, 11] = ordered_logistic_lpmf(
        resource_problems[i] |lambda[6] * scarcity[i], c6
      );
    }

    // ─────────────────────────────────────────────────────
    // Public opinion yrep
    // ─────────────────────────────────────────────────────

    gossip_government_rep[i] =
      bernoulli_logit_rng(alpha[6] + public_opinion[i]);

    gossip_politics_rep[i] =
      bernoulli_logit_rng(alpha[7] + lambda[7] * public_opinion[i]);

    gossip_family_rep[i] =
      bernoulli_logit_rng(alpha[8] + lambda[8] * public_opinion[i]);

    if (in_array(i, idx_gossip)) {
      lp[i, 12] = bernoulli_logit_lpmf(
        gossip_government[i] | alpha[6] + public_opinion[i]
      );
      lp[i, 13] = bernoulli_logit_lpmf(
        gossip_politics[i] | alpha[7] + lambda[7] * public_opinion[i]
      );
      lp[i, 14] = bernoulli_logit_lpmf(
        gossip_family[i] | alpha[8] + lambda[8] * public_opinion[i]
      );
    }

    // ─────────────────────────────────────────────────────
    // Sanctions yrep
    // ─────────────────────────────────────────────────────

    checks_power_rep[i] =
      ordered_logistic_rng(sanctions[i], c7);

    remove_leaders_rep[i] =
      ordered_logistic_rng(lambda[9] * sanctions[i], c8);

    political_fission_rep[i] =
      ordered_logistic_rng(lambda[10] * sanctions[i], c9);

    if (in_array(i, idx_checks)) {
      lp[i, 15] = ordered_logistic_lpmf(
        checks_power[i] | sanctions[i], c7
      );
    }

    if (in_array(i, idx_remove)) {
      lp[i, 16] = ordered_logistic_lpmf(
        remove_leaders[i] | lambda[9] * sanctions[i], c8
      );
    }

    if (in_array(i, idx_fission)) {
      lp[i, 17] = ordered_logistic_lpmf(
        political_fission[i] | lambda[10] * sanctions[i], c9
      );
    }

  }

  // sum over rows to get one log_lik per society
  log_lik = lp * rep_vector(1.0, 17);

}
