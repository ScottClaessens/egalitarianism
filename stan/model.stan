data {
  int<lower=0> N;
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
  int<lower=0, upper=1> prior_only;
}

transformed data {
  vector[N] temperature_variance_log;
  vector[N] temperature_variance_log_std;
  vector[N] temperature_unpredict;
  vector[N] precipitation_unpredict;
  // log and standardise temperature variance
  temperature_variance_log = log(temperature_variance);
  temperature_variance_log_std =
    (temperature_variance_log - mean(temperature_variance_log)) /
    sd(temperature_variance_log);
  // reverse climate predictability variables (higher = more unpredictable)
  temperature_unpredict = 1 - temperature_predict;
  precipitation_unpredict = 1 - precipitation_predict;
}

parameters {
  // loadings
  array[8] real lambda;
  // variances
  real<lower=0> sigma;
  // beta precision
  array[2] real<lower=0> phi;
  // ordinal cutpoints
  ordered[9] c1;
  ordered[6] c2;
  ordered[2] c3;
  ordered[3] c4;
  ordered[3] c5;
  ordered[3] c6;
  ordered[3] c7;
  ordered[2] c8;
  // latent variables
  array[N] real climate_variation;
  array[N] real subsistence;
  array[N] real scarcity;
  array[N] real public_opinion;
  array[N] real sanctions;
}

model {

  // initialise vectors
  vector[N] mu1;
  vector[N] mu2;

  // priors for parameters
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

  // priors for latent variables
  climate_variation ~ normal(0, 1);
  subsistence ~ normal(0, 1);
  scarcity ~ normal(0, 1);
  public_opinion ~ normal(0, 1);
  sanctions ~ normal(0, 1);

  // likelihood
  if (!prior_only) {

    for (i in 1:N) {

      // climate variability submodel
      temperature_variance_log_std[i] ~ normal(1 * climate_variation[i], sigma);
      mu1[i] = inv_logit(lambda[1] * climate_variation[i]);
      mu2[i] = inv_logit(lambda[2] * climate_variation[i]);
      temperature_unpredict[i] ~ beta(
        mu1[i] * phi[1],
        (1.0 - mu1[i]) * phi[1]
      );
      precipitation_unpredict[i] ~ beta(
        mu2[i] * phi[2],
        (1.0 - mu2[i]) * phi[2]
      );

      // subsistence submodel
      percent_hunting[i] ~ ordered_logistic(1 * subsistence[i], c1);
      if (large_game_hunting[i] != -9999) {
        large_game_hunting[i] ~ bernoulli_logit(lambda[1] * subsistence[i]);
      }
      if (food_sharing[i] != -9999) {
        food_sharing[i] ~ ordered_logistic(lambda[2] * subsistence[i], c2);
      }

      // scarcity submodel
      if (starvation_occurrence[i] != -9999) {
        starvation_occurrence[i] ~ ordered_logistic(1 * scarcity[i], c3);
      }
      if (famine_occurrence[i] != -9999) {
        famine_occurrence[i] ~ ordered_logistic(lambda[3] * scarcity[i], c4);
      }
      if (resource_problems[i] != -9999) {
        resource_problems[i] ~ ordered_logistic(lambda[4] * scarcity[i], c5);
      }

      // gossip submodel
      if (gossip_government[i] != -9999) {
        gossip_government[i] ~ bernoulli_logit(1 * public_opinion[i]);
        gossip_politics[i] ~ bernoulli_logit(lambda[5] * public_opinion[i]);
        gossip_family[i] ~ bernoulli_logit(lambda[6] * public_opinion[i]);
      }

      // sanctions submodel
      if (checks_power[i] != -9999) {
        checks_power[i] ~ ordered_logistic(1 * sanctions[i], c6);
      }
      if (remove_leaders[i] != -9999) {
        remove_leaders[i] ~ ordered_logistic(lambda[7] * sanctions[i], c7);
      }
      if (political_fission[i] != -9999) {
        political_fission[i] ~ ordered_logistic(lambda[8] * sanctions[i], c8);
      }

    }

  }

}

