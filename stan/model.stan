data {
  int<lower=0> N;
  array[N] int percent_hunting;
  array[N] int large_game_hunting;
  array[N] int food_sharing;
  array[N] int gossip_government;
  array[N] int gossip_politics;
  array[N] int gossip_family;
  array[N] int checks_power;
  array[N] int remove_leaders;
  array[N] int political_fission;
  int<lower=0, upper=1> prior_only;
}

parameters {
  // loadings
  array[6] real lambda;
  // ordinal cutpoints
  ordered[9] phi1;
  ordered[6] phi2;
  ordered[3] phi3;
  ordered[3] phi4;
  ordered[2] phi5;
  // latent variables
  array[N] real subsistence;
  array[N] real public_opinion;
  array[N] real sanctions;
}

model {

  // priors
  lambda         ~ normal(1, 0.5);
  phi1           ~ normal(0, 2);
  phi2           ~ normal(0, 2);
  phi3           ~ normal(0, 2);
  phi4           ~ normal(0, 2);
  phi5           ~ normal(0, 2);
  subsistence    ~ normal(0, 1);
  public_opinion ~ normal(0, 1);
  sanctions      ~ normal(0, 1);

  // likelihood
  if (!prior_only) {

    for (i in 1:N) {

      // subsistence submodel
      percent_hunting[i] ~ ordered_logistic(1 * subsistence[i], phi1);
      if (large_game_hunting[i] != -9999) {
        large_game_hunting[i] ~ bernoulli_logit(lambda[1] * subsistence[i]);
      }
      if (food_sharing[i] != -9999) {
        food_sharing[i] ~ ordered_logistic(lambda[2] * subsistence[i], phi2);
      }

      // gossip submodel
      if (gossip_government[i] != -9999) {
        gossip_government[i] ~ bernoulli_logit(1 * public_opinion[i]);
        gossip_politics[i] ~ bernoulli_logit(lambda[3] * public_opinion[i]);
        gossip_family[i] ~ bernoulli_logit(lambda[4] * public_opinion[i]);
      }

      // sanctions submodel
      if (checks_power[i] != -9999) {
        checks_power[i] ~ ordered_logistic(1 * sanctions[i], phi3);
      }
      if (remove_leaders[i] != -9999) {
        remove_leaders[i] ~ ordered_logistic(lambda[5] * sanctions[i], phi4);
      }
      if (political_fission[i] != -9999) {
        political_fission[i] ~ ordered_logistic(lambda[6] * sanctions[i], phi5);
      }

    }

  }

}

