data {
  int<lower=0> N;
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

parameters {
  // loadings
  array[8] real lambda;
  // ordinal cutpoints
  ordered[9] phi1;
  ordered[6] phi2;
  ordered[2] phi3;
  ordered[3] phi4;
  ordered[3] phi5;
  ordered[3] phi6;
  ordered[3] phi7;
  ordered[2] phi8;
  // latent variables
  array[N] real subsistence;
  array[N] real scarcity;
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
  phi6           ~ normal(0, 2);
  phi7           ~ normal(0, 2);
  phi8           ~ normal(0, 2);
  subsistence    ~ normal(0, 1);
  scarcity       ~ normal(0, 1);
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

      // scarcity submodel
      if (starvation_occurrence[i] != -9999) {
        starvation_occurrence[i] ~ ordered_logistic(1 * scarcity[i], phi3);
      }
      if (famine_occurrence[i] != -9999) {
        famine_occurrence[i] ~ ordered_logistic(lambda[3] * scarcity[i], phi4);
      }
      if (resource_problems[i] != -9999) {
        resource_problems[i] ~ ordered_logistic(lambda[4] * scarcity[i], phi5);
      }

      // gossip submodel
      if (gossip_government[i] != -9999) {
        gossip_government[i] ~ bernoulli_logit(1 * public_opinion[i]);
        gossip_politics[i] ~ bernoulli_logit(lambda[5] * public_opinion[i]);
        gossip_family[i] ~ bernoulli_logit(lambda[6] * public_opinion[i]);
      }

      // sanctions submodel
      if (checks_power[i] != -9999) {
        checks_power[i] ~ ordered_logistic(1 * sanctions[i], phi6);
      }
      if (remove_leaders[i] != -9999) {
        remove_leaders[i] ~ ordered_logistic(lambda[7] * sanctions[i], phi7);
      }
      if (political_fission[i] != -9999) {
        political_fission[i] ~ ordered_logistic(lambda[8] * sanctions[i], phi8);
      }

    }

  }

}

