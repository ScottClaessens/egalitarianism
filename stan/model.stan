data {
  int<lower=0> N;
  array[N] int gossip_government;
  array[N] int gossip_politics;
  array[N] int gossip_family;
  array[N] int checks_power;
  array[N] int remove_leaders;
  array[N] int political_fission;
}

parameters {
  // loadings
  array[4] real lambda;
  // ordinal cutpoints
  ordered[3] phi_1;
  ordered[3] phi_2;
  ordered[2] phi_3;
  // latent variables
  array[N] real public_opinion;
  array[N] real sanctions;
}

model {

  // priors
  lambda           ~ normal(1, 0.5);
  phi_1            ~ normal(0, 2);
  phi_2            ~ normal(0, 2);
  phi_3            ~ normal(0, 2);
  public_opinion   ~ normal(0, 1);
  sanctions        ~ normal(0, 1);

  // likelihood
  for (i in 1:N) {

    // gossip submodel
    if (gossip_government[i] != -9999) {
      gossip_government[i] ~ bernoulli_logit(1 * public_opinion[i]);
      gossip_politics[i]   ~ bernoulli_logit(lambda[1] * public_opinion[i]);
      gossip_family[i]     ~ bernoulli_logit(lambda[2] * public_opinion[i]);
    }

    // sanctions submodel
    if (checks_power[i] != -9999) {
      checks_power[i] ~ ordered_logistic(1 * sanctions[i], phi_1);
    }
    if (remove_leaders[i] != -9999) {
      remove_leaders[i] ~ ordered_logistic(lambda[3] * sanctions[i], phi_2);
    }
    if (political_fission[i] != -9999) {
      political_fission[i] ~ ordered_logistic(lambda[4] * sanctions[i], phi_3);
    }

  }

}

