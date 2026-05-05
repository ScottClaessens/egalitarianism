data {
  int<lower=0> N;
  array[N] int gossip_government;
  array[N] int gossip_politics;
  array[N] int gossip_family;
}

parameters {
  array[2] real lambda;
  array[N] real public_opinion;
}

model {

  // priors
  lambda ~ normal(1, 0.5);
  public_opinion ~ normal(0, 1);

  // likelihood
  for (i in 1:N) {

    // gossip submodel
    if (gossip_government[i] != -9999) {
      gossip_government[i] ~ bernoulli_logit(1 * public_opinion[i]);
      gossip_politics[i]   ~ bernoulli_logit(lambda[1] * public_opinion[i]);
      gossip_family[i]     ~ bernoulli_logit(lambda[2] * public_opinion[i]);
    }

  }

}

