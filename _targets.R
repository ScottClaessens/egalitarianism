options(tidyverse.quiet = TRUE)
library(stantargets)
library(targets)
library(tarchetypes)
library(tidyverse)

tar_option_set(
  packages = c("ape", "bayesplot", "loo", "patchwork", "phangorn",
               "rnaturalearth", "scales", "sf", "tidybayes", "tidyverse")
)
tar_source()

list(
  # get data urls
  tar_target(
    dplace_data_url,
    paste0(
      "https://raw.githubusercontent.com/D-PLACE/dplace-cldf/",
      "6c2008c187a297d1955b41d8ae80d8e31d404f6c/cldf/data.csv"
    ),
    format = "url"
  ),
  tar_target(
    dplace_societies_url,
    paste0(
      "https://raw.githubusercontent.com/D-PLACE/dplace-cldf/",
      "6c2008c187a297d1955b41d8ae80d8e31d404f6c/cldf/societies.csv"
    ),
    format = "url"
  ),
  tar_target(
    glottolog_languages_url,
    paste0(
      "https://raw.githubusercontent.com/glottolog/glottolog-cldf/",
      "072ca0d0410039fb8b779be8fc165bac575d2cda/cldf/languages.csv"
    ),
    format = "url"
  ),
  # get data file paths
  tar_target(tree_file, "data/tree/dplace.nxs", format = "file"),
  # load tree
  tar_target(tree, read.nexus(tree_file)),
  # compute maximum clade credibility tree
  tar_target(mcc_tree, phangorn::mcc(tree)),
  # load dplace data
  tar_target(
    data,
    load_dplace_data(
      dplace_data_url, dplace_societies_url,
      glottolog_languages_url, mcc_tree
    )
  ),
  # plot variable coverage
  tar_target(plot_variable_coverage, plot_coverage(data)),
  # plot variables on world map
  tar_map(
    values = tibble(
      variable = c(
        "temperature_variance", "temperature_predict", "precipitation_predict",
        "egalitarianism", "percent_hunting", "large_game_hunting",
        "food_sharing", "starvation_occurrence", "famine_occurrence",
        "resource_problems", "gossip_government", "gossip_politics",
        "gossip_family", "checks_power", "remove_leaders", "political_fission",
        "political_violence"
      )
    ),
    tar_target(plot_map, plot_variable_on_map(data, variable))
  ),
  # run prior only model
  tar_stan_mcmc(
    name = prior,
    stan_files = "stan/model_full.stan",
    data = wrangle_data_list(data, mcc_tree, prior_only = 1),
    parallel_chains = 4,
    adapt_delta = 0.95,
    seed = 1
  ),
  # plot prior predictive check
  tar_target(
    plot_prior_check,
    plot_predictive_check(data, prior_draws_model_full, prior = TRUE)
  ),
  # run simulation validation
  tar_stan_mcmc(
    name = sim,
    stan_files = "stan/model_full.stan",
    data = wrangle_data_list(simulate_data(data), mcc_tree),
    parallel_chains = 4,
    adapt_delta = 0.95,
    seed = 1
  ),
  # plot simulation validation results
  tar_target(
    plot_simulation,
    plot_results(sim_draws_model_full, simulation = TRUE)
  ),
  # fit baseline model
  tar_stan_mcmc(
    name = fit_baseline,
    stan_files = "stan/model_baseline.stan",
    data = wrangle_data_list(data, mcc_tree),
    parallel_chains = 4,
    seed = 1
  ),
  # fit measurement only model
  tar_stan_mcmc(
    name = fit_measurement_only,
    stan_files = "stan/model_measurement_only.stan",
    data = wrangle_data_list(data, mcc_tree),
    parallel_chains = 4,
    seed = 1
  ),
  # fit full model
  tar_stan_mcmc(
    name = fit_full,
    stan_files = "stan/model_full.stan",
    data = wrangle_data_list(data, mcc_tree),
    parallel_chains = 4,
    adapt_delta = 0.95,
    seed = 1
  ),
  # get loo-cv scores
  tar_target(
    fit_baseline_loo_model_baseline,
    fit_baseline_mcmc_model_baseline$loo()
  ),
  tar_target(
    fit_measurement_only_loo_model_measurement_only,
    fit_measurement_only_mcmc_model_measurement_only$loo()
  ),
  tar_target(
    fit_full_loo_model_full,
    fit_full_mcmc_model_full$loo()
  ),
  # plot model results
  tar_target(plot_model, plot_results(fit_full_draws_model_full)),
  # plot posterior predictive check
  tar_target(
    plot_posterior_check,
    plot_predictive_check(data, fit_full_draws_model_full)
  ),
  # plot total causal effects
  tar_target(
    plot_total,
    plot_total_causal_effects(fit_full_draws_model_full)
  )
)
