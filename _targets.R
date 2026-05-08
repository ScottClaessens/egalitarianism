library(stantargets)
library(targets)
library(tarchetypes)

tar_option_set(packages = c("ape", "bayesplot", "patchwork", "phangorn",
                            "scales", "tidyverse"))
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
  # run prior only model
  tar_stan_mcmc(
    name = prior,
    stan_files = "stan/model.stan",
    data = wrangle_data_list(data, mcc_tree, prior_only = 1),
    parallel_chains = 4,
    adapt_delta = 0.95,
    seed = 1
  ),
  # plot prior predictive check
  tar_target(
    plot_prior_check,
    plot_predictive_check(data, prior_draws_model, prior = TRUE)
  ),
  # run simulation validation
  tar_stan_mcmc(
    name = sim,
    stan_files = "stan/model.stan",
    data = wrangle_data_list(simulate_data(data), mcc_tree),
    parallel_chains = 4,
    adapt_delta = 0.95,
    seed = 1
  ),
  # plot simulation validation results
  tar_target(plot_simulation, plot_results(sim_draws_model, simulation = TRUE)),
  # fit model
  tar_stan_mcmc(
    name = fit,
    stan_files = "stan/model.stan",
    data = wrangle_data_list(data, mcc_tree),
    parallel_chains = 4,
    adapt_delta = 0.95,
    seed = 1
  ),
  # plot model results
  tar_target(plot_model, plot_results(fit_draws_model)),
  # plot posterior predictive check
  tar_target(
    plot_posterior_check,
    plot_predictive_check(data, fit_draws_model)
  )
)
