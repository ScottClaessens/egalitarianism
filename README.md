# The Evolution of Egalitarianism

## Getting Started

### System requirements

This software has been tested on a MacBook Pro.

### Installation

To run this code, you will need to [install R](https://www.r-project.org/) and 
the following R packages:

```r
install.packages(
  c("ape", "bayesplot", "cmdstanr", "patchwork", "phangorn", "remotes", 
    "tarchetypes", "targets", "tidyverse")
)
remotes::install_github("ropensci/stantargets")
```

### Execute code

To run the pipeline:

1. Clone this repository to your local machine
2. Open the R Project file `egalitarianism.Rproj`
3. In the console, run the full pipeline with `targets::tar_make()`

## Help

Any issues, please email scott.claessens@gmail.com.

## Authors

Scott Claessens, scott.claessens@gmail.com
