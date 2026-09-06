# Coexistence and biodiversity effects under resource competition

This repository reproduces the numerical simulations and figures associated with the manuscript *Unifying species coexistence and biodiversity–ecosystem functioning: a geometric resource-use framework*. It implements a two-species, two-resource consumer-resource model and evaluates coexistence, transgressive overyielding, niche and fitness differences, and biodiversity effects across resource-parameter gradients.

## Reproduce the analysis

The project uses [`renv`](https://rstudio.github.io/renv/) to record R package versions and [`targets`](https://books.ropensci.org/targets/) to define the computational workflow. Install R and Quarto, clone or download this repository, and run the following commands from its root directory:

``` bash
Rscript setup.R
Rscript run.R
```

`setup.R` restores the package versions recorded in `renv.lock`. `run.R` builds only targets that are missing or out of date. Runtime depends on the computer because the complete workflow solves the consumer-resource model repeatedly for all parameter scenarios.

To render only the report after the pipeline is current:

``` bash
quarto render report.qmd
```

## Outputs

The pipeline creates:

- `output/figures/`: publication figures for all simulation scenarios;
- `output/tables/simulation-summary.csv`: combined numerical results;
- `output/tables/simulation-parameters.csv`: parameters reconstructed from the
  exact inputs used by each simulation scenario;
- `output/simulation-results/simulations.rds`: complete simulation objects;
- `output/simulation-results/plots.rds`: reusable R plot objects; and
- `output/report/report.html`: the rendered reproducibility report.

The analysis is simulation-based and requires no external empirical data. All model inputs and parameter scenarios are defined in `R/functions.R` and tracked by the pipeline.

## Repository structure

``` text
.
├── R/
│   ├── analysis.R        # assembly and file-export helpers
│   ├── figures.R         # plotting functions
│   └── functions.R       # parameters, model, and simulations
├── _targets.R            # complete computational workflow
├── report.qmd            # reproducibility report
├── DESCRIPTION           # direct R dependencies
├── renv.lock             # exact package versions
├── setup.R               # restore the R environment
├── run.R                 # build missing or outdated targets
├── Makefile              # command-line shortcuts
├── LICENSE               # software license
└── CITATION.cff          # citation metadata
```

## Reproducibility checks

After a successful run, inspect the pipeline with:

``` r
targets::tar_outdated()
targets::tar_visnetwork()
renv::status()
```

## Citation and license

Citation metadata are provided in `CITATION.cff`. Update that file with the manuscript DOI and repository DOI when they become available.

The analysis code is released under the MIT License. See `LICENSE` for details.
