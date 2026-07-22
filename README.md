# Stochastic cost-benefit analysis of mediation in medical malpractice disputes

This repository contains an interactive R/Shiny application and a reproducible
analysis script for projecting monetary and aggregate case-resolution time
savings from a mediation strategy compared with an all-trial strategy.

The model is intended for scenario analysis. Its outputs are conditional on the
user-supplied cost, duration, implementation, and mediation-success assumptions;
they are not empirical estimates for a particular jurisdiction.

## Case-flow model

For each year, the mediation strategy contains three pathways:

1. claims resolved through mediation;
2. unsuccessful mediation attempts that subsequently proceed to trial; and
3. claims that proceed directly to trial.

The **mediation-resolution share** is the proportion of all eligible claims
ultimately resolved through mediation. It is distinct from the **conditional
mediation-success probability**, which is the probability that an attempted
mediation succeeds. If `M_t` claims are resolved through mediation and `p` is
the conditional success probability, expected mediation attempts are
`A_t = M_t / p`. Failed attempts therefore equal `A_t - M_t`.

The comparator assumes that all eligible claims resolve through full trial.
Under the mediation strategy, annual incremental savings are:

```text
monetary savings = M_t × trial cost − A_t × mediation cost
time savings     = M_t × trial time − A_t × mediation time
```

Thus, every attempt incurs mediation cost and time, while only successful
mediation resolutions avoid trial. Trial costs and durations for claims that
remain trial-resolved occur under both strategies and cancel from the
incremental comparison.

## Primary defaults

The app opens with the illustrative high-income scenario used in the revised
manuscript:

- 10-year horizon;
- 2,000 eligible claims per year for scaled outputs;
- initial and final mediation-resolution shares of 15% and 75%;
- conditional mediation success of 95%;
- annual monetary discount rate of 5%;
- 10,000 Monte Carlo iterations and random seed 42100;
- monetary inputs in 2014 US dollars; and
- triangular distributions specified by minimum, most likely, and maximum
  values.

The primary uncertainty structure samples costs and durations independently for
each annual cohort within every Monte Carlo iteration. The app also provides a
fixed-horizon structural sensitivity in which one sampled set of values is held
across all years. The latter generally produces wider cumulative simulation
intervals because horizon-wide values do not average across years.

## Run the app

Install R and the required packages:

```r
install.packages(c("shiny", "ggplot2", "triangle", "reshape2", "scales"))
```

Then launch the app from the repository folder:

```r
shiny::runApp("CBA_medmed_app.R")
```

Select the uncertainty interpretation, change any inputs, and click **Go**.
The app reports yearly and cumulative results and a one-way conditional-success
scenario analysis.

## Reproduce the manuscript analyses

Run:

```r
source("run_all_analyses.R")
```

or, from a terminal:

```text
Rscript run_all_analyses.R
```

The script regenerates all CSV files in `results/`, including:

- primary totals and yearly results for the three illustrative scenarios;
- uncertainty-structure, conditional-success, discount-rate,
  mediation-resolution-ceiling, and case-mix sensitivity analyses;
- the high-income one-way sensitivity ranking;
- convergence results;
- combined total- and year-level output; and
- automated validation checks and an output manifest.

The sensitivity ranges are illustrative scenario bounds, not empirical
confidence intervals. The case-mix sensitivity jointly multiplies the
counterfactual trial cost and duration distributions for mediation-resolved
claims by 0.50, 0.75, or 1.00.

## Repository files

- `CBA_medmed_app.R` — Shiny application and model engine.
- `run_all_analyses.R` — fully reproducible primary and sensitivity analyses.
- `results/` — machine-readable outputs generated with seed 42100.
- `LICENSE` — repository licence.

## Adapting the model

The triangular distributions use inputs that non-specialist users can supply
readily when detailed empirical distributions are unavailable. Because the code
is open, users may replace them with other parametric distributions or adapt the
comparator when internally consistent pathway-specific cost and duration data
are available.

## AI-assisted development and editing

OpenAI’s ChatGPT was used during the revision process to assist with editing, restructuring, documenting, and checking the R/Shiny application and reproducible analysis code, as well as with language refinement of the associated manuscript and supplementary materials. All model assumptions, methodological decisions, code, analyses, and reported outputs were reviewed and verified by the authors, who take full responsibility for the final work.
