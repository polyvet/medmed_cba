# Cost–Benefit Analysis (CBA) Shiny App for Medical Mediation

This repository contains a single-file [Shiny](https://shiny.posit.co/) application (`CBA_medmed_app.R`) for estimating the economic and time savings from using mediation instead of litigation in medical malpractice disputes. The app implements a stochastic simulation model with user-customizable cost, duration, and uptake parameters.

## Features

- **Interactive interface** for scenario setup and results visualization
- **Stochastic simulation** using triangular distributions and sigmoid uptake modeling
- **Estimates** both monetary (NPV) and time savings
- **Configurable** for high-, middle-, and low-income country scenarios

## Usage

1. **Download or clone this repository.**
2. **Open** `CBA_medmed_app.R` in [RStudio](https://posit.co/products/open-source/rstudio/) or any R session.
3. **Install required packages** (if needed):

   ```r
   install.packages(c("shiny", "ggplot2", "triangle", "reshape2"))
