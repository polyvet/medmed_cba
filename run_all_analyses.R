command_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command_args, value = TRUE)
source_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
script_path <- if (length(file_arg) > 0) {
  normalizePath(sub("^--file=", "", file_arg[1]))
} else if (!is.null(source_path)) {
  normalizePath(source_path)
} else {
  normalizePath("run_all_analyses.R")
}
script_dir <- dirname(script_path)
revision_root <- normalizePath(file.path(script_dir, ".."))

app_candidates <- c(
  file.path(script_dir, "CBA_medmed_app.R"),
  file.path(script_dir, "app.R"),
  file.path(revision_root, "04_revised_Shiny_app", "app.R"),
  file.path(revision_root, "app.R")
)
app_path <- app_candidates[file.exists(app_candidates)][1]
if (is.na(app_path)) {
  stop("Could not locate the revised app.R model engine.")
}

model_environment <- new.env()
sys.source(app_path, model_environment)
output_dir <- file.path(script_dir, "results")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Reproducible analysis constants --------------------------------------------
time_horizon <- 10
base_cohort <- 100
annual_eligible_cases <- 2000
scale_to_2000 <- annual_eligible_cases / base_cohort
iterations <- 10000
random_seed <- 42100
primary_success_rate <- 0.95
primary_discount_rate <- 0.05
uncertainty_structures <- c("fixed_horizon", "annual_independent")

# These are the three illustrative scenarios used in the revised manuscript.
# The high-income scenario retains its 75% final resolution share.
scenarios <- list(
  "High-income" = list(
    min_cost_mediation = 1000,
    mode_cost_mediation = 3000,
    max_cost_mediation = 6000,
    min_cost_trial = 5000,
    mode_cost_trial = 15000,
    max_cost_trial = 40000,
    min_time_mediation = 1,
    mode_time_mediation = 3,
    max_time_mediation = 6,
    min_time_trial = 6,
    mode_time_trial = 18,
    max_time_trial = 36,
    mediation_resolution_start = 0.15,
    mediation_resolution_end = 0.75,
    k = 1.0
  ),
  "Middle-income" = list(
    min_cost_mediation = 500,
    mode_cost_mediation = 1200,
    max_cost_mediation = 2500,
    min_cost_trial = 3000,
    mode_cost_trial = 8000,
    max_cost_trial = 20000,
    min_time_mediation = 1,
    mode_time_mediation = 4,
    max_time_mediation = 8,
    min_time_trial = 12,
    mode_time_trial = 30,
    max_time_trial = 48,
    mediation_resolution_start = 0.10,
    mediation_resolution_end = 0.60,
    k = 0.8
  ),
  "Low-income" = list(
    min_cost_mediation = 200,
    mode_cost_mediation = 500,
    max_cost_mediation = 1000,
    min_cost_trial = 1000,
    mode_cost_trial = 4000,
    max_cost_trial = 10000,
    min_time_mediation = 1,
    mode_time_mediation = 6,
    max_time_mediation = 12,
    min_time_trial = 18,
    mode_time_trial = 36,
    max_time_trial = 72,
    mediation_resolution_start = 0.05,
    mediation_resolution_end = 0.35,
    k = 0.6
  )
)

scenario_inputs <- do.call(rbind, lapply(names(scenarios), function(name) {
  values <- scenarios[[name]]
  data.frame(
    Scenario = name,
    Min_Mediation_Cost_USD = values$min_cost_mediation,
    Mode_Mediation_Cost_USD = values$mode_cost_mediation,
    Max_Mediation_Cost_USD = values$max_cost_mediation,
    Min_Trial_Cost_USD = values$min_cost_trial,
    Mode_Trial_Cost_USD = values$mode_cost_trial,
    Max_Trial_Cost_USD = values$max_cost_trial,
    Min_Mediation_Time_Months = values$min_time_mediation,
    Mode_Mediation_Time_Months = values$mode_time_mediation,
    Max_Mediation_Time_Months = values$max_time_mediation,
    Min_Trial_Time_Months = values$min_time_trial,
    Mode_Trial_Time_Months = values$mode_time_trial,
    Max_Trial_Time_Months = values$max_time_trial,
    Initial_Mediation_Resolution_Share =
      values$mediation_resolution_start,
    Final_Mediation_Resolution_Share = values$mediation_resolution_end,
    Logistic_Steepness_k = values$k,
    Currency = "USD",
    Price_Year = 2014,
    stringsAsFactors = FALSE
  )
}))

write.csv(
  scenario_inputs,
  file.path(output_dir, "01_scenario_inputs.csv"),
  row.names = FALSE
)

# Build the complete analysis grid -------------------------------------------
make_grid <- function(analysis_family,
                      scenario_names,
                      uncertainty_values,
                      success_values = primary_success_rate,
                      discount_values = primary_discount_rate,
                      final_share_values = NA_real_,
                      case_mix_values = 1) {
  grid <- expand.grid(
    Scenario = scenario_names,
    Uncertainty_Structure = uncertainty_values,
    Conditional_Success_Rate = success_values,
    Discount_Rate = discount_values,
    Final_Share_Override = final_share_values,
    Case_Mix_Cost_Multiplier = case_mix_values,
    Case_Mix_Time_Multiplier = case_mix_values,
    stringsAsFactors = FALSE
  )
  # Case-mix multipliers are paired, rather than fully crossed with each other.
  grid <- grid[
    grid$Case_Mix_Cost_Multiplier == grid$Case_Mix_Time_Multiplier,
  ]
  grid$Analysis_Family <- analysis_family
  grid
}

analysis_grid <- rbind(
  make_grid(
    "Primary scenarios",
    names(scenarios),
    "annual_independent"
  ),
  make_grid(
    "Uncertainty-structure sensitivity",
    names(scenarios),
    uncertainty_structures
  ),
  make_grid(
    "Conditional-success sensitivity",
    names(scenarios),
    uncertainty_structures,
    success_values = c(1.00, 0.95, 0.90, 0.85, 0.80, 0.75)
  ),
  make_grid(
    "Discount-rate sensitivity",
    names(scenarios),
    uncertainty_structures,
    discount_values = c(0.03, 0.05, 0.08)
  ),
  make_grid(
    "Resolution-ceiling sensitivity",
    "High-income",
    uncertainty_structures,
    final_share_values = c(0.60, 0.75)
  ),
  make_grid(
    "Illustrative paired case-mix sensitivity",
    "High-income",
    uncertainty_structures,
    case_mix_values = c(0.50, 0.75, 1.00)
  )
)

analysis_grid$Run_ID <- sprintf("RUN_%03d", seq_len(nrow(analysis_grid)))

# Run one model specification -------------------------------------------------
run_one <- function(specification) {
  scenario <- scenarios[[specification$Scenario]]
  k_value <- scenario$k
  scenario$k <- NULL

  if (!is.na(specification$Final_Share_Override)) {
    scenario$mediation_resolution_end <-
      specification$Final_Share_Override
  }

  # This sensitivity represents the possibility that cases ultimately resolved
  # through mediation would have had lower counterfactual trial cost and duration.
  scenario$min_cost_trial <- scenario$min_cost_trial *
    specification$Case_Mix_Cost_Multiplier
  scenario$mode_cost_trial <- scenario$mode_cost_trial *
    specification$Case_Mix_Cost_Multiplier
  scenario$max_cost_trial <- scenario$max_cost_trial *
    specification$Case_Mix_Cost_Multiplier
  scenario$min_time_trial <- scenario$min_time_trial *
    specification$Case_Mix_Time_Multiplier
  scenario$mode_time_trial <- scenario$mode_time_trial *
    specification$Case_Mix_Time_Multiplier
  scenario$max_time_trial <- scenario$max_time_trial *
    specification$Case_Mix_Time_Multiplier

  result <- model_environment$cost_benefit_analysis_per_cohort(
    max_years = time_horizon,
    params = scenario,
    interest_rate = specification$Discount_Rate,
    k_value = k_value,
    mediation_success_rate = specification$Conditional_Success_Rate,
    total_cases = base_cohort,
    n_runs = iterations,
    random_seed = random_seed,
    uncertainty_structure = specification$Uncertainty_Structure
  )

  monetary_draws <-
    result$simulation_totals$Total_NPV_Monetary_Savings
  time_draws <- result$simulation_totals$Total_Time_Savings_Months
  year_rows <- result$yearly

  total_row <- data.frame(
    Analysis_Family = specification$Analysis_Family,
    Run_ID = specification$Run_ID,
    Scenario = specification$Scenario,
    Uncertainty_Structure = specification$Uncertainty_Structure,
    Conditional_Success_Rate = specification$Conditional_Success_Rate,
    Discount_Rate = specification$Discount_Rate,
    Initial_Mediation_Resolution_Share =
      scenario$mediation_resolution_start,
    Final_Mediation_Resolution_Share =
      scenario$mediation_resolution_end,
    Logistic_Steepness_k = k_value,
    Case_Mix_Cost_Multiplier =
      specification$Case_Mix_Cost_Multiplier,
    Case_Mix_Time_Multiplier =
      specification$Case_Mix_Time_Multiplier,
    Iterations = iterations,
    Random_Seed = random_seed,
    Annual_Eligible_Cases = annual_eligible_cases,
    Time_Horizon_Years = time_horizon,
    Currency = "USD",
    Price_Year = 2014,
    Comparator = "All eligible claims resolve through trial",
    Mediation_Resolved_Per_100_Annual_Cases =
      sum(year_rows$Mediation_Resolved),
    Mediation_Attempts_Per_100_Annual_Cases =
      sum(year_rows$Mediation_Attempts),
    Failed_Attempts_To_Trial_Per_100_Annual_Cases =
      sum(year_rows$Failed_Mediation_to_Trial),
    Direct_To_Trial_Per_100_Annual_Cases =
      sum(year_rows$Direct_to_Trial),
    Trial_Resolved_Per_100_Annual_Cases =
      sum(year_rows$Trial_Resolved),
    Mediation_Resolved_2000_Annual_Cases =
      sum(year_rows$Mediation_Resolved) * scale_to_2000,
    Mediation_Attempts_2000_Annual_Cases =
      sum(year_rows$Mediation_Attempts) * scale_to_2000,
    Failed_Attempts_To_Trial_2000_Annual_Cases =
      sum(year_rows$Failed_Mediation_to_Trial) * scale_to_2000,
    Trial_Resolved_2000_Annual_Cases =
      sum(year_rows$Trial_Resolved) * scale_to_2000,
    Mean_NPV_Savings_Per_100_Annual_Cases_USD = mean(monetary_draws),
    Median_NPV_Savings_Per_100_Annual_Cases_USD = median(monetary_draws),
    LB95_NPV_Savings_Per_100_Annual_Cases_USD =
      unname(quantile(monetary_draws, 0.025)),
    UB95_NPV_Savings_Per_100_Annual_Cases_USD =
      unname(quantile(monetary_draws, 0.975)),
    Probability_Positive_NPV_Savings = mean(monetary_draws > 0),
    Mean_NPV_Savings_2000_Annual_Cases_USD =
      mean(monetary_draws) * scale_to_2000,
    Median_NPV_Savings_2000_Annual_Cases_USD =
      median(monetary_draws) * scale_to_2000,
    LB95_NPV_Savings_2000_Annual_Cases_USD =
      unname(quantile(monetary_draws, 0.025)) * scale_to_2000,
    UB95_NPV_Savings_2000_Annual_Cases_USD =
      unname(quantile(monetary_draws, 0.975)) * scale_to_2000,
    Mean_Months_Saved_Per_100_Annual_Cases = mean(time_draws),
    Median_Months_Saved_Per_100_Annual_Cases = median(time_draws),
    LB95_Months_Saved_Per_100_Annual_Cases =
      unname(quantile(time_draws, 0.025)),
    UB95_Months_Saved_Per_100_Annual_Cases =
      unname(quantile(time_draws, 0.975)),
    Probability_Positive_Time_Savings = mean(time_draws > 0),
    Mean_Years_Saved_2000_Annual_Cases =
      mean(time_draws) * scale_to_2000 / 12,
    Median_Years_Saved_2000_Annual_Cases =
      median(time_draws) * scale_to_2000 / 12,
    LB95_Years_Saved_2000_Annual_Cases =
      unname(quantile(time_draws, 0.025)) * scale_to_2000 / 12,
    UB95_Years_Saved_2000_Annual_Cases =
      unname(quantile(time_draws, 0.975)) * scale_to_2000 / 12,
    Year_1_Mediation_Resolution_Share =
      year_rows$Mediation_Resolution_Share[1],
    Year_5_Mediation_Resolution_Share =
      year_rows$Mediation_Resolution_Share[5],
    Year_10_Mediation_Resolution_Share =
      year_rows$Mediation_Resolution_Share[10],
    stringsAsFactors = FALSE
  )

  yearly_rows <- data.frame(
    Analysis_Family = specification$Analysis_Family,
    Run_ID = specification$Run_ID,
    Scenario = specification$Scenario,
    Uncertainty_Structure = specification$Uncertainty_Structure,
    Conditional_Success_Rate = specification$Conditional_Success_Rate,
    Discount_Rate = specification$Discount_Rate,
    Final_Mediation_Resolution_Share =
      scenario$mediation_resolution_end,
    Case_Mix_Cost_Multiplier =
      specification$Case_Mix_Cost_Multiplier,
    Case_Mix_Time_Multiplier =
      specification$Case_Mix_Time_Multiplier,
    Year = year_rows$Year,
    Mediation_Resolution_Share =
      year_rows$Mediation_Resolution_Share,
    Mediation_Resolved_Per_100_Cases = year_rows$Mediation_Resolved,
    Mediation_Attempts_Per_100_Cases = year_rows$Mediation_Attempts,
    Failed_Attempts_To_Trial_Per_100_Cases =
      year_rows$Failed_Mediation_to_Trial,
    Direct_To_Trial_Per_100_Cases = year_rows$Direct_to_Trial,
    Trial_Resolved_Per_100_Cases = year_rows$Trial_Resolved,
    Mean_NPV_Savings_Per_100_Cases_USD =
      year_rows$Mean_Monetary_Savings,
    LB95_NPV_Savings_Per_100_Cases_USD = year_rows$LB_Monetary,
    UB95_NPV_Savings_Per_100_Cases_USD = year_rows$UB_Monetary,
    Mean_Months_Saved_Per_100_Cases = year_rows$Mean_Time_Savings,
    LB95_Months_Saved_Per_100_Cases = year_rows$LB_Time,
    UB95_Months_Saved_Per_100_Cases = year_rows$UB_Time,
    Mean_NPV_Savings_2000_Cases_USD =
      year_rows$Mean_Monetary_Savings * scale_to_2000,
    LB95_NPV_Savings_2000_Cases_USD =
      year_rows$LB_Monetary * scale_to_2000,
    UB95_NPV_Savings_2000_Cases_USD =
      year_rows$UB_Monetary * scale_to_2000,
    Mean_Years_Saved_2000_Cases =
      year_rows$Mean_Time_Savings * scale_to_2000 / 12,
    LB95_Years_Saved_2000_Cases =
      year_rows$LB_Time * scale_to_2000 / 12,
    UB95_Years_Saved_2000_Cases =
      year_rows$UB_Time * scale_to_2000 / 12,
    stringsAsFactors = FALSE
  )

  list(total = total_row, yearly = yearly_rows)
}

run_results <- lapply(seq_len(nrow(analysis_grid)), function(index) {
  run_one(analysis_grid[index, ])
})
all_totals <- do.call(rbind, lapply(run_results, `[[`, "total"))
all_yearly <- do.call(rbind, lapply(run_results, `[[`, "yearly"))

# Family-specific CSV outputs -------------------------------------------------
primary_results <- all_totals[
  all_totals$Analysis_Family == "Primary scenarios",
]
write.csv(
  primary_results,
  file.path(output_dir, "02_primary_scenario_totals.csv"),
  row.names = FALSE
)

primary_results_yearly <- all_yearly[
  all_yearly$Analysis_Family == "Primary scenarios",
]
write.csv(
  primary_results_yearly,
  file.path(output_dir, "03_primary_scenario_yearly.csv"),
  row.names = FALSE
)

family_files <- c(
  "Uncertainty-structure sensitivity" =
    "04_uncertainty_structure_sensitivity.csv",
  "Conditional-success sensitivity" =
    "05_conditional_success_sensitivity.csv",
  "Discount-rate sensitivity" =
    "06_discount_rate_sensitivity.csv",
  "Resolution-ceiling sensitivity" =
    "07_resolution_ceiling_sensitivity.csv",
  "Illustrative paired case-mix sensitivity" =
    "08_illustrative_case_mix_sensitivity.csv"
)

for (family_name in names(family_files)) {
  write.csv(
    all_totals[all_totals$Analysis_Family == family_name, ],
    file.path(output_dir, family_files[[family_name]]),
    row.names = FALSE
  )
}

# Compact one-way driver ranking for the high scenario -----------------------
# The supplement reports this ranking under the fixed-horizon structure.
fixed_horizon_base <- all_totals[
  all_totals$Analysis_Family == "Uncertainty-structure sensitivity" &
    all_totals$Scenario == "High-income" &
    all_totals$Uncertainty_Structure == "fixed_horizon",
]
base_npv <- fixed_horizon_base$Mean_NPV_Savings_2000_Annual_Cases_USD
base_time <- fixed_horizon_base$Mean_Years_Saved_2000_Annual_Cases

ranking_sources <- list(
  "Conditional success rate" = all_totals[
    all_totals$Analysis_Family == "Conditional-success sensitivity" &
      all_totals$Scenario == "High-income" &
      all_totals$Uncertainty_Structure == "fixed_horizon",
  ],
  "Discount rate" = all_totals[
    all_totals$Analysis_Family == "Discount-rate sensitivity" &
      all_totals$Scenario == "High-income" &
      all_totals$Uncertainty_Structure == "fixed_horizon",
  ],
  "Final mediation-resolution share" = all_totals[
    all_totals$Analysis_Family == "Resolution-ceiling sensitivity" &
      all_totals$Uncertainty_Structure == "fixed_horizon",
  ],
  "Paired counterfactual trial case-mix multiplier" = all_totals[
    all_totals$Analysis_Family ==
      "Illustrative paired case-mix sensitivity" &
      all_totals$Uncertainty_Structure == "fixed_horizon",
  ]
)

range_labels <- c(
  "Conditional success rate" = "0.75 to 1.00; base value 0.95",
  "Discount rate" = "0.03 to 0.08; base value 0.05",
  "Final mediation-resolution share" =
    "0.60 to 0.75; base value 0.75",
  "Paired counterfactual trial case-mix multiplier" =
    "0.50 to 1.00; base value 1.00"
)

one_way_ranking <- do.call(rbind, lapply(names(ranking_sources), function(name) {
  source <- ranking_sources[[name]]
  npv_values <- source$Mean_NPV_Savings_2000_Annual_Cases_USD
  time_values <- source$Mean_Years_Saved_2000_Annual_Cases
  data.frame(
    Factor = name,
    Tested_Range = unname(range_labels[name]),
    Fixed_Horizon_Base_Mean_NPV_USD = base_npv,
    Minimum_Mean_NPV_USD = min(npv_values),
    Maximum_Mean_NPV_USD = max(npv_values),
    Absolute_NPV_Range_USD = max(npv_values) - min(npv_values),
    NPV_Range_Percent_Of_Fixed_Horizon_Base =
      100 * (max(npv_values) - min(npv_values)) / base_npv,
    Fixed_Horizon_Base_Mean_Years_Saved = base_time,
    Minimum_Mean_Years_Saved = min(time_values),
    Maximum_Mean_Years_Saved = max(time_values),
    Absolute_Time_Range_Years = max(time_values) - min(time_values),
    Time_Range_Percent_Of_Fixed_Horizon_Base =
      100 * (max(time_values) - min(time_values)) / base_time,
    Scope_Note = paste(
      "One factor changed at a time in the high-income fixed-horizon",
      "scenario; tested ranges are illustrative, not empirical confidence bounds."
    ),
    stringsAsFactors = FALSE
  )
}))
one_way_ranking$NPV_Driver_Rank <- rank(
  -one_way_ranking$Absolute_NPV_Range_USD,
  ties.method = "min"
)
one_way_ranking$Time_Driver_Rank <- rank(
  -one_way_ranking$Absolute_Time_Range_Years,
  ties.method = "min"
)
one_way_ranking <- one_way_ranking[order(one_way_ranking$NPV_Driver_Rank), ]
row.names(one_way_ranking) <- NULL

write.csv(
  one_way_ranking,
  file.path(output_dir, "09_one_way_sensitivity_driver_ranking.csv"),
  row.names = FALSE
)

# Convergence analysis --------------------------------------------------------
iteration_counts <- c(1000, 2500, 5000, 10000, 20000)
convergence_rows <- list()
convergence_index <- 1

for (scenario_name in names(scenarios)) {
  scenario <- scenarios[[scenario_name]]
  k_value <- scenario$k
  scenario$k <- NULL

  for (iteration_count in iteration_counts) {
    result <- model_environment$cost_benefit_analysis_per_cohort(
      max_years = time_horizon,
      params = scenario,
      interest_rate = primary_discount_rate,
      k_value = k_value,
      mediation_success_rate = primary_success_rate,
      total_cases = base_cohort,
      n_runs = iteration_count,
      random_seed = random_seed,
      uncertainty_structure = "fixed_horizon"
    )
    convergence_rows[[convergence_index]] <- data.frame(
      Scenario = scenario_name,
      Iterations = iteration_count,
      Mean_NPV_Savings_Per_100_Annual_Cases_USD =
        result$total$Mean_Total_Monetary,
      LB95_NPV_Savings_Per_100_Annual_Cases_USD =
        result$total$LB_Total_Monetary,
      UB95_NPV_Savings_Per_100_Annual_Cases_USD =
        result$total$UB_Total_Monetary,
      Mean_Months_Saved_Per_100_Annual_Cases =
        result$total$Mean_Total_Time,
      LB95_Months_Saved_Per_100_Annual_Cases =
        result$total$LB_Total_Time,
      UB95_Months_Saved_Per_100_Annual_Cases =
        result$total$UB_Total_Time,
      stringsAsFactors = FALSE
    )
    convergence_index <- convergence_index + 1
  }
}

convergence <- do.call(rbind, convergence_rows)
reference <- convergence[convergence$Iterations == max(iteration_counts), c(
  "Scenario",
  "Mean_NPV_Savings_Per_100_Annual_Cases_USD",
  "Mean_Months_Saved_Per_100_Annual_Cases"
)]
names(reference)[2:3] <- c("Reference_NPV", "Reference_Time")
convergence <- merge(convergence, reference, by = "Scenario", sort = FALSE)
convergence$NPV_Percent_Difference_From_20000 <- 100 * (
  convergence$Mean_NPV_Savings_Per_100_Annual_Cases_USD /
    convergence$Reference_NPV - 1
)
convergence$Time_Percent_Difference_From_20000 <- 100 * (
  convergence$Mean_Months_Saved_Per_100_Annual_Cases /
    convergence$Reference_Time - 1
)
convergence$Reference_NPV <- NULL
convergence$Reference_Time <- NULL
convergence$Scenario <- factor(
  convergence$Scenario,
  levels = names(scenarios)
)
convergence <- convergence[order(
  convergence$Scenario,
  convergence$Iterations
), ]
convergence$Scenario <- as.character(convergence$Scenario)
row.names(convergence) <- NULL

write.csv(
  convergence,
  file.path(output_dir, "10_convergence_results.csv"),
  row.names = FALSE
)

write.csv(
  all_totals,
  file.path(output_dir, "11_all_model_run_totals.csv"),
  row.names = FALSE
)
write.csv(
  all_yearly,
  file.path(output_dir, "12_all_model_run_yearly_results.csv"),
  row.names = FALSE
)

# Validation checks -----------------------------------------------------------
validation_checks <- data.frame(
  Check = character(),
  Status = character(),
  Details = character(),
  stringsAsFactors = FALSE
)

add_check <- function(name, passed, details) {
  validation_checks <<- rbind(
    validation_checks,
    data.frame(
      Check = name,
      Status = if (isTRUE(passed)) "PASS" else "FAIL",
      Details = details,
      stringsAsFactors = FALSE
    )
  )
}

add_check(
  "Expected total run count",
  nrow(all_totals) == 73,
  sprintf("Observed %d total model runs; expected 73.", nrow(all_totals))
)
add_check(
  "Expected yearly row count",
  nrow(all_yearly) == 730,
  sprintf("Observed %d yearly rows; expected 730.", nrow(all_yearly))
)
add_check(
  "No missing total outputs",
  !anyNA(all_totals),
  "All total-level specification and result fields are populated."
)
add_check(
  "Mediation-resolved plus trial-resolved cases",
  all(abs(
    all_totals$Mediation_Resolved_Per_100_Annual_Cases +
      all_totals$Trial_Resolved_Per_100_Annual_Cases -
      base_cohort * time_horizon
  ) < 1e-8),
  "The two mutually exclusive final-resolution pathways sum to 1,000 cases in every run."
)
add_check(
  "Mediation attempts identity",
  all(abs(
    all_totals$Mediation_Attempts_Per_100_Annual_Cases -
      all_totals$Mediation_Resolved_Per_100_Annual_Cases /
        all_totals$Conditional_Success_Rate
  ) < 1e-8),
  "Attempted mediations equal completed mediation resolutions divided by conditional success."
)
add_check(
  "Trial pathway identity",
  all(abs(
    all_totals$Failed_Attempts_To_Trial_Per_100_Annual_Cases +
      all_totals$Direct_To_Trial_Per_100_Annual_Cases -
      all_totals$Trial_Resolved_Per_100_Annual_Cases
  ) < 1e-8),
  "Failed mediation attempts plus direct-to-trial cases equal all trial-resolved cases."
)

success_subset <- all_totals[
  all_totals$Analysis_Family == "Conditional-success sensitivity",
]
success_monotonic <- TRUE
for (scenario_name in names(scenarios)) {
  for (structure in uncertainty_structures) {
    subset <- success_subset[
      success_subset$Scenario == scenario_name &
        success_subset$Uncertainty_Structure == structure,
    ]
    subset <- subset[order(subset$Conditional_Success_Rate), ]
    success_monotonic <- success_monotonic &&
      all(diff(subset$Mean_NPV_Savings_2000_Annual_Cases_USD) > 0) &&
      all(diff(subset$Mean_Years_Saved_2000_Annual_Cases) > 0)
  }
}
add_check(
  "Conditional-success monotonicity",
  success_monotonic,
  "Mean monetary and time savings increase as conditional mediation success increases."
)

discount_subset <- all_totals[
  all_totals$Analysis_Family == "Discount-rate sensitivity",
]
discount_monotonic <- TRUE
discount_time_constant <- TRUE
for (scenario_name in names(scenarios)) {
  for (structure in uncertainty_structures) {
    subset <- discount_subset[
      discount_subset$Scenario == scenario_name &
        discount_subset$Uncertainty_Structure == structure,
    ]
    subset <- subset[order(subset$Discount_Rate), ]
    discount_monotonic <- discount_monotonic &&
      all(diff(subset$Mean_NPV_Savings_2000_Annual_Cases_USD) < 0)
    discount_time_constant <- discount_time_constant &&
      diff(range(subset$Mean_Years_Saved_2000_Annual_Cases)) < 1e-8
  }
}
add_check(
  "Discount-rate monotonicity",
  discount_monotonic,
  "Mean NPV savings decrease as the monetary discount rate increases."
)
add_check(
  "Time remains undiscounted",
  discount_time_constant,
  "Time-savings outputs are identical across monetary discount-rate runs."
)

ceiling_subset <- all_totals[
  all_totals$Analysis_Family == "Resolution-ceiling sensitivity" &
    all_totals$Uncertainty_Structure == "fixed_horizon",
]
ceiling_subset <- ceiling_subset[order(
  ceiling_subset$Final_Mediation_Resolution_Share
), ]
add_check(
  "Resolution-ceiling direction",
  all(diff(ceiling_subset$Mean_NPV_Savings_2000_Annual_Cases_USD) > 0) &&
    all(diff(ceiling_subset$Mean_Years_Saved_2000_Annual_Cases) > 0),
  "The 75% high-scenario ceiling produces greater mean savings than the conservative 60% sensitivity."
)

case_mix_subset <- all_totals[
  all_totals$Analysis_Family ==
    "Illustrative paired case-mix sensitivity" &
    all_totals$Uncertainty_Structure == "fixed_horizon",
]
case_mix_subset <- case_mix_subset[order(
  case_mix_subset$Case_Mix_Cost_Multiplier
), ]
add_check(
  "Case-mix direction",
  all(diff(case_mix_subset$Mean_NPV_Savings_2000_Annual_Cases_USD) > 0) &&
    all(diff(case_mix_subset$Mean_Years_Saved_2000_Annual_Cases) > 0),
  "Savings rise as the counterfactual trial cost/time multiplier rises from 0.50 to 1.00."
)

submitted <- data.frame(
  Scenario = c("High-income", "Middle-income", "Low-income"),
  Submitted_NPV_2000 = c(115.22e6, 47.97e6, 13.77e6),
  Submitted_Years_2000 = c(13349, 16052, 12705),
  stringsAsFactors = FALSE
)
original_equivalent <- success_subset[
  success_subset$Conditional_Success_Rate == 1 &
    success_subset$Uncertainty_Structure == "fixed_horizon",
]
reconciliation <- merge(original_equivalent, submitted, by = "Scenario")
reconciliation$NPV_Relative_Difference <-
  reconciliation$Mean_NPV_Savings_2000_Annual_Cases_USD /
    reconciliation$Submitted_NPV_2000 - 1
reconciliation$Time_Relative_Difference <-
  reconciliation$Mean_Years_Saved_2000_Annual_Cases /
    reconciliation$Submitted_Years_2000 - 1
add_check(
  "Original 100% success result reconciliation",
  all(abs(reconciliation$NPV_Relative_Difference) < 0.02) &&
    all(abs(reconciliation$Time_Relative_Difference) < 0.01),
  "Fixed-seed 100% success runs remain within 2% (NPV) and 1% (time) of the submitted scenario totals."
)

fixed_widths <- all_totals[
  all_totals$Analysis_Family == "Uncertainty-structure sensitivity" &
    all_totals$Uncertainty_Structure == "fixed_horizon",
  c(
    "Scenario",
    "LB95_NPV_Savings_2000_Annual_Cases_USD",
    "UB95_NPV_Savings_2000_Annual_Cases_USD"
  )
]
annual_widths <- all_totals[
  all_totals$Analysis_Family == "Uncertainty-structure sensitivity" &
    all_totals$Uncertainty_Structure == "annual_independent",
  c(
    "Scenario",
    "LB95_NPV_Savings_2000_Annual_Cases_USD",
    "UB95_NPV_Savings_2000_Annual_Cases_USD"
  )
]
width_comparison <- merge(
  fixed_widths,
  annual_widths,
  by = "Scenario",
  suffixes = c("_Fixed", "_Annual")
)
fixed_interval_width <-
  width_comparison$UB95_NPV_Savings_2000_Annual_Cases_USD_Fixed -
  width_comparison$LB95_NPV_Savings_2000_Annual_Cases_USD_Fixed
annual_interval_width <-
  width_comparison$UB95_NPV_Savings_2000_Annual_Cases_USD_Annual -
  width_comparison$LB95_NPV_Savings_2000_Annual_Cases_USD_Annual
add_check(
  "Uncertainty-structure interval behavior",
  all(fixed_interval_width > annual_interval_width),
  "Fixed uncertain parameters yield wider total-NPV intervals than independently redrawn annual values."
)

convergence_10000 <- convergence[convergence$Iterations == 10000, ]
add_check(
  "10,000-iteration convergence",
  all(abs(convergence_10000$NPV_Percent_Difference_From_20000) < 1) &&
    all(abs(convergence_10000$Time_Percent_Difference_From_20000) < 1),
  "For all scenarios, 10,000-run means are within 1% of the 20,000-run reference means."
)

write.csv(
  validation_checks,
  file.path(output_dir, "13_validation_checks.csv"),
  row.names = FALSE
)

manifest <- data.frame(
  File = c(
    "01_scenario_inputs.csv",
    "02_primary_scenario_totals.csv",
    "03_primary_scenario_yearly.csv",
    unname(family_files),
    "09_one_way_sensitivity_driver_ranking.csv",
    "10_convergence_results.csv",
    "11_all_model_run_totals.csv",
    "12_all_model_run_yearly_results.csv",
    "13_validation_checks.csv"
  ),
  Rows = c(
    nrow(scenario_inputs),
    nrow(primary_results),
    nrow(primary_results_yearly),
    vapply(names(family_files), function(family_name) {
      sum(all_totals$Analysis_Family == family_name)
    }, numeric(1)),
    nrow(one_way_ranking),
    nrow(convergence),
    nrow(all_totals),
    nrow(all_yearly),
    nrow(validation_checks)
  ),
  Purpose = c(
    "Illustrative scenario inputs used in the revised analysis, in 2014 USD.",
    "Primary results: three scenarios, 95% conditional success, 5% discount rate, annual-independent sampling.",
    "Year-by-year version of the primary results.",
    "Comparison of fixed-horizon parameter uncertainty with independent annual variation.",
    "Conditional mediation success from 75% to 100%, including the 95% primary value.",
    "Monetary discount rates of 3%, 5%, and 8%; time remains undiscounted.",
    "High-scenario final mediation-resolution share of 60% versus the unchanged 75% value.",
    "Illustrative selection check using paired trial cost/time multipliers of 0.50, 0.75, and 1.00.",
    "High-scenario one-way ranking across the tested sensitivity ranges.",
    "Monte Carlo convergence at 1,000 to 20,000 iterations.",
    "Every total-level model run in one file.",
    "Every year-level result for every model run in one file.",
    "Automated arithmetic, direction, reconciliation, and convergence checks."
  ),
  stringsAsFactors = FALSE
)
write.csv(
  manifest,
  file.path(output_dir, "ANALYSIS_MANIFEST.csv"),
  row.names = FALSE
)

if (any(validation_checks$Status != "PASS")) {
  print(validation_checks)
  stop("One or more validation checks failed. See 13_validation_checks.csv.")
}

cat("ALL_ANALYSES_COMPLETE\n")
print(primary_results[, c(
  "Scenario",
  "Mean_NPV_Savings_2000_Annual_Cases_USD",
  "LB95_NPV_Savings_2000_Annual_Cases_USD",
  "UB95_NPV_Savings_2000_Annual_Cases_USD",
  "Mean_Years_Saved_2000_Annual_Cases",
  "LB95_Years_Saved_2000_Annual_Cases",
  "UB95_Years_Saved_2000_Annual_Cases"
)])
print(validation_checks)
