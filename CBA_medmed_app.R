# Libraries -------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(triangle)
library(reshape2)

# Core function: savings for a cohort of cases --------------------------------
cost_benefit_analysis_per_cohort <- function(max_years,
                                             params,
                                             interest_rate,
                                             k_value,
                                             mediation_success_rate = 0.95,
                                             total_cases = 100,
                                             n_runs = 10000,
                                             random_seed = 42100,
                                             uncertainty_structure =
                                               "annual_independent") {
  set.seed(random_seed)

  valid_uncertainty_structures <- c(
    "fixed_horizon",
    "annual_independent"
  )
  if (!(uncertainty_structure %in% valid_uncertainty_structures)) {
    stop(
      "uncertainty_structure must be 'fixed_horizon' or ",
      "'annual_independent'."
    )
  }

  # The mediation-resolution share is the proportion of all eligible cases
  # ultimately resolved through mediation. It combines all implementation
  # factors that lead to a completed mediation resolution (offer, participation,
  # and successful resolution). It is not the success probability per attempt.
  L_low <- params$mediation_resolution_start
  L_up <- params$mediation_resolution_end
  x0 <- max_years / 2
  k <- k_value
  mediation_resolution_share <- L_low + (L_up - L_low) /
    (1 + exp(-k * (seq_len(max_years) - x0)))

  cases_mediation_resolved <- round(total_cases * mediation_resolution_share)

  # Conditional success among attempted mediations is used to infer failures.
  # Expected (possibly fractional) case counts are appropriate for this cohort
  # model. A failed attempt incurs mediation cost/time and then proceeds to trial.
  mediation_attempts <- cases_mediation_resolved / mediation_success_rate
  failed_mediation_to_trial <- mediation_attempts - cases_mediation_resolved
  direct_to_trial <- total_cases - mediation_attempts
  trial_resolved <- total_cases - cases_mediation_resolved

  if (any(direct_to_trial < -sqrt(.Machine$double.eps))) {
    stop(
      "The conditional mediation success rate must be at least as high as ",
      "the maximum realised mediation-resolution share."
    )
  }
  direct_to_trial <- pmax(direct_to_trial, 0)

  # Baseline: all cases resolve through trial.
  # Mediation strategy: successful resolutions avoid trial; failed attempts
  # incur mediation cost/time before trial. Trial-only cases cancel between
  # strategies, leaving the incremental expressions below.
  if (uncertainty_structure == "fixed_horizon") {
    # Structural sensitivity: each iteration draws one fixed value for each
    # uncertain cost and duration input and carries it across the full horizon.
    # This represents uncertainty about fixed but unknown scenario parameters.
    # Row-wise uniforms make smaller iteration runs exact prefixes of larger
    # runs when the seed is unchanged, enabling a clean convergence assessment.
    uniform_draws <- matrix(runif(n_runs * 4), ncol = 4, byrow = TRUE)
    cost_med <- qtriangle(
      uniform_draws[, 1], params$min_cost_mediation,
      params$max_cost_mediation, params$mode_cost_mediation
    )
    cost_tri <- qtriangle(
      uniform_draws[, 2], params$min_cost_trial,
      params$max_cost_trial, params$mode_cost_trial
    )
    time_med <- qtriangle(
      uniform_draws[, 3], params$min_time_mediation,
      params$max_time_mediation, params$mode_time_mediation
    )
    time_tri <- qtriangle(
      uniform_draws[, 4], params$min_time_trial,
      params$max_time_trial, params$mode_time_trial
    )

    monetary_savings_matrix <-
      outer(cost_tri, cases_mediation_resolved) -
      outer(cost_med, mediation_attempts)
    time_savings_matrix <-
      outer(time_tri, cases_mediation_resolved) -
      outer(time_med, mediation_attempts)
  } else {
    # Primary analysis: draw cost and duration values independently for every
    # year within each iteration. This reflects variation across annual cohorts
    # and allows high and low yearly draws to offset over the horizon.
    draw_triangular_matrix <- function(minimum, maximum, mode) {
      matrix(
        qtriangle(
          runif(n_runs * max_years), minimum, maximum, mode
        ),
        nrow = n_runs,
        ncol = max_years
      )
    }

    cost_med <- draw_triangular_matrix(
      params$min_cost_mediation,
      params$max_cost_mediation,
      params$mode_cost_mediation
    )
    cost_tri <- draw_triangular_matrix(
      params$min_cost_trial,
      params$max_cost_trial,
      params$mode_cost_trial
    )
    monetary_savings_matrix <-
      sweep(cost_tri, 2, cases_mediation_resolved, "*") -
      sweep(cost_med, 2, mediation_attempts, "*")

    time_med <- draw_triangular_matrix(
      params$min_time_mediation,
      params$max_time_mediation,
      params$mode_time_mediation
    )
    time_tri <- draw_triangular_matrix(
      params$min_time_trial,
      params$max_time_trial,
      params$mode_time_trial
    )
    time_savings_matrix <-
      sweep(time_tri, 2, cases_mediation_resolved, "*") -
      sweep(time_med, 2, mediation_attempts, "*")
  }

  discount_factors <- (1 + interest_rate)^seq_len(max_years)
  npv_monetary_savings_matrix <- sweep(
    monetary_savings_matrix, 2, discount_factors, "/"
  )

  # Yearly summaries
  mean_monetary <- colMeans(npv_monetary_savings_matrix)
  mean_time <- colMeans(time_savings_matrix)
  lb_monetary <- apply(npv_monetary_savings_matrix, 2, quantile, 0.025)
  ub_monetary <- apply(npv_monetary_savings_matrix, 2, quantile, 0.975)
  lb_time <- apply(time_savings_matrix, 2, quantile, 0.025)
  ub_time <- apply(time_savings_matrix, 2, quantile, 0.975)

  # Totals
  tot_monetary_vec <- rowSums(npv_monetary_savings_matrix)
  tot_time_vec <- rowSums(time_savings_matrix)

  total_df <- data.frame(
    Mean_Total_Monetary = mean(tot_monetary_vec),
    LB_Total_Monetary = quantile(tot_monetary_vec, 0.025),
    UB_Total_Monetary = quantile(tot_monetary_vec, 0.975),
    Mean_Total_Time = mean(tot_time_vec),
    LB_Total_Time = quantile(tot_time_vec, 0.025),
    UB_Total_Time = quantile(tot_time_vec, 0.975)
  )

  yearly_df <- data.frame(
    Year = seq_len(max_years),
    Mediation_Resolution_Share = mediation_resolution_share,
    Mediation_Resolved = cases_mediation_resolved,
    Mediation_Attempts = mediation_attempts,
    Failed_Mediation_to_Trial = failed_mediation_to_trial,
    Direct_to_Trial = direct_to_trial,
    Trial_Resolved = trial_resolved,
    Mean_Monetary_Savings = mean_monetary,
    LB_Monetary = lb_monetary,
    UB_Monetary = ub_monetary,
    Mean_Time_Savings = mean_time,
    LB_Time = lb_time,
    UB_Time = ub_time
  )

  list(
    yearly = yearly_df,
    total = total_df,
    simulation_totals = data.frame(
      Total_NPV_Monetary_Savings = tot_monetary_vec,
      Total_Time_Savings_Months = tot_time_vec
    ),
    success_rate = mediation_success_rate,
    uncertainty_structure = uncertainty_structure
  )
}

# Shiny UI --------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Stochastic Cost-Benefit Analysis for Medical Mediation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("max_years", "Max Years", 10, 1, 100),
      helpText(
        "Defaults reproduce the revised high-income illustrative scenario."
      ),
      numericInput("min_cost_mediation", "Min Cost of Mediation (USD)", 1000),
      numericInput("mode_cost_mediation", "Most Probable Cost (USD)", 3000),
      numericInput("max_cost_mediation", "Max Cost of Mediation (USD)", 6000),
      numericInput("min_cost_trial", "Min Cost of Trial (USD)", 5000),
      numericInput("mode_cost_trial", "Most Probable Cost (USD)", 15000),
      numericInput("max_cost_trial", "Max Cost of Trial (USD)", 40000),
      numericInput("min_time_mediation", "Min Months Mediation", 1),
      numericInput("mode_time_mediation", "Most Probable Months", 3),
      numericInput("max_time_mediation", "Max Months Mediation", 6),
      numericInput("min_time_trial", "Min Months Trial", 6),
      numericInput("mode_time_trial", "Most Probable Months", 18),
      numericInput("max_time_trial", "Max Months Trial", 36),
      numericInput(
        "mediation_resolution_start",
        "Initial mediation-resolution share",
        0.15, 0, 1, 0.01
      ),
      numericInput(
        "mediation_resolution_end",
        "Final mediation-resolution share",
        0.75, 0, 1, 0.01
      ),
      helpText(
        "The mediation-resolution share is the proportion of all eligible cases ",
        "ultimately resolved through mediation. It combines offering, participation, ",
        "and successful resolution."
      ),
      numericInput(
        "mediation_success_rate",
        "Conditional success among mediation attempts",
        0.95, 0.01, 1, 0.01
      ),
      helpText(
        "Used to infer unsuccessful attempts that incur mediation cost and time ",
        "before proceeding to trial. Default: 95%."
      ),
      numericInput("k_value", "Steepness (k)", 1.0, 0.1, 1, 0.01),
      numericInput("interest_rate", "Discount Rate", 0.05, 0, 1, 0.01),
      numericInput(
        "n_runs",
        "Monte Carlo iterations",
        value = 10000,
        min = 1000,
        max = 100000,
        step = 1000
      ),
      selectInput(
        "uncertainty_structure",
        "Uncertainty interpretation",
        choices = c(
          "Independent annual variation (primary)" =
            "annual_independent",
          "Fixed uncertain parameters across horizon (sensitivity)" =
            "fixed_horizon"
        ),
        selected = "annual_independent"
      ),
      helpText(
        "The primary option redraws cost and duration inputs for each annual ",
        "cohort. The sensitivity option holds one sampled set of values fixed ",
        "across the full horizon."
      ),
      numericInput(
        "annual_cases",
        "Annual malpractice filings",
        value = 2000,
        min = 10,
        max = 10000,
        step = 10
      ),
      actionButton("goButton", "Go")
    ),
    mainPanel(
      wellPanel(
        strong("Interpretation of the comparison"),
        p(
          "The comparator strategy assumes that all eligible cases resolve through ",
          "trial. The mediation strategy is a mixed pathway: some cases resolve ",
          "through mediation, unsuccessful mediation attempts proceed to trial, and ",
          "the remaining cases proceed directly to trial. Particularly during early ",
          "implementation years, a substantial share therefore remains trial-resolved."
        ),
        uiOutput("uncertainty_note")
      ),
      plotOutput("plot_input_distributions"),
      plotOutput("plot_resolution_share"),
      plotOutput("plot_monetary_savings"),
      plotOutput("plot_time_savings"),
      h3("Selected success-rate scenario"),
      tableOutput("results"),
      htmlOutput("headline"),
      h3("One-way scenario analysis: conditional mediation success"),
      p(
        "Each feasible row changes only the conditional success probability. ",
        "All other inputs and random draws are held constant."
      ),
      tableOutput("success_sensitivity")
    )
  )
)

# Shiny server ---------------------------------------------------------------
server <- function(input, output) {
  observeEvent(input$goButton, {
    triangular_inputs_valid <-
      input$min_cost_mediation < input$mode_cost_mediation &&
      input$mode_cost_mediation < input$max_cost_mediation &&
      input$min_cost_trial < input$mode_cost_trial &&
      input$mode_cost_trial < input$max_cost_trial &&
      input$min_time_mediation < input$mode_time_mediation &&
      input$mode_time_mediation < input$max_time_mediation &&
      input$min_time_trial < input$mode_time_trial &&
      input$mode_time_trial < input$max_time_trial

    if (!triangular_inputs_valid) {
      showModal(modalDialog(
        title = "Validation Error",
        "Please ensure all triangular inputs satisfy Min < Mode < Max.",
        easyClose = TRUE
      ))
      return(NULL)
    }

    if (!(input$mediation_resolution_start < input$mediation_resolution_end)) {
      showModal(modalDialog(
        title = "Validation Error",
        "The initial mediation-resolution share must be lower than the final share.",
        easyClose = TRUE
      ))
      return(NULL)
    }

    params <- list(
      min_cost_mediation = input$min_cost_mediation,
      mode_cost_mediation = input$mode_cost_mediation,
      max_cost_mediation = input$max_cost_mediation,
      min_cost_trial = input$min_cost_trial,
      mode_cost_trial = input$mode_cost_trial,
      max_cost_trial = input$max_cost_trial,
      min_time_mediation = input$min_time_mediation,
      mode_time_mediation = input$mode_time_mediation,
      max_time_mediation = input$max_time_mediation,
      min_time_trial = input$min_time_trial,
      mode_time_trial = input$mode_time_trial,
      max_time_trial = input$max_time_trial,
      mediation_resolution_start = input$mediation_resolution_start,
      mediation_resolution_end = input$mediation_resolution_end
    )

    resolution_share <-
      params$mediation_resolution_start +
      (params$mediation_resolution_end - params$mediation_resolution_start) /
        (1 + exp(-input$k_value *
          (seq_len(input$max_years) - input$max_years / 2)))
    realised_resolution_share <-
      round(100 * resolution_share) / 100
    minimum_feasible_success <- max(realised_resolution_share)

    if (input$mediation_success_rate < minimum_feasible_success) {
      showModal(modalDialog(
        title = "Infeasible success-rate scenario",
        sprintf(
          paste0(
            "The selected conditional success rate must be at least %.0f%%, ",
            "because the mediation-resolution share reaches %.0f%% of eligible cases."
          ),
          100 * minimum_feasible_success,
          100 * minimum_feasible_success
        ),
        easyClose = TRUE
      ))
      return(NULL)
    }

    # Run the selected scenario.
    result_data <- cost_benefit_analysis_per_cohort(
      max_years = input$max_years,
      params = params,
      interest_rate = input$interest_rate,
      k_value = input$k_value,
      mediation_success_rate = input$mediation_success_rate,
      total_cases = 100,
      n_runs = input$n_runs,
      uncertainty_structure = input$uncertainty_structure
    )

    uncertainty_label <- if (
      input$uncertainty_structure == "fixed_horizon"
    ) {
      "Fixed uncertain parameters across the horizon (structural sensitivity)"
    } else {
      "Independent annual variation (primary)"
    }

    output$uncertainty_note <- renderUI({
      if (input$uncertainty_structure == "fixed_horizon") {
        p(
          "Structural sensitivity: within each Monte Carlo iteration, ",
          "one value is drawn for each cost and duration input and retained across ",
          "all years. This represents persistent uncertainty about fixed but ",
          "unknown scenario parameters."
        )
      } else {
        p(
          "Primary analysis: cost and duration values are drawn independently for ",
          "each annual cohort. The intervals reflect annual variation under ",
          "the illustrative distributions and do not represent complete decision ",
          "uncertainty."
        )
      }
    })

    output$headline <- renderUI({
      scale <- input$annual_cases / 100
      tot <- result_data$total * scale
      total_failed <- sum(result_data$yearly$Failed_Mediation_to_Trial) * scale

      horizon <- input$max_years
      yr_txt <- ifelse(horizon == 1, "Year", "Years")

      HTML(sprintf(
        paste0(
          '<div style="background:#f8f9fa;padding:15px;',
          'border:1px solid #dee2e6;border-radius:6px;">',
          '<h4 style="margin-top:0;">',
          'Projection for <strong>%s</strong> Claims / Year<br>',
          '<small>%s-%s %s Horizon; conditional mediation success %s%%<br>',
          '%s</small>',
          '</h4>',
          '<p style="font-size:1.2em;margin:0;">',
          '<strong>NPV savings:</strong> $%s million<br>',
          '<small>(95%% interval $%s m - $%s m)</small><br><br>',
          '<strong>Court-years saved:</strong> %s<br>',
          '<strong>Failed mediation attempts proceeding to trial:</strong> %s',
          '</p></div>'
        ),
        prettyNum(input$annual_cases, big.mark = ","),
        1, horizon, yr_txt,
        formatC(100 * input$mediation_success_rate, format = "f", digits = 0),
        uncertainty_label,
        formatC(tot$Mean_Total_Monetary / 1e6, format = "f", digits = 2),
        formatC(tot$LB_Total_Monetary / 1e6, format = "f", digits = 2),
        formatC(tot$UB_Total_Monetary / 1e6, format = "f", digits = 2),
        prettyNum(round(tot$Mean_Total_Time) / 12, big.mark = ",", digits = 2),
        prettyNum(round(total_failed, 1), big.mark = ",")
      ))
    })

    # Input distributions
    set.seed(42101)
    dist_df <- data.frame(
      Cost_Mediation = rtriangle(
        1000, params$min_cost_mediation,
        params$max_cost_mediation, params$mode_cost_mediation
      ),
      Cost_Trial = rtriangle(
        1000, params$min_cost_trial,
        params$max_cost_trial, params$mode_cost_trial
      ),
      Time_Mediation = rtriangle(
        1000, params$min_time_mediation,
        params$max_time_mediation, params$mode_time_mediation
      ),
      Time_Trial = rtriangle(
        1000, params$min_time_trial,
        params$max_time_trial, params$mode_time_trial
      )
    )
    dist_melt <- melt(dist_df)

    output$plot_input_distributions <- renderPlot({
      ggplot(dist_melt, aes(value)) +
        geom_histogram(aes(fill = variable), bins = 30, alpha = 0.7) +
        facet_wrap(~variable, scales = "free_x") +
        labs(title = "Input Distributions", x = "Value", y = "Frequency") +
        theme_minimal()
    })

    output$plot_resolution_share <- renderPlot({
      ggplot(
        data.frame(
          Year = seq_len(input$max_years),
          Mediation_Resolution_Share = resolution_share
        ),
        aes(Year, Mediation_Resolution_Share)
      ) +
        geom_line(color = "blue") +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(
          title = "Mediation-Resolution Share over Time",
          x = "Year",
          y = "Eligible cases ultimately resolved through mediation"
        ) +
        theme_minimal()
    })

    output$plot_monetary_savings <- renderPlot({
      ggplot(result_data$yearly, aes(Year, Mean_Monetary_Savings)) +
        geom_line(color = "blue") +
        geom_ribbon(
          aes(ymin = LB_Monetary, ymax = UB_Monetary),
          alpha = 0.2, fill = "blue"
        ) +
        labs(
          title = "NPV Monetary Savings per 100 Cases",
          y = "NPV Saved (USD)",
          x = "Year"
        ) +
        theme_minimal()
    })

    output$plot_time_savings <- renderPlot({
      ggplot(result_data$yearly, aes(Year, Mean_Time_Savings)) +
        geom_line(color = "red") +
        geom_ribbon(
          aes(ymin = LB_Time, ymax = UB_Time),
          alpha = 0.2, fill = "red"
        ) +
        labs(
          title = "Time Savings per 100 Cases",
          y = "Months Saved",
          x = "Year"
        ) +
        theme_minimal()
    })

    output$results <- renderTable({
      display_yearly <- result_data$yearly
      display_yearly$Mediation_Resolution_Share <-
        100 * display_yearly$Mediation_Resolution_Share

      total_row <- data.frame(
        Year = "Total",
        Mediation_Resolution_Share = NA_real_,
        Mediation_Resolved = sum(result_data$yearly$Mediation_Resolved),
        Mediation_Attempts = sum(result_data$yearly$Mediation_Attempts),
        Failed_Mediation_to_Trial =
          sum(result_data$yearly$Failed_Mediation_to_Trial),
        Direct_to_Trial = sum(result_data$yearly$Direct_to_Trial),
        Trial_Resolved = sum(result_data$yearly$Trial_Resolved),
        Mean_Monetary_Savings = result_data$total$Mean_Total_Monetary,
        LB_Monetary = result_data$total$LB_Total_Monetary,
        UB_Monetary = result_data$total$UB_Total_Monetary,
        Mean_Time_Savings = result_data$total$Mean_Total_Time,
        LB_Time = result_data$total$LB_Total_Time,
        UB_Time = result_data$total$UB_Total_Time
      )
      rbind(display_yearly, total_row)
    }, digits = 1)

    output$success_sensitivity <- renderTable({
      scenario_rates <- sort(unique(c(
        1.00, 0.95, 0.90, 0.85, 0.80, 0.75,
        input$mediation_success_rate
      )), decreasing = TRUE)

      scenario_rows <- lapply(scenario_rates, function(success_rate) {
        if (success_rate < minimum_feasible_success) {
          return(data.frame(
            Success_Rate = 100 * success_rate,
            Status = "Not feasible for selected resolution share",
            Total_Failed_Attempts = NA_real_,
            Mean_Total_NPV_Savings = NA_real_,
            Mean_Total_Months_Saved = NA_real_
          ))
        }

        scenario_result <- cost_benefit_analysis_per_cohort(
          max_years = input$max_years,
          params = params,
          interest_rate = input$interest_rate,
          k_value = input$k_value,
          mediation_success_rate = success_rate,
          total_cases = 100,
          n_runs = input$n_runs,
          random_seed = 42100,
          uncertainty_structure = input$uncertainty_structure
        )

        data.frame(
          Success_Rate = 100 * success_rate,
          Status = "Feasible",
          Total_Failed_Attempts =
            sum(scenario_result$yearly$Failed_Mediation_to_Trial),
          Mean_Total_NPV_Savings =
            scenario_result$total$Mean_Total_Monetary,
          Mean_Total_Months_Saved =
            scenario_result$total$Mean_Total_Time
        )
      })

      do.call(rbind, scenario_rows)
    }, digits = 1)
  })
}

# Launch app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)
