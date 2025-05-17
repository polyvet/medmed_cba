# ── Libraries ───────────────────────────────────────────────────────────────────
library(shiny)
library(ggplot2)
library(triangle)
library(reshape2)

# ── Core function: savings for a cohort of cases ────────────────────────────────
cost_benefit_analysis_per_cohort <- function(max_years,
                                             params,
                                             interest_rate,
                                             k_value,
                                             total_cases = 100,
                                             n_runs      = 1000) {
  npv_monetary_savings_matrix <- matrix(0, n_runs, max_years)
  time_savings_matrix         <- matrix(0, n_runs, max_years)
  
  # Sigmoid uptake
  L_low <- params$success_mediation_start
  L_up  <- params$success_mediation_end
  x0    <- max_years / 2
  k     <- k_value
  acceptance_rate <- L_low + (L_up - L_low) /
    (1 + exp(-k * (seq_len(max_years) - x0)))
  
  cases_mediated_per_year <- round(total_cases * acceptance_rate)
  cases_trial_per_year    <- total_cases - cases_mediated_per_year
  
  # Monte‑Carlo simulation
  for (run in seq_len(n_runs)) {
    for (year in seq_len(max_years)) {
      
      cost_med  <- rtriangle(1, params$min_cost_mediation,
                             params$max_cost_mediation,
                             params$mode_cost_mediation)
      cost_tri  <- rtriangle(1, params$min_cost_trial,
                             params$max_cost_trial,
                             params$mode_cost_trial)
      time_med  <- rtriangle(1, params$min_time_mediation,
                             params$max_time_mediation,
                             params$mode_time_mediation)
      time_tri  <- rtriangle(1, params$min_time_trial,
                             params$max_time_trial,
                             params$mode_time_trial)
      
      monetary_saving_total <- (cost_tri  - cost_med) *
        cases_mediated_per_year[year]
      time_saving_total     <- (time_tri  - time_med) *
        cases_mediated_per_year[year]
      
      npv_monetary_savings_matrix[run, year] <-
        monetary_saving_total / ((1 + interest_rate) ^ year)
      time_savings_matrix[run, year] <- time_saving_total
    }
  }
  
  # Yearly summaries
  mean_monetary <- colMeans(npv_monetary_savings_matrix)
  mean_time     <- colMeans(time_savings_matrix)
  lb_monetary   <- apply(npv_monetary_savings_matrix, 2, quantile, 0.025)
  ub_monetary   <- apply(npv_monetary_savings_matrix, 2, quantile, 0.975)
  lb_time       <- apply(time_savings_matrix,         2, quantile, 0.025)
  ub_time       <- apply(time_savings_matrix,         2, quantile, 0.975)
  
  # Totals
  tot_monetary_vec <- rowSums(npv_monetary_savings_matrix)
  tot_time_vec     <- rowSums(time_savings_matrix)
  
  total_df <- data.frame(
    Mean_Total_Monetary = mean(tot_monetary_vec),
    LB_Total_Monetary   = quantile(tot_monetary_vec, 0.025),
    UB_Total_Monetary   = quantile(tot_monetary_vec, 0.975),
    Mean_Total_Time     = mean(tot_time_vec),
    LB_Total_Time       = quantile(tot_time_vec, 0.025),
    UB_Total_Time       = quantile(tot_time_vec, 0.975)
  )
  
  yearly_df <- data.frame(
    Year               = seq_len(max_years),
    Cases_Mediated     = cases_mediated_per_year,
    Cases_Trial        = cases_trial_per_year,
    Mean_Monetary_Savings = mean_monetary,
    LB_Monetary           = lb_monetary,
    UB_Monetary           = ub_monetary,
    Mean_Time_Savings     = mean_time,
    LB_Time               = lb_time,
    UB_Time               = ub_time
  )
  
  list(yearly = yearly_df, total = total_df)
}

# ── Shiny UI ────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Stochastic Cost Benefit Analysis for Medical Mediation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("max_years", "Max Years", 10, 1, 100),
      numericInput("min_cost_mediation",   "Min Cost of Mediation", 900),
      numericInput("mode_cost_mediation",  "Most Probable Cost",   1000),
      numericInput("max_cost_mediation",   "Max Cost of Mediation",1100),
      numericInput("min_cost_trial",       "Min Cost of Trial",    1900),
      numericInput("mode_cost_trial",      "Most Probable Cost",   2000),
      numericInput("max_cost_trial",       "Max Cost of Trial",    2100),
      numericInput("min_time_mediation",   "Min Months Mediation", 4),
      numericInput("mode_time_mediation",  "Most Probable Months", 5),
      numericInput("max_time_mediation",   "Max Months Mediation", 6),
      numericInput("min_time_trial",       "Min Months Trial",     9),
      numericInput("mode_time_trial",      "Most Probable Months",10),
      numericInput("max_time_trial",       "Max Months Trial",    11),
      numericInput("success_mediation_start", "Initial Uptake", 0.8, 0, 1, 0.01),
      numericInput("success_mediation_end",   "Final Uptake",   0.9, 0, 1, 0.01),
      numericInput("k_value", "Steepness (k)", 0.3, 0.1, 1, 0.01),
      numericInput("interest_rate", "Discount Rate", 0.05, 0, 1, 0.01),
      numericInput(
        "annual_cases",
        "Annual malpractice filings",
        value = 100,   # user default
        min   = 10,
        max   = 10000,
        step  = 10
      ),
      actionButton("goButton", "Go")
    ),
    mainPanel(
      plotOutput("plot_input_distributions"),
      plotOutput("plot_acceptance_rate"),
      plotOutput("plot_monetary_savings"),
      plotOutput("plot_time_savings"),
      tableOutput("results"),
      htmlOutput("headline")
    )
  )
)

# ── Shiny server ────────────────────────────────────────────────────────────────
server <- function(input, output) {
  observeEvent(input$goButton, {
    
    # Basic validation of triangular parameters
    if (!(input$min_cost_mediation  < input$mode_cost_mediation  &&
          input$mode_cost_mediation < input$max_cost_mediation)  ||
        !(input$min_cost_trial      < input$mode_cost_trial      &&
          input$mode_cost_trial     < input$max_cost_trial)      ||
        !(input$min_time_mediation  < input$mode_time_mediation  &&
          input$mode_time_mediation < input$max_time_mediation)  ||
        !(input$min_time_trial      < input$mode_time_trial      &&
          input$mode_time_trial     < input$max_time_trial)      ||
        !(input$success_mediation_start < input$success_mediation_end)) {
      
      showModal(modalDialog(
        title = "Validation Error",
        "Please ensure all triangular inputs satisfy Min < Mode < Max.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    
    params <- list(
      min_cost_mediation   = input$min_cost_mediation,
      mode_cost_mediation  = input$mode_cost_mediation,
      max_cost_mediation   = input$max_cost_mediation,
      min_cost_trial       = input$min_cost_trial,
      mode_cost_trial      = input$mode_cost_trial,
      max_cost_trial       = input$max_cost_trial,
      min_time_mediation   = input$min_time_mediation,
      mode_time_mediation  = input$mode_time_mediation,
      max_time_mediation   = input$max_time_mediation,
      min_time_trial       = input$min_time_trial,
      mode_time_trial      = input$mode_time_trial,
      max_time_trial       = input$max_time_trial,
      success_mediation_start = input$success_mediation_start,
      success_mediation_end   = input$success_mediation_end
    )
    
    # Run analysis
    result_data <- cost_benefit_analysis_per_cohort(
      max_years     = input$max_years,
      params        = params,
      interest_rate = input$interest_rate,
      k_value       = input$k_value,
      total_cases   = 100
    )
    
    output$headline <- renderUI({
      # Scale totals from the fixed 100‑case simulation
      scale <- input$annual_cases / 100
      tot   <- result_data$total * scale
      
      horizon <- input$max_years
      yr_txt  <- ifelse(horizon == 1, "Year", "Years")
      
      HTML(sprintf(
        '<div style="background:#f8f9fa;padding:15px;
                 border:1px solid #dee2e6;border-radius:6px;">
        <h4 style="margin-top:0;">
          Projection for <strong>%s</strong> Claims&nbsp;/&nbsp;Year<br>
          <small>%s‑%s %s Horizon</small>
        </h4>
        <p style="font-size:1.2em;margin:0;">
          <strong>NPV savings:</strong> €%s&nbsp;million<br>
          <small>(95%% CrI €%s&nbsp;m – €%s&nbsp;m)</small><br><br>
          <strong>Court‑years saved:</strong> %s
        </p>
      </div>',
        prettyNum(input$annual_cases, big.mark = ","),
        1, horizon, yr_txt,
        formatC(tot$Mean_Total_Monetary / 1e6, format = "f", digits = 2),
        formatC(tot$LB_Total_Monetary   / 1e6, format = "f", digits = 2),
        formatC(tot$UB_Total_Monetary   / 1e6, format = "f", digits = 2),
        prettyNum(round(tot$Mean_Total_Time)/12, big.mark = ",", digits = 2)
      ))
    })
    
    # ---- Plots and outputs ----
    # Input distributions
    dist_df <- data.frame(
      Cost_Mediation = rtriangle(1000, params$min_cost_mediation,
                                 params$max_cost_mediation, params$mode_cost_mediation),
      Cost_Trial     = rtriangle(1000, params$min_cost_trial,
                                 params$max_cost_trial, params$mode_cost_trial),
      Time_Mediation = rtriangle(1000, params$min_time_mediation,
                                 params$max_time_mediation, params$mode_time_mediation),
      Time_Trial     = rtriangle(1000, params$min_time_trial,
                                 params$max_time_trial, params$mode_time_trial)
    )
    dist_melt <- melt(dist_df)
    
    output$plot_input_distributions <- renderPlot({
      ggplot(dist_melt, aes(value)) +
        geom_histogram(aes(fill = variable), bins = 30, alpha = 0.7) +
        facet_wrap(~variable, scales = "free_x") +
        labs(title = "Input Distributions", x = "Value", y = "Frequency") +
        theme_minimal()
    })
    
    # Acceptance rate curve
    L_low <- params$success_mediation_start
    L_up  <- params$success_mediation_end
    x0    <- input$max_years / 2
    k     <- input$k_value
    accept_rate <- L_low + (L_up - L_low) /
      (1 + exp(-k * (seq_len(input$max_years) - x0)))
    
    output$plot_acceptance_rate <- renderPlot({
      ggplot(data.frame(Year = seq_len(input$max_years),
                        Acceptance_Rate = accept_rate),
             aes(Year, Acceptance_Rate)) +
        geom_line(color = "blue") +
        labs(title = "Acceptance Rate over Time",
             x = "Year", y = "Acceptance Rate") +
        theme_minimal()
    })
    
    # Monetary savings plot
    output$plot_monetary_savings <- renderPlot({
      ggplot(result_data$yearly, aes(Year, Mean_Monetary_Savings)) +
        geom_line(color = "blue") +
        geom_ribbon(aes(ymin = LB_Monetary, ymax = UB_Monetary),
                    alpha = 0.2, fill = "blue") +
        labs(title = "NPV Monetary Savings per 100 Cases",
             y = "NPV (€) Saved", x = "Year") +
        theme_minimal()
    })
    
    # Time savings plot
    output$plot_time_savings <- renderPlot({
      ggplot(result_data$yearly, aes(Year, Mean_Time_Savings)) +
        geom_line(color = "red") +
        geom_ribbon(aes(ymin = LB_Time, ymax = UB_Time),
                    alpha = 0.2, fill = "red") +
        labs(title = "Time Savings per 100 Cases",
             y = "Months Saved", x = "Year") +
        theme_minimal()
    })
    
    # Results table
    output$results <- renderTable({
      total_row <- data.frame(
        Year                = "Total",
        Cases_Mediated      = sum(result_data$yearly$Cases_Mediated),
        Cases_Trial         = sum(result_data$yearly$Cases_Trial),
        Mean_Monetary_Savings = result_data$total$Mean_Total_Monetary,
        LB_Monetary           = result_data$total$LB_Total_Monetary,
        UB_Monetary           = result_data$total$UB_Total_Monetary,
        Mean_Time_Savings     = result_data$total$Mean_Total_Time,
        LB_Time               = result_data$total$LB_Total_Time,
        UB_Time               = result_data$total$UB_Total_Time
      )
      rbind(result_data$yearly, total_row)
    }, digits = 0)
  })
}

# ── Launch app ─────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)