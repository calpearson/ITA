library(dplyr)
library(lubridate)
library(MASS)
library(ggplot2)
library(broom)
library(gt)

# Multiple-interruption ITS with start/end periods
run_ITS_model_multi <- function(df,
                                organism_name = "Unknown Organism",
                                date_var = "year_month",
                                outcome_var = "measure_unit",
                                pop_include = FALSE,
                                pop_var = "population",
                                interruptions,
                                return_gt_table = TRUE) {
  
  DF <- df %>%
    mutate(
      date_month = if (inherits(.data[[date_var]], "Date")) {
        as.Date(.data[[date_var]])
      } else {
        as.Date(paste0(substr(.data[[date_var]], 1, 4), "-",
                       substr(.data[[date_var]], 6, 7), "-01"))
      }
    ) %>%
    arrange(date_month)
  
  DF <- DF %>%
    mutate(
      time = row_number(),
      month = month(date_month)
    )
  
  # Create interruption indicators and slope terms
  for (i in seq_len(nrow(interruptions))) {
    start_i <- interruptions$start[i]
    end_i   <- interruptions$end[i]
    
    DF[[paste0("int", i)]] <- as.integer(DF$date_month >= start_i & DF$date_month <= end_i)
    start_index <- which.min(abs(DF$date_month - start_i))
    DF[[paste0("t2_int", i)]] <- pmax(0, DF$time - start_index) * DF[[paste0("int", i)]]
  }
  
  # Build formula dynamically
  level_terms <- paste0("int", seq_len(nrow(interruptions)), collapse = " + ")
  slope_terms <- paste0("t2_int", seq_len(nrow(interruptions)), collapse = " + ")
  
  rhs <- paste("factor(month)", level_terms, slope_terms, sep = " + ")
  
  if (pop_include) {
    rhs <- paste0(rhs, " + offset(log(", pop_var, "))")
  }
  
  formula_full <- as.formula(paste(outcome_var, "~", rhs))
  
  model_full <- glm.nb(formula_full, data = DF)
  
  # Model output
  model_output <- broom::tidy(model_full, exponentiate = TRUE, conf.int = TRUE)
  
  # GT table
  gt_table <- NULL
  if (return_gt_table) {
    gt_table <- model_output %>%
      filter(term != "(Intercept)") %>%
      gt() %>%
      tab_header(
        title = md(paste0("**ITS Model Summary (Multiple Interruptions): ", organism_name, "**"))
      ) %>%
      cols_label(
        term = "Variable",
        estimate = ifelse(pop_include, "IRR", "CR"),
        conf.low = "Lower 95% CI",
        conf.high = "Upper 95% CI",
        p.value = "p-value"
      ) %>%
      fmt_number(columns = c(estimate, conf.low, conf.high), decimals = 2) %>%
      fmt_number(columns = p.value, decimals = 3)
  }
  
  # Predictions
  DF$predicted_full <- predict(model_full, type = "response")
  
  # Counterfactual: all interruptions set to 0
  DF_cf <- DF
  for (i in seq_len(nrow(interruptions))) {
    DF_cf[[paste0("int", i)]] <- 0
    DF_cf[[paste0("t2_int", i)]] <- 0
  }
  
  DF$counterfactual_pred <- predict(model_full, newdata = DF_cf, type = "response")
  
  # Plot with gray interruption bars
  footnote_text <- paste(
    "How the three lines relate:",
    "Observed = actual data values.",
    "Fitted = model's expected values given the data including interruptions.",
    "Counterfactual = expected values if interruptions had never occurred.",
    "Assumptions:",
    "1. Linear trend in each segment i.e. slope changes are linear during each interruption.",
    "2. Interruption effects are immediate and additive i.e. each level and slope term adds to the baseline trend.",
    "3. No strong autocorrelation unaccounted for i.e. Negative Binomial regression assumes observations are independent conditional on predictors. Small autocorrelation is usually okay, but strong serial correlation may bias standard errors.",
    "4. No other unmeasured interventions coincide i.e. each change is attributed solely to the specified interruptions.",
    
    sep = " \n"
  )
  
  plot <- ggplot(DF, aes(x = date_month)) +
    geom_rect(
      data = interruptions,
      aes(xmin = start, xmax = end),
      ymin = -Inf,
      ymax = Inf,
      fill = "gray",
      alpha = 0.2,
      inherit.aes = FALSE
    ) +
    geom_line(aes(y = .data[[outcome_var]], color = "Observed"), linewidth = 1) +
    geom_line(aes(y = predicted_full, color = "Fitted"), linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = counterfactual_pred, color = "Counterfactual"), linewidth = 1) +
    labs(
      title = paste("ITS with Multiple Interruptions:", organism_name),
      x = "Date",
      y = ifelse(pop_include, "Incidence Rate", "Count"),
      color = "",
      caption = footnote_text
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(list(
    model_full = model_full,
    model_summary = model_output,
    gt_table = gt_table,
    plot = plot
  ))
}


# Example dataset with interruptions
example_ita_data <- function() {
  library(tibble)
  library(dplyr)
  library(lubridate)
  
  df <- tibble(
    year_month = seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by = "month")
  ) %>%
    mutate(
      interruption = ifelse(year_month >= as.Date("2020-01-01"), 1, 0),
      # baseline lambda before interruption
      lambda = ifelse(interruption == 1, 75, 50),   # increase after interruption
      measure_unit = rpois(n(), lambda = lambda),
      population = round(1e5 + (year(year_month) - 2018) * 2000)  # realistic mid-year population
    )
  
  return(df)
}

# Generate dataset
df <- example_ita_data()

# Define interruptions\interruptions <- data.frame(
interruptions <- data.frame(
  start = as.Date(c("2020-01-01",
                    "2021-06-01",
                    "2022-09-01")),
  end   = as.Date(c("2020-06-01",
                    "2021-12-01",
                    "2023-03-01"))
)

# Run the multi-interruption ITS
results <- run_ITS_model_multi(
  df = df,
  organism_name = "Influenza-like Illness",
  pop_include = TRUE,
  interruptions = interruptions
)

# View outputs
results$gt_table
results$plot

# Extract interruption coefficients for level/slope changes
coef_df <- results$model_summary %>%
  filter(grepl("^int|t2_int", term)) %>%  # keep only interruption terms
  mutate(
    type = ifelse(grepl("^int", term), "Level change", "Slope change"),
    interruption = gsub("int|t2_int", "", term)
  )