library(dplyr)
library(lubridate)
library(MASS)
library(ggplot2)
library(broom)
library(gt)

run_ITS_model <- function(df,
                          organism_name = "Unknown Organism",
                          covid_start = "2020-01-01",
                          date_var = "year_month",
                          outcome_var = "measure_unit",
                          pop_include = FALSE,
                          pop_var = "population",
                          return_gt_table = TRUE) {
  
  # 1. Copy data ----
  DF <- df
  
  # 2. Ensure date variable and sort ----
  DF <- DF %>%
    mutate(
      date_month = if (inherits(.data[[date_var]], "Date")) {
        as.Date(.data[[date_var]])
      } else {
        as.Date(paste0(substr(.data[[date_var]], 1, 4), "-",
                       substr(.data[[date_var]], 6, 7), "-01"))
      }
    ) %>%
    arrange(date_month)
  
  # 3. Create time and interruption variables ----
  DF <- DF %>%
    mutate(
      time = row_number(),
      interruption = as.integer(date_month >= as.Date(covid_start)),
      month = month(date_month)
    )
  
  interruption_index <- which.min(abs(DF$date_month - as.Date(covid_start)))
  DF <- DF %>% mutate(t2 = time - interruption_index)
  
  # 4. Fit full ITS model ----
  if (pop_include) {
    model_full <- glm.nb(
      as.formula(paste0(outcome_var,
                        " ~ factor(month) + t2 * interruption + offset(log(",
                        pop_var, "))")),
      data = DF
    )
    measure_label <- "Incidence Rate Ratio (IRR)"
  } else {
    model_full <- glm.nb(
      as.formula(paste0(outcome_var,
                        " ~ factor(month) + t2 * interruption")),
      data = DF
    )
    measure_label <- "Count Ratio (CR)"
  }
  
  # 5. Model summary ----
  model_output <- broom::tidy(model_full, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value < 0.1 ~ ".",
        TRUE ~ ""
      ),
      term = gsub("interruption", "Interruption", term)
    )
  
  # 6. GT table ----
  gt_table <- NULL
  if (return_gt_table) {
    gt_table <- model_output %>%
      filter(term != "(Intercept)") %>%
      gt() %>%
      tab_header(
        title = md(paste0("**ITS Model Summary: ", organism_name, "**"))
      ) %>%
      cols_label(
        term = "Variable",
        estimate = measure_label,
        conf.low = "Lower 95% CI",
        conf.high = "Upper 95% CI",
        p.value = "p-value",
        sig = "Significance"
      ) %>%
      fmt_number(columns = c(estimate, conf.low, conf.high), decimals = 2) %>%
      fmt_number(columns = p.value, decimals = 3)
  }
  
  # 7. Predictions ----
  DF$predicted_full <- predict(model_full, type = "response")
  
  # Counterfactual: same model, interruption set to 0 ----
  DF_cf <- DF %>% mutate(interruption = 0)
  DF$counterfactual_pred <- predict(model_full, newdata = DF_cf, type = "response")
  DF$counterfactual_pred[DF$interruption == 0] <- NA
  
  # 8. Plot ----
  plot <- ggplot(DF, aes(x = date_month)) +
    geom_line(aes(y = .data[[outcome_var]], color = "Observed"), linewidth = 1) +
    geom_line(aes(y = predicted_full, color = "Full Model Prediction"),
              linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = counterfactual_pred, color = "Counterfactual (No Interruption)"),
              linewidth = 1) +
    geom_vline(xintercept = as.Date(covid_start), linetype = "dotted") +
    scale_color_manual(values = c(
      "Observed" = "blue",
      "Full Model Prediction" = "red",
      "Counterfactual (No Interruption)" = "darkgreen"
    )) +
    labs(
      title = paste("ITS Analysis:", organism_name),
      x = "Date",
      y = ifelse(pop_include, "Incidence Rate", "Count"),
      color = ""
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # 9. Return ----
  return(list(
    model_summary = model_output,
    gt_table = gt_table,
    plot = plot,
    model_full = model_full
  ))
}




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


# Example usage
df <- example_ita_data()

results <- run_ITS_model(df = df, 
                         organism_name = "Influenza-like Illness",
                         pop_include = TRUE)

results$gt_table
results$plot


