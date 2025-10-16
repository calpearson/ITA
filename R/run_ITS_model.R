library(dplyr)
library(lubridate)
library(MASS)
library(ggplot2)
library(broom)
library(gt)

run_ITS_model <- function(df,
                          organism_name = "Unknown Organism",
                          covid_start = "2020-01-01",
                          covid_var = "interruption",
                          date_var = "year_month",
                          outcome_var = "measure_unit",
                          pop_include = FALSE,
                          pop_var = "population",
                          return_gt_table = TRUE) {
  
  # 1. Use provided data ----
  DF <- df
  
  # 2. Process date and generate time variables ----
  DF <- DF %>%
    mutate(
      date_month = as.Date(paste0(substr(!!sym(date_var), 1, 4), "-", substr(!!sym(date_var), 6, 7), "-01")),
      time = row_number(),
      t2 = time - which.min(abs(as.numeric(date_month - as.Date(covid_start)))),
      month = month(date_month),
      interruption = as.factor(!!sym(covid_var))
    ) %>%
    arrange(date_month)
  
  # 3. Fit the full Negative Binomial model ----
  if (pop_include) {
    model_full <- glm.nb(formula(paste0(outcome_var, " ~ factor(month) + t2 * ", covid_var,
                                        " + offset(log(", pop_var, "))")), data = DF)
    measure_label <- "Incidence Rate Ratio (IRR)"
  } else {
    model_full <- glm.nb(formula(paste0(outcome_var, " ~ factor(month) + t2 * ", covid_var)), data = DF)
    measure_label <- "Count Ratio (CR)"
  }
  
  # 4. Format model output ----
  model_output <- broom::tidy(model_full, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value < 0.1 ~ ".",
        TRUE ~ ""
      ),
      term = gsub(paste0(covid_var), "Interruption", term)
    )
  
  # Optional: create gt table directly, exclude intercept and remove measure ----
  gt_table <- NULL
  if (return_gt_table) {
    gt_table <- model_output %>%
      filter(term != "(Intercept)") %>%
      gt() %>%
      tab_header(title = md(paste0("**ITS Model Summary: ", organism_name, "**"))) %>%
      cols_label(
        term = "Variable",
        estimate = measure_label,
        conf.low = "Lower CI",
        conf.high = "Upper CI",
        p.value = "p-value",
        sig = "Significance"
      ) %>%
      fmt_number(columns = vars(estimate, conf.low, conf.high), decimals = 2) %>%
      fmt_number(columns = vars(p.value), decimals = 3) %>%
      tab_footnote(
        footnote = ifelse(pop_include,
                          "Exponentiated coefficients from Negative Binomial regression; interpreted as IRR using population offset.",
                          "Exponentiated coefficients from Negative Binomial regression; interpreted as multiplicative effect on expected counts (CR)."),
        locations = cells_column_labels(columns = vars(estimate))
      )
  }
  
  # 5. Predict fitted values ----
  DF$predicted_full <- predict(model_full, type = "response")
  
  # 6. Build counterfactual (no interruption) predictions ----
  DF_temp <- DF %>%
    filter(!!sym(covid_var) == 1) %>%
    mutate(
      predset = 1,
      interruption = as.factor(0),
      !!sym(outcome_var) := NA
    )
  
  DF_combined <- bind_rows(DF, DF_temp) %>% arrange(date_month)
  
  if (pop_include) {
    model_pre <- glm.nb(formula(paste0(outcome_var, " ~ factor(month) + t2 + offset(log(", pop_var, "))")),
                        data = DF_combined %>% filter(!!sym(covid_var) == 0))
  } else {
    model_pre <- glm.nb(formula(paste0(outcome_var, " ~ factor(month) + t2")),
                        data = DF_combined %>% filter(!!sym(covid_var) == 0))
  }
  
  DF_combined$counterfactual_pred <- predict(model_pre, newdata = DF_combined, type = "response")
  
  DF_combined <- DF_combined %>%
    filter(!is.na(!!sym(outcome_var))) %>%
    mutate(counterfactual_pred = ifelse(interruption == "0", NA, counterfactual_pred))
  
  # 7. Plot ----
  plot <- ggplot() +
    geom_line(data = DF, aes(x = date_month, y = !!sym(outcome_var), color = "Observed"), size = 1) +
    geom_line(data = DF, aes(x = date_month, y = predicted_full, color = "Full Model Prediction"), linetype = "dashed", size = 1) +
    geom_line(data = DF_combined, aes(x = date_month, y = counterfactual_pred, color = "Counterfactual (No Interruption)"), size = 1) +
    geom_vline(xintercept = as.Date(covid_start), linetype = "dotted", color = "black", size = 1) +
    annotate("text", x = as.Date(covid_start), y = max(DF[[outcome_var]], na.rm = TRUE),
             label = "Interruption Start", angle = 90, vjust = -0.5, hjust = 1.2, size = 3.5) +
    scale_color_manual(values = c(
      "Observed" = "blue",
      "Full Model Prediction" = "red",
      "Counterfactual (No Interruption)" = "darkgreen"
    )) +
    labs(
      title = paste("ITS Incidence: Observed vs Predicted vs Counterfactual for", organism_name),
      x = "Date", y = ifelse(pop_include, "Incidence Rate", "Count"), color = ""
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # 8. Return results ----
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
