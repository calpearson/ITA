library(dplyr)
library(lubridate)
library(MASS)
library(ggplot2)
library(broom)

# === Function Definition (takes data frame directly) ===
run_ITS_model <- function(df,
                          organism_name = "Unknown Organism",
                          covid_start = "2020-01-01",
                          covid_var = "covid",
                          date_var = "year_month",
                          outcome_var = "items") {
  
  # 1. Use provided data ----
  DF <- df
  
  # 2. Process date and generate time variables ----
  DF <- DF %>%
    mutate(
      date_month = as.Date(paste0(substr(!!sym(date_var), 1, 4), "-", substr(!!sym(date_var), 6, 7), "-01")),
      time = row_number(),
      t2 = time - which.min(abs(as.numeric(as.Date(paste0(substr(!!sym(date_var), 1, 4), "-", substr(!!sym(date_var), 6, 7), "-01")) - as.Date(covid_start)))),
      month = month(date_month),
      covid = as.factor(!!sym(covid_var))
    ) %>%
    arrange(date_month)
  
  # 3. Fit the full Negative Binomial model ----
  model_full <- glm.nb(formula(paste0(outcome_var, " ~ factor(month) + t2 * ", covid_var)), data = DF)
  
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
      term = gsub(paste0(covid_var), "COVID", term)
    )
  
  # 5. Predict fitted values ----
  DF$predicted_full <- predict(model_full, type = "response")
  
  # 6. Build counterfactual (no-COVID) predictions ----
  DF_temp <- DF %>%
    filter(!!sym(covid_var) == 1) %>%
    mutate(
      predset = 1,
      covid = as.factor(0),
      !!sym(outcome_var) := NA
    )
  
  DF_combined <- bind_rows(DF, DF_temp) %>% arrange(date_month)
  
  # Fit pre-COVID model
  model_pre <- glm.nb(formula(paste0(outcome_var, " ~ factor(month) + t2")),
                      data = DF_combined %>% filter(!!sym(covid_var) == 0))
  
  DF_combined$counterfactual_pred <- predict(model_pre, newdata = DF_combined, type = "response")
  
  DF_combined <- DF_combined %>%
    filter(!is.na(!!sym(outcome_var))) %>%
    mutate(counterfactual_pred = ifelse(covid == "0", NA, counterfactual_pred))
  
  # 7. Plot ----
  plot <- ggplot() +
    geom_line(data = DF, aes(x = date_month, y = !!sym(outcome_var), color = "Observed"), size = 1) +
    geom_line(data = DF, aes(x = date_month, y = predicted_full, color = "Full Model Prediction"), linetype = "dashed", size = 1) +
    geom_line(data = DF_combined, aes(x = date_month, y = counterfactual_pred, color = "Counterfactual (No COVID)"), size = 1) +
    geom_vline(xintercept = as.Date(covid_start), linetype = "dotted", color = "black", size = 1) +
    annotate("text", x = as.Date(covid_start), y = max(DF[[outcome_var]], na.rm = TRUE),
             label = "COVID Start", angle = 90, vjust = -0.5, hjust = 1.2, size = 3.5) +
    scale_color_manual(values = c(
      "Observed" = "blue",
      "Full Model Prediction" = "red",
      "Counterfactual (No COVID)" = "darkgreen"
    )) +
    labs(
      title = paste("ITS Incidence: Observed vs Predicted vs Counterfactual for", organism_name),
      x = "Date", y = "Cases", color = ""
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # 8. Return results ----
  return(list(
    model_summary = model_output,
    plot = plot,
    model_full = model_full
  ))
}


# Example dataframe (replace this with your actual data)
example_ita_data <- function() {
  library(tibble)
  library(dplyr)
  
  tibble(
    year_month = seq(as.Date("2018-01-01"), as.Date("2023-12-01"), by = "month"),
    items = rpois(72, lambda = 50),
    covid = ifelse(year_month >= as.Date("2020-01-01"), 1, 0)
  )
}

# get example of data
df <- example_ita_data()

# Run the model
results <- run_ITS_model(df = df, organism_name = "Influenza-like Illness")

# View model summary
results$model_summary

# View plot
results$plot
