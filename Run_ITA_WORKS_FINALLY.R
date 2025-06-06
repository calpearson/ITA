

# === 1. Load Libraries
library(readxl)
library(dplyr)
library(lubridate)
library(MASS)
library(ggplot2)
library(broom)

# Citrobacter spp.
# === 2. Read in Excel data
DF <- read_excel("F:/Projects & programmes/COVID-19/HPRU impact of COVID/PAPER1_bsi_increases/3) Code/18) ITA/3) output/cit.xlsx", sheet = "Sheet 1")

# === 3. Process date and generate time variables
DF <- DF %>%
  mutate(
    date_month = as.Date(paste0(substr(year_month, 1, 4), "-", substr(year_month, 6, 7), "-01")),
    time = row_number(),
    t2 = time - 62,
    month = month(date_month),
    covid = as.factor(covid)
  ) %>%
  arrange(date_month)

# === 4. Fit the full model with interaction term
model_full <- glm.nb(items ~ factor(month) + t2 * covid, data = DF)

# display the model results
model_output_citrobacter <- tidy(model_full, exponentiate = TRUE, conf.int = TRUE)

# === 5. Save model predictions across all data
DF$predicted_full <- predict(model_full, type = "response")

# === 6. Save just predicted values and merge back like Stata
DF_model_only <- DF %>% dplyr::select(year_month, model_fit = predicted_full)

# === 7. Simulate post-COVID data without COVID
DF_temp <- DF %>%
  filter(covid == 1) %>%
  mutate(
    predset = 1,
    covid = as.factor(0),  # simulate no COVID
    items = NA              # remove observed items
  )

# === 8. Append counterfactual dataset to base
DF_combined <- bind_rows(DF, DF_temp) %>%
  arrange(date_month)

# === 9. Fit pre-COVID model only
model_pre <- glm.nb(items ~ factor(month) + t2, data = DF_combined %>% filter(covid == 0))

# === 10. Predict from pre-COVID model to generate counterfactual
DF_combined$counterfactual_pred <- predict(model_pre, newdata = DF_combined, type = "response")
DF_combined <- DF_combined %>% filter(!is.na(items)) %>% mutate(counterfactual_pred = ifelse(covid=="0", NA, counterfactual_pred))

# === 11. Plot observed, full model, and counterfactual trends
cit_plot <- ggplot() +
  geom_line(data = DF, aes(x = date_month, y = items, color = "Observed"), size = 1) +
  geom_line(data = DF, aes(x = date_month, y = predicted_full, color = "Full Model Prediction"), linetype = "dashed", size = 1) +
  geom_line(data = DF_combined , aes(x = date_month, y = counterfactual_pred, color = "Counterfactual (No COVID)"),, size = 1) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted", color = "black", size = 1) +
  annotate("text", x = as.Date("2020-01-01"), y = max(DF$items, na.rm = TRUE),
           label = "COVID Start", angle = 90, vjust = -0.5, hjust = 1.2, size = 3.5) +
  scale_color_manual(values = c(
    "Observed" = "blue",
    "Full Model Prediction" = "red",
    "Counterfactual (No COVID)" = "darkgreen"
  )) +
  labs(title = "ITS Incidence: Observed vs Predicted vs Counterfactual for Citrobacter spp.",
       x = "Date", y = "Cases", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# view it
cit_plot

# === 12. Format date column (already in use above)
DF$date_month <- format(DF$date_month, "%Y-%m")  # optional if needed for output or exporting


# Enterobacter spp.
# === 2. Read in Excel data
DF <- read_excel("F:/Projects & programmes/COVID-19/HPRU impact of COVID/PAPER1_bsi_increases/3) Code/18) ITA/3) output/ent.xlsx", sheet = "Sheet 1")

# === 3. Process date and generate time variables
DF <- DF %>%
  mutate(
    date_month = as.Date(paste0(substr(year_month, 1, 4), "-", substr(year_month, 6, 7), "-01")),
    time = row_number(),
    t2 = time - 62,
    month = month(date_month),
    covid = as.factor(covid)
  ) %>%
  arrange(date_month)

# === 4. Fit the full model with interaction term
model_full <- glm.nb(items ~ factor(month) + t2 * covid, data = DF)

# display the model results
model_output_enterobacter <- tidy(model_full, exponentiate = TRUE, conf.int = TRUE)

# === 5. Save model predictions across all data
DF$predicted_full <- predict(model_full, type = "response")

# === 6. Save just predicted values and merge back like Stata
DF_model_only <- DF %>% dplyr::select(year_month, model_fit = predicted_full)

# === 7. Simulate post-COVID data without COVID
DF_temp <- DF %>%
  filter(covid == 1) %>%
  mutate(
    predset = 1,
    covid = as.factor(0),  # simulate no COVID
    items = NA              # remove observed items
  )

# === 8. Append counterfactual dataset to base
DF_combined <- bind_rows(DF, DF_temp) %>%
  arrange(date_month)

# === 9. Fit pre-COVID model only
model_pre <- glm.nb(items ~ factor(month) + t2, data = DF_combined %>% filter(covid == 0))

# === 10. Predict from pre-COVID model to generate counterfactual
DF_combined$counterfactual_pred <- predict(model_pre, newdata = DF_combined, type = "response")
DF_combined <- DF_combined %>% filter(!is.na(items)) %>% mutate(counterfactual_pred = ifelse(covid=="0", NA, counterfactual_pred))

# === 11. Plot observed, full model, and counterfactual trends
ent_plot <- ggplot() +
  geom_line(data = DF, aes(x = date_month, y = items, color = "Observed"), size = 1) +
  geom_line(data = DF, aes(x = date_month, y = predicted_full, color = "Full Model Prediction"), linetype = "dashed", size = 1) +
  geom_line(data = DF_combined , aes(x = date_month, y = counterfactual_pred, color = "Counterfactual (No COVID)"),, size = 1) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted", color = "black", size = 1) +
  annotate("text", x = as.Date("2020-01-01"), y = max(DF$items, na.rm = TRUE),
           label = "COVID Start", angle = 90, vjust = -0.5, hjust = 1.5, size = 3.5) +
  scale_color_manual(values = c(
    "Observed" = "blue",
    "Full Model Prediction" = "red",
    "Counterfactual (No COVID)" = "darkgreen"
  )) +
  labs(title = "ITS Incidence: Observed vs Predicted vs Counterfactual for Enterobacter spp.",
       x = "Date", y = "Cases", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# view it
ent_plot

# === 12. Format date column (already in use above)
DF$date_month <- format(DF$date_month, "%Y-%m")  # optional if needed for output or exporting


# Serratia spp.
# === 2. Read in Excel data
DF <- read_excel("F:/Projects & programmes/COVID-19/HPRU impact of COVID/PAPER1_bsi_increases/3) Code/18) ITA/3) output/ser.xlsx", sheet = "Sheet 1")

# === 3. Process date and generate time variables
DF <- DF %>%
  mutate(
    date_month = as.Date(paste0(substr(year_month, 1, 4), "-", substr(year_month, 6, 7), "-01")),
    time = row_number(),
    t2 = time - 62,
    month = month(date_month),
    covid = as.factor(covid)
  ) %>%
  arrange(date_month)

# === 4. Fit the full model with interaction term
model_full <- glm.nb(items ~ factor(month) + t2 * covid, data = DF)

# display the model results
model_output_serratia <- tidy(model_full, exponentiate = TRUE, conf.int = TRUE)

# === 5. Save model predictions across all data
DF$predicted_full <- predict(model_full, type = "response")

# === 6. Save just predicted values and merge back like Stata
DF_model_only <- DF %>% dplyr::select(year_month, model_fit = predicted_full)

# === 7. Simulate post-COVID data without COVID
DF_temp <- DF %>%
  filter(covid == 1) %>%
  mutate(
    predset = 1,
    covid = as.factor(0),  # simulate no COVID
    items = NA              # remove observed items
  )

# === 8. Append counterfactual dataset to base
DF_combined <- bind_rows(DF, DF_temp) %>%
  arrange(date_month)

# === 9. Fit pre-COVID model only
model_pre <- glm.nb(items ~ factor(month) + t2, data = DF_combined %>% filter(covid == 0))

# === 10. Predict from pre-COVID model to generate counterfactual
DF_combined$counterfactual_pred <- predict(model_pre, newdata = DF_combined, type = "response")
DF_combined <- DF_combined %>% filter(!is.na(items)) %>% mutate(counterfactual_pred = ifelse(covid=="0", NA, counterfactual_pred))

# === 11. Plot observed, full model, and counterfactual trends
ser_plot <- ggplot() +
  geom_line(data = DF, aes(x = date_month, y = items, color = "Observed"), size = 1) +
  geom_line(data = DF, aes(x = date_month, y = predicted_full, color = "Full Model Prediction"), linetype = "dashed", size = 1) +
  geom_line(data = DF_combined , aes(x = date_month, y = counterfactual_pred, color = "Counterfactual (No COVID)"),, size = 1) +
  geom_vline(xintercept = as.Date("2020-02-01"), linetype = "dotted", color = "black", size = 1) +
  annotate("text", x = as.Date("2020-02-01"), y = max(DF$items, na.rm = TRUE),
           label = "COVID Start", angle = 90, vjust = -0.5, hjust = 1.5, size = 3.5) +
  scale_color_manual(values = c(
    "Observed" = "blue",
    "Full Model Prediction" = "red",
    "Counterfactual (No COVID)" = "darkgreen"
  )) +
  labs(title = "ITS Incidence: Observed vs Predicted vs Counterfactual for Citrobacter spp.",
       x = "Date", y = "Cases", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# display the result
ser_plot

# === 12. Format date column (already in use above)
DF$date_month <- format(DF$date_month, "%Y-%m")  # optional if needed for output or exporting





#............
# CFR section
#............

# Here we will be running the CFR section as the items element



# Citrobacter spp.
# === 2. Read in Excel data
DF <- read_excel("F:/Projects & programmes/COVID-19/HPRU impact of COVID/PAPER1_bsi_increases/3) Code/18) ITA/3) output/cit_cfr.xlsx", sheet = "Sheet 1") %>%
  dplyr::select(year_month,sy,items=CFR,covid) %>%
  filter(!year_month %in% c("2023.12","2023.11","2023.10","2023.09","2023.08","2023.07","2023.06","2023.05",
                            "2013.12","2013.11","2013.10","2013.09","2013.08","2013.07","2013.06","2013.05","2013.04","2013.03","2013.02","2013.01",
                            "2014.12","2014.11","2014.10","2014.09","2014.08","2014.07","2014.06","2014.05","2014.04","2014.03","2014.02","2014.01"))

# === 3. Process date and generate time variables
DF <- DF %>%
  mutate(
    date_month = as.Date(paste0(substr(year_month, 1, 4), "-", substr(year_month, 6, 7), "-01")),
    time = row_number(),
    t2 = time - 62,
    month = month(date_month),
    covid = as.factor(covid)
  ) %>%
  arrange(date_month)

# === 4. Fit the full model with interaction term
model_full <- glm.nb(items ~ factor(month) + t2 * covid, data = DF)

# display the model results
model_output_citrobacter_cfr <- tidy(model_full, exponentiate = TRUE, conf.int = TRUE)

# === 5. Save model predictions across all data
DF$predicted_full <- predict(model_full, type = "response")

# === 6. Save just predicted values and merge back like Stata
DF_model_only <- DF %>% dplyr::select(year_month, model_fit = predicted_full)

# === 7. Simulate post-COVID data without COVID
DF_temp <- DF %>%
  filter(covid == 1) %>%
  mutate(
    predset = 1,
    covid = as.factor(0),  # simulate no COVID
    items = NA              # remove observed items
  )

# === 8. Append counterfactual dataset to base
DF_combined <- bind_rows(DF, DF_temp) %>%
  arrange(date_month)

# === 9. Fit pre-COVID model only
model_pre <- glm.nb(items ~ factor(month) + t2, data = DF_combined %>% filter(covid == 0))

# === 10. Predict from pre-COVID model to generate counterfactual
DF_combined$counterfactual_pred <- predict(model_pre, newdata = DF_combined, type = "response")
DF_combined <- DF_combined %>% filter(!is.na(items)) %>% mutate(counterfactual_pred = ifelse(covid=="0", NA, counterfactual_pred))

# === 11. Plot observed, full model, and counterfactual trends
cit_plot <- ggplot() +
  geom_line(data = DF, aes(x = date_month, y = items, color = "Observed"), size = 1) +
  geom_line(data = DF, aes(x = date_month, y = predicted_full, color = "Full Model Prediction"), linetype = "dashed", size = 1) +
  geom_line(data = DF_combined , aes(x = date_month, y = counterfactual_pred, color = "Counterfactual (No COVID)"),, size = 1) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted", color = "black", size = 1) +
  annotate("text", x = as.Date("2020-01-01"), y = max(DF$items, na.rm = TRUE),
           label = "COVID Start", angle = 90, vjust = -0.5, hjust = 1.2, size = 3.5) +
  scale_color_manual(values = c(
    "Observed" = "blue",
    "Full Model Prediction" = "red",
    "Counterfactual (No COVID)" = "darkgreen"
  )) +
  labs(title = "ITS CFR: Observed vs Predicted vs Counterfactual for Citrobacter spp.",
       x = "Date", y = "CFR (%)", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# view it
cit_plot





# Enterobacter spp.
# === 2. Read in Excel data
DF <- read_excel("F:/Projects & programmes/COVID-19/HPRU impact of COVID/PAPER1_bsi_increases/3) Code/18) ITA/3) output/ent_cfr.xlsx", sheet = "Sheet 1") %>%
  dplyr::select(year_month,sy,items=CFR,covid) %>%
  filter(!year_month %in% c("2023.12","2023.11","2023.10","2023.09","2023.08","2023.07","2023.06","2023.05",
                            "2013.12","2013.11","2013.10","2013.09","2013.08","2013.07","2013.06","2013.05","2013.04","2013.03","2013.02","2013.01",
                            "2014.12","2014.11","2014.10","2014.09","2014.08","2014.07","2014.06","2014.05","2014.04","2014.03","2014.02","2014.01"))

# === 3. Process date and generate time variables
DF <- DF %>%
  mutate(
    date_month = as.Date(paste0(substr(year_month, 1, 4), "-", substr(year_month, 6, 7), "-01")),
    time = row_number(),
    t2 = time - 62,
    month = month(date_month),
    covid = as.factor(covid)
  ) %>%
  arrange(date_month)

# === 4. Fit the full model with interaction term
model_full <- glm.nb(items ~ factor(month) + t2 * covid, data = DF)

# display the model results
model_output_enterobacter_cfr <- tidy(model_full, exponentiate = TRUE, conf.int = TRUE)

# === 5. Save model predictions across all data
DF$predicted_full <- predict(model_full, type = "response")

# === 6. Save just predicted values and merge back like Stata
DF_model_only <- DF %>% dplyr::select(year_month, model_fit = predicted_full)

# === 7. Simulate post-COVID data without COVID
DF_temp <- DF %>%
  filter(covid == 1) %>%
  mutate(
    predset = 1,
    covid = as.factor(0),  # simulate no COVID
    items = NA              # remove observed items
  )

# === 8. Append counterfactual dataset to base
DF_combined <- bind_rows(DF, DF_temp) %>%
  arrange(date_month)

# === 9. Fit pre-COVID model only
model_pre <- glm.nb(items ~ factor(month) + t2, data = DF_combined %>% filter(covid == 0))

# === 10. Predict from pre-COVID model to generate counterfactual
DF_combined$counterfactual_pred <- predict(model_pre, newdata = DF_combined, type = "response")
DF_combined <- DF_combined %>% filter(!is.na(items)) %>% mutate(counterfactual_pred = ifelse(covid=="0", NA, counterfactual_pred))

# === 11. Plot observed, full model, and counterfactual trends
ent_plot <- ggplot() +
  geom_line(data = DF, aes(x = date_month, y = items, color = "Observed"), size = 1) +
  geom_line(data = DF, aes(x = date_month, y = predicted_full, color = "Full Model Prediction"), linetype = "dashed", size = 1) +
  geom_line(data = DF_combined , aes(x = date_month, y = counterfactual_pred, color = "Counterfactual (No COVID)"),, size = 1) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted", color = "black", size = 1) +
  annotate("text", x = as.Date("2020-01-01"), y = max(DF$items, na.rm = TRUE),
           label = "COVID Start", angle = 90, vjust = -0.5, hjust = 1, size = 3.5) +
  scale_color_manual(values = c(
    "Observed" = "blue",
    "Full Model Prediction" = "red",
    "Counterfactual (No COVID)" = "darkgreen"
  )) +
  labs(title = "ITS CFR: Observed vs Predicted vs Counterfactual for Enterobacter spp.",
       x = "Date", y = "CFR (%)", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# view it
ent_plot






# Serratia spp.
# === 2. Read in Excel data
DF <- read_excel("F:/Projects & programmes/COVID-19/HPRU impact of COVID/PAPER1_bsi_increases/3) Code/18) ITA/3) output/ser_cfr.xlsx", sheet = "Sheet 1") %>%
  dplyr::select(year_month,sy,items=CFR,covid) %>%
  filter(!year_month %in% c("2023.12","2023.11","2023.10","2023.09","2023.08","2023.07","2023.06","2023.05",
                            "2013.12","2013.11","2013.10","2013.09","2013.08","2013.07","2013.06","2013.05","2013.04","2013.03","2013.02","2013.01",
                            "2014.12","2014.11","2014.10","2014.09","2014.08","2014.07","2014.06","2014.05","2014.04","2014.03","2014.02","2014.01"))

# === 3. Process date and generate time variables
DF <- DF %>%
  mutate(
    date_month = as.Date(paste0(substr(year_month, 1, 4), "-", substr(year_month, 6, 7), "-01")),
    time = row_number(),
    t2 = time - 62,
    month = month(date_month),
    covid = as.factor(covid)
  ) %>%
  arrange(date_month)

# === 4. Fit the full model with interaction term
model_full <- glm.nb(items ~ factor(month) + t2 * covid, data = DF)

# display the model results
model_output_serratia_cfr <- tidy(model_full, exponentiate = TRUE, conf.int = TRUE)

# === 5. Save model predictions across all data
DF$predicted_full <- predict(model_full, type = "response")

# === 6. Save just predicted values and merge back like Stata
DF_model_only <- DF %>% dplyr::select(year_month, model_fit = predicted_full)

# === 7. Simulate post-COVID data without COVID
DF_temp <- DF %>%
  filter(covid == 1) %>%
  mutate(
    predset = 1,
    covid = as.factor(0),  # simulate no COVID
    items = NA              # remove observed items
  )

# === 8. Append counterfactual dataset to base
DF_combined <- bind_rows(DF, DF_temp) %>%
  arrange(date_month)

# === 9. Fit pre-COVID model only
model_pre <- glm.nb(items ~ factor(month) + t2, data = DF_combined %>% filter(covid == 0))

# === 10. Predict from pre-COVID model to generate counterfactual
DF_combined$counterfactual_pred <- predict(model_pre, newdata = DF_combined, type = "response")
DF_combined <- DF_combined %>% filter(!is.na(items)) %>% mutate(counterfactual_pred = ifelse(covid=="0", NA, counterfactual_pred))

# === 11. Plot observed, full model, and counterfactual trends
ser_plot <- ggplot() +
  geom_line(data = DF, aes(x = date_month, y = items, color = "Observed"), size = 1) +
  geom_line(data = DF, aes(x = date_month, y = predicted_full, color = "Full Model Prediction"), linetype = "dashed", size = 1) +
  geom_line(data = DF_combined , aes(x = date_month, y = counterfactual_pred, color = "Counterfactual (No COVID)"),, size = 1) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dotted", color = "black", size = 1) +
  annotate("text", x = as.Date("2020-01-01"), y = max(DF$items, na.rm = TRUE),
           label = "COVID Start", angle = 90, vjust = -0.5, hjust = 1, size = 3.5) +
  scale_color_manual(values = c(
    "Observed" = "blue",
    "Full Model Prediction" = "red",
    "Counterfactual (No COVID)" = "darkgreen"
  )) +
  labs(title = "ITS CFR: Observed vs Predicted vs Counterfactual for Serratia spp.",
       x = "Date", y = "CFR (%)", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# view it
ser_plot
