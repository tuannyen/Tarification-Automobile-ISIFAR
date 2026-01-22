library(dplyr)
load("data/clean_data.RData")       
load("data/clean_sev_pos.RData")    
freq_fit <- readRDS("models/nb_fit.rds")            # modèle fréquence
sev_fit  <- readRDS("models/gamma_log_sev.rds")     # modèle sévérité

summary(freq_fit)
#1) Prédictions par contrat (échantillon test)

df_calib_freq_test <- df_freq_test %>%
  mutate(
    logExposure = log(Exposure),
    logDensity  = log(Density + 1)
  )

df_calib_freq_test <- df_calib_freq_test %>%
  mutate(
    mu_freq = predict(
      freq_fit,
      newdata = df_calib_freq_test,
      type    = "response"
    ),
    mu_sev = predict(
      sev_fit,
      newdata = df_calib_freq_test,
      type    = "response"
    ),
    pure_premium_hat = mu_freq * mu_sev
  )

#2) Coût observé par contrat

cost_by_policy_test <- df_sev_full_test %>%
  group_by(PolicyID) %>%
  summarise(
    total_claim = sum(ClaimAmount, na.rm = TRUE),
    .groups = "drop"
  )

df_calib_test <- df_calib_freq_test %>%
  left_join(cost_by_policy_test, by = "PolicyID") %>%
  mutate(
    total_claim = ifelse(is.na(total_claim), 0, total_claim)
  )

#3) Indicateurs de calibration globale

total_cost_obs_test  <- sum(df_calib_test$total_claim)
total_cost_pred_test <- sum(df_calib_test$pure_premium_hat)

calibration_ratio_test <- total_cost_pred_test / total_cost_obs_test

mean_cost_obs_test  <- mean(df_calib_test$total_claim)
mean_cost_pred_test <- mean(df_calib_test$pure_premium_hat)

rmse_pure_premium_test <- sqrt(
  mean((df_calib_test$pure_premium_hat - df_calib_test$total_claim)^2)
)

#4) Résumé / sauvegarde 

calibration_summary_test <- list(
  total_cost_obs_test    = total_cost_obs_test,
  total_cost_pred_test   = total_cost_pred_test,
  calibration_ratio_test = calibration_ratio_test,
  mean_cost_obs_test     = mean_cost_obs_test,
  mean_cost_pred_test    = mean_cost_pred_test,
  rmse_pure_premium_test = rmse_pure_premium_test
)

calibration_summary_test
saveRDS(calibration_summary_test, file = "data/resultats_5.3_test.rds")

summary(freq_fit)
summary(sev_fit)

