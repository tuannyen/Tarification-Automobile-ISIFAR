library(MASS)
load("data/clean_sev_pos.RData")
#4.2 Modèle Gamma 
#1) Préparation des covariables
#Cohérence avec la partie fréquence
df_sev_pos$logDensity <- log(df_sev_pos$Density + 1)

#2) GLM Gamma avec lien log sur le coût brut
gamma_log_fit <- glm(
  ClaimAmount ~ DriverAge + CarAge + Power + Brand +
    Gas + Region + logDensity,
  family = Gamma(link = "log"),
  data   = df_sev_pos
)

summary(gamma_log_fit)

#3) Diagnostics numériques

#i) Dispersion de Pearson
res_pearson_gamma <- residuals(gamma_log_fit, type = "pearson")
df_resid_gamma    <- gamma_log_fit$df.residual
phi_gamma         <- sum(res_pearson_gamma^2) / df_resid_gamma
phi_gamma

#ii)  Indicateurs globaux
res_gamma <- list(
  null_dev  = summary(gamma_log_fit)$null.deviance,
  resid_dev = summary(gamma_log_fit)$deviance,
  AIC       = AIC(gamma_log_fit),
  phi       = phi_gamma
)
res_gamma

#4 ) Diagnostics graphiques

#i) Résidus déviance vs valeurs ajustées
plot(
  fitted(gamma_log_fit),
  residuals(gamma_log_fit, type = "deviance"),
  xlab = "Valeurs ajustées",
  ylab = "Résidus de déviance",
  main = "Résidus vs ajustés - modèle Gamma (lien log)",
  pch  = 19, cex = 0.4
)
abline(h = 0, lty = 2)

#ii) QQ-plot des résidus de déviance
qqnorm(residuals(gamma_log_fit, type = "deviance"),
       main = "QQ-plot des résidus - modèle Gamma (lien log)",
       pch = 19, cex = 0.5)
qqline(residuals(gamma_log_fit, type = "deviance"))

#iii) Histogramme des résidus de déviance
hist(
  residuals(gamma_log_fit, type = "deviance"),
  breaks = 50,
  main   = "Distribution des résidus - modèle Gamma (lien log)",
  xlab   = "Résidus de déviance"
)

#6) Enregistrement des valeurs/modèle 
saveRDS(gamma_log_fit, file = "models/gamma_log_sev.rds")
save(res_gamma, file = "data/resultats_4.2.RData")
