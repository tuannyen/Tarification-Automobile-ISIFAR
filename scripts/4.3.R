library(MASS)          
load("data/clean_sev_pos.RData")
#4.3 Modèle Log-normal
#1) Préparation des covariables
#Cohérence avec la partie fréquence
df_sev_pos$logDensity <- log(df_sev_pos$Density + 1)

#2) Régression gaussienne sur le log-coût
ln_fit <- lm(
  logClaimAmount ~ DriverAge + CarAge + Power + Brand +
    Gas + Region + logDensity,
  data = df_sev_pos
)

summary(ln_fit)

#3) Paramètres log-normaux sur l'échelle du coût

#i) Paramètres sur l'échelle du log
mu_log   <- fitted(ln_fit)                     # E[log(Y)|X]
sigma    <- summary(ln_fit)$sigma             # écart-type des résidus
sigma2   <- sigma^2

#ii) Espérance du coût sur l'échelle brute
# pour un log-normal : E[Y|X] = exp(mu + 0.5 * sigma^2)
pred_lognorm <- exp(mu_log + 0.5 * sigma2)

#4) Log-vraisemblance et AIC sur l'échelle du coût

logY   <- df_sev_pos$logClaimAmount
Y      <- df_sev_pos$ClaimAmount
n      <- nrow(df_sev_pos)
k      <- length(coef(ln_fit)) + 1            # paramètres beta + sigma

#log-vraisemblance du modèle log-normal pour Y
logLik_lognorm <- sum(
  - log(Y) -
    0.5 * log(2 * pi) -
    log(sigma) -
    (logY - mu_log)^2 / (2 * sigma2)
)

AIC_lognorm <- -2 * logLik_lognorm + 2 * k

#5) Valeur de comparaison
res_lognorm <- list(
  sigma      = sigma,
  logLik     = logLik_lognorm,
  AIC        = AIC_lognorm,
  R2         = summary(ln_fit)$r.squared,
  R2_adj     = summary(ln_fit)$adj.r.squared
)
res_lognorm

#6) Diagnostics graphiques (sur l'échelle du log)

resid_ln <- residuals(ln_fit)
fit_ln   <- fitted(ln_fit)

#i) Résidus vs ajustés
plot(
  fit_ln, resid_ln,
  xlab = "Valeurs ajustées (log-coût)",
  ylab = "Résidus",
  main = "Résidus vs ajustés - modèle Log-normal",
  pch  = 19, cex = 0.4
)
abline(h = 0, lty = 2)

#ii) QQ-plot des résidus
qqnorm(
  resid_ln,
  main = "QQ-plot des résidus - modèle Log-normal",
  pch = 19, cex = 0.5
)
qqline(resid_ln, col = "red", lwd = 2)

#iii) Histogramme des résidus
hist(
  resid_ln,
  breaks = 50,
  main   = "Distribution des résidus - modèle Log-normal",
  xlab   = "Résidus",
  col    = "lightgray",
  border = "white"
)

#7)  Enregistrement des valeurs/modèle 
saveRDS(ln_fit, file = "models/lognormal_sev.rds")
save(res_lognorm, pred_lognorm,
     file = "data/resultats_4.3.RData")
