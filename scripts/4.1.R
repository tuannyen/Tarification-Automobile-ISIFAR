library(ggplot2)
load("data/clean_sev_pos.RData")
#4.1 Point de départ : régression linéaire sur le log-coût
#1) Préparation des covariables
#Cohérence avec la partie fréquence
df_sev_pos$logDensity <- log(df_sev_pos$Density + 1)

#2) Modèle linéaire sur log(ClaimAmount)
lm_sev <- lm(
  logClaimAmount ~ DriverAge + CarAge + Power + Brand +
    Gas + Region + logDensity,
  data = df_sev_pos
)

summary(lm_sev)

# 3) Diagnostics graphiques de base

#i) Résidus vs valeurs ajustées
plot(
  fitted(lm_sev), resid(lm_sev),
  xlab = "Valeurs ajustées",
  ylab = "Résidus",
  main = "Résidus vs ajustés - modèle linéaire log-coût"
)
abline(h = 0, lty = 2)

#ii) QQ-plot des résidus
qqnorm(resid(lm_sev),
       main = "QQ-plot des résidus - modèle linéaire log-coût")
qqline(resid(lm_sev))

#iii) Histogramme des résidus
hist(resid(lm_sev),
     breaks = 50,
     main   = "Distribution des résidus - modèle linéaire log-coût",
     xlab   = "Résidus")

#iv) Indicateurs numériques simples
res_lm_sev <- list(
  sigma = summary(lm_sev)$sigma,        # écart-type résiduel
  R2    = summary(lm_sev)$r.squared,
  R2_adj= summary(lm_sev)$adj.r.squared
)
res_lm_sev

#5) Enregistrement des valeurs/modèle 
saveRDS(lm_sev, file = "models/lm_sev_logcost.rds")
save(res_lm_sev, file = "data/resultats_4.1.RData")
