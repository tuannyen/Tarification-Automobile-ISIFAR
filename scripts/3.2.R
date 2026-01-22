library(ggplot2)
library(MASS)     
load("data/clean_data.RData")

pois_fit <- readRDS("models/pois_fit.rds")
nb_fit   <- readRDS("models/nb_fit.rds")

#3.2
#1) Préparations des données
#i) Selection des variables
data_comptage <- df_freq[, c(
  "ClaimNb", "Exposure", "DriverAge", "CarAge",
  "Power", "Brand", "Gas", "Region", "Density"
)]

#ii) Transformations 
data_comptage$logExposure <- log(data_comptage$Exposure)
data_comptage$logDensity  <- log(data_comptage$Density + 1)

#2) Modèle de régression de Poisson
#i) Modèle
pois_fit <- glm(
  ClaimNb ~ DriverAge + CarAge + Power + Brand + Gas + Region + logDensity +
    offset(logExposure),
  family = poisson(link = "log"),
  data   = data_comptage
)

summary(pois_fit)

#ii) Diagnostic de surdispersion

df_resid_pois <- pois_fit$df.residual
res_pearson_pois <- residuals(pois_fit, type = "pearson")
phi_pearson_pois <- sum(res_pearson_pois^2) / df_resid_pois
phi_pearson_pois

#3) Modèle de régression binomial négatif
#i) Modèle
nb_fit <- glm.nb(
  ClaimNb ~ DriverAge + CarAge + Power + Brand + Gas + Region + logDensity +
    offset(logExposure),
  data = data_comptage
)

summary(nb_fit)

#ii) Diagnostic de surdispersion 
res_pearson_nb <- residuals(nb_fit, type = "pearson")
df_resid_nb    <- nb_fit$df.residual
phi_pearson_nb <- sum(res_pearson_nb^2) / df_resid_nb
phi_pearson_nb

#4) Comparaison des modèles 
#i) Extraction des scalaires Poisson
res_pois <- list(
  null_dev   = summary(pois_fit)$null.deviance,
  resid_dev  = summary(pois_fit)$deviance,
  AIC        = AIC(pois_fit),
  dispersion = 1,
  phi        = phi_pearson_pois,
  theta      = NA
)

#ii) Extraction des scalaires NB
res_nb <- list(
  null_dev   = summary(nb_fit)$null.deviance,
  resid_dev  = summary(nb_fit)$deviance,
  AIC        = AIC(nb_fit),
  theta      = nb_fit$theta      # paramètre NB
)

res_nb
res_pois

#5)Enregistrement des modèles et des scalaires
saveRDS(pois_fit, "models/pois_fit.rds")
saveRDS(nb_fit,   "models/nb_fit.rds")
save(res_pois, res_nb,
     file = "data/resultats_3.2.RData"
)







