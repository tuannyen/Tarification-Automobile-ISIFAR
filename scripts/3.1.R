library(ggplot2)
library(caret)
library(pROC)
library(xgboost)
load("data/clean_data.RData")
logit_fit <- readRDS("models/logit_fit.rds")
xgb_fit   <- readRDS("models/xgb_fit.rds")

#3.1
#1) Préparation des données pour les modèles de classification
#i) Variable cible

df_freq$HasClaim <- as.integer(df_freq$ClaimNb > 0)

#Ajout de la binarité
df_freq$HasClaimFac <- factor(
  ifelse(df_freq$HasClaim == 1, "Yes", "No"),
  levels = c("No", "Yes")
)

#ii) Sélection des variables

data_logit_full <- df_freq[, c(
  "HasClaimFac", "Exposure", "DriverAge", "CarAge",
  "Power", "Brand", "Gas", "Region", "Density"
)]

# Transformations
data_logit_full$logExposure <- log(data_logit_full$Exposure)
data_logit_full$logDensity  <- log(data_logit_full$Density + 1)

#iii) Sous-échantillon de 300 000 observations

set.seed(32)
idx <- sample(nrow(data_logit_full), 300000)
data_logit <- data_logit_full[idx, ]

#iv) Modèle

form_logit <- HasClaimFac ~ logExposure + DriverAge + CarAge +
  Power + Brand + Gas + Region + logDensity

#Modèle régression logistique
#i) Contrôle validation croisée (AUC ROC)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

#ii) Entraînement du modèle

logit_fit <- train(
  form_logit,
  data   = data_logit,
  method = "glm",
  family = binomial(link = "logit"),
  metric = "ROC",
  trControl = ctrl
)

logit_fit
saveRDS(logit_fit, "models/logit_fit.rds")


#iii) AUC et seuil optimal (critère de Youden)
probs <- logit_fit$pred$Yes
obs   <- logit_fit$pred$obs
roc_obj <- roc(obs, probs)
youden_logit <- coords(roc_obj, "best", ret=c("threshold","sens","spec"), best.method="youden")
youden_logit












#3) Boosting
#i) Paramètres
grid_xgb <- expand.grid(
  nrounds          = c(150, 300),   
  max_depth        = c(3, 5),       
  eta              = c(0.05, 0.1),  
  gamma            = 0,             
  colsample_bytree = 0.8,           
  min_child_weight = 1,            
  subsample        = 0.8            
)

#ii) Entraînement du modèle
xgb_fit <- train(
  form_logit,
  data      = data_logit,
  method    = "xgbTree",
  metric    = "ROC",
  trControl = ctrl,      # même CV que pour la logistique
  tuneGrid  = grid_xgb
)

xgb_fit
saveRDS(xgb_fit, "models/xgb_fit.rds")


#iii)  AUC et seuil optimal (critère de Youden)
pred_xgb <- xgb_fit$pred
roc_xgb  <- roc(pred_xgb$obs, pred_xgb$Yes)
auc_xgb  <- auc(roc_xgb)
auc_xgb
youden_xgb <- coords(
  roc_xgb, "best",
  ret = c("threshold", "sensitivity", "specificity"),
  best.method = "youden"
)

auc_xgb
youden_xgb



#Résultats figés de la régression logistique et du boosting (obtenus en local)
auc_logit <- 0.644  # AUC CV approx

youden_logit <- c(
  threshold = 0.037,
  sens      = 0.697,
  spec      = 0.514
)

auc_logit
youden_logit

auc_xgb <- 0.664

youden_xgb <- c(
  threshold  = 0.0339,
  sensitivity = 0.770,
  specificity = 0.463
)

auc_xgb
youden_xgb

save(
  auc_logit, youden_logit,
  auc_xgb, youden_xgb,
  file = "data/resultats_3.1.RData"
)


