  #2 Présentation des données
  
  
  #2.2 Préparation et nettoyage des données
  
  #i) Chargement des données
  df_freq <- read.csv("data/freMTPLfreq.csv")
  df_sev  <- read.csv("data/freMTPLsev.csv")
  
  #ii) Contrôles basiques
  #Verification des valeurs (manquantes absurde)
  summary(df_freq$Exposure) #résumé statistique
  summary(df_freq$ClaimNb)
  summary(df_sev$ClaimAmount)
  colSums(is.na(df_freq)) #valeur manquante
  colSums(is.na(df_sev))
  colSums(is.na(df_freq[, c("Exposure", "ClaimNb")]))
  colSums(is.na(df_sev[, "ClaimAmount", drop = FALSE]))
  any(duplicated(df_freq$PolicyID)) #contrôle valeur aberrante ou doublon
  any(df_freq$Exposure <= 0) #Valeur attendue : FALSE
  any(df_freq$ClaimNb   <  0) #valeur attendue : FALSE
  any(df_sev$ClaimAmount < 0) #Valeur attendue : FALSE
  all(df_freq$ClaimNb %% 1 == 0) #Valeur attendue : int
  any(df_freq$ClaimNb > 0 & df_freq$Exposure == 0) #Valeur attendue : FALSE pas de sinistre si Exposure = 0
  any(duplicated(df_freq$PolicyID)) #Valeur attendue : FALSE Doublon sur l'id
  #AGe conducteur 
  summary(df_freq$DriverAge)
  any(df_freq$DriverAge < 18)
  any(df_freq$DriverAge > 100)
  #Age véhicule
  summary(df_freq$CarAge)
  any(df_freq$CarAge < 0)
  any(df_freq$CarAge > 50)
  #Densité (population)
  summary(df_freq$Density)
  hist(df_freq$Density, breaks = 50)
  
  #iii) Conversion des types
  df_freq$Power   <- factor(df_freq$Power)
  df_freq$Brand   <- factor(df_freq$Brand)
  df_freq$Gas     <- factor(df_freq$Gas)
  df_freq$Region  <- factor(df_freq$Region)
  
  #iv) Fusion pour la sévérité
  df_sev_full <- merge(df_sev, df_freq, by = "PolicyID")
  any(df_sev_full$ClaimAmount > 0 & df_sev_full$ClaimNb == 0) #Valeur attendue : FALSE pas de cout si sinistre = 0
  
  #v) Echantillonnage fréquence réduit
  set.seed(1)
  df_freq_sub <- df_freq[sample(nrow(df_freq), 50000), ]
  
  #vi) Visualisations de contrôle
  table(df_freq$ClaimNb) #fréquence
  barplot(table(df_freq$ClaimNb))
  hist(log(df_sev$ClaimAmount), breaks = 50) #coût
  hist(df_freq$Exposure, breaks = 30, xlim = c(0,1))#durée d'exposition
  
  
  
  # Fréquence : distribution du nombre de sinistres
  table(df_freq$ClaimNb)
  barplot(table(df_freq$ClaimNb),
          main = "Nombre de sinistres par contrat",
          xlab = "ClaimNb", ylab = "Effectif")
  
  # Sévérité : distribution du log des montants
  hist(log(df_sev$ClaimAmount),
       breaks = 50,
       main = "Distribution de log(ClaimAmount)",
       xlab = "log(ClaimAmount)")
  
  # Exposure : répartition des durées d’exposition
  hist(df_freq$Exposure,
       breaks = 30, xlim = c(0, 1),
       main = "Distribution de l'exposition",
       xlab = "Exposure (années)")
  
  
  
  
  
  #2.3) Création de l'échantillon test  

  set.seed(42)
  
  #i) Indices test sur la fréquence
  n <- nrow(df_freq)
  idx_test <- sample(seq_len(n), size = floor(0.20 * n))
  
  #ii) Test explicitement séparé
  df_freq_test <- df_freq[idx_test, ]
  
  #iii) Train = df_freq existant, inchangé
  df_freq      <- df_freq[-idx_test, ]   # écrase l’objet → devient train
  
  #iv) Sévérité synchronisée par PolicyID
  pid_test  <- df_freq_test$PolicyID
  pid_train <- df_freq$PolicyID
  
  df_sev_full_test <- df_sev_full[df_sev_full$PolicyID %in% pid_test, ]
  df_sev_full      <- df_sev_full[df_sev_full$PolicyID %in% pid_train, ]  # écrasement → train
  
  
  
  save(df_freq, df_freq_sub, df_sev, df_sev_full,df_freq_test, df_sev_full_test,
       file = "data/clean_data.RData")