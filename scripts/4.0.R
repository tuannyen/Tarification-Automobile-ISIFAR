load("data/clean_data.RData")
#4.0 Extraction des sinistres positifs
#i) Restriction aux sinistres strictement positifs
df_sev_pos <- subset(df_sev_full, ClaimAmount > 0)

#ii) Transformation de la variable cible pour le modèle
df_sev_pos$logClaimAmount <- log(df_sev_pos$ClaimAmount)

#iii) Apercu
summary(df_sev_pos$ClaimAmount)
summary(df_sev_pos$logClaimAmount)

#iv) Visualisation
hist(df_sev_pos$logClaimAmount,
     breaks = 50,
     main   = "Distribution de log(ClaimAmount)",
     xlab   = "log(ClaimAmount)")

#v ) Enregistrement
save(df_sev_pos, file = "data/clean_sev_pos.RData")
