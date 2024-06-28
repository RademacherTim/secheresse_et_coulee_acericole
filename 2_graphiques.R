#===============================================================================
# Produire des graphiques pour les donneés 
#-------------------------------------------------------------------------------

# lire les données -------------------------------------------------------------
source ("0_lire_données.R")

# graphique du volume total de sève par traitement et arbre ----
#-------------------------------------------------------------------------------
par(mar = c(5, 5, 1, 1))
plot(x = jitter(as.numeric(d_saison$t[d_saison$t == 2]), factor = 1),
     y = d_saison$vol_s[d_saison$t == 2],
     xlim = c(0.5, 2.5), ylim = c(0, 60),
     #xlab = "Traitement", ylab = "Volume de sève (litres)",
     xlab = "Treatment", ylab = "Sap volume (litres)",
     axes = FALSE, pch = 19, col = "#67a9cfaa")
points(x = jitter(as.numeric(d_saison$t[d_saison$t == 1]), factor = 1),
       y = d_saison$vol_s[d_saison$t == 1], pch = 19, col = "#ef8a62aa")
axis(side = 1, at = 1:2, labels = c("Exclusion", "Control"))
#axis(side = 1, at = 1:2, labels = c("Exclusion", "Témoin"))
axis(side = 2, las = 1)
# ajoute l'écart type et les moyennes ----
segments(x0 = mean(as.numeric(d_saison$t[d_saison$t == 1])) - 0.2,
         x1 = mean(as.numeric(d_saison$t[d_saison$t == 1])) + 0.2,
         y0 = mean(d_saison$vol_s[d_saison$t == 1]),
         y1 = mean(d_saison$vol_s[d_saison$t == 1]),
         col = "#aaaaaa", lwd = 6)
points(x = mean(as.numeric(d_saison$t[d_saison$t == 1])),
       y = mean(d_saison$vol_s[d_saison$t == 1]), 
       col = "#aaaaaa", bg = "#ef8a62", lwd = 6, pch = 21, cex = 4)
segments(x0 = mean(as.numeric(d_saison$t[d_saison$t == 2])) - 0.2,
         x1 = mean(as.numeric(d_saison$t[d_saison$t == 2])) + 0.2,
         y0 = mean(d_saison$vol_s[d_saison$t == 2]),
         y1 = mean(d_saison$vol_s[d_saison$t == 2]),
         col = "#aaaaaa", pch = 19, cex = 4, lwd = 6)
points(x = mean(as.numeric(d_saison$t[d_saison$t == 2])),
       y = mean(d_saison$vol_s[d_saison$t == 2]), 
       col = "#aaaaaa", bg = "#67a9cf", pch = 21, cex = 4, lwd = 6)

# graphique du taux de sucre par traitement ----
#-------------------------------------------------------------------------------
par(mar = c(5, 5, 1, 1))
plot(x = jitter(as.numeric(d$t[d$t == 2]), factor = 1),
     y = d$brix[d$t == 2],
     xlim = c(0.5, 2.5), ylim = c(0, 4.5),
     #xlab = "Traitement", ylab = "Concentration de saccharose (°Brix)",
     xlab = "Treatment", ylab = "Estimated succrose concentration (°Brix)",
     axes = FALSE, pch = 19, col = "#67a9cf88", lwd = 1)
points(x = jitter(as.numeric(d$t[d$t == 1]), factor = 1),
       y = d$brix[d$t == 1], pch = 19, col = "#ef8a6288", lwd = 1)
#axis(side = 1, at = 1:2, labels = c("Exclusion", "Témoin"))
axis(side = 1, at = 1:2, labels = c("Exclusion", "Control"))
axis(side = 2, las = 1)
# ajoute l'écart type et les moyennes ----
segments(x0 = mean(as.numeric(d$t[d$t == 1])) - 0.2,
         x1 = mean(as.numeric(d$t[d$t == 1])) + 0.2,
         y0 = mean(d$brix[d$t == 1], na.rm = TRUE),
         y1 = mean(d$brix[d$t == 1], na.rm = TRUE),
         col = "#aaaaaa", lwd = 6)
points(x = mean(as.numeric(d$t[d$t == 1])),
       y = mean(d$brix[d$t == 1], na.rm = TRUE), 
       col = "#aaaaaa", bg = "#ef8a62", lwd = 6, pch = 21, cex = 4)
segments(x0 = mean(as.numeric(d$t[d$t == 2])) - 0.2,
         x1 = mean(as.numeric(d$t[d$t == 2])) + 0.2,
         y0 = mean(d$brix[d$t == 2], na.rm = TRUE),
         y1 = mean(d$brix[d$t == 2], na.rm = TRUE),
         col = "#aaaaaa", lwd = 6)
points(x = mean(as.numeric(d$t[d$t == 2])),
       y = mean(d$brix[d$t == 2], na.rm = TRUE), 
       col = "#aaaaaa", bg = "#67a9cf", pch = 21, cex = 4, lwd = 6)

#===============================================================================
