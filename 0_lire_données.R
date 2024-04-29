#===============================================================================
# Lire les données receuilli aux sites à Kenauk pendant la saison des sucres 
# 2024
#-------------------------------------------------------------------------------

# dépendances ----
if (!existsFunction("read_excel")) library("readxl")
if (!existsFunction("%>%")) library("tidyverse")

# lire les données du fichier excel (Données_sécheresse_2024.xlsx) qui sont 
# dans un format longue ----
d_mesures <- read_excel(path = "../données/Donnée_sécheresse_2024.xlsx",
                        sheet = "Mesures",
                        col_names = c("s", "t", "a", "id", "dhp", "sec_lettre", 
                                      "sec_chiffre", "date", "C", "G", "p_tot", 
                                      "p_vide", "p_sève", "brix1", "brix2", 
                                      "brix3", "brix", "Commentaires"),
                        skip = 1,
                        na = "NA") %>%
  mutate(sec_chiffre = as.character(sec_chiffre),
         date = as_date(date),
         C = as.logical(C),
         G = as.logical(G))

# liste chronologique des dates de mesures ----
dates <- unique(d_mesures$date)[order(unique(d_mesures$date))]

# lire les poids des barils vides du fichier excel 
# (Données_sécheresse_2024.xlsx) ----
d_barils <- read_excel(path = "../données/Donnée_sécheresse_2024.xlsx",
                       sheet = "Barils",
                       col_names = c("n", "id", "p_baril"),
                       skip = 1,
                       na = "NA") %>% 
  select(-n)

# fusionne les données du poids des barils et les données des mesures ----
d <- left_join(d_mesures, d_barils, by = "id")

# calculer le poids résiduel de la sève dans le barils (p. ex. glace à 
# l'intérieur du baril) ----
d <- d %>% mutate(p_res = ifelse (p_vide - p_baril > 0, p_vide - p_baril, 0))

# boucle pour corriger le poids de sève en tenant compte du poids résiduel ----
d$p_corrigé <- NA
for (r in 1:dim(d)[1]) {
  
  # calcule le volume de la sève pendant la prmière visite de mesure y compris 
  # la sève résiduelle dans le baril ----
  if (d$date[r] == dates[1]) {
    d$p_corrigé[r] <- d$p_tot[r] - d$p_baril[r]
  
  # pour toute les visites suivantes distrait la sève résiduelle de la visite 
  # précédente du poids de la sève mesuré ----
  } else {
    # trouve la date précédente ----
    date_precédente <- dates[which(d$date[r] == dates) - 1]
    
    # enlève le poids de la sève résiduelle ----
    d$p_corrigé[r] <- d$p_tot[r] - d$p_baril[r] -
      d$p_res[which(d$date == date_precédente & d$id == d$id[r])]
  }
}

# coéfficients pour calculer la densité spécifique en fonction du brix d'après 
# Allard (1999) ----------------------------------------------------------------
a_coef <- 0.999992956631726
b_coef <- -0.00925262369550272
c_coef <- -0.00539288034998694
d_coef <- 0.0000222308169457791

# convertir le poids de la sève dans un volume avec la densité en fonction du 
# brix d'après l'info-fiche d'Allard (1999) ------------------------------------
d <- d %>% mutate(rho_s = (a_coef + (c_coef * brix)) / 
                          (1 + b_coef * brix + d_coef * brix**2),
                  vol_s = p_corrigé * rho_s)

# convertir t (traitement) comme facteur ---------------------------------------
d <- d %>% mutate(s = factor(s),
                  t = factor(t),
                  a = factor(a))

# assurer que le volume minimale est de zéro (p. ex., rien a coulé) ------------
d$vol_s[d$vol_s < 0] <- 0

# nettoyer l'espace de travail -------------------------------------------------
rm(d_barils, d_mesures, a_coef, b_coef, c_coef, d_coef, date_precédente, dates, 
   r)

#===============================================================================