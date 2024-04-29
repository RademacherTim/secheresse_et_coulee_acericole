#===============================================================================
# Convertir et analyser les données
#-------------------------------------------------------------------------------

# dépendances ----
if (!existsFunction('brms')) library('brms')
if (!existsFunction('pp_check')) library('rstanarm')

# lire les données ----
source("0_lire_données.R")

# calcul de la concentration de sucre moyenne et du volume totale de sève par 
# arbre pour la saison # dépendances -------------------------------------------
d_saison <- d %>% group_by(s, t, a, f, dhp) %>% 
  summarise(vol_s = sum(vol_s, na.rm = TRUE),
            brix = mean(brix, na.rm = TRUE),
            f = mean(f, na.rm = TRUE),
            dhp = mean(dhp, na.rm = TRUE), 
            .groups = "drop")

# détermine le facteur de assèchement en fonction du secteur -------------------
d <- d %>% mutate(f = case_when(
  (is.na(sec_lettre) & is.na(sec_chiffre)) ~ 0,
  (sec_lettre %in% c("A","F")) | (sec_chiffre %in% c("1", "7")) ~ 1,
  (sec_lettre %in% c("B","E")) | (sec_chiffre %in% c("2", "6")) ~ 2,
  (sec_lettre %in% c("C","D")) & (sec_chiffre %in% c("3", "5")) ~ 3,
  (sec_lettre %in% c("C","D")) & (sec_chiffre == "4") ~ 4,
))

# quantifier l'effet de l'exclusion sur la concentration de sucre -----
mod_brix <- brms::brm(brms::bf(brix ~ 
                               #t +           # traitement (témoin vs exclusion)
                               f +           # facteur d'assèchement (ne pas utiliser avec le traitement)
                               dhp +         # taille de l'arbre (DHP)
                               (1 | s / a) + # différences entre les arbres et les sites
                               (1 | date)),  # différences entre les dates de mesure
                     data = d,
                     family = gaussian(), 
                     prior = c(set_prior('normal(2.5, 10)', class = 'Intercept'),
                               set_prior('exponential(1)', class = 'sigma'),
                               set_prior('normal(0, 2)', class = 'b')),
                     cores = 4, chains = 4,
                     iter = 6000,
                     seed = 42,
                     backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_brix)
pp_check(mod_brix, ndraws = 100)
pp_check(mod_brix, type = 'error_hist',  ndraws = 10)
pp_check(mod_brix, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_brix)
# La concentration en sucre est relativement faible à Kenauk avec un moyenne 
# d'environ 1,7 °Brix pour les érables dans le traitement témoin. 
summary(mod_brix)$fixed
ranef(mod_brix)$s [, , 'Intercept']
ranef(mod_brix)$`s:a` [, , 'Intercept']
ranef(mod_brix)$date [, , 'Intercept']
# Avec l'effet du DHP, le traitement témoin a un brix de 0,22 [0,11; 0,33] plus 
# élévé que le traitement exclusion. 
# Sans l'effet du DHP, l'effet est quasiment égale (i.e., 0,22 [0,11; 0,32]), 
# ce qui n'est pas surprenant vu qu'on a choisi deux cohorte avec des diamètres 
# similaires. 

# Si le facteur d'assèchement (f) est inclu dans le modèle ---------------------
plot(conditional_effects(mod_brix))
# L'effet est plus fort, le plus que les arbres sont au centre des parcelles 
# d'exclusion. La pente de l'effet est de -0,10 [-0,15; -0,05] tenant compte de 
# l'effet du diamètre et -0,10 [-0,14; -0,05] ne pas tenant compte de l'effet 
# du diamètre.

# quantifier l'effet de l'exclusion sur le volume de sève ----------------------
mod_vol <- brms::brm(brms::bf(vol_s ~ 
                                 #t +           # traitement (témoin vs exclusion)
                                 f +           # facteur d'assèchement (ne pas utiliser avec le traitement)
                                 dhp +         # taille de l'arbre (DHP)
                                 (1 | s / a)), # différences entre les arbres et les sites
                      data = d_saison,
                      family = lognormal(), 
                      prior = c(set_prior('normal(3.7, 10)', class = 'Intercept'),
                                set_prior('exponential(1)', class = 'sigma'),
                                set_prior('normal(0, 2)', class = 'b')),
                      cores = 4, chains = 4,
                      iter = 6000,
                      seed = 42,
                      backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_vol)
pp_check(mod_vol, ndraws = 100)
pp_check(mod_vol, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol, type = 'scatter_avg', ndraws = 100)

# effets conditionels du diamètre et du traitement -----------------------------
plot(conditional_effects(mod_vol))

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol)
summary(mod_vol)$fixed
# L'effet du traitement est négatif mais pas significatif avec -0,03 [-0,28; 
# 0,34]. L'effet est plus large au centre des parcelles sans devenir 
# significatif [beta_f = -0,01 [-0,15; 0,13]]. 
ranef(mod_vol)$s [, , 'Intercept']
ranef(mod_vol)$`s:a`[, , 'Intercept']
#===============================================================================