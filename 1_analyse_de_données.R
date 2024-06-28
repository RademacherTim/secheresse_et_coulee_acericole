#===============================================================================
# Convertir et analyser les données recueillies pendant le temps des sucres 2024.
#-------------------------------------------------------------------------------

# dépendances ------------------------------------------------------------------
if (!existsFunction('brms')) library ('brms')
if (!existsFunction('pp_check')) library ('rstanarm')
if (!existsFunction("spread_draws")) library ("tidybayes")
if (!existsFunction("panel_border")) library ("cowplot")
if (!existsFunction("scale_y_continuous")) library ("ggplot2")

# lire les données -------------------------------------------------------------
if(!exists("d")) source ("0_lire_données.R")

# détermine le thème des graphiques --------------------------------------------
theme_set(theme_tidybayes() + panel_border())

# détermine le facteur de assèchement en fonction du secteur -------------------
d <- d %>% mutate(f = case_when(
  (is.na(sec_lettre) & is.na(sec_chiffre)) ~ 0,
  (sec_lettre %in% c("A","F")) | (sec_chiffre %in% c("1", "7")) ~ 1,
  (sec_lettre %in% c("B","E")) | (sec_chiffre %in% c("2", "6")) ~ 2,
  (sec_lettre %in% c("C","D")) & (sec_chiffre %in% c("3", "5")) ~ 3,
  (sec_lettre %in% c("C","D")) & (sec_chiffre == "4") ~ 4,
))

# calcul de la concentration de sucre moyenne et du volume totale de sève par 
# arbre pour la saison ---------------------------------------------------------
d_saison <- d %>% group_by(s, t, a, f, dhp) %>% 
  summarise(vol_s = sum(vol_s, na.rm = TRUE),
            brix = mean(brix, na.rm = TRUE),
            f = mean(f, na.rm = TRUE),
            dhp = mean(dhp, na.rm = TRUE), 
            .groups = "drop")

# quantifier l'effet de l'exclusion sur la concentration de sucre --------------
mod_brix_t <- brms::brm(brms::bf(brix | mi(brix_se) ~ 
                               t +           # traitement (témoin vs exclusion)
                               dhp +         # taille de l'arbre (DHP)
                               (1 | s / a) + # différences entre les arbres et les sites
                               (1 | date)),  # différences entre les dates de mesure
                     data = d %>% select(s, t, a, dhp, date, brix) %>% 
                       filter(!is.na(brix)) %>% add_column(brix_se = 0.2),
                     family = gaussian(), 
                     prior = c(set_prior('normal(2.5, 10)', class = 'Intercept'),
                               set_prior('exponential(1)', class = 'sigma'),
                               set_prior('normal(0, 2)', class = 'b')),
                     #control = list(adapt_delta = 0.9, max_treedepth = 11),
                     cores = 4, chains = 4,
                     iter = 6000,
                     seed = 42,
                     backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_brix_t)
pp_check(mod_brix_t, ndraws = 100)
pp_check(mod_brix_t, type = 'error_hist',  ndraws = 10)
pp_check(mod_brix_t, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_brix_t)
# La concentration en sucre est relativement faible à Kenauk avec un moyenne 
# d'environ 1,7 °Brix pour les érables dans le traitement témoin. 
summary(mod_brix_t)$fixed
ranef(mod_brix_t)$s [, , 'Intercept']
ranef(mod_brix_t)$`s:a` [, , 'Intercept']
ranef(mod_brix_t)$date [, , 'Intercept']
# Avec l'effet du DHP, le traitement témoin a un brix de 0,22 [0,11; 0,32] plus 
# élévé que le traitement exclusion. 
# Sans l'effet du DHP, l'effet est quasiment égale (i.e., 0,22 [0,11; 0,32]), 
# ce qui n'est pas surprenant vu qu'on a choisi deux cohorte avec des diamètres 
# similaires. 

# Extraire les distributions postérieures --------------------------------------
plot(conditional_effects(mod_brix_t, effects = "t")) [[1]] + 
  scale_y_continuous(name = "Estimated succrose concentration (°Brix)", 
                     limits = c(0, 3)) +
  scale_x_discrete(name = "Treatment", labels = c("Exclusion", "Control"))

# quantifier l'effet de l'exclusion sur la concentration de sucre --------------
mod_brix_ti <- brms::brm(brms::bf(brix | mi(brix_se) ~ # Altago Pal-Maple a une précision de 0.2 °Brix
                                    t *           # facteur d'assèchement
                                    dhp +         # taille de l'arbre (DHP)
                                    (1 | s / a) + # différences entre les arbres et les sites
                                    (1 | date)),  # différences entre les dates de mesure
                         data = d %>% select(s, t, a, dhp, date, brix) %>% 
                           filter(!is.na(brix)) %>% add_column(brix_se = 0.2),
                         family = gaussian(), 
                         prior = c(set_prior('normal(2.5, 10)', class = 'Intercept'),
                                   set_prior('exponential(1)', class = 'sigma'),
                                   set_prior('normal(0, 2)', class = 'b')),
                         control = list(adapt_delta = 0.9, max_treedepth = 11),
                         cores = 4, chains = 4,
                         iter = 6000,
                         seed = 42,
                         backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_brix_ti)
pp_check(mod_brix_ti, ndraws = 100)
pp_check(mod_brix_ti, type = 'error_hist',  ndraws = 10)
pp_check(mod_brix_ti, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_brix_ti)
summary(mod_brix_ti)$fixed
ranef(mod_brix_ti)$s [, , 'Intercept']
ranef(mod_brix_ti)$`s:a` [, , 'Intercept']
ranef(mod_brix_ti)$date [, , 'Intercept']

# quantifier l'effet de l'exclusion sur la concentration de sucre --------------
mod_brix_f <- brms::brm(brms::bf(brix | mi(brix_se) ~ # Altago Pal-Maple a une précision de 0.2 °Brix
                                   f +           # facteur d'assèchement
                                   dhp +         # taille de l'arbre (DHP)
                                   (1 | s / a) + # différences entre les arbres et les sites
                                   (1 | date)),  # différences entre les dates de mesure
                        data = d %>% select(s, f, a, dhp, date, brix) %>% 
                          filter(!is.na(brix)) %>% add_column(brix_se = 0.2),
                        family = gaussian(), 
                        prior = c(set_prior('normal(2.5, 10)', class = 'Intercept'),
                                  set_prior('exponential(1)', class = 'sigma'),
                                  set_prior('normal(0, 2)', class = 'b')),
                        control = list(adapt_delta = 0.9, max_treedepth = 11),
                        cores = 4, chains = 4,
                        iter = 6000,
                        seed = 42,
                        backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_brix_f)
pp_check(mod_brix_f, ndraws = 100)
pp_check(mod_brix_f, type = 'error_hist',  ndraws = 10)
pp_check(mod_brix_f, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_brix_f)
summary(mod_brix_f)$fixed
ranef(mod_brix_f)$s [, , 'Intercept']
ranef(mod_brix_f)$`s:a` [, , 'Intercept']
ranef(mod_brix_f)$date [, , 'Intercept']

# Si le facteur d'assèchement (f) est inclu dans le modèle ---------------------
plot(conditional_effects(mod_brix_f, effects = "f")) [[1]] +
  scale_y_continuous(name ="Estimated succrose concentration (°Brix)",
                     limits = c(0, 3.5)) +
  scale_x_continuous(name ="Exclusion intensity (unitless)") + 
  geom_line(color = "#ef8a62", size = 2) + 
  geom_ribbon(fill = "#ef8a6266", alpha = 0.3)
plot(conditional_effects(mod_brix_f, effects = "dhp")) [[1]] +
  scale_y_continuous(name ="Estimated succrose concentration (°Brix)",
                     limits = c(0, 3.5)) +
  scale_x_continuous(name ="Diameter at breast height (cm)") +
  geom_line(color = "#ef8a62", size = 2) + 
  geom_ribbon(fill = "#ef8a6266", alpha = 0.3)
# L'effet est plus fort, le plus que les arbres sont au centre des parcelles 
# d'exclusion. La pente de l'effet est de -0,10 [-0,15; -0,05] tenant compte de 
# l'effet du diamètre et -0,10 [-0,14; -0,05] ne pas tenant compte de l'effet 
# du diamètre. Encore une fois, ce n'est pas surprennant que tenir compte du 
# DHP ne change pas les résultats vue qu'on a choisi deux cohortes avec des 
# diamètres moyennes similaires.

# quantifier l'effet de l'exclusion sur la concentration de sucre --------------
mod_brix_fi <- brms::brm(brms::bf(brix | mi(brix_se) ~ # Altago Pal-Maple a une précision de 0.2 °Brix
                                   f *           # facteur d'assèchement
                                   dbh +         # taille de l'arbre (DHP)
                                   (1 | s / a) + # différences entre les arbres et les sites
                                   (1 | date)),  # différences entre les dates de mesure
                        data = d %>% select(s, f, a, dhp, date, brix) %>% 
                          filter(!is.na(brix)) %>% add_column(brix_se = 0.2) %>%
                          rename("dbh" = dhp),
                        family = gaussian(), 
                        prior = c(set_prior('normal(2.5, 10)', class = 'Intercept'),
                                  set_prior('exponential(1)', class = 'sigma'),
                                  set_prior('normal(0, 2)', class = 'b')),
                        control = list(adapt_delta = 0.9, max_treedepth = 11),
                        cores = 4, chains = 4,
                        iter = 6000,
                        seed = 42,
                        backend = 'cmdstanr')

# vérifier la distribution postérieur ------------------------------------------
plot(mod_brix_fi)
pp_check(mod_brix_fi, ndraws = 100)
pp_check(mod_brix_fi, type = 'error_hist',  ndraws = 10)
pp_check(mod_brix_fi, type = 'scatter_avg', ndraws = 100)
# erreur de la distribution postérieur semble être distribuée normalement

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_brix_fi)
summary(mod_brix_fi)$fixed
ranef(mod_brix_fi)$s [, , 'Intercept']
ranef(mod_brix_fi)$`s:a` [, , 'Intercept']
ranef(mod_brix_fi)$date [, , 'Intercept']

# Si le facteur d'assèchement (f) et son interaction avec la taille de l'arbre 
# sont inclu dans le modèle ----------------------------------------------------
plot(conditional_effects(mod_brix_fi, effects = "f")) [[1]] +
  scale_y_continuous(name ="Estimated succrose concentration (°Brix)",
                     limits = c(0, 3.5)) +
  scale_x_continuous(name ="Exclusion intensity (unitless)") + 
  geom_line(color = "#ef8a62", size = 2) + 
  geom_ribbon(fill = "#ef8a6266", alpha = 0.3)
plot(conditional_effects(mod_brix_fi, effects = "dbh")) [[1]] +
  scale_y_continuous(name ="Estimated succrose concentration (°Brix)",
                     limits = c(0, 3.5)) +
  scale_x_continuous(name ="Diameter at breast height (cm)") +
  geom_line(color = "#ef8a62", size = 2) + 
  geom_ribbon(fill = "#ef8a6266", alpha = 0.3)
plot(conditional_effects(mod_brix_fi, effects = "f:dbh")) [[1]] +
  scale_y_continuous(name ="Estimated succrose concentration (°Brix)",
                     limits = c(0, 3.5)) +
  scale_x_continuous(name ="Exclusion intensity (unitless)") +
  scale_fill_manual(values = c("18.21"= "transparent", 
                             "27.36"= "transparent", 
                             "36.51"= "transparent")) +
  scale_color_manual(values = c("18.21"= "#91b9a4", 
                                "27.36"= "#00bdcc", 
                                "36.51"= "#106470"))

# comparer les modèles du brix avec LOO et WAIC --------------------------------
loo_brix_t <- brms::loo(mod_brix_t)
loo_brix_ti <- brms::loo(mod_brix_ti)
loo_brix_f <- brms::loo(mod_brix_f)
loo_brix_fi <- brms::loo(mod_brix_fi)
brms::loo_compare(loo_brix_t, loo_brix_ti, loo_brix_f, loo_brix_fi)
waic_brix_t <- brms::waic(mod_brix_t)
waic_brix_ti <- brms::waic(mod_brix_ti)
waic_brix_f <- brms::waic(mod_brix_f)
waic_brix_fi <- brms::waic(mod_brix_fi)
loo::waic_compare(waic_brix_t, waic_brix_ti, waic_brix_f, waic_brix_fi)

# quantifier l'effet de l'exclusion sur le volume de sève ----------------------
mod_vol_t <- brms::brm(brms::bf(vol_s ~ 
                                 t +           # traitement (témoin vs exclusion)
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
plot(mod_vol_t)
pp_check(mod_vol_t, ndraws = 100)
pp_check(mod_vol_t, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol_t, type = 'scatter_avg', ndraws = 100)

# effets conditionels du diamètre et du traitement -----------------------------
plot(conditional_effects(mod_vol_t)) [[1]] + 
  scale_y_continuous(name ="Total seasonal sap volume (litres)",
                     limits = c(0, 100)) +
  scale_x_discrete(name ="Treatment") 
plot(conditional_effects(mod_vol_t)) [[2]] + 
  scale_y_continuous(name ="Total seasonal sap volume (litres)",
                     limits = c(0, 160)) +
  scale_x_continuous(name ="Diameter at breast height (cm)", limits = c(10, 55)) + 
  geom_line(color = "#ef8a62", size = 2) + 
  geom_ribbon(fill = "#ef8a6266", alpha = 0.3)

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol_t)
summary(mod_vol_t)$fixed
# L'effet du traitement est négatif mais pas du tout clair avec -0,03 [-0,28; 
# 0,34]. 
ranef(mod_vol_t)$s [, , 'Intercept']
ranef(mod_vol_t)$`s:a`[, , 'Intercept']

# quantifier l'effet de l'exclusion sur le volume de sève ----------------------
mod_vol_f <- brms::brm(brms::bf(vol_s ~ 
                                  f +           # facteur d'assèchement
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
plot(mod_vol_f)
pp_check(mod_vol_f, ndraws = 100)
pp_check(mod_vol_f, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol_f, type = 'scatter_avg', ndraws = 100)

# effets conditionels du diamètre et du traitement -----------------------------
plot(conditional_effects(mod_vol_f, effects = "f")) [[1]] + 
  scale_x_continuous(name = "Exclusion intensity (unitless)") +
  scale_y_continuous(name ="Total seasonal sap volume (L)", limits = c(0, 60)) +
  geom_line(color = "#ef8a62", size = 2) + 
  geom_ribbon(fill = "#ef8a6266", alpha = 0.3)
plot(conditional_effects(mod_vol_f, effects = "dhp")) [[1]] + 
  scale_x_continuous(name = "Diameter at breast height (cm)", limits = c(10, 55)) +
  scale_y_continuous(name ="Total seasonal sap volume (litres)", limits = c(0, 160)) +
  geom_line(color = "#ef8a62", size = 2) + 
  geom_ribbon(fill = "#ef8a6266", alpha = 0.3)

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol_f)
summary(mod_vol_f)$fixed
# L'effet du traitement est négatif mais pas du tout clair avec -0,01 [-0,15; 
# 0,13] pour chaque niveau d'intensité. 
ranef(mod_vol_f)$s [, , 'Intercept']
ranef(mod_vol_f)$`s:a`[, , 'Intercept']

# quantifier l'effet de l'exclusion (intensité de l'exclusion) sur le volume de 
# sève avec une interaction entre la taille de l'arbre (dhp). et l'effet de 
# l'intensité ------------------------------------------------------------------
mod_vol_fi <- brms::brm(brms::bf(vol_s ~ 
                                  f *           # facteur d'assèchement
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
plot(mod_vol_fi)
pp_check(mod_vol_fi, ndraws = 100)
pp_check(mod_vol_fi, type = 'error_hist',  ndraws = 10)
pp_check(mod_vol_fi, type = 'scatter_avg', ndraws = 100)

# effets conditionels du diamètre et du traitement -----------------------------
plot(conditional_effects(mod_vol_fi, effects = "f")) [[1]] + 
  scale_x_continuous(name = "Exclusion intensity (unitless)") +
  scale_y_continuous(name ="Total seasonal sap volume (L)", limits = c(0, 60)) +
  geom_line(color = "#ef8a62", size = 2) + 
  geom_ribbon(fill = "#ef8a6266", alpha = 0.3)
plot(conditional_effects(mod_vol_fi, effects = "dhp")) [[1]] + 
  scale_x_continuous(name = "Diameter at breast height (cm)", limits = c(10, 55)) +
  scale_y_continuous(name ="Total seasonal sap volume (L)", limits = c(0, 160)) +
  geom_line(color = "#ef8a62", size = 2) + 
  geom_ribbon(fill = "#ef8a6266", alpha = 0.3)
plot(conditional_effects(mod_vol_fi, effects = "f:dhp")) [[1]] +
  scale_y_continuous(name ="Total seasonal sap volume (L)",
                     limits = c(0, 60)) +
  scale_x_continuous(name ="Exclusion intensity (unitless)") +
  scale_fill_manual(values = c("18.06"= "transparent", 
                               "27.37"= "transparent", 
                               "36.67"= "transparent")) +
  scale_color_manual(values = c("18.06"= "#91b9a4", 
                                "27.37"= "#00bdcc", 
                                "36.67"= "#106470"))

# regarder le sommaire et les coéfficients -------------------------------------
summary(mod_vol_fi)
summary(mod_vol_fi)$fixed
# L'effet du traitement est plus négatif avec que sans l'interaction avec la 
# taille, mais l'interaction est contre-intuitive. Nous sommes possiblement en 
# train d'introduire un artifact
ranef(mod_vol_fi)$s [, , 'Intercept']
ranef(mod_vol_fi)$`s:a`[, , 'Intercept']

#===============================================================================