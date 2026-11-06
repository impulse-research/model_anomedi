# =============================================================================
# Évaluation des modèles contre les ruptures
# =============================================================================
#
# Deux jeux de ruptures :
# - "confirmees" : ruptures confirmées seules ;
# - "toutes"     : ruptures confirmées et non confirmées.
#
# Deux niveaux :
# - événement : une rupture est détectée si une alerte tombe entre `avance_periodes`
#   périodes avant son début et la fin de la rupture ; délai = date de la première
#   alerte - date de début (négatif = alerte anticipée). Une fausse alerte est une
#   suite d'alertes consécutives qui ne touche aucune rupture (tous types confondus).
# - période : chaque période de test est positive si elle est en rupture
#   (`min_jours_rupture`) ; précision et rappel des alertes. Pour le jeu
#   "confirmees", les périodes en rupture non confirmée sont retirées du calcul.
#
# Seule la phase de test est évaluée. Les modèles doivent fournir le format commun
# (colonnes `variante, freq, code_atc, date, phase, anomalie_basse, signal_persistant`).

#' Évalue les alertes d'un ou plusieurs modèles
#'
#' @param resultats Résultats au format commun (ex. `run_grille_mp()`).
#' @param ruptures Sortie de `read_ruptures()`.
#' @param min_jours_rupture Jours de rupture minimaux pour qu'une période soit positive.
#' @param avance_periodes Nombre de périodes avant le début d'une rupture où une alerte compte encore.
#' @param signaux Colonnes d'alerte évaluées.
#' @return list(par_atc, resume) :
#'   - `par_atc` : une ligne par variante × fréquence × ATC × jeu × signal ;
#'   - `resume`  : agrégé sur les ATC (événements et périodes poolés).
evaluer_modeles <- function(resultats, ruptures, min_jours_rupture = 1, avance_periodes = 1,
                            signaux = c("anomalie_basse", "signal_persistant")) {

  test <- resultats %>% filter(phase == "test")

  # Périodes en rupture, par fréquence
  rp <- test %>%
    distinct(freq, date) %>%
    group_split(freq) %>%
    map_dfr(function(d) {
      ruptures_par_periode(ruptures, d$date, d$freq[1], min_jours_rupture) %>% mutate(freq = d$freq[1])
    })

  test <- test %>%
    left_join(rp %>% select(freq, code_atc, date, en_rupture, en_rupture_confirmee),
              by = c("freq", "code_atc", "date")) %>%
    mutate(en_rupture = coalesce(en_rupture, FALSE), en_rupture_confirmee = coalesce(en_rupture_confirmee, FALSE))

  grille <- expand_grid(jeu = c("confirmees", "toutes"), signal = signaux)

  par_atc <- test %>%
    group_by(variante, freq, code_atc) %>%
    group_split() %>%
    map_dfr(function(d) {
      freq <- d$freq[1]; atc <- d$code_atc[1]
      pas <- .pas_freq(freq)
      d <- d %>% arrange(date)
      debut_test <- min(d$date); fin_test <- max(d$date)
      annees_test <- max(as.numeric(fin_test - debut_test) / 365.25, 1 / 12)
      r_atc <- ruptures %>% filter(code_atc == atc, date_fin_rupture >= debut_test, date_debut_rupture <= fin_test)

      # Fenêtre de détection de chaque rupture (en débuts de période)
      debut_periode <- function(x) vapply(x, function(z) { k <- d$date[d$date <= z]; if (length(k)) max(k) else min(d$date) }, numeric(1))
      fenetre <- r_atc %>%
        mutate(
          deb = as.Date(debut_periode(date_debut_rupture), origin = "1970-01-01"),
          deb = as.Date(vapply(deb, function(z) seq(z, by = paste0("-", pas), length.out = avance_periodes + 1)[avance_periodes + 1], numeric(1)), origin = "1970-01-01"),
          fin = as.Date(debut_periode(date_fin_rupture), origin = "1970-01-01")
        )

      pmap_dfr(grille, function(jeu, signal) {
        alerte <- coalesce(d[[signal]], FALSE)
        cibles <- if (jeu == "confirmees") fenetre %>% filter(Type == "Rupture confirmée") else fenetre
        dates_alerte <- d$date[alerte]

        # Événements
        detect <- map_dfr(seq_len(nrow(cibles)), function(k) {
          a <- dates_alerte[dates_alerte >= cibles$deb[k] & dates_alerte <= cibles$fin[k]]
          tibble(detectee = length(a) > 0,
                 delai_jours = if (length(a)) as.numeric(min(a) - cibles$date_debut_rupture[k]) else NA_real_)
        })
        if (nrow(detect) == 0) detect <- tibble(detectee = logical(), delai_jours = numeric())

        # Fausses alertes : suites d'alertes hors de toute fenêtre de rupture
        r <- rle(alerte); fins <- cumsum(r$lengths); debs <- fins - r$lengths + 1
        suites <- which(r$values)
        n_fa <- sum(vapply(suites, function(k) {
          dd <- d$date[debs[k]:fins[k]]
          !any(vapply(seq_len(nrow(fenetre)), function(j) any(dd >= fenetre$deb[j] & dd <= fenetre$fin[j]), logical(1)))
        }, logical(1)))

        # Périodes
        pos <- if (jeu == "confirmees") d$en_rupture_confirmee else d$en_rupture
        garder <- if (jeu == "confirmees") !(d$en_rupture & !d$en_rupture_confirmee) else rep(TRUE, nrow(d))
        tp <- sum(alerte & pos & garder); fp <- sum(alerte & !pos & garder); fn <- sum(!alerte & pos & garder)

        tibble(
          jeu = jeu, signal = signal,
          debut_test = debut_test, fin_test = fin_test,
          n_ruptures = nrow(cibles),
          n_detectees = sum(detect$detectee),
          delai_median_jours = if (any(detect$detectee)) median(detect$delai_jours, na.rm = TRUE) else NA_real_,
          n_alertes = length(suites), n_fausses_alertes = n_fa,
          fausses_alertes_par_an = n_fa / annees_test,
          vp_periodes = tp, fp_periodes = fp, fn_periodes = fn
        )
      }) %>%
        mutate(variante = d$variante[1], freq = freq, code_atc = atc, .before = 1)
    }) %>%
    mutate(
      sensibilite = if_else(n_ruptures > 0, n_detectees / n_ruptures, NA_real_),
      precision_periodes = if_else(vp_periodes + fp_periodes > 0, vp_periodes / (vp_periodes + fp_periodes), NA_real_),
      rappel_periodes = if_else(vp_periodes + fn_periodes > 0, vp_periodes / (vp_periodes + fn_periodes), NA_real_)
    )

  resume <- par_atc %>%
    group_by(variante, freq, jeu, signal) %>%
    summarise(
      n_atc = n_distinct(code_atc),
      n_atc_avec_rupture = n_distinct(code_atc[n_ruptures > 0]),
      n_ruptures = sum(n_ruptures),
      n_detectees = sum(n_detectees),
      sensibilite = if_else(n_ruptures > 0, n_detectees / n_ruptures, NA_real_),
      delai_median_jours = median(delai_median_jours, na.rm = TRUE),
      fausses_alertes_par_an = mean(fausses_alertes_par_an),
      precision_periodes = sum(vp_periodes) / pmax(sum(vp_periodes) + sum(fp_periodes), 1),
      rappel_periodes = sum(vp_periodes) / pmax(sum(vp_periodes) + sum(fn_periodes), 1),
      .groups = "drop"
    ) %>%
    mutate(delai_median_jours = if_else(is.nan(delai_median_jours), NA_real_, delai_median_jours)) %>%
    arrange(jeu, signal, freq, desc(sensibilite), fausses_alertes_par_an)

  list(par_atc = par_atc, resume = resume)
}
