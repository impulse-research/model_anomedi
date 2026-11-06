# =============================================================================
# Modèle : Matrix Profile (implémentation native, exacte)
# =============================================================================
#
# Le Matrix Profile (MP) mesure, pour chaque sous-séquence de longueur m, la
# distance à la sous-séquence la plus ressemblante. Une valeur élevée signale un
# motif jamais vu ailleurs (discord).
#
# Deux modes :
# - "ab"   (défaut) : chaque fenêtre de la période de test est comparée aux seules
#            fenêtres de l'apprentissage ; le seuil est le quantile du MP de
#            l'apprentissage contre lui-même. Comparable à un modèle de prévision.
# - "self" : auto-jointure sur toute la série (comme tsmp::tsmp(x, window_size = m)).
#
# Deux normalisations :
# - "z"     : chaque fenêtre est centrée-réduite -> compare des formes ; une baisse
#             durable de niveau peut passer inaperçue.
# - "brute" : distance euclidienne sur les valeurs (÷ sqrt(m)) -> sensible au niveau.
#
# Le score d'une date est celui de la fenêtre qui SE TERMINE à cette date : il
# n'utilise que des valeurs passées (détection en temps réel).
#
# Implémentation native plutôt que tsmp : séries courtes (≤ ~130 points) donc calcul
# exact instantané, AB-join + seuil d'apprentissage + normalisation brute maîtrisés.

`%||%` <- function(a, b) if (is.null(a)) b else a

.pas_freq <- function(freq) c(mois = "1 month", trimestre = "3 months", semestre = "6 months", annee = "1 year")[[freq]]

# Fenêtres glissantes (une ligne par fenêtre)
.fenetres <- function(x, m) {
  n <- length(x) - m + 1
  if (n < 1) return(matrix(numeric(0), nrow = 0, ncol = m))
  t(vapply(seq_len(n), function(i) x[i:(i + m - 1)], numeric(m)))
}

# Préparation des fenêtres selon la normalisation (fenêtre constante -> vecteur nul en z)
.preparer <- function(S, normalisation) {
  if (normalisation == "brute" || nrow(S) == 0) return(S)
  mu <- rowMeans(S)
  sdv <- apply(S, 1, sd)
  Z <- (S - mu) / sdv
  Z[!is.na(sdv) & sdv == 0, ] <- 0
  Z
}

# Distances euclidiennes entre les lignes de A et de B, ÷ sqrt(m)
.distances <- function(A, B) {
  m <- ncol(A)
  d2 <- outer(rowSums(A^2), rowSums(B^2), "+") - 2 * A %*% t(B)
  sqrt(pmax(d2, 0)) / sqrt(m)
}

#' Matrix Profile d'une série (auto-jointure) ou d'une requête contre une référence (AB-join)
#'
#' @param x Série de référence.
#' @param m Longueur des fenêtres.
#' @param query Série requête (`NULL` : auto-jointure de `x`).
#' @param normalisation `"z"` ou `"brute"`.
#' @param exclusion Zone d'exclusion des correspondances triviales en auto-jointure
#'   (|i - j| <= exclusion), défaut `ceiling(m / 2)`.
#' @return tibble `i` (début de fenêtre dans la requête), `mp`, `nn` (fenêtre voisine dans `x`).
#'   Les fenêtres contenant un NA ont `mp = NA` et ne servent pas de voisines.
matrix_profile <- function(x, m, query = NULL, normalisation = c("z", "brute"),
                           exclusion = ceiling(m / 2)) {
  normalisation <- match.arg(normalisation)
  self <- is.null(query)
  Sx <- .preparer(.fenetres(x, m), normalisation)
  Sq <- if (self) Sx else .preparer(.fenetres(query, m), normalisation)
  if (nrow(Sq) == 0) return(tibble(i = integer(), mp = numeric(), nn = integer()))
  if (nrow(Sx) == 0) return(tibble(i = seq_len(nrow(Sq)), mp = NA_real_, nn = NA_integer_))

  D <- .distances(Sq, Sx)
  D[is.na(D)] <- Inf
  if (self) D[abs(row(D) - col(D)) <= exclusion] <- Inf
  # fenêtres requête incomplètes
  D[!stats::complete.cases(Sq), ] <- Inf

  nn <- max.col(-D, ties.method = "first")
  mp <- D[cbind(seq_len(nrow(D)), nn)]
  mp[is.infinite(mp)] <- NA
  nn[is.na(mp)] <- NA_integer_
  tibble(i = seq_len(nrow(Sq)), mp = mp, nn = nn)
}

# Marque les points appartenant à une suite d'au moins n anomalies consécutives
.persistant <- function(anomalie, n) {
  a <- coalesce(anomalie, FALSE)
  r <- rle(a)
  rep(r$values & r$lengths >= n, r$lengths)
}

#' Date de fin d'apprentissage pour un ATC
#'
#' @param dates Dates de la série.
#' @param ruptures_atc Ruptures de l'ATC (toutes, confirmées ou non).
#' @param split `"premiere_rupture"` ou une date (`"2022-01-01"`).
#' @param date_defaut Repli si pas de rupture ou apprentissage trop court.
#' @param min_train_annees Durée minimale d'apprentissage.
#' @param min_points Nombre minimal de points d'apprentissage.
#' @return list(date_split, source)
choisir_split <- function(dates, ruptures_atc, split = "premiere_rupture", date_defaut = "2022-01-01",
                          min_train_annees = 3, min_points = 8) {
  dates <- sort(unique(dates))
  debut_periode <- function(d) { k <- dates[dates <= d]; if (length(k) == 0) dates[1] else max(k) }
  assez_long <- function(d) {
    as.numeric(d - min(dates)) / 365.25 >= min_train_annees && sum(dates < d) >= min_points
  }

  if (!identical(split, "premiere_rupture")) {
    d <- debut_periode(as.Date(split))
    return(list(date_split = d, source = if (assez_long(d)) "date_fixe" else "date_fixe (apprentissage court)"))
  }
  if (nrow(ruptures_atc) > 0) {
    d <- debut_periode(min(ruptures_atc$date_debut_rupture))
    if (assez_long(d)) return(list(date_split = d, source = "premiere_rupture"))
    source_repli <- "defaut (premiere rupture trop precoce)"
  } else {
    source_repli <- "defaut (aucune rupture)"
  }
  d <- debut_periode(as.Date(date_defaut))
  if (!assez_long(d)) return(list(date_split = as.Date(NA), source = "ignore (apprentissage trop court)"))
  list(date_split = d, source = source_repli)
}

#' Matrix Profile pour un ATC et une configuration
#'
#' @param df Série d'un ATC : `date`, `valeur` (+ `code_atc`, `libelle`, `freq`, `periode`).
#' @param date_split Première date de test.
#' @param m,normalisation,quantile_seuil,n_persistance Paramètres du modèle.
#' @param mode `"ab"` ou `"self"`.
#' @param exclure_covid Met à NA les valeurs d'apprentissage dans les périodes de référence.
#' @param periodes_ref Sortie de `get_periodes_reference()`.
#' @return Résultats au format commun (une ligne par date).
mp_atc <- function(df, date_split, m, normalisation = "z", quantile_seuil = 0.95, n_persistance = 2,
                   mode = c("ab", "self"), exclure_covid = FALSE, periodes_ref = NULL) {
  mode <- match.arg(mode)
  df <- df %>% arrange(date)
  freq <- df$freq[1]
  x <- df$valeur
  n <- length(x)
  train <- df$date < date_split

  # Exclusion des périodes de référence (COVID) dans l'apprentissage
  x_train <- x
  if (exclure_covid && !is.null(periodes_ref) && nrow(periodes_ref) > 0) {
    fin_periode <- as.Date(vapply(df$date, function(d) seq(d, by = .pas_freq(freq), length.out = 2)[2],
                                  numeric(1)), origin = "1970-01-01") - 1
    dans_ref <- map_lgl(seq_len(n), function(k) any(df$date[k] <= periodes_ref$Fin & fin_periode[k] >= periodes_ref$Debut))
    x_train[dans_ref & train] <- NA
  }

  score <- rep(NA_real_, n); basse <- rep(NA, n); seuil <- NA_real_
  fin_fenetre <- function(i) i + m - 1

  if (mode == "ab") {
    n_train <- sum(train)
    ref <- x_train[train]
    # apprentissage contre lui-même : scores et seuil
    mp_tr <- matrix_profile(ref, m, normalisation = normalisation)
    if (nrow(mp_tr) > 0) {
      k <- fin_fenetre(mp_tr$i)
      score[k] <- mp_tr$mp
      Sref <- .fenetres(ref, m)
      basse[k] <- rowMeans(Sref[mp_tr$i, , drop = FALSE]) <
        ifelse(is.na(mp_tr$nn), NA, rowMeans(Sref[pmax(mp_tr$nn, 1), , drop = FALSE]))
      seuil <- unname(stats::quantile(mp_tr$mp, quantile_seuil, na.rm = TRUE))
    }
    # test contre apprentissage : fenêtres se terminant dans le test
    mp_te <- matrix_profile(ref, m, query = x, normalisation = normalisation)
    if (nrow(mp_te) > 0) {
      garder <- fin_fenetre(mp_te$i) > n_train
      mp_te <- mp_te[garder, ]
      k <- fin_fenetre(mp_te$i)
      score[k] <- mp_te$mp
      Sq <- .fenetres(x, m); Sref <- .fenetres(ref, m)
      basse[k] <- rowMeans(Sq[mp_te$i, , drop = FALSE]) <
        ifelse(is.na(mp_te$nn), NA, rowMeans(Sref[pmax(mp_te$nn, 1), , drop = FALSE]))
    }
    anomalie <- !train & coalesce(score > seuil & basse, FALSE)
  } else {
    xs <- ifelse(train, x_train, x)
    mp_all <- matrix_profile(xs, m, normalisation = normalisation)
    if (nrow(mp_all) > 0) {
      k <- fin_fenetre(mp_all$i)
      score[k] <- mp_all$mp
      S <- .fenetres(xs, m)
      basse[k] <- rowMeans(S[mp_all$i, , drop = FALSE]) <
        ifelse(is.na(mp_all$nn), NA, rowMeans(S[pmax(mp_all$nn, 1), , drop = FALSE]))
      seuil <- unname(stats::quantile(mp_all$mp, quantile_seuil, na.rm = TRUE))
    }
    anomalie <- coalesce(score > seuil & basse, FALSE)
  }

  df %>%
    transmute(
      code_atc, libelle, freq, date, periode,
      phase = if_else(train, "apprentissage", "test"),
      observes = valeur,
      predits = NA_real_, ic_inf = NA_real_, ic_sup = NA_real_,
      score = score, seuil = seuil,
      sens = case_when(is.na(basse) ~ NA_character_, basse ~ "basse", TRUE ~ "haute"),
      anomalie_basse = anomalie,
      signal_persistant = .persistant(anomalie, n_persistance)
    )
}

#' Lance le Matrix Profile sur tous les ATC, fréquences et configurations
#'
#' @param series Liste nommée par fréquence de sorties de `read_series()`
#'   (ex. `list(mois = ..., trimestre = ..., semestre = ...)`).
#' @param ruptures Sortie de `read_ruptures()`.
#' @param parametres Table des configurations (`config/parametres_mp.csv`).
#' @param periodes_ref Sortie de `get_periodes_reference()`.
#' @param modes `"ab"` et/ou `"self"`.
#' @param exclure_covid `FALSE` et/ou `TRUE` (les deux sont testés par défaut).
#' @param split,date_defaut,min_train_annees Voir `choisir_split()`.
#' @return Résultats au format commun, toutes variantes empilées. Colonnes de
#'   variante : `modele, variante, mode, m, normalisation, exclure_covid,
#'   quantile_seuil, n_persistance, date_split, split_source`.
run_grille_mp <- function(series, ruptures, parametres, periodes_ref = NULL,
                          modes = "ab", exclure_covid = c(FALSE, TRUE),
                          split = "premiere_rupture", date_defaut = "2022-01-01",
                          min_train_annees = 3) {

  parametres <- parametres %>% filter(freq %in% names(series))
  configs <- parametres %>%
    tidyr::expand_grid(mode = modes, exclure_covid = exclure_covid) %>%
    mutate(variante = sprintf("MP %s m=%d %s %s", mode, as.integer(m), normalisation,
                              if_else(exclure_covid, "sans-covid", "avec-covid")))

  journal_split <- list()
  res <- pmap_dfr(configs, function(freq, m, normalisation, quantile_seuil, n_persistance,
                                    mode, exclure_covid, variante, ...) {
    s <- series[[freq]]
    s %>%
      group_split(code_atc) %>%
      map_dfr(function(df) {
        atc <- df$code_atc[1]
        r_atc <- ruptures %>% filter(code_atc == atc)
        sp <- choisir_split(df$date, r_atc, split, date_defaut, min_train_annees, min_points = 2 * m + 1)
        journal_split[[length(journal_split) + 1]] <<- tibble(
          variante = variante, freq = freq, code_atc = atc, date_split = sp$date_split, split_source = sp$source)
        if (is.na(sp$date_split)) return(tibble())
        mp_atc(df, sp$date_split, m = m, normalisation = normalisation, quantile_seuil = quantile_seuil,
               n_persistance = n_persistance, mode = mode, exclure_covid = exclure_covid,
               periodes_ref = periodes_ref) %>%
          mutate(modele = "Matrix Profile", variante = variante, mode = mode, m = as.integer(m),
                 normalisation = normalisation, exclure_covid = exclure_covid,
                 quantile_seuil = quantile_seuil, n_persistance = as.integer(n_persistance),
                 date_split = sp$date_split, split_source = sp$source,
                 n_ruptures_apprentissage = sum(r_atc$date_debut_rupture < sp$date_split))
      })
  })

  attr(res, "splits") <- bind_rows(journal_split) %>% distinct()
  ignores <- attr(res, "splits") %>% filter(is.na(date_split))
  if (nrow(ignores) > 0) {
    message(nrow(ignores), " combinaison(s) ATC × variante ignorée(s) : apprentissage trop court (voir attr(res, 'splits'))")
  }
  res %>% relocate(modele, variante, freq, code_atc, libelle, date, periode, phase)
}
