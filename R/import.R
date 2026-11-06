library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tidyr)

# =============================================================================
# Import : séries Medic'AM (dépôt Parsing_MEDIC_AM_data) et ruptures
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# Texte en ASCII minuscule, espaces normalisés ("Quétiapine " -> "quetiapine")
.norm_txt <- function(x) {
  x %>% enc2utf8() %>% stringi::stri_trans_general("Latin-ASCII") %>% str_to_lower() %>% str_squish()
}

# Noms de colonnes en snake_case ASCII
.clean_col_names <- function(x) {
  x %>% .norm_txt() %>% str_replace_all("[^a-z0-9]+", "_") %>% str_replace_all("^_|_$", "")
}

# Dates depuis Excel : Date, POSIXct, numéro de série Excel ou texte (AAAA-MM-JJ, JJ/MM/AAAA)
.as_date_flex <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  x <- str_squish(as.character(x))
  out <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  dmy <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
  serie <- suppressWarnings(as.numeric(x))
  out <- coalesce(out, dmy)
  out[is.na(out) & !is.na(serie)] <- as.Date(serie[is.na(out) & !is.na(serie)], origin = "1899-12-30")
  out
}

# -----------------------------------------------------------------------------
# Séries
# -----------------------------------------------------------------------------

#' Lit une série ATC écrite par `write_medicam_series()` (dépôt Parsing_MEDIC_AM_data)
#'
#' @param freq `"mois"`, `"trimestre"` ou `"semestre"` (fichier `atc_<freq>.rds`).
#' @param chemin_series Dossier `outpout/series` de l'autre dépôt.
#' @param mesure Colonne modélisée (défaut : boîtes par jour).
#' @param exclure_incompletes Retire les périodes incomplètes (`periode_complete = FALSE`).
#' @return tibble `code_atc, libelle, freq, periode, date, valeur` (+ `nb_jours`).
read_series <- function(freq = c("mois", "trimestre", "semestre", "annee"),
                        chemin_series = "../Parsing_MEDIC_AM_data/outpout/series",
                        mesure = "nb_boite_par_jour",
                        exclure_incompletes = TRUE) {
  freq <- match.arg(freq)
  path <- file.path(chemin_series, paste0("atc_", freq, ".rds"))
  if (!file.exists(path)) {
    stop("Série introuvable : ", path, "\nLancer main.R dans le dépôt Parsing_MEDIC_AM_data, ",
         "ou corriger `chemin_series`.")
  }
  s <- readRDS(path)
  if (!mesure %in% names(s)) {
    stop("Mesure '", mesure, "' absente de ", path, ". Disponibles : ",
         paste(names(s)[vapply(s, is.numeric, logical(1))], collapse = ", "))
  }
  if (exclure_incompletes && "periode_complete" %in% names(s)) {
    n_inc <- sum(!s$periode_complete, na.rm = TRUE)
    if (n_inc > 0) message(freq, " : ", n_distinct(s$date[!s$periode_complete]), " période(s) incomplète(s) retirée(s)")
    s <- s %>% filter(coalesce(periode_complete, TRUE))
  }
  s %>%
    transmute(code_atc = id, libelle, freq = freq, periode, date,
              valeur = .data[[mesure]],
              nb_jours = if ("nb_jours" %in% names(s)) nb_jours else NA_integer_) %>%
    arrange(code_atc, date)
}

#' Périodes de référence (COVID, …) : tibble `type, Debut, Fin`
get_periodes_reference <- function(path = "config/periodes_reference.csv") {
  read_csv(path, col_types = cols(.default = col_character()), comment = "#") %>%
    transmute(type, Debut = as.Date(debut), Fin = as.Date(fin))
}

# -----------------------------------------------------------------------------
# Ruptures
# -----------------------------------------------------------------------------

#' Chemin de la dernière version de `Dates_ruptures_v<version>.xlsx`
#'
#' Versions comparées comme des numéros (v10 > v9, v1.2 > v1.1). Les fichiers
#' temporaires d'Excel (`~$...`) sont ignorés.
derniere_version_ruptures <- function(dir = "data", prefixe = "Dates_ruptures_v") {
  fichiers <- list.files(dir, pattern = paste0("^", prefixe, "[0-9]+([._][0-9]+)*\\.xlsx$"),
                         full.names = TRUE, ignore.case = TRUE)
  if (length(fichiers) == 0) stop("Aucun fichier ", prefixe, "<version>.xlsx dans ", dir)
  versions <- basename(fichiers) %>%
    str_remove(regex(paste0("^", prefixe), ignore_case = TRUE)) %>%
    str_remove(regex("\\.xlsx$", ignore_case = TRUE)) %>%
    str_replace_all("_", ".")
  choisi <- fichiers[order(numeric_version(versions), decreasing = TRUE)][1]
  message("Fichier de ruptures : ", basename(choisi),
          if (length(fichiers) > 1) paste0(" (", length(fichiers), " versions trouvées)") else "")
  choisi
}

#' Lit les ruptures et les rattache aux codes ATC
#'
#' @description
#' - onglet `rupture` : `rupture_id`, colonne de molécule (en-tête quelconque,
#'   repérée par élimination), `date_debut_rupture`, `date_fin_rupture`,
#'   `source_id`, `source_id2`, … `source_idn`.
#' - onglet `sources` : `source_id`, `classification` (et autres colonnes).
#' - onglet `molecules` : `dci`, `code_atc` (lien molécule -> ATC, une DCI peut avoir plusieurs ATC).
#'
#' `Type` = "Rupture confirmée" si au moins une source est classée "rupture confirmee",
#' sinon "Rupture non confirmée". `niveau_max` = classification la plus forte des sources
#' (confirmée > tension d'approvisionnement > risque de rupture > autre) ;
#' "remise a disposition" n'est pas un signal de rupture.
#'
#' @param path Fichier xlsx (défaut : dernière version dans `dir`).
#' @param atc_suivis Codes ATC suivis (les autres sont journalisés puis ignorés). `NULL` : tous.
#' @param date_fin_defaut Date de fin pour une rupture sans fin renseignée (en cours).
#' @param log_dir Dossier du journal `ruptures_ignorees.csv`.
#' @param col_molecule Nom de la colonne molécule de l'onglet rupture. `NULL` (défaut) :
#'   détectée automatiquement, comme la colonne dont les valeurs correspondent le
#'   mieux aux `dci` de l'onglet molecules.
#' @return tibble `rupture_id, molecule, code_atc, date_debut_rupture, date_fin_rupture,
#'   Type, niveau_max, n_sources` (une ligne par rupture × ATC).
read_ruptures <- function(path = NULL, dir = "data", atc_suivis = NULL,
                          date_fin_defaut = Sys.Date(), log_dir = "outpout/logs",
                          col_molecule = NULL) {

  path <- path %||% derniere_version_ruptures(dir)
  sheets <- readxl::excel_sheets(path)
  pick <- function(motif) {
    # noms d'onglets ET motif sans accents ni casse : "molécules", "Molecules" -> "molecules"
    s <- sheets[str_detect(.clean_col_names(sheets), .norm_txt(motif))][1]
    if (is.na(s)) stop("Onglet introuvable (motif '", motif, "') dans ", basename(path),
                       ". Onglets : ", paste(sheets, collapse = ", "))
    s
  }
  lire <- function(sheet) {
    df <- readxl::read_xlsx(path, sheet = sheet, .name_repair = "unique_quiet")
    names(df) <- .clean_col_names(names(df))
    df
  }

  rup <- lire(pick("^rupture"))
  src <- lire(pick("^source"))
  mol <- lire(pick("^molecule"))

  # --- Colonne molécule
  # Détection : parmi les colonnes non reconnues (hors colonnes sans en-tête "...43"),
  # celle dont les valeurs se retrouvent le plus dans les dci de l'onglet molecules.
  connues <- c("rupture_id", "date_debut_rupture", "date_fin_rupture")
  cols_source <- names(rup)[str_detect(names(rup), "^source_idx?\\d*$")]
  dci_connues <- unique(.norm_txt(as.character(mol$dci)))
  if (!is.null(col_molecule)) {
    col_mol <- .clean_col_names(col_molecule)
    if (!col_mol %in% names(rup)) stop("Onglet rupture : colonne '", col_molecule, "' absente. Colonnes : ",
                                       paste(names(rup), collapse = ", "))
  } else {
    candidats <- setdiff(names(rup), c(connues, cols_source))
    candidats <- candidats[!str_detect(candidats, "^\\d+$")]
    score <- vapply(candidats, function(cl) {
      v <- .norm_txt(as.character(rup[[cl]])); v <- v[!is.na(v) & v != ""]
      if (length(v) == 0) 0 else mean(v %in% dci_connues)
    }, numeric(1))
    if (length(score) == 0 || max(score) < 0.5) {
      stop("Onglet rupture : colonne molécule introuvable (part des valeurs présentes dans molecules$dci : ",
           paste(sprintf("%s = %.0f %%", names(score), 100 * score), collapse = ", "),
           "). Préciser `col_molecule`.")
    }
    col_mol <- names(score)[which.max(score)]
    message(sprintf("Colonne molécule : '%s' (%.0f %% des valeurs reconnues dans molecules$dci)",
                    col_mol, 100 * max(score)))
  }
  autres <- col_mol
  manquantes <- setdiff(connues, names(rup))
  if (length(manquantes) > 0) stop("Onglet rupture : colonnes manquantes : ", paste(manquantes, collapse = ", "))

  rup <- rup %>%
    transmute(
      rupture_id = as.character(rupture_id),
      molecule   = as.character(.data[[autres]]),
      date_debut_rupture = .as_date_flex(date_debut_rupture),
      date_fin_rupture   = .as_date_flex(date_fin_rupture),
      across(all_of(cols_source), ~ str_squish(as.character(.x)))
    ) %>%
    filter(!is.na(rupture_id) | !is.na(molecule))

  journal <- list()
  noter <- function(df, motif) if (nrow(df) > 0) journal[[length(journal) + 1]] <<- df %>% mutate(motif = motif)

  noter(rup %>% filter(is.na(date_debut_rupture)) %>% select(rupture_id, molecule), "date de début manquante")
  rup <- rup %>% filter(!is.na(date_debut_rupture))
  en_cours <- is.na(rup$date_fin_rupture)
  if (any(en_cours)) message(sum(en_cours), " rupture(s) sans date de fin : fin fixée au ", date_fin_defaut)
  rup$date_fin_rupture[en_cours] <- as.Date(date_fin_defaut)

  # --- Sources -> classification
  src <- src %>%
    transmute(source_id = str_squish(as.character(source_id)),
              classification = .norm_txt(as.character(classification))) %>%
    filter(!is.na(source_id))

  ordre <- c("rupture confirmee", "tension d'approvisionnement", "risque de rupture", "autre")
  classif <- rup %>%
    select(rupture_id, all_of(cols_source)) %>%
    pivot_longer(all_of(cols_source), values_to = "source_id") %>%
    filter(!is.na(source_id), source_id != "") %>%
    # "3;5" dans une même cellule
    separate_rows(source_id, sep = "\\s*[;,]\\s*") %>%
    left_join(src, by = "source_id") %>%
    group_by(rupture_id) %>%
    summarise(
      n_sources  = n_distinct(source_id),
      confirmee  = any(classification == "rupture confirmee", na.rm = TRUE),
      niveau_max = {
        k <- match(classification, ordre)
        if (all(is.na(k))) NA_character_ else ordre[min(k, na.rm = TRUE)]
      },
      .groups = "drop"
    )

  rup <- rup %>%
    select(-all_of(cols_source)) %>%
    left_join(classif, by = "rupture_id") %>%
    mutate(
      n_sources = coalesce(n_sources, 0L),
      Type = if_else(coalesce(confirmee, FALSE), "Rupture confirmée", "Rupture non confirmée")
    ) %>%
    select(-confirmee)
  noter(rup %>% filter(n_sources == 0) %>% select(rupture_id, molecule), "aucune source (classée non confirmée)")

  # --- Molécule -> ATC
  mol <- mol %>%
    transmute(dci_norm = .norm_txt(as.character(dci)), code_atc = str_squish(as.character(code_atc))) %>%
    filter(!is.na(dci_norm), !is.na(code_atc)) %>%
    distinct()

  rup <- rup %>%
    mutate(dci_norm = .norm_txt(molecule)) %>%
    left_join(mol, by = "dci_norm", relationship = "many-to-many")
  noter(rup %>% filter(is.na(code_atc)) %>% distinct(rupture_id, molecule), "molécule absente de l'onglet molecules")
  rup <- rup %>% filter(!is.na(code_atc))

  if (!is.null(atc_suivis)) {
    noter(rup %>% filter(!code_atc %in% atc_suivis) %>% distinct(rupture_id, molecule, code_atc), "ATC non suivi")
    rup <- rup %>% filter(code_atc %in% atc_suivis)
  }

  # --- Journal
  if (!is.null(log_dir)) {
    dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
    j <- bind_rows(journal)
    if (nrow(j) == 0) j <- tibble(rupture_id = character(), molecule = character(), motif = character())
    write_csv(j, file.path(log_dir, "ruptures_ignorees.csv"))
    if (nrow(j) > 0) message(nrow(j), " ligne(s) de rupture signalée(s) dans ", file.path(log_dir, "ruptures_ignorees.csv"))
  }

  res <- rup %>%
    select(rupture_id, molecule, code_atc, date_debut_rupture, date_fin_rupture, Type, niveau_max, n_sources) %>%
    arrange(code_atc, date_debut_rupture)
  attr(res, "fichier") <- basename(path)
  message(n_distinct(res$rupture_id), " rupture(s) sur ", n_distinct(res$code_atc), " ATC (",
          sum(res$Type == "Rupture confirmée"), " lignes confirmées)")
  res
}

#' Indique, pour chaque ATC et chaque période, si elle est en rupture
#'
#' @param ruptures Sortie de `read_ruptures()`.
#' @param dates Débuts de période (`Date`), ex. `unique(series$date)`.
#' @param freq Fréquence des périodes.
#' @param min_jours_rupture Nombre minimal de jours de rupture dans la période
#'   pour qu'elle compte comme "en rupture" (1 : un seul jour suffit).
#' @return tibble `code_atc, date, jours_rupture, jours_rupture_confirmee,
#'   en_rupture, en_rupture_confirmee`. Les jours couverts par plusieurs
#'   ruptures ne sont comptés qu'une fois.
ruptures_par_periode <- function(ruptures, dates, freq, min_jours_rupture = 1) {
  pas <- c(mois = "1 month", trimestre = "3 months", semestre = "6 months", annee = "1 year")[[freq]]
  dates <- sort(unique(as.Date(dates)))
  periodes <- tibble(date = dates,
                     fin = as.Date(vapply(dates, function(d) seq(d, by = pas, length.out = 2)[2], numeric(1)),
                                   origin = "1970-01-01") - 1)

  jours <- ruptures %>%
    mutate(jour = map2(date_debut_rupture, date_fin_rupture, ~ seq(.x, max(.x, .y), by = "1 day"))) %>%
    select(code_atc, Type, jour) %>%
    unnest(jour) %>%
    group_by(code_atc, jour) %>%
    summarise(confirmee = any(Type == "Rupture confirmée"), .groups = "drop")

  grille <- expand_grid(code_atc = unique(ruptures$code_atc), periodes)
  if (nrow(jours) == 0 || nrow(grille) == 0) {
    return(grille %>% transmute(code_atc, date, jours_rupture = 0L, jours_rupture_confirmee = 0L,
                                en_rupture = FALSE, en_rupture_confirmee = FALSE))
  }

  jours %>%
    mutate(idx  = findInterval(jour, periodes$date),
           date = if_else(idx > 0, periodes$date[pmax(idx, 1)], as.Date(NA))) %>%
    select(-idx) %>%
    filter(!is.na(date)) %>%
    left_join(periodes, by = "date") %>%
    filter(jour <= fin) %>%
    group_by(code_atc, date) %>%
    summarise(jours_rupture = n(), jours_rupture_confirmee = sum(confirmee), .groups = "drop") %>%
    right_join(grille %>% select(code_atc, date), by = c("code_atc", "date")) %>%
    mutate(
      jours_rupture           = coalesce(jours_rupture, 0L),
      jours_rupture_confirmee = coalesce(jours_rupture_confirmee, 0L),
      en_rupture              = jours_rupture >= min_jours_rupture,
      en_rupture_confirmee    = jours_rupture_confirmee >= min_jours_rupture
    ) %>%
    arrange(code_atc, date)
}
