library(tidyverse)

list.files("R", pattern = "\\.R$", full.names = TRUE) %>%
  walk(source, encoding = "UTF-8")

# -----------------------------------------------------------------------------
# Paramètres
# -----------------------------------------------------------------------------
chemin_series     <- "../Parsing_MEDIC_AM_data/outpout/series"   # séries écrites par l'autre dépôt
mesure            <- "nb_boite_par_jour"                         # ou "nb_ddd_par_jour"
frequences        <- c("mois", "trimestre", "semestre")
min_jours_rupture <- 1        # jours de rupture minimaux pour qu'une période soit "en rupture"
split             <- "premiere_rupture"   # ou une date fixe, ex. "2022-01-01"
date_defaut       <- "2022-01-01"         # ATC sans rupture, ou première rupture trop précoce
min_train_annees  <- 3

# -----------------------------------------------------------------------------
# 1. Données
# -----------------------------------------------------------------------------
series <- frequences %>% set_names() %>%
  map(~ read_series(.x, chemin_series = chemin_series, mesure = mesure))

ruptures     <- read_ruptures(dir = "data", atc_suivis = unique(series$mois$code_atc))   # dernière version de Dates_ruptures_v*.xlsx
periodes_ref <- get_periodes_reference("config/periodes_reference.csv")
parametres   <- read_csv("config/parametres_mp.csv", comment = "#", show_col_types = FALSE)

# -----------------------------------------------------------------------------
# 2. Matrix Profile : toutes les configurations, avec et sans COVID dans l'apprentissage
# -----------------------------------------------------------------------------
resultats_mp <- run_grille_mp(
  series, ruptures, parametres, periodes_ref,
  modes = "ab", exclure_covid = c(FALSE, TRUE),
  split = split, date_defaut = date_defaut, min_train_annees = min_train_annees
)

# -----------------------------------------------------------------------------
# 3. Évaluation
# -----------------------------------------------------------------------------
evaluation <- evaluer_modeles(resultats_mp, ruptures, min_jours_rupture = min_jours_rupture)
print(evaluation$resume, n = 50, width = Inf)

# -----------------------------------------------------------------------------
# 4. Sorties
# -----------------------------------------------------------------------------
dir.create("outpout/resultats", showWarnings = FALSE, recursive = TRUE)
saveRDS(resultats_mp, "outpout/resultats/resultats_mp.rds")
write_csv(resultats_mp, "outpout/resultats/resultats_mp.csv", na = "")
write_csv(attr(resultats_mp, "splits"), "outpout/resultats/splits_mp.csv", na = "")
write_csv(evaluation$par_atc, "outpout/resultats/evaluation_mp_par_atc.csv", na = "")
write_csv(evaluation$resume, "outpout/resultats/evaluation_mp_resume.csv", na = "")

# Un PDF par variante et fréquence (une page par ATC)
sauver_graphiques(resultats_mp, ruptures, periodes_ref, dir = "outpout/graphiques",
                  y_lab = if (mesure == "nb_ddd_par_jour") "DDD délivrées / jour" else "Boîtes délivrées / jour")
