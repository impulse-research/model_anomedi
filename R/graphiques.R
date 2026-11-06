# =============================================================================
# Graphiques communs à tous les modèles
# =============================================================================
#
# graphique_modele() reprend la mise en forme du modèle ETS 1 :
# - périodes de référence (COVID) et ruptures en fond ;
# - série observée sur toute la période ;
# - prévision + intervalle, s'ils existent (modèles de prévision) ;
# - anomalies basses (petits points rouges) et signaux persistants (gros points) ;
# - pour le Matrix Profile, un second panneau : score et seuil.

#' Graphique d'un modèle pour un ATC
#'
#' @param code_atc_test Code ATC.
#' @param resultats Résultats au format commun, filtrés sur UNE variante.
#' @param ruptures Sortie de `read_ruptures()`.
#' @param periodes_ref Sortie de `get_periodes_reference()`.
#' @param noms_atc Vecteur nommé code ATC -> libellé (défaut : libellé des résultats).
#' @param y_lab Titre de l'axe des ordonnées.
#' @return Un objet ggplot (ou patchwork si un score est disponible).
graphique_modele <- function(code_atc_test, resultats, ruptures, periodes_ref = NULL,
                             noms_atc = NULL, y_lab = "Boîtes délivrées / jour") {

  resultats_graph <- resultats %>% filter(code_atc == code_atc_test) %>% arrange(date)
  if (nrow(resultats_graph) == 0) stop("Aucun résultat pour ", code_atc_test)
  if (n_distinct(resultats_graph$variante) > 1) stop("Filtrer `resultats` sur une seule variante.")

  nom <- if (!is.null(noms_atc) && code_atc_test %in% names(noms_atc)) noms_atc[[code_atc_test]] else resultats_graph$libelle[1]
  variante <- resultats_graph$variante[1]
  date_split <- resultats_graph$date_split[1]

  # Périodes de rupture propres à la molécule (dans la fenêtre du graphique)
  ruptures_mol <- ruptures %>%
    filter(code_atc == code_atc_test) %>%
    distinct(rupture_id, date_debut_rupture, date_fin_rupture, Type)
  periode_covid <- periodes_ref %||% tibble(type = character(), Debut = as.Date(character()), Fin = as.Date(character()))

  observations <- resultats_graph %>% select(Date = date, Disp_jour = observes)
  predictions  <- resultats_graph %>% filter(!is.na(predits)) %>% select(Date = date, Predits = predits, IC_inf = ic_inf, IC_sup = ic_sup)
  anomalies    <- resultats_graph %>% filter(anomalie_basse) %>% select(Date = date, Observes = observes)
  persistants  <- resultats_graph %>% filter(signal_persistant) %>% select(Date = date, Observes = observes)

  bornes <- range(resultats_graph$date)
  sous_titre <- sprintf("%s | Apprentissage %s - %s | Test %s - %s",
                        variante, format(bornes[1], "%Y-%m"), format(date_split - 1, "%Y-%m"),
                        format(date_split, "%Y-%m"), format(bornes[2], "%Y-%m"))

  p <- ggplot2::ggplot() +

    # Périodes de référence (COVID)
    ggplot2::geom_rect(
      data = periode_covid,
      ggplot2::aes(xmin = Debut, xmax = Fin + 1, ymin = -Inf, ymax = Inf, fill = type),
      inherit.aes = FALSE, alpha = 0.15) +

    # Périodes de rupture spécifiques à la molécule
    ggplot2::geom_rect(
      data = ruptures_mol,
      ggplot2::aes(xmin = date_debut_rupture, xmax = date_fin_rupture + 1, ymin = -Inf, ymax = Inf, fill = Type),
      inherit.aes = FALSE, alpha = 0.20) +

    # Début de la période de test
    ggplot2::geom_vline(xintercept = date_split, linetype = "dotted", colour = "grey40") +

    # Série observée
    ggplot2::geom_line(data = observations, ggplot2::aes(x = Date, y = Disp_jour, color = "Observés"), linewidth = 0.9)

  # Prévision et intervalle (modèles de prévision uniquement)
  if (nrow(predictions) > 0) {
    p <- p +
      ggplot2::geom_ribbon(data = predictions, ggplot2::aes(x = Date, ymin = IC_inf, ymax = IC_sup),
                           fill = "grey70", alpha = 0.4) +
      ggplot2::geom_line(data = predictions, ggplot2::aes(x = Date, y = Predits, color = "Prédits"),
                         linetype = "dashed", linewidth = 0.8)
  }

  p <- p +
    # Anomalies
    ggplot2::geom_point(data = anomalies, ggplot2::aes(x = Date, y = Observes), color = "red", size = 2.5) +
    # Anomalies persistantes
    ggplot2::geom_point(data = persistants, ggplot2::aes(x = Date, y = Observes), color = "red", size = 4) +

    ggplot2::scale_color_manual(values = c("Observés" = "blue", "Prédits" = "black")) +
    ggplot2::scale_fill_manual(
      name = "Périodes de référence",
      values = c("Période COVID" = "grey50", "Rupture confirmée" = "red3", "Rupture non confirmée" = "blue"),
      drop = TRUE
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::coord_cartesian(xlim = bornes + c(-15, 15)) +
    ggplot2::labs(
      title = paste(resultats_graph$modele[1], "-", nom, paste0("(", code_atc_test, ")")),
      subtitle = sous_titre,
      x = "Année", y = y_lab, color = NULL, fill = "Référence"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank())

  # Second panneau : score (Matrix Profile) et seuil
  if (any(!is.na(resultats_graph$score))) {
    sc <- resultats_graph %>% filter(!is.na(score))
    p2 <- ggplot2::ggplot(sc, ggplot2::aes(x = date, y = score)) +
      ggplot2::geom_rect(data = ruptures_mol,
                         ggplot2::aes(xmin = date_debut_rupture, xmax = date_fin_rupture + 1, ymin = -Inf, ymax = Inf, fill = Type),
                         inherit.aes = FALSE, alpha = 0.20, show.legend = FALSE) +
      ggplot2::geom_vline(xintercept = date_split, linetype = "dotted", colour = "grey40") +
      ggplot2::geom_line(ggplot2::aes(linetype = phase), colour = "grey20") +
      ggplot2::geom_hline(yintercept = sc$seuil[1], colour = "red", linetype = "dashed") +
      ggplot2::geom_point(data = sc %>% filter(anomalie_basse), colour = "red", size = 1.8) +
      ggplot2::scale_fill_manual(values = c("Rupture confirmée" = "red3", "Rupture non confirmée" = "blue")) +
      ggplot2::scale_linetype_manual(values = c(apprentissage = "solid", test = "solid"), guide = "none") +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::coord_cartesian(xlim = bornes + c(-15, 15)) +
      ggplot2::labs(x = NULL, y = "Matrix Profile",
                    caption = "Score de la fenêtre se terminant à la date ; ligne rouge : seuil (quantile de l'apprentissage)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank())
    p <- patchwork::wrap_plots(p, p2, ncol = 1, heights = c(3, 1.3))
  }
  p
}

#' Un PDF multipage par variante (une page par ATC)
#'
#' @param resultats Résultats au format commun (plusieurs variantes possibles).
#' @param dir Dossier de sortie.
#' @return Chemins des PDF (invisible).
sauver_graphiques <- function(resultats, ruptures, periodes_ref = NULL, dir = "outpout/graphiques",
                              y_lab = "Boîtes délivrées / jour", largeur = 11, hauteur = 7) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  chemins <- resultats %>%
    group_split(variante, freq) %>%
    map_chr(function(r) {
      fichier <- file.path(dir, paste0(str_replace_all(paste(r$freq[1], r$variante[1]), "[^A-Za-z0-9=._-]+", "_"), ".pdf"))
      grDevices::pdf(fichier, width = largeur, height = hauteur)
      on.exit(grDevices::dev.off(), add = TRUE)
      for (atc in sort(unique(r$code_atc))) {
        print(graphique_modele(atc, r, ruptures, periodes_ref, y_lab = y_lab))
      }
      fichier
    })
  message(length(chemins), " PDF écrits dans ", dir)
  invisible(chemins)
}
