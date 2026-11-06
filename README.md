# 🔎 Détection de ruptures Medic'AM — Matrix Profile

Ce dépôt teste des modèles de détection de ruptures d'approvisionnement sur les séries de dispensation **Medic'AM**. Les séries sont produites par le dépôt `Parsing_MEDIC_AM_data`.

Le premier modèle est le **Matrix Profile**, appliqué à tous les ATC d'intérêt au mois, au trimestre et au semestre. Les résultats suivent un **format commun** : d'autres modèles (ETS, …) pourront être ajoutés puis évalués et tracés avec les mêmes fonctions.

---

## 🛠️ Structure

```text
.
├── R/
│   ├── import.R        # séries, ruptures (dernière version du fichier), périodes de référence
│   ├── modele_mp.R     # Matrix Profile (implémentation native), découpage apprentissage / test
│   ├── evaluation.R    # sensibilité, délai, fausses alertes, précision / rappel par période
│   └── graphiques.R    # graphique commun à tous les modèles
├── config/
│   ├── periodes_reference.csv   # période COVID (dates à ajuster)
│   └── parametres_mp.csv        # configurations du Matrix Profile
├── data/
│   └── Dates_ruptures_v<version>.xlsx   # non versionné
├── outpout/            # résultats, évaluations, graphiques, journaux (non versionné)
└── main.R
```

## 📋 Prérequis

```r
install.packages(c("tidyverse", "readxl", "stringi", "patchwork"))
```

Les séries doivent avoir été écrites par `main.R` du dépôt `Parsing_MEDIC_AM_data`, dans `outpout/series/atc_mois.rds`, `atc_trimestre.rds` et `atc_semestre.rds`. Par défaut, les deux dépôts sont côte à côte :

```text
parent/
├── Parsing_MEDIC_AM_data/
└── <ce dépôt>/
```

---

## 🚀 Utilisation

Ajuster les paramètres en tête de `main.R`, puis lancer le script :

```r
chemin_series     <- "../Parsing_MEDIC_AM_data/outpout/series"
mesure            <- "nb_boite_par_jour"     # ou "nb_ddd_par_jour"
frequences        <- c("mois", "trimestre", "semestre")
min_jours_rupture <- 1                       # jours de rupture minimaux pour qu'une période soit "en rupture"
split             <- "premiere_rupture"      # ou une date fixe "2022-01-01"
date_defaut       <- "2022-01-01"
min_train_annees  <- 3
```

**Sorties :**

| Fichier | Contenu |
|---|---|
| `outpout/resultats/resultats_mp.rds` / `.csv` | Résultats au format commun, toutes variantes |
| `outpout/resultats/splits_mp.csv` | Date de fin d'apprentissage par ATC et variante, et sa justification |
| `outpout/resultats/evaluation_mp_par_atc.csv` | Évaluation par ATC |
| `outpout/resultats/evaluation_mp_resume.csv` | Évaluation agrégée par variante, fréquence, jeu de ruptures et signal |
| `outpout/graphiques/<freq>_<variante>.pdf` | Un PDF par variante, une page par ATC |
| `outpout/logs/ruptures_ignorees.csv` | Ruptures non rattachées : molécule inconnue, ATC non suivi, sans date… |

---

## 1. Données

### Séries : `read_series(freq, chemin_series, mesure)`

* Lit `atc_<freq>.rds` et renvoie `code_atc, libelle, freq, periode, date, valeur, nb_jours`.
* La mesure par défaut est `nb_boite_par_jour` : boîtes / nombre de jours de la période, années bissextiles comprises.
* Les **périodes incomplètes** (dernier trimestre ou semestre en cours) sont retirées par défaut (`exclure_incompletes = TRUE`).

### Ruptures : `read_ruptures(dir = "data", atc_suivis)`

* **Fichier :** la dernière version de `Dates_ruptures_v<version>.xlsx` est prise automatiquement (`derniere_version_ruptures()`). Les versions sont comparées comme des numéros : v10 > v9, v2.1 > v2. Les fichiers temporaires d'Excel sont ignorés.
* **Onglet `rupture` :** `rupture_id`, la colonne de molécule (repérée par élimination, quel que soit son en-tête), `date_debut_rupture`, `date_fin_rupture`, et `source_id`, `source_id2`, … `source_idn`.
* **Onglet `sources` :** `source_id` → `classification`.
* **Onglet `molecules` :** `dci` → `code_atc`. La comparaison se fait sans accents ni casse. Une DCI liée à plusieurs ATC applique la rupture à chacun.
* **`Type` :** « Rupture confirmée » si **au moins une** source est classée `rupture confirmee`, sinon « Rupture non confirmée ».
* **`niveau_max` :** classification la plus forte parmi les sources (confirmée > tension d'approvisionnement > risque de rupture > autre). `remise a disposition` ne compte pas comme signal.
* **Rupture sans date de fin :** elle est considérée en cours jusqu'à aujourd'hui (`date_fin_defaut`).

### Ruptures par période : `ruptures_par_periode(ruptures, dates, freq, min_jours_rupture)`

Une période est « en rupture » si elle contient **au moins `min_jours_rupture` jours** de rupture. Les jours couverts par plusieurs ruptures ne comptent qu'une fois.

### Périodes de référence : `config/periodes_reference.csv`

Elles contiennent par défaut la période COVID, large : **1er mars 2020 → 30 juin 2021** (du premier confinement à la fin du troisième et à la levée du couvre-feu). Ces dates sont à ajuster.

---

## 2. Matrix Profile

Le **Matrix Profile** (MP) mesure, pour chaque fenêtre de *m* périodes, la distance à la fenêtre la plus ressemblante. Une valeur élevée signale un motif inédit.

* **Mode `"ab"` (défaut) :** chaque fenêtre de la période de test est comparée aux seules fenêtres de l'apprentissage. Le **seuil** est le quantile (95 % par défaut) du MP de l'apprentissage contre lui-même. Le mode `"self"` correspond à l'auto-jointure classique sur toute la série (comme `tsmp::tsmp(x, window_size = m)`).
* **Normalisation `"z"` :** chaque fenêtre est centrée-réduite, ce qui compare des **formes**. Une baisse durable de niveau peut passer inaperçue, puisque le niveau est effacé.
* **Normalisation `"brute"` :** distance sur les valeurs, donc **sensible au niveau**.
* **Date d'un score :** c'est celle de la **fin de la fenêtre**. Le score n'utilise que le passé, ce qui donne un délai de détection réaliste.
* **`anomalie_basse` :** score > seuil **et** fenêtre en moyenne plus basse que sa plus proche voisine.
* **`signal_persistant` :** au moins `n_persistance` anomalies consécutives.
* **Implémentation native**, sans `tsmp`. Les séries étant courtes (≤ 130 points), le calcul exact est instantané. Cela donne aussi la jointure apprentissage → test, le seuil calibré sur l'apprentissage et la normalisation brute.

### Configurations : `config/parametres_mp.csv`

| freq | m | normalisation | quantile_seuil | n_persistance |
|---|---|---|---|---|
| mois | 3, 6, 12 | z, brute | 0.95 | 3 |
| trimestre | 4 | z, brute | 0.95 | 2 |
| semestre | 4 | z, brute | 0.95 | 2 |

Une ligne correspond à une configuration. Chaque configuration est lancée **avec et sans** les périodes COVID dans l'apprentissage (`exclure_covid = c(FALSE, TRUE)`). Le nom de variante est de la forme `MP ab m=6 brute sans-covid`.

> ⚠️ **Semestre :** environ 22 points de 2015 à 2025, donc peu de fenêtres d'apprentissage. Les résultats sont fragiles.

### Découpage apprentissage / test : `choisir_split()`

* **`split = "premiere_rupture"` :** l'apprentissage s'arrête juste avant la période contenant la **première rupture** de l'ATC (tous types). Aucune rupture n'est donc vue à l'entraînement.
* **Repli sur `date_defaut` :** si l'ATC n'a pas de rupture, ou si sa première rupture laisse moins de `min_train_annees` ans (ou moins de 2m+1 points) d'apprentissage. La raison est indiquée dans `split_source`. Les ruptures restées dans l'apprentissage sont comptées dans `n_ruptures_apprentissage`.
* **`split = "2022-01-01"` :** date fixe pour tous les ATC.

### Lancer une grille : `run_grille_mp()`

```r
resultats <- run_grille_mp(series, ruptures, parametres, periodes_ref,
                           modes = "ab", exclure_covid = c(FALSE, TRUE),
                           split = "premiere_rupture", date_defaut = "2022-01-01")
```

---

## 3. Format commun des résultats

C'est ce format qui permet de comparer les modèles : tout nouveau modèle doit le produire.

| Colonne | Description |
|---|---|
| `modele`, `variante` | Nom du modèle, et de la configuration |
| `freq`, `code_atc`, `libelle`, `date`, `periode` | Série et période |
| `phase` | `"apprentissage"` ou `"test"` |
| `observes` | Valeur observée |
| `predits`, `ic_inf`, `ic_sup` | Prévision et intervalle (NA pour le Matrix Profile) |
| `score`, `seuil` | Score d'anomalie et seuil (NA pour un modèle de prévision) |
| `anomalie_basse`, `signal_persistant` | Alertes (`TRUE` / `FALSE`) |
| `date_split`, `split_source` | Découpage utilisé |

Les colonnes propres au Matrix Profile s'y ajoutent : `mode`, `m`, `normalisation`, `exclure_covid`, `quantile_seuil`, `n_persistance`, `sens`, `n_ruptures_apprentissage`.

---

## 4. Évaluation : `evaluer_modeles(resultats, ruptures, min_jours_rupture, avance_periodes)`

L'évaluation porte sur la **phase de test seulement**, avec **deux jeux** de ruptures : `confirmees` (confirmées seules) et `toutes`. Les deux signaux sont évalués séparément : `anomalie_basse` et `signal_persistant`.

| Indicateur | Définition |
|---|---|
| `sensibilite` | Ruptures détectées / ruptures. Une rupture est détectée si une alerte tombe entre `avance_periodes` périodes avant son début et sa fin. |
| `delai_median_jours` | Première alerte − début de la rupture. Une valeur négative signifie une alerte anticipée. Les dates d'alerte sont des débuts de période. |
| `fausses_alertes_par_an` | Suites d'alertes consécutives ne touchant aucune rupture (tous types), par année de test |
| `precision_periodes`, `rappel_periodes` | Au niveau des périodes en rupture (`min_jours_rupture`). Pour le jeu `confirmees`, les périodes en rupture non confirmée sont retirées du calcul. |

`$par_atc` donne le détail par ATC, `$resume` l'agrégat par variante × fréquence × jeu × signal.

---

## 5. Graphiques : `graphique_modele()` et `sauver_graphiques()`

```r
r <- resultats %>% filter(variante == "MP ab m=6 brute avec-covid", freq == "mois")
graphique_modele("N05AH04", r, ruptures, periodes_ref)

sauver_graphiques(resultats, ruptures, periodes_ref, dir = "outpout/graphiques")
```

Le graphique reprend la mise en forme du modèle ETS 1 :
* en fond, la période COVID et les ruptures (confirmées en rouge, non confirmées en bleu) ;
* la série observée ;
* la prévision et son intervalle, **si le modèle en fournit** ;
* les anomalies basses (petits points rouges) et les signaux persistants (gros points).

Pour le Matrix Profile, un **second panneau** montre le score et le seuil. Une ligne pointillée verticale marque le début du test.

---

## ➕ Ajouter un modèle

1. Écrire une fonction qui renvoie le **format commun** (section 3) pour un ATC, une fréquence et un découpage. `choisir_split()` peut être réutilisée pour avoir les mêmes découpages que le Matrix Profile.
2. Empiler ses résultats avec ceux du Matrix Profile : `bind_rows(resultats_mp, resultats_ets)`.
3. `evaluer_modeles()` et `sauver_graphiques()` fonctionnent sans modification.
