# ============================================================================
# RAPPORT STATISTIQUE : TOURISME ET VOYAGES
# ============================================================================

# 0. PACKAGES ET IMPORTATION -----------------------------------------------
# À exécuter une seule fois si nécessaire :
install.packages(c("readr", "dplyr", "ggplot2", "scales", "forcats",
                   "summarytools", "gtsummary", "flextable", "car"))
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(forcats)
library(summarytools)
library(gtsummary)
library(flextable)
library(car)

st_options(lang = "fr")
theme_gtsummary_language(language = "fr", decimal.mark = ",", big.mark = " ")

# ============================================================
# 1.2. IMPORTATION DU FICHIER EXCEL
# ============================================================

# Installer une seule fois si nécessaire
install.packages("readxl")

# Charger le package
library(readxl)

# Sélectionner le fichier Excel (.xlsx) ou CSV
tourisme <- read_excel(file.choose())
tourisme <- read.csv2(file.choose())

# Contrôle de structure et des colonnes nécessaires
colonnes_requises <- c(
  "id", "age", "sexe", "revenu_categorie", "nombre_voyages_par_an",
  "budget_moyen_voyage_euros", "destination_preferee", "type_hebergement",
  "mode_transport_principal", "motif_principal", "reservation_en_ligne",
  "satisfaction_dernier_voyage_10"
)
manquantes <- setdiff(colonnes_requises, names(tourisme))
if (length(manquantes) > 0) {
  stop("Colonnes absentes du fichier : ", paste(manquantes, collapse = ", "))
}

# ============================================================================
# PREMIÈRE PARTIE : DONNÉES ET MÉTHODES
# ============================================================================

# 1.1. Vue d'ensemble ------------------------------------------------------
head(tourisme)
str(tourisme)
dim(tourisme)
summary(tourisme)
summarytools::dfSummary(tourisme)

# 1.2. Harmonisation des noms ---------------------------------------------
tourisme <- tourisme %>%
  rename(
    revenu       = revenu_categorie,
    nb_voyages   = nombre_voyages_par_an,
    budget       = budget_moyen_voyage_euros,
    destination  = destination_preferee,
    hebergement  = type_hebergement,
    transport    = mode_transport_principal,
    motif        = motif_principal,
    reservation  = reservation_en_ligne,
    satisfaction = satisfaction_dernier_voyage_10
  )

# 1.3. Valeurs manquantes et doublons -------------------------------------
colSums(is.na(tourisme))
sum(duplicated(tourisme$id))
table(is.na(tourisme$satisfaction), tourisme$nb_voyages == 0,
      useNA = "ifany")
tourisme %>%
  filter(is.na(budget)) %>%
  count(a_voyage = nb_voyages > 0)

# 1.4. Recodage des variables ---------------------------------------------
# Réservation doit être codée 0/1 pour la régression logistique.
if (!all(na.omit(unique(tourisme$reservation)) %in% c(0, 1))) {
  stop("La variable reservation doit être codée 0/1.")
}

tourisme <- tourisme %>%
  mutate(
    sexe = factor(sexe),
    revenu = factor(revenu, levels = c("Faible", "Moyen", "Élevé")),
    destination = fct_relevel(factor(destination),
                              "France", "Europe", "International"),
    hebergement = fct_infreq(factor(hebergement)),
    transport = fct_infreq(factor(transport)),
    motif = fct_infreq(factor(motif)),
    reserv_f = factor(reservation, levels = c(0, 1), labels = c("Non", "Oui")),
    classe_age = cut(
      age, breaks = c(17, 29, 39, 49, 59, 69, 79),
      labels = c("18-29 ans", "30-39 ans", "40-49 ans",
                 "50-59 ans", "60-69 ans", "70-79 ans")
    ),
    satisf_classe = case_when(
      is.na(satisfaction) ~ NA_character_,
      satisfaction >= 8 ~ "Élevée",
      satisfaction >= 5 ~ "Moyenne",
      TRUE ~ "Faible"
    ),
    satisf_classe = factor(satisf_classe,
                          levels = c("Faible", "Moyenne", "Élevée")),
    age10 = age / 10
  )

# Vérifications des recodages
levels(tourisme$revenu)
tourisme %>% count(classe_age)
tourisme %>% count(satisf_classe)
tourisme %>% count(reserv_f)

# 1.5. Valeurs atypiques ---------------------------------------------------
boxplot.stats(tourisme$nb_voyages)$out %>% table()
tourisme %>% filter(nb_voyages > 6) %>% count(nb_voyages)

ggplot(tourisme, aes(y = age)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red") +
  labs(title = "Boîte à moustaches de l'âge", y = "Âge (années)") +
  theme_minimal()

ggplot(tourisme, aes(y = nb_voyages)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red") +
  labs(title = "Boîte à moustaches du nombre de voyages par an",
       y = "Nombre de voyages par an") +
  theme_minimal()

ggplot(tourisme, aes(y = budget)) +
  geom_boxplot(fill = "orange", outlier.color = "red") +
  labs(title = "Boîte à moustaches du budget moyen de voyage",
       y = "Budget moyen de voyage (€)") +
  theme_minimal()

# Fonction pour mesurer l'intensité des associations qualitatives
cramer_v <- function(tab) {
  kh <- as.numeric(chisq.test(tab)$statistic)
  sqrt(kh / (sum(tab) * (min(dim(tab)) - 1)))
}

# ============================================================================
# DEUXIÈME PARTIE : ANALYSE DESCRIPTIVE
# ============================================================================

# 2.1. Variables qualitatives ---------------------------------------------
summarytools::freq(tourisme$sexe, report.nas = FALSE)
summarytools::freq(tourisme$revenu, report.nas = FALSE)
summarytools::freq(tourisme$destination, report.nas = FALSE)
summarytools::freq(tourisme$hebergement, order = "freq", report.nas = FALSE)
summarytools::freq(tourisme$transport, order = "freq", report.nas = FALSE)
summarytools::freq(tourisme$motif, order = "freq", report.nas = FALSE)
summarytools::freq(tourisme$reserv_f, report.nas = FALSE)
summarytools::freq(tourisme$satisf_classe, report.nas = FALSE)

# 2.2. Variables quantitatives --------------------------------------------
summarytools::descr(
  tourisme[, c("age", "nb_voyages", "budget")],
  stats = c("mean", "sd", "med", "min", "max", "q1", "q3", "cv"),
  transpose = TRUE
)

# 2.3. Tableau descriptif global ------------------------------------------
tableau_global <- tourisme %>%
  select(age, sexe, revenu, nb_voyages, budget, destination, hebergement,
         transport, motif, reserv_f, satisf_classe) %>%
  tbl_summary(
    label = list(
      age ~ "Âge", sexe ~ "Sexe", revenu ~ "Catégorie de revenu",
      nb_voyages ~ "Nombre de voyages par an",
      budget ~ "Budget moyen de voyage (€)",
      destination ~ "Destination préférée",
      hebergement ~ "Type d'hébergement",
      transport ~ "Mode de transport principal",
      motif ~ "Motif principal", reserv_f ~ "Réservation en ligne",
      satisf_classe ~ "Niveau de satisfaction"
    ),
    type = list(nb_voyages ~ "continuous"),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = all_continuous() ~ 1,
    missing = "ifany", missing_text = "Valeurs manquantes"
  ) %>%
  bold_labels()
tableau_global

# ============================================================================
# TROISIÈME PARTIE : ANALYSE DES RELATIONS ENTRE LES VARIABLES
# ============================================================================

# 3.1. Revenu et budget moyen de voyage -----------------------------------
# H0 : le budget moyen est identique entre les catégories de revenu.
tourisme %>%
  group_by(revenu) %>%
  summarise(
    n = sum(!is.na(budget)),
    moyenne = round(mean(budget, na.rm = TRUE), 2),
    mediane = round(median(budget, na.rm = TRUE), 2),
    ecart_type = round(sd(budget, na.rm = TRUE), 2),
    .groups = "drop"
  )

ggplot(tourisme, aes(x = revenu, y = budget, fill = revenu)) +
  geom_boxplot() +
  labs(title = "Budget de voyage selon la catégorie de revenu",
       x = "Catégorie de revenu", y = "Budget moyen de voyage (€)") +
  theme_minimal() + theme(legend.position = "none")

oneway.test(budget ~ revenu, data = tourisme, var.equal = FALSE)
kruskal.test(budget ~ revenu, data = tourisme)

tourisme %>%
  select(revenu, budget) %>%
  tbl_summary(by = revenu, statistic = all_continuous() ~ "{mean} ({sd})",
              missing = "no") %>%
  add_p(test = all_continuous() ~ "oneway.test")

# 3.2. Revenu et nombre de voyages par an ---------------------------------
tourisme %>%
  group_by(revenu) %>%
  summarise(n = n(), moyenne = round(mean(nb_voyages), 2),
            mediane = median(nb_voyages),
            ecart_type = round(sd(nb_voyages), 2), .groups = "drop")

ggplot(tourisme, aes(x = revenu, y = nb_voyages)) +
  stat_summary(fun = mean, geom = "bar", fill = "#2e86c1") +
  labs(title = "Nombre moyen de voyages annuels selon le revenu",
       x = "Catégorie de revenu", y = "Nombre moyen de voyages par an") +
  theme_minimal()

kruskal.test(nb_voyages ~ revenu, data = tourisme)

tourisme %>%
  select(revenu, nb_voyages) %>%
  tbl_summary(by = revenu, type = list(nb_voyages ~ "continuous"),
              statistic = all_continuous() ~ "{median} ({p25}, {p75})") %>%
  add_p(test = all_continuous() ~ "kruskal.test")

# 3.3. Hébergement et niveau de satisfaction ------------------------------
# H0 : le type d'hébergement et la satisfaction sont indépendants.
tab_heb <- table(tourisme$hebergement, tourisme$satisf_classe)
tab_heb
round(prop.table(tab_heb, 1) * 100, 2)
test_heb <- chisq.test(tab_heb)
test_heb
test_heb$expected
cramer_v(tab_heb)

ggplot(tourisme, aes(x = factor(satisfaction))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Répartition des scores de satisfaction",
       x = "Score de satisfaction (1 à 10)", y = "Effectif") +
  theme_minimal()

tourisme %>%
  filter(!is.na(satisf_classe)) %>%
  ggplot(aes(x = hebergement, fill = satisf_classe)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Satisfaction selon le type d'hébergement",
       x = "Type d'hébergement", y = "Proportion",
       fill = "Niveau de satisfaction") +
  theme_minimal()

tourisme %>%
  select(hebergement, satisf_classe) %>%
  tbl_cross(row = hebergement, col = satisf_classe,
            label = list(hebergement ~ "Type d'hébergement",
                         satisf_classe ~ "Niveau de satisfaction"),
            percent = "row", missing = "no") %>%
  add_p() %>% bold_p(t = 0.05) %>% bold_labels()

# 3.4. Âge et nombre de voyages par an ------------------------------------
# H0 : absence d'association monotone entre l'âge et le nombre de voyages.
tourisme %>%
  group_by(classe_age) %>%
  summarise(n = n(), moyenne = round(mean(nb_voyages), 2),
            mediane = median(nb_voyages),
            ecart_type = round(sd(nb_voyages), 2),
            part_non_voyageurs = round(100 * mean(nb_voyages == 0), 1),
            .groups = "drop")

moyennes_age <- tourisme %>%
  group_by(classe_age) %>%
  summarise(moyenne = mean(nb_voyages, na.rm = TRUE), .groups = "drop")

ggplot(moyennes_age, aes(x = classe_age, y = moyenne)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.2f", moyenne)), vjust = -0.3, size = 4) +
  geom_hline(yintercept = mean(tourisme$nb_voyages, na.rm = TRUE),
             color = "red", linetype = "dashed") +
  labs(x = "Tranche d'âge", y = "Nombre moyen de voyages par an") +
  theme_classic() +
  coord_cartesian(ylim = c(0, max(2.6, moyennes_age$moyenne, na.rm = TRUE) * 1.1))

cor.test(tourisme$age, tourisme$nb_voyages,
         method = "spearman", exact = FALSE)

# Bootstrap de rho sur les observations complètes
set.seed(123)
donnees_age <- tourisme %>% filter(!is.na(age), !is.na(nb_voyages))
rho_boot <- replicate(1000, {
  i <- sample(seq_len(nrow(donnees_age)), replace = TRUE)
  cor(donnees_age$age[i], donnees_age$nb_voyages[i], method = "spearman")
})
quantile(rho_boot, c(0.025, 0.975), na.rm = TRUE)

kruskal.test(nb_voyages ~ classe_age, data = tourisme)

mod_pois <- glm(nb_voyages ~ age10, data = tourisme, family = poisson)
summary(mod_pois)
exp(coef(mod_pois)["age10"])
exp(confint.default(mod_pois)["age10", ])
sum(residuals(mod_pois, type = "pearson")^2) / df.residual(mod_pois)

# 3.5. Âge et réservation en ligne ----------------------------------------
# H0 : l'âge n'est pas associé à la réservation en ligne.
tourisme %>%
  group_by(classe_age) %>%
  summarise(n = n(), en_ligne = sum(reservation == 1, na.rm = TRUE),
            part = round(100 * mean(reservation == 1, na.rm = TRUE), 1),
            .groups = "drop")

taux_age <- tourisme %>%
  group_by(classe_age) %>%
  summarise(taux = mean(reservation, na.rm = TRUE) * 100, .groups = "drop")

ggplot(taux_age, aes(x = classe_age, y = taux)) +
  geom_col(fill = "#245580") +
  geom_text(aes(label = sprintf("%.1f %%", taux)), vjust = -0.3, size = 4) +
  geom_hline(yintercept = mean(tourisme$reservation, na.rm = TRUE) * 100,
             color = "red", linetype = "dashed") +
  labs(x = "Tranche d'âge", y = "Part de réservation en ligne (%)") +
  theme_classic() +
  coord_cartesian(ylim = c(0, max(48, taux_age$taux, na.rm = TRUE) * 1.1))

t.test(age ~ reserv_f, data = tourisme)
wilcox.test(age ~ reserv_f, data = tourisme)

# d de Cohen avec écart-type combiné (descriptif ; distinct de Welch)
age_reserv <- tourisme %>% filter(!is.na(age), !is.na(reserv_f))
m <- tapply(age_reserv$age, age_reserv$reserv_f, mean)
s <- tapply(age_reserv$age, age_reserv$reserv_f, var)
n_groupes <- tapply(age_reserv$age, age_reserv$reserv_f, length)
sp <- sqrt(((n_groupes[1] - 1) * s[1] + (n_groupes[2] - 1) * s[2]) /
             (sum(n_groupes) - 2))
unname((m["Oui"] - m["Non"]) / sp)

chisq.test(table(tourisme$classe_age, tourisme$reserv_f))

mod_logit <- glm(reservation ~ age10, data = tourisme, family = binomial)
summary(mod_logit)
exp(coef(mod_logit)["age10"])
exp(confint.default(mod_logit)["age10", ])
exp(coef(mod_logit)["age10"] / 10)

# R² de McFadden, rapport de vraisemblance et AUC
1 - mod_logit$deviance / mod_logit$null.deviance
anova(mod_logit, test = "Chisq")
pred <- fitted(mod_logit)
obs_logit <- model.response(model.frame(mod_logit))
n1 <- sum(obs_logit == 1)
n0 <- sum(obs_logit == 0)
unname(wilcox.test(pred[obs_logit == 1], pred[obs_logit == 0])$statistic /
         (n1 * n0))

tourisme %>%
  select(reserv_f, age) %>%
  tbl_summary(by = reserv_f, label = list(age ~ "Âge"),
              statistic = all_continuous() ~ "{mean} ({sd})",
              digits = all_continuous() ~ 1) %>%
  add_p(test = all_continuous() ~ "t.test") %>% bold_p(t = 0.05)

# 3.6. Revenu et destination préférée -------------------------------------
# H0 : revenu et destination sont indépendants.
tab_dest <- table(tourisme$revenu, tourisme$destination)
tab_dest
round(100 * prop.table(tab_dest, 1), 1)
test_dest <- chisq.test(tab_dest)
test_dest
test_dest$expected
round(test_dest$stdres, 2)
cramer_v(tab_dest)

tourisme %>%
  select(revenu, destination) %>%
  tbl_cross(row = revenu, col = destination,
            label = list(revenu ~ "Catégorie de revenu",
                         destination ~ "Destination préférée"),
            percent = "row") %>%
  add_p() %>% bold_p(t = 0.05) %>% bold_labels() %>% italicize_levels()

tourisme %>%
  count(revenu, destination) %>%
  group_by(revenu) %>%
  mutate(pourcentage = n / sum(n) * 100,
         etiquette = paste0(sprintf("%.1f", pourcentage), " %")) %>%
  ggplot(aes(x = revenu, y = pourcentage, fill = destination)) +
  geom_col() +
  geom_text(aes(label = etiquette),
            position = position_stack(vjust = 0.5), size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("France" = "#204F78", "Europe" = "#3189C0",
                               "International" = "#83BFE5")) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100),
                     labels = function(x) paste0(x, " %")) +
  labs(x = "Catégorie de revenu",
       y = "Répartition des destinations préférées (%)", fill = NULL) +
  theme_classic() + theme(legend.position = "top")

# 3.7. Budget moyen et destination préférée -------------------------------
# H0 : le budget moyen est identique entre les destinations.
tourisme %>%
  group_by(destination) %>%
  summarise(n = sum(!is.na(budget)),
            moyenne = round(mean(budget, na.rm = TRUE), 2),
            mediane = median(budget, na.rm = TRUE),
            ecart_type = round(sd(budget, na.rm = TRUE), 2),
            .groups = "drop")

tourisme %>%
  filter(!is.na(budget)) %>%
  ggplot(aes(x = destination, y = budget, fill = destination)) +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, fill = "red") +
  labs(x = "Destination préférée", y = "Budget moyen de voyage (€)") +
  theme_minimal()

leveneTest(budget ~ destination, data = tourisme)
oneway.test(budget ~ destination, data = tourisme)
kruskal.test(budget ~ destination, data = tourisme)

# Eta carré de l'ANOVA classique (pas celui de Welch)
a <- anova(lm(budget ~ destination, data = tourisme))
a[1, 2] / (a[1, 2] + a[2, 2])

pairwise.wilcox.test(tourisme$budget, tourisme$destination,
                     p.adjust.method = "bonferroni")

round(with(tourisme, tapply(budget, list(revenu, destination),
                            mean, na.rm = TRUE)), 0)

tourisme %>%
  group_by(revenu) %>%
  summarise(p_kruskal = kruskal.test(budget ~ destination)$p.value,
            .groups = "drop")

# Comparer les modèles sur le même sous-échantillon complet.
donnees_modeles <- tourisme %>%
  filter(!is.na(budget), !is.na(revenu), !is.na(destination))
mod_rev <- lm(budget ~ revenu, data = donnees_modeles)
mod_full <- lm(budget ~ revenu + destination, data = donnees_modeles)
anova(mod_rev, mod_full)

tourisme %>%
  select(destination, budget) %>%
  tbl_summary(by = destination,
              statistic = all_continuous() ~ "{mean} ({sd})",
              missing = "no") %>%
  add_p(test = all_continuous() ~ "oneway.test")

# ============================================================================
# EXPORT DES TABLEAUX VERS WORD
# ============================================================================

tab_revenu <- tourisme %>%
  select(revenu, age, nb_voyages, budget, destination) %>%
  tbl_summary(
    by = revenu,
    type = list(nb_voyages ~ "continuous"),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_n() %>%
  add_overall(last = TRUE) %>%
  add_p(test = list(all_continuous() ~ "kruskal.test",
                    all_categorical() ~ "chisq.test")) %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  modify_spanning_header(all_stat_cols(stat_0 = FALSE) ~
                           "**Catégorie de revenu**") %>%
  modify_caption("**Profil des individus selon la catégorie de revenu**")

tab_revenu

tab_revenu %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "profil_revenu.docx")

# FIN DU SCRIPT
