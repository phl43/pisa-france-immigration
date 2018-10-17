library(tidyverse)
library(haven)
library(devtools)
library(RColorBrewer)

# il faut la version du package intsvy qui est sur GitHub pour avoir
# les fonctions nécessaires à l'analyse des résultats de 2015
if (require(intsvy) == FALSE) {
  install_github("eldafani/intsvy")
  library(intsvy)
}

# source : http://www.oecd.org/pisa/data/2015database/
pisa2015 <- read_spss("CY6_MS_CMB_STU_QQQ.sav")

# partitionne les données pour ne garder que les participants à l'enquête
# en France en fonction du lieu de naissance de leurs parents

pisa2015_fr <- pisa2015 %>%
  filter(CNTRYID == 250)

fds <- pisa2015_fr %>%
  filter(ST019AQ01T == 1 & ST019BQ01T == 1 & ST019CQ01T == 1)

descendants1 <- pisa2015_fr %>%
  filter(ST019AQ01T == 1 & ((ST019BQ01T == 2 & ST019CQ01T == 1) | (ST019BQ01T == 1 & ST019CQ01T == 2)))

descendants2 <- pisa2015_fr %>%
  filter(ST019AQ01T == 1 & (ST019BQ01T == 2 & ST019CQ01T == 2))

# définition des niveaux de compétence par sujet : https://nces.ed.gov/surveys/pisa/pisa2015/pisa2015highlights_8f.asp
niveaux_compétence_maths <- c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30)
niveaux_compétence_lecture <- c(262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32)
niveaux_compétence_science <- c(260.54, 334.94, 409.54, 484.14, 558.73, 633.33, 707.93)

# calcule le pourcentage de participants à chaque niveau de compétence dans chaque catégorie

fds_maths <- pisa2015.ben.pv(pvlabel = "MATH", cutoff = niveaux_compétence_maths, data = fds) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, type = "Deux parents nés en France")
desc1_maths <- pisa2015.ben.pv(pvlabel = "MATH", cutoff = niveaux_compétence_maths, data = descendants1) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, type = "Un seul parent né à l'étranger")
desc2_maths <- pisa2015.ben.pv(pvlabel = "MATH", cutoff = niveaux_compétence_maths, data = descendants2) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, type = "Deux parents nés à l'étranger")
fds_lecture <- pisa2015.ben.pv(pvlabel = "READ", cutoff = niveaux_compétence_lecture, data = fds) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, type = "Deux parents nés en France")
desc1_lecture <- pisa2015.ben.pv(pvlabel = "READ", cutoff = niveaux_compétence_lecture, data = descendants1) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, type = "Un seul parent né à l'étranger")
desc2_lecture <- pisa2015.ben.pv(pvlabel = "READ", cutoff = niveaux_compétence_lecture, data = descendants2) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, type = "Deux parents nés à l'étranger")
fds_science <- pisa2015.ben.pv(pvlabel = "SCIE", cutoff = niveaux_compétence_science, data = fds) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, type = "Deux parents nés en France")
desc1_science <- pisa2015.ben.pv(pvlabel = "SCIE", cutoff = niveaux_compétence_science, data = descendants1) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, type = "Un seul parent né à l'étranger")
desc2_science <- pisa2015.ben.pv(pvlabel = "SCIE", cutoff = niveaux_compétence_science, data = descendants2) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, type = "Deux parents nés à l'étranger")

# prépare les données pour les graphiques

maths <- rbind(fds_maths, desc1_maths, desc2_maths) %>%
  mutate(niveau = recode_factor(Benchmarks,
                             "<= 357.77" = "Niveau 0",
                             "(357.77, 420.07]" = "Niveau 1",
                             "(420.07, 482.38]" = "Niveau 2",
                             "(482.38, 544.68]" = "Niveau 3",
                             "(544.68, 606.99]" = "Niveau 4",
                             "(606.99, 669.3]" = "Niveau 5",
                             "> 669.3" = "Niveau 6")) %>%
  mutate(type = factor(type,
                       levels = c("Deux parents nés en France",
                                  "Un seul parent né à l'étranger",
                                  "Deux parents nés à l'étranger"))) %>%
  rename(pourcentage = Percentage) %>%
  select(niveau, pourcentage, position, type)

lecture <- rbind(fds_lecture, desc1_lecture, desc2_lecture) %>%
  mutate(niveau = recode_factor(Benchmarks,
                                "<= 262.04" = "Niveau 0",
                                "(262.04, 334.75]" = "Niveau 1b",
                                "(334.75, 407.47]" = "Niveau 1a",
                                "(407.47, 480.18]" = "Niveau 2",
                                "(480.18, 552.89]" = "Niveau 3",
                                "(552.89, 625.61]" = "Niveau 4",
                                "(625.61, 698.32]" = "Niveau 5",
                                "> 698.32" = "Niveau 6")) %>%
  mutate(type = factor(type,
                       levels = c("Deux parents nés en France",
                                  "Un seul parent né à l'étranger",
                                  "Deux parents nés à l'étranger"))) %>%
  rename(pourcentage = Percentage) %>%
  select(niveau, pourcentage, position, type)

science <- rbind(fds_science, desc1_science, desc2_science) %>%
  mutate(niveau = recode_factor(Benchmarks,
                                "<= 260.54" = "Niveau 0",
                                "(260.54, 334.94]" = "Niveau 1b",
                                "(334.94, 409.54]" = "Niveau 1a",
                                "(409.54, 484.14]" = "Niveau 2",
                                "(484.14, 558.73]" = "Niveau 3",
                                "(558.73, 633.33]" = "Niveau 4",
                                "(633.33, 707.93]" = "Niveau 5",
                                "> 707.93" = "Niveau 6")) %>%
  mutate(type = factor(type,
                       levels = c("Deux parents nés en France",
                                  "Un seul parent né à l'étranger",
                                  "Deux parents nés à l'étranger"))) %>%
  rename(pourcentage = Percentage) %>%
  select(niveau, pourcentage, position, type)

# crée les graphiques

ggplot(maths, aes(x = type, y = pourcentage, fill = forcats::fct_rev(niveau))) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = type, y = position, label = paste0(pourcentage, "%")), size = 2) +
  ggtitle("Répartition dans les différents niveaux de compétence de PISA en mathématiques\nselon le lieu de naissance des parents (2015)") +
  labs(x = "", y = "", fill = "Niveau de compétence") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(lecture, aes(x = type, y = pourcentage, fill = forcats::fct_rev(niveau))) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = type, y = position, label = paste0(pourcentage, "%")), size = 2) +
  ggtitle("Répartition dans les différents niveaux de compétence de PISA en lecture\nselon le lieu de naissance des parents (2015)") +
  labs(x = "", y = "", fill = "Niveau de compétence") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(science, aes(x = type, y = pourcentage, fill = forcats::fct_rev(niveau))) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = type, y = position, label = paste0(pourcentage, "%")), size = 2) +
  ggtitle("Répartition dans les différents niveaux de compétence de PISA en science\nselon le lieu de naissance des parents (2015)") +
  labs(x = "", y = "", fill = "Niveau de compétence") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(plot.title = element_text(hjust = 0.5))
