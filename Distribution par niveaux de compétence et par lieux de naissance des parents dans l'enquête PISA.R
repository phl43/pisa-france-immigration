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

# participants à l'enquête en France
pisa2015_fr <- pisa2015 %>%
  filter(CNTRYID == 250)

# participants nés en France de parents nés en France
fds <- pisa2015_fr %>%
  filter(ST019AQ01T == 1 & ST019BQ01T == 1 & ST019CQ01T == 1)

# participants nés à l'étranger de parents nés à l'étranger
immigrés <- pisa2015_fr %>%
  filter(ST019AQ01T == 2 & ST019BQ01T == 2 & ST019CQ01T == 2)

# participants nés en France ayant un parent né à l'étranger
descendants1 <- pisa2015_fr %>%
  filter(ST019AQ01T == 1 & ((ST019BQ01T == 2 & ST019CQ01T == 1) | (ST019BQ01T == 1 & ST019CQ01T == 2)))

# participants nés en France dont les deux parents sont nés à l'étranger
descendants2 <- pisa2015_fr %>%
  filter(ST019AQ01T == 1 & (ST019BQ01T == 2 & ST019CQ01T == 2))

# définition des niveaux de compétence par sujet : https://nces.ed.gov/surveys/pisa/pisa2015/pisa2015highlights_8f.asp
niveaux_compétence_maths <- c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30)
niveaux_compétence_lecture <- c(262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32)
niveaux_compétence_science <- c(260.54, 334.94, 409.54, 484.14, 558.73, 633.33, 707.93)

# calcule le pourcentage de participants à chaque niveau de compétence dans chaque catégorie

fds_maths <- pisa2015.ben.pv(pvlabel = "MATH", cutoff = niveaux_compétence_maths, data = fds) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, catégorie = "Né en France avec deux\nparents nés en France")
immigrés_maths <- pisa2015.ben.pv(pvlabel = "MATH", cutoff = niveaux_compétence_maths, data = immigrés) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, catégorie = "Né à l'étranger avec deux\nparents nés à l'étranger")
desc1_maths <- pisa2015.ben.pv(pvlabel = "MATH", cutoff = niveaux_compétence_maths, data = descendants1) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, catégorie = "Né en France avec un seul\nparent né à l'étranger")
desc2_maths <- pisa2015.ben.pv(pvlabel = "MATH", cutoff = niveaux_compétence_maths, data = descendants2) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, catégorie = "Né en France avec deux parents\nnés à l'étranger")
fds_lecture <- pisa2015.ben.pv(pvlabel = "READ", cutoff = niveaux_compétence_lecture, data = fds) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, catégorie = "Né en France avec deux\nparents nés en France")
immigrés_lecture <- pisa2015.ben.pv(pvlabel = "READ", cutoff = niveaux_compétence_lecture, data = immigrés) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, catégorie = "Né à l'étranger avec deux\nparents nés à l'étranger")
desc1_lecture <- pisa2015.ben.pv(pvlabel = "READ", cutoff = niveaux_compétence_lecture, data = descendants1) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, catégorie = "Né en France avec un seul\nparent né à l'étranger")
desc2_lecture <- pisa2015.ben.pv(pvlabel = "READ", cutoff = niveaux_compétence_lecture, data = descendants2) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, catégorie = "Né en France avec deux parents\nnés à l'étranger")
fds_science <- pisa2015.ben.pv(pvlabel = "SCIE", cutoff = niveaux_compétence_science, data = fds) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, catégorie = "Né en France avec deux\nparents nés en France")
immigrés_science <- pisa2015.ben.pv(pvlabel = "SCIE", cutoff = niveaux_compétence_science, data = immigrés) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, catégorie = "Né à l'étranger avec deux\nparents nés à l'étranger")
desc1_science <- pisa2015.ben.pv(pvlabel = "SCIE", cutoff = niveaux_compétence_science, data = descendants1) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, catégorie = "Né en France avec un seul\nparent né à l'étranger")
desc2_science <- pisa2015.ben.pv(pvlabel = "SCIE", cutoff = niveaux_compétence_science, data = descendants2) %>%
  mutate(position = cumsum(Percentage) - 0.5 * Percentage, catégorie = "Né en France avec deux parents\nnés à l'étranger")

# prépare les données pour les graphiques

niveaux <- c("Né en France avec deux\nparents nés en France",
            "Né à l'étranger avec deux\nparents nés à l'étranger",
            "Né en France avec un seul\nparent né à l'étranger",
            "Né en France avec deux parents\nnés à l'étranger")

maths <- rbind(fds_maths, immigrés_maths, desc1_maths, desc2_maths) %>%
  mutate(niveau = recode_factor(Benchmarks,
                             "<= 357.77" = "Niveau 0",
                             "(357.77, 420.07]" = "Niveau 1",
                             "(420.07, 482.38]" = "Niveau 2",
                             "(482.38, 544.68]" = "Niveau 3",
                             "(544.68, 606.99]" = "Niveau 4",
                             "(606.99, 669.3]" = "Niveau 5",
                             "> 669.3" = "Niveau 6")) %>%
  mutate(catégorie = factor(catégorie,
                       levels = niveaux)) %>%
  rename(pourcentage = Percentage) %>%
  select(niveau, pourcentage, position, catégorie)

lecture <- rbind(fds_lecture, immigrés_lecture, desc1_lecture, desc2_lecture) %>%
  mutate(niveau = recode_factor(Benchmarks,
                                "<= 262.04" = "Niveau 0",
                                "(262.04, 334.75]" = "Niveau 1b",
                                "(334.75, 407.47]" = "Niveau 1a",
                                "(407.47, 480.18]" = "Niveau 2",
                                "(480.18, 552.89]" = "Niveau 3",
                                "(552.89, 625.61]" = "Niveau 4",
                                "(625.61, 698.32]" = "Niveau 5",
                                "> 698.32" = "Niveau 6")) %>%
  mutate(catégorie = factor(catégorie,
                       levels = niveaux)) %>%
  rename(pourcentage = Percentage) %>%
  select(niveau, pourcentage, position, catégorie)

science <- rbind(fds_science, immigrés_science, desc1_science, desc2_science) %>%
  mutate(niveau = recode_factor(Benchmarks,
                                "<= 260.54" = "Niveau 0",
                                "(260.54, 334.94]" = "Niveau 1b",
                                "(334.94, 409.54]" = "Niveau 1a",
                                "(409.54, 484.14]" = "Niveau 2",
                                "(484.14, 558.73]" = "Niveau 3",
                                "(558.73, 633.33]" = "Niveau 4",
                                "(633.33, 707.93]" = "Niveau 5",
                                "> 707.93" = "Niveau 6")) %>%
  mutate(catégorie = factor(catégorie,
                       levels = niveaux)) %>%
  rename(pourcentage = Percentage) %>%
  select(niveau, pourcentage, position, catégorie)

# crée les graphiques

ggplot(maths, aes(x = catégorie, y = pourcentage, fill = forcats::fct_rev(niveau))) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = catégorie, y = position, label = paste0(pourcentage, "%")), size = 2) +
  ggtitle("Répartition dans les différents niveaux de compétence de PISA en mathématiques\nselon le lieu de naissance des participants et de leurs parents (2015)") +
  labs(x = "", y = "", fill = "Niveau de compétence") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(lecture, aes(x = catégorie, y = pourcentage, fill = forcats::fct_rev(niveau))) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = catégorie, y = position, label = paste0(pourcentage, "%")), size = 2) +
  ggtitle("Répartition dans les différents niveaux de compétence de PISA en lecture\nselon le lieu de naissance des participants et de leurs parents (2015)") +
  labs(x = "", y = "", fill = "Niveau de compétence") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(science, aes(x = catégorie, y = pourcentage, fill = forcats::fct_rev(niveau))) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = catégorie, y = position, label = paste0(pourcentage, "%")), size = 2) +
  ggtitle("Répartition dans les différents niveaux de compétence de PISA en science\nselon le lieu de naissance des participants et de leurs parents (2015)") +
  labs(x = "", y = "", fill = "Niveau de compétence") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(plot.title = element_text(hjust = 0.5))
