# intro_ggplot.R
# 
# Très brève intro à ggplot2
#
# Par G Loignon
# guillaume.loignon@umontreal.ca
#
# Mars 2019

library("tidyverse")  # ggplot2 est déjà dedans, joie !
library("data.table")
library("ggrepel")


#load("/cloud/project/data/dt_cegep_clean.rda")
load("/cloud/project/data/dt_synth2.rda")
dt.cegep <- dt.synth2

#---- Histogramme simple et courbes de densité  ----
hist(dt.cegep$MGS,
     xlab = "MGS",
     ylab = "Fréquence",
     main = "Histogramme - Moyennes générales au secondaire") # R de base

ggplot(dt.cegep, aes(x = MGS)) +
  geom_histogram() + 
  scale_x_continuous(name = "MGS") +
  scale_y_continuous(name = "Fréquence")

# on peut en faire un objet
mon.graphe <- ggplot(dt.cegep, aes(x = MGS)) +
  geom_histogram() + 
  xlab("MGS") +
  ylab("Fréquence")

mon.graphe

# et on peut ajouter à cet objet
mon.graphe + 
  theme_bw() +
  ggtitle(label = "Moyennes générales au secondaire")

# Courbe de densité simple
ggplot(dt.cegep, aes(x=MGS)) + 
  geom_density(fill="red")

# Histogramme et courbe de densité
ggplot(dt.cegep, aes(x=MGS)) + 
  geom_histogram(aes(y=..density..), color="white", fill="lightgrey")+
  geom_density(color="red") 
  
# Comparaison de deux distributions (ici, sc pures vs sc humaines)
dt.pre.u <- dt.cegep[Programme %in% c("Sc nature", "Sc humaines")]
ggplot(dt.pre.u, aes(x=MGS, color=Programme)) +
  geom_density() +
  theme_classic()

   # note: la librairie ggthemes contient plusieurs "thèmes" pour
   #       ggplot2, ça peut être amusant d'aller y voir quand 
   #       vous aurez envie de vous lancer dans la production d'infographies
   #       spectaculaires.


#---- Nuage de points et droites de tendance ----#

ggplot(dt.cegep, aes(x = MGS, y = moyNoteSpec)) + 
  geom_point(size = 1) + 
  geom_smooth(method = "lm")

 #on voit comment le tidyverse fonctionne en synergie!
tbl.gss <- as_tibble(gss_cat) %>%
  filter(marital %in% c("Married", "Divorced"), age > 21) %>%
  drop_na() %>%
  group_by(year, marital) %>%
  summarise(tv = mean(tvhours))

ggplot(tbl.gss,
       aes(x=year,
           y=tv,
           color=marital)) +
  geom_line() + 
  geom_point() + 
  theme_light()

#---- étiquettes sur les points ----

# Espace 2D dans lequel on situe les programmes d'étude
dt.programmes <- dt.cegep[cours == "philo101", .(
  MGS = median(MGS),
  moyNoteSpec = median(moyNoteSpec),
  philo101 = median(noteMax)
), by="Programme"]

ggplot(dt.programmes, aes(x = MGS, y = moyNoteSpec)) + 
  geom_point(size = 2, color = "red") + 
  geom_text_repel(aes(label=Programme), size = 3) + 
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed")


# À VOTRE TOUR ----

# Nous allons pour ce petit TP produire une version filtrée de
# faketucky avec seulement 2 districts

load("./data/faketucky.rda")
filt.faketucky <- 
  filter(faketucky, first_dist_code %in% c(103, 106))
filt.faketucky$first_dist_code <- factor(filt.faketucky$first_dist_code)

# - Produisez un graphique de type nuage de points avec droite de tendance
#   avec: sur l'axe des x: scale_score_8_math
#         sur l'axe des y: scale_score_11_math
#         groupes: first_dist_code
#
# - Vous pouvez personnaliser avec un titre, renommer les axes, etc.
#
# Truc: si vous voulez une seule droite de tendance pour les deux groupes,
#       vous n'avez qu'à spécifier une couleur dans geom_smooth()









# Solution plus bas....


























ggplot(filt.faketucky,
       aes(x = scale_score_8_math,
           y = scale_score_11_math,
           color = first_dist_code
           )) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "black")


 