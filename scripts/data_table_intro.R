# data_table_intro.R
# Survol des packages data.table et tidyverse
# Par G Loignon
# guillaume.loignon@umontreal.ca
# Février 2019

<<<<<<< HEAD
=======

>>>>>>> d3f13debe5cc365df3bfff24883375e7171626db
#---- Bon à savoir ----

# Pour expérimenter avec vos propres fichiers:
#   Le tutoriel sur l'ouverture de fichiers ne provenant pas de R (SPSS, SAS, STATA, excel, etc.) :
#     https://www.datacamp.com/community/tutorials/r-data-import-tutorial
#   et pour les vastes fichiers de données, voir :
#     https://www.datacamp.com/community/tutorials/importing-data-r-part-two
#

# Rappels: * data.table est un type de structure de données qui modifie data frame
#          * le tidyverse est une collection de packages
#          * data.table et tidyverse sont des "paradigmes" en R, mais ils sont compatibles!
#
# Feuille de triche data.table: https://s3.amazonaws.com/assets.datacamp.com/blog_assets/datatable_Cheat_Sheet_R.pdf

library("data.table")
library(tidyverse) #nous allons faire quelques comparaisons de syntaxe
library(tictoc) #pour ceux et celles qui aiment chronométrer la performance de leur code

#---- convertir ses données en data.table ----
load("data/faketucky.rda")
is.data.table(faketucky) #vérification: FALSE
is.data.frame(faketucky) #vérification: TRUE!

  #1ère méthode: copy() et setDT()  plus rapide
dt.faketucky <- copy(faketucky)
setDT(dt.faketucky)

  #2e méthode: mon.dt <- data.table(mon.df) moins rapide
dt.faketucky <- data.table(faketucky)

  # setDT() est plus rapide, mais il ne faut pas oublier de faire  
  # une copie explicitement avec copy()
  # Faire ceci peut entrainer des résultats innatendus:
  
mon.df <- mtcars        # mauvaise
mon.dt <- setDT(mon.df) #   méthode
is.data.table(mon.df)   #     ne pas faire!
rm(mon.dt) #plus besoin
rm(mon.df) #plus besoin

#---- Sélectionner des données avec data.table ou le tidyverse ----
View(dt.faketucky)

#   Consigne: faites un sous-ensemble d'élèves décrocheurs (dropout == 1) pour lesquels
#             nous avons une note en maths (scale_score_6_maths n'est pas un NA)

# Nous allons tester trois méthodes.

  #méthode 1: data frame et R de base 
df.decrocheurs <- faketucky[faketucky$hs_diploma == 0 & !is.na(faketucky$scale_score_6_math) , ] #n'oubliez pas la virgule à la fin!

  #méthode 2: data.table
dt.decrocheurs <- dt.faketucky[hs_diploma == 0 & !is.na(scale_score_6_math)]

  #méthode 3: tidyverse
tidy.decrocheurs <- faketucky %>% filter( hs_diploma == 0 & !is.na(scale_score_6_math) )

  #et pourquoi pas: tidyverse avec un objet data.table
tidy.decrocheurs <- dt.faketucky %>% filter( hs_diploma == 0 & !is.na(scale_score_6_math) )

  #autre syntaxe pour la méthode tidyverse
dt.faketucky %>% filter( hs_diploma == 0 & !is.na(scale_score_6_math) ) -> tidy.decrocheurs


#   Consigne: je veux envoyer une bourse de félicitations aux élèves de milieu défavorisés qui ont gradué dans les 
#             délais prévus. Créez une nouvelle structure de données avec ces élèves en sélectionnant seulement
#             les colonnes sid et first_hs_urbanicity
#             On opérationalise "pauvre" comme ayant déjà été inscrtit à un programme de diners
#             subventionnés (frpl_ever_in_hs == 1) et les délais normaux sont indiqués par ontime_grad == 1

dt.champions <- dt.faketucky[frpl_ever_in_hs == 1 & ontime_grad == 1, "first_hs_urbanicity"]

#   Question: Combien de bourse va-t-on envoyer? Quel est l'indice moyen d'urbanité de mes boursiers? 
nrow(dt.champions)
mean(dt.champions$first_hs_urbanicity, na.rm=T)

#---- Regroupements par catégorie dans data.table ----

#   Consigne: faites un "dossier" pour chaque étudiant, dans lequel on aurait les données suivantes:
#      sid, first_hs_code, scale_score_6_read, scale_score_8_math, scale_score_8_read, race_ethnicity, dropout
#   La table produite devra respecter le format une entrée = un étudiant


# C'est ici que data.table brille vraiment!
dt.dossiers <- dt.faketucky[, .(
  first_hs_code, scale_score_6_read, scale_score_8_math, scale_score_8_read, race_ethnicity, dropout), 
  by=sid] # le by indique la variable de regroupement
head(dt.dossiers)


#  Démo: on s'intéresse à l'assiduité scolaire et à l'attitude de l'établissement face aux absences, le tout
#        en contexte urbain.

dt.ecoles <- dt.faketucky[first_hs_urbanicity > 5, .(
  moy_abs = mean(pct_absent_in_hs, na.rm=T), 
  moy_excuse = mean(pct_excused_in_hs, na.rm=T),
  sd_abs = sd(pct_absent_in_hs, na.rm=T), 
  sd_excuse = sd(pct_excused_in_hs, na.rm=T),
  cor_abs_excuse = cor(pct_absent_in_hs, pct_excused_in_hs, use="pairwise.complete.obs"),
  .N
  ), by=first_hs_code][N >= 30] #la derrière paire de crochets permet de sous-sélectionner dans la même instruction

# Question: pourquoi les messages d'erreur? Comment pourrait-on les éviter?

head(dt.ecoles)

#---- Ajouter une ou plusieurs colonnes calculées ----

# Consigne: Vous avez l'intuition en regardant vos données que certaines écoles excusent trop facilement
#           les absences. Faites le modèle linéaire, puis ajoutez une colonne indiquant pour chaque école la 
#           taille l'effet entre les absences et les absences excusées.

ecart.type <- ( sd(dt.ecoles$moy_abs, na.rm=T) + sd(dt.ecoles$moy_excuse, na.rm=T) ) / 2
ecart.type #2 environ

dt.ecoles[, cohenD := (moy_abs - moy_excuse) / ecart.type] #remarquez la syntaxe sans assignation et l'opérateur :=
head(dt.ecoles) 

# Consigne: Vous voulez refaire le d de Cohen, mais sur une version standardisée (score Z) des moyennes par école.
#           Ajoutez les colonnes standardisées puis recalculez la taille d'effet
#           Pour les scores Z, nous allons utiliser scale().
#           La taille d'effet sera de 1, donc on peut simplement soustraire les moyennes standardisées.
dt.ecoles[, ':='(
 z_moy_abs = scale(moy_abs),
 z_moy_excuse = scale(moy_excuse)
)]
dt.ecoles[, z_cohenD := (z_moy_abs - z_moy_excuse)]

# Question: pourquoi faire ces opérations en deux instructions? Qu'arriverait-il si je voulais créer
#           z_CohenD en même temps que z_moy_abs et z_moy_excuse ?

head(dt.ecoles)

#---- Fusion de jeu de données avec data.table ---

# La syntaxe est la même: on utilise merge
# Exemple: je voudrais ajouter l'indice d'urbanité à mes dossiers d'école.

urbanite <- dt.faketucky[, .(
  indice_urbanite = max(first_hs_urbanicity) #le max est utile car l'indice d'urbanité pourrait avoir changé
  ), by=first_hs_code]
length(unique(dt.faketucky$first_hs_code)) #j'ai 403 écoles en tout
nrow(urbanite) #et j'ai 403 ici aussi, donc tout va bien.

dt.ecoles.v2 <- merge(dt.ecoles, urbanite) #data.table "comprend" ce que je veux faire
head(dt.ecoles.v2) #ma colonne a été fusionnée.
rm(urbanite) #plus besoin


#---- Enregistrement de mes données data.table

# On fait save() tout simplement
save(dt.ecoles.v2, file="data/dt_ecoles.rda")

#---- Bonus: dcast et melt ----

# Certains tests statistiques demandent une structure de donnée particulière (long ou large).
  # Long: une observation = une valeur pour une variable d'un individu
  # Large: une observation = toutes les valeurs de toutes les variables d'un individu

# Que faire si je passer d'un tableau "long" à un large, ou l'inverse?

# Exemple: de long à large
load("data/dt_synth2.rda")
head(dt.synth2) #c'est un tableau long
dt.cegep.large <- dcast(data = dt.synth2, 
      formula = nom + Programme + MGS + moyNoteSpec ~ Cours, 
      value.var = "noteMax",
      fun.aggregate = max)
head(dt.cegep.large) #voilà, un tableau large!

# Exemple: de large à long
dt.cegep.long <- melt(dt.cegep.large, measure.vars = c("philo101", "philo102", "philo103"),
     variable.name = "Cours", 
     value.name = "maxNote" 
)
head(dt.cegep.long)
