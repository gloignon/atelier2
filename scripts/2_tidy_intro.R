# tidy_intro.R
# 
# Très brève intro à la syntaxe du tidyverse
#
# Par G Loignon
# guillaume.loignon@umontreal.ca
#
# Hiver 2019

library("tidyverse")  # va charger tous les packages de la collection tidy

#---- créer un tibble ----
# créons nous d'abord quelques données 
df.fleurs <- iris  # données déjà incluses dans R de base
df.fleurs
class(df.fleurs)  # class me donne le(s) type(s) de structure de données

tib.fleurs <- as_tibble(df.fleurs)
class(tib.fleurs) 

#---- filtrer ses données ----
  #voir aussi http://www.sthda.com/english/wiki/tibble-data-format-in-r-best-and-modern-way-to-work-with-your-data

  # syntaxe générique
  # mon.data.set %>% filter(liste de conditions)

# scénario: je veux les fleurs de l'espèce versicolor ayant Petal.Length > 3
#           et Petal.Width > 1.5
tib.gros.versi <- tib.fleurs %>% 
  filter(Species == "versicolor", Petal.Length > 3, Petal.Width > 1.5)

# et maintenant je veux les trier en ordre de Sepal.Length
tib.gros.versi <- tib.gros.versi %>% arrange(Sepal.Length)
tib.gros.versi

#---- ordonner les données ----

# je veux les petits spécimens de l'espèce setosa ET je veux les trier
# en ordre de Sepal.Length
tib.petit.setosa <- tib.fleurs %>% 
  filter(Species == "setosa", Petal.Length < 3, Petal.Width < 1.5) %>%
  arrange(Sepal.Length) #arrange ordonne les données
tib.petit.setosa

# même exemple + je veux exclure la colonne indiquant l'espèce et je
# veux un ordre inverse (décroissant)
tib.petit.setosa <- tib.fleurs %>% 
  filter(Species == "setosa", Petal.Length < 3, Petal.Width < 1.5) %>%
  arrange(desc(Sepal.Length)) %>%
  select(-Species)
tib.petit.setosa

# syntaxe alternative ♡( ◡‿◡ )
tib.fleurs %>% 
  filter(Species == "setosa", Petal.Length < 3, Petal.Width < 1.5) %>%
  arrange(desc(Sepal.Length)) %>%
  select(-Species) -> tib.petit.setosa 
tib.petit.setosa

#---- regrouper les données ----
  # nous allons utiliser group_by() et summarise()

par.espece <- iris %>% group_by(Species)
head(par.espece)  # en apparence, ça ne change rien?
class(par.espece) # j'ai cependant ajouté la propriété "grouped_df"
group_vars(par.espece)  # on confirme que le regroupement est par Species

# C'est avec summarise() que la magie va opérer:
par.espece %>% summarise (
  moy.long = mean(Petal.Length),
  moy.larg = mean(Petal.Width),
  sd.long = sd(Petal.Length),
  sd.larg = sd(Petal.Width),
  compte = n()
)

#---- gestion des valeurs manquantes (NA) ----

# Pour cet exemple, nous allons provoquer des NA:
par.espece[4, 1] <- NA  # 4e rangée, 1ère colonne
par.espece[3, 4] <- NA  # 3e rangée, 4ère colonne
par.espece[101, ] <- NA  # la 101e observation

#< NA # 3e rangée, 4ère colonne

# On essaie à nouveau le code précédent:
par.espece %>% summarise (
  moy.long = mean(Petal.Length),
  moy.larg = mean(Petal.Width),
  sd.long = sd(Petal.Length),
  sd.larg = sd(Petal.Width),
  compte = n()
)

# Zut! Pourquoi ça marche pas? (•ิ_•ิ)?

# Car R ne peut plus calculer les moyennes puisqu'on ne lui a pas
# dit comment traiter les valeurs manquantes (NA).

# 1ère méthode: retirer ponctuellement les NA avec na.rm = T
par.espece %>% summarise (
  moy.long = mean(Petal.Length, na.rm = T),
  moy.larg = mean(Petal.Width, na.rm = T),
  sd.long = sd(Petal.Length, na.rm = T),
  sd.larg = sd(Petal.Width, na.rm = T),
  compte = n()
)

# 2e méthode: éliminer les observations non-complètes
par.espece %>%
  drop_na() %>%
  summarise (
    moy.long = mean(Petal.Length, na.rm = T),
    moy.larg = mean(Petal.Width, na.rm = T),
    sd.long = sd(Petal.Length, na.rm = T),
    sd.larg = sd(Petal.Width, na.rm = T),
    compte = n()
  )

#---- Exemple synthèse ----

  # D'abord une petite mise en scène qui ne fait pas partie
  # de l'exemple en tant que tel...
  iris.avec.na <- as_tibble(iris)
  iris.avec.na[c(1:5, 50:55, 100:105), 1:4] <- NA

# Combinons maintenant ce que nous avons appris! 	＼(＾▽＾)／
iris.avec.na %>%
  filter(Species != "versicolor") %>%
  drop_na() %>%
  group_by(Species) %>%
  summarise (
    moy.long.sepal = mean(Sepal.Length),
    moy.larg.sepal = mean(Sepal.Width),
    record.long.petal = max(Petal.Length),
    record.larg.petal = max(Petal.Width),
    compte = n()
  ) %>% 
  arrange (moy.long.sepal) -> mon.tableau.synthese

mon.tableau.synthese


## À VOTRE TOUR!  ----
# 
# - En vous inspirant de l'exemple synthèse, et en une seule commande:
#     1. Récupérez les données du tableau gss_cat (inclus dans la librairie tidyverse).
#     2. Éliminez les répondants ayant des valeurs manquantes
#     3. Sélectionnez les individus divorcés ou séparés (colonne marital, valeurs 
#        Divorced ou Separated).
#     4. Regroupez les par affiliation politique (colonne partyid)
#     5. Produisez un tableau synthèse montrant pour chaque groupe le nombre médian 
#        d'heures d'écoute de télévision, l'âge médian et le nombre d'individus.
#     6. Triez les groupes par le nombre médian d'heures d'écoute de télévision.
#     7. Écrivez ces données dans un nouvel objet tibble, tbl.resume
#
#  Trucs: 
#   Vous pouvez en apprendre plus sur gss_cat en faisant ?gss_cat dans la
#   console.
#  Vous allez avoir besoin de drop_na() pour éliminer les lignes ayant des NA
#  La solution se trouve plus bas dans ce script





































#---- Solution -----

gss_cat %>%
  drop_na() %>%
  filter(marital != "Divorced", marital != "Separated") %>%
  group_by(partyid) %>%
  summarise (
    med.ecoute.tele = median(tvhours),
    med.age = median(age),
    compte = n()
  ) %>%
  arrange (med.ecoute.tele) -> tbl.resume

tbl.resume