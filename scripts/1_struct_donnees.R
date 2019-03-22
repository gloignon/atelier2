# struct_donnees.R
#
# Intro aux structures de données dans R de base
#
# Par G Loignon
# guillaume.loignon@umontreal.ca
# 
# Hiver 2019

#---- Révision du premier atelier ----

# opérations mathématiques simples
1 + 1
12 / 4
(3 * 2) / 4

# mettre une valeur dans une variable
ma.variable <- 12
x <- 3
ma.variable / x

# vecteurs, concaténation et intervales
mon.vecteur <- c(1, 2, 3, 4, 5)
mon.vecteur
## Résultat: 
## > [1] 1 2 3 4 5
mon.vecteur <- 1:5
mon.vecteur  # même résultat :)
mon.autre.vecteur <- seq(10, 12) # syntaxe alternative pour 10:12

rep(x, 10) # va répéter la valeur de x 10 fois

# objets matrix
ma.matrice <- matrix(1:9, nrow=3)
ma.matrice
une.autre.matrice <- matrix()
ma.diag <- diag(x=1, nrow=3, ncol=3)  # pratique pour opérations booléennes
ma.diag

# opérations de base sur vecteur et matrice
ma.matrice * 2
ma.matrice * ma.diag
ma.matrice * mon.autre.vecteur
ma.nouvelle.matrice <- ma.matrice + mon.autre.vecteur 
ma.nouvelle.matrice & ma.diag  # opération booléenne, ça peut être pratique

# récupération d'une valeur dans une matrice ou un vecteur
mon.autre.vecteur[2]
mon.autre.vecteur[x]
ma.nouvelle.matrice[2, 3]  # rangée 2, colonne 3
ma.nouvelle.matrice[2, 2:3]
ma.nouvelle.matrice[2] # affiche la 2e valeur
                       # pourquoi pas la 2e rangée complète? c'est une particularité
                       # des structures de type matrix, qui se comportent un peu
                       # comme des vecteurs.

# À VOTRE TOUR:
#
# - Créez un vecteur mon.vecteur avec les nombres de 1 à 100
# - Créez une matrice ma.matrice de dimension 10 x 10 à partir de mon.vecteur 

# Changer une matrice en data.frame
df.test <- data.frame(ma.nouvelle.matrice)
is.data.frame(df.test)  # va afficher TRUE
df.test # il manque les noms de colonnes
colnames(df.test) <- c("colA", "colB", "colC")
df.test

# Créer un data.frame à partir de zéro
  # remarquez l'utilisation du symbole = 
df.pop <- data.frame (
  toto = 1001:1008,
  blabla = 3, 
  nom = c("Baby", "Scary", "Sporty", "Posh", "Jonathan", "Jordan", "Joey", "Danny")
)
df.pop

# Récupération de valeurs dans un data.frame
df.pop[3]  # toute la colonne
df.pop["nom"]  # plus propre par le nom de la colonne
df.pop[, "toto"]  # aussi toute une colonne, mais affichée sous forme de vecteur
df.pop$nom  #syntaxe alternative
df.pop[2, ]  # une rangée (observation, cas, répondant, etc)
df.pop[2, 3]  # une case
df.pop[2, "nom"]  # une case (remarque: R affiche aussi les niveaux de la variable)


# éliminer des éléments dans un data.frame
df.pop <- df.pop[-4, ] #élimine la 4e rangée
df.cars <- mtcars
df.cars
df.cars <- df.cars[-3] # élimine la 3e colonne
df.cars <- df.cars[-(4:9)] # élimine les col 4 < 9

  # et éliminer/sélectionner plusieurs par nom?
  # c'est compliqué avec un data.frame...
  # on utilise plutôt les librairies tidyverse et data.table pour simplifier