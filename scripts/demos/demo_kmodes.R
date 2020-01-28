#---- k-modes ----
library("data.table")  # nous allons aussi utiliser data.table pour remanier les données
library("klaR")  # pour la classification par K-Modes et K-means. Fun fun fun!

#---- k-modes ----
#install.packages("titanic")
library(titanic)
dt.titanic <- data.table(titanic_train)  # voir https://www.rdocumentation.org/packages/titanic/versions/0.1.0/topics/titanic_train
#View(dt.titanic)

cat.titanic <- dt.titanic[, .(
  id = PassengerId,
  survie = as.factor(Survived == 1),
  classe = as.factor(Pclass),
  genre = as.factor(Sex),
  majeur = as.factor(Age > 18),
  port = as.factor(Embarked),
  famille = as.factor(SibSp > 0)
)]
cat.titanic <- na.omit(cat.titanic)  # pour cet exemple, on finalise en retirant les données manquantes

kmodes.classi <-
  kmodes(cat.titanic[, -c("id", "survie")], 2, iter.max = 256)  # je fais la classification
cat.titanic$classiKMode <- as.factor(kmodes.classi$cluster)  # ajout de la classifiation aux données

mon.tablo <- table(cat.titanic$survie, cat.titanic$classiKMode)  
mon.tablo  # le taux de survie semble plus élevé dans une des catégories trouvées par K-Modes
chisq.test(mon.tablo)   # prédiction meilleure que le hasard

  # on peut utiliser les fonctionalités de filtre dans View(cat.titanic)
  # afin de voir ce que la catégorie "chanceuse" avait en commun
  
# Bidouillage: peut-on améliorer la classification en retirant des variables?
#              ou peut-être en créant des variables catégorielles avec les variables continues?