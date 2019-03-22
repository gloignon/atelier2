# Démo adaptée depuis Lantz (2013), Machine Learning with R. 
#
# Montre une méthode (rudimentaire) de machine learning, la classification par plus proche voisin
# Voir http://griemetic.ca/fr/2018/12/05/demo-machine-learning-avec-r-classification-par-recherche-des-plus-proches-voisins/

library(class)
library(gmodels)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

wbcd <- read.csv("https://resources.oreilly.com/examples/9781784393908/raw/ac9fe41596dd42fc3877cfa8ed410dd346c43548/Machine%20Learning%20with%20R,%20Second%20Edition_Code/Chapter%2003/wisc_bc_data.csv")
head(wbcd)

rownames(wbcd) <- wbcd$id #on prend la colonne id comme noms de rangée
wbcd$id <- NULL #la colonne id peut être enlevée, elle ne servira plus
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize)) #normalisation

wbcd_train <- wbcd_n[1:469, ] #données de training
wbcd_test <- wbcd_n[470:569, ] #données pour vérifier le modèle
wbcd_train_labels <- wbcd[1:469, 1] #diagnostics des données de training
wbcd_test_labels <- wbcd[470:569, 1] #diagnostics des données de test

#---- essais de classification ----
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=12)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_z <- as.data.frame(scale(wbcd[-1])) #conversion de tout en scores Z (sauf la col 1 qui est le diagnostic)
wbcd_train_z <- wbcd_z[1:469, ]
wbcd_test_z <- wbcd_z[470:569, ]
wbcd_test_pred <- knn(train = wbcd_train_z, test = wbcd_test_z, cl = wbcd_train_labels, k=12) #1er essai de classification
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=20) #2e essai avec k=20
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train_z, test = wbcd_test_z, cl = wbcd_train_labels, k=23) #3e essai avec Z
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=20, l=17) #4e essai avec l=17
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)