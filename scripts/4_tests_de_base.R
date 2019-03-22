# tests_de_base.R
# Démos, test du chi2, test t et anova
#
# Adapté de diverses sources par G Loignon
# guillaume.loignon@umontreal.ca
# Dernière révision: février 2019

library(MASS) #on charge le package MASS, un petit bijou de fonctions et de données d'exemple!

# ---- Test du chi carré ------------------------------------------------------------------------

# voir http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence

data(survey)  # charge le dataset "survey", qui est inclus dans la librairie MASS
head(survey)  # coup d'oeil à nos données

tbl <- table(survey$Smoke, survey$Exer == "Freq",
             dnn = c("Fumeur", "ExerciceFreq")) #conversion en tableau de contingence
tbl  # affichons cette merveille.

chisq.test(tbl) # le test du chi-2 en tant que tel

# Autre exemple tiré de http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
housetasks <- read.delim("http://www.sthda.com/sthda/RDoc/data/housetasks.txt", row.names = 1)
housetasks #valeurs observées
mon.test.du.chi2 <- chisq.test(housetasks)
mon.test.du.chi2
val.attendues <- round(mon.test.du.chi2$expected)  # valeurs attendues, arrondies


#---- Test t ----------------------------------------------------------------------------------------

#voir http://www.r-tutor.com/elementary-statistics/inference-about-two-populations/population-mean-between-two-independent-samples

#Nous allons utiliser le dataset (inclu par défaut dans R) mtcars
#Regardons les premières lignes. 
data(mtcars) #facultatif, on importe le jeu de données dans notre projet
head(mtcars)  # la colonne am indique le type de ransmission
              # automatiques (am=1) ou manuelles (am=0)

# Scénario: On souhaite comparer l'efficience des voitures (mpg) en fonction du type de transmission.

#1ère méthode: avec deux vecteurs, test sur échantillons indépendants

# 1) On sélectionne le mpg des voitures automatiques
automatiques <- mtcars[mtcars$am == 1, "mpg"]
head(automatiques) #ça marche, j'ai sélectionné juste le mpg des voitures automatiques

# 2) On sélectionne le mpg des voitures manuelles
manuelles <- mtcars[mtcars$am != 1, "mpg"] #j'utilise ! pour faire la négation de mon vecteur

# 3) On fait le test t
t.test(automatiques, manuelles)
# moyenne des automatiques: 24.39 mpg
# moyenne des manuelleS: 17.15 mpg
# intervale de confiance: la différence entre les deux est située entre 3.21 et 11.28 mpg

# 2e méthode: notation par formule
t.test(data=mtcars, mpg ~ am) #une seule ligne! :)
  #group 0 = manuelle; group 1 = automatique

# Pour le test de Bartlett: bartlett.test(mtcars$mpg ~ mtcars$am) dans le package stats
# Et le test de Levene? Utilisez la fonction leveneTest() dans le package car
  
#---- ANOVA ------------------------------------------------------------------------------------

# En R de base, l'ANOVA se fait habituellement avec la fonction aov().

# Application de l'ANOVA à 1 et plusieurs variables indépendantes.
# Comme dans le livre de Howell, on va utiliser le dataset d'Eysenk (1994) 
# voir http://www.statsci.org/data/general/eysenck.html

eysenck <- read.delim("http://www.statsci.org/data/general/eysenck.txt") # lu depuis les internettes
head(eysenck)
table(eysenck$Process) #5 stratégies, 20 individus dans chaque

#Question: Les stratégies sont-elles équivalentes?
mon.anova.1 <- aov(eysenck$Words ~ eysenck$Process)
  # le tilde permet de distinguer la var dépendante et indépendante comme ceci:
  # varDépendante ~ varIndépendante (la var dépendante toujours à gauche)

summary(mon.anova.1)
# >           Df Sum Sq Mean Sq  F value  Pr(>F)    
# > Process      4   1515   378.7    31.21  <2e-16 ***
# > Residuals   95   1153    12.1                   

# Question: les groupes d'âges et les stratégies employées sont-ils équivalents?
#           Y a-t-il une interaction entre la stratégie et l'âge?

mon.anova.2 <- aov(eysenck$Words ~ eysenck$Process + eysenck$Age)
summary(mon.anova.2)

# Consigne: décrivez les résultats.

# Bonus: production de boites à moustache
plot(eysenck$Words ~ eysenck$Process,
     main="Comparaison de stratégies de mémorisation",
     xlab="Stratégies",
     ylab="Nb de mots retenus")

#---- Vérification et comparaison de deux ANOVA ----
#voir http://vislab-ccom.unh.edu/~schwehr/rt/25-R-lab3-ANOVA.pdf
#on va utiliser le dataset "crabs" tiré du package MASS

data(crabs)
head(crabs) #coup d'oeil aux données
help(crabs) #pour en savoir plus sur les crabes, nos amis aquatiques à pinces ミ[°°]ミ
table(crabs$sp, crabs$sex) #ça alors, ce design expérimental est parfaitement balancé! :)

# Pour comparer deux ANOVAs, nous allons utiliser la fonction anova()
#   syntaxe: anova(modele.1, modele.2)

# Question: soit un modèle de type ANOVA utilisant l'espèce (sp) et le sexe (sex) pour
#           expliquer la taille de la carapace (BD). Y a-t-il une différence
#           entre un modèle avec et sans interaction? Si oui, lequel est le "meilleur" ?

# on produit d'abord nos ANOVAs
anova1 <- aov(data = crabs, BD ~ sex + CL)
anova2 <- aov(data = crabs, BD ~ sex + sp) 

#lecture des résultats
summary(anova1)
summary(anova2)
coef(anova1) #montre seulement les coefficients
coef(anova2)

#on peut aussi tester quelques postulats
  #si vous avez des questions sur les tests, contactez Sébastien
  #au sebastien.beland@umontreal.ca
plot(anova1) #va produire des graphiques fréquemment employés pour diagnostiquer le modèle
plot(anova2)
shapiro.test(anova1$residuals) 
shapiro.test(anova2$residuals) 

#comparaison des modèles
compar.anova <- anova(anova1, anova2)
compar.anova

# À VOTRE TOUR ----:
#   Pour le dataset gss_cat (inclus dans R par défaut),
#   - produisez avec aov() deux modèles dont la variable dépendante sera tvhours,
#   - comparez les deux modèles avec anova()
#
#   - Optionnel: Utilisez une approche exploratoire pour améliorer le modèle.
#                Comparez les résultats avec vos camarades de classe.