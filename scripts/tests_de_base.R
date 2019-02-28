# tests_de_base.R
# Démos, test du chi2, test t et anova
#
# Adapté de diverses sources par G Loignon
# guillaume.loignon@umontreal.ca
# Dernière révision: février 2019


# ---- Test du chi carré ------------------------------------------------------------------------

#voir http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence

library(MASS) #on charge le package MASS
head(survey) #coup d'oeil à nos données
tbl <- table(survey$Smoke, survey$Exer) #conversion en tableau de contingence
tbl #affichons cette merveille. Ça y est, nous sommes prêts pour le test du chi-2
chisq.test(tbl) #je reçois un message d'avertissement, dans ce cas car il y a des
                #valeurs < 5 dans mon tableau de contingence, ce qui enfreint
                #un des postulats du test du chi-2.
ctbl <- cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"]) #combinaison de colonnes
  #la fonction cbind colle par colonne
  #ici on a collé la somme des col None et Some
  #avec la col Freq pour faire un nouveau tableau
ctbl #regardons le résultat. Problème réglé! je n'ai plus de cases avec moins de 5 dedans.
chisq.test(ctbl) #ça marche, message d'erreur disparu!

#     Pearson's Chi-squared test
#data:  tbl
#X-squared = 5.4885, df = 6, p-value = 0.4828
# Il faudrait donc rejeter H0 et on pourrait affirmer que les groupes ne sont pas 
# indépendants.


#---- Test t ----------------------------------------------------------------------------------------

#voir http://www.r-tutor.com/elementary-statistics/inference-about-two-populations/population-mean-between-two-independent-samples

#Nous allons utiliser le dataset (inclu par défaut dans R) mtcars
#Regardons les premières lignes. 
data(mtcars) #facultatif, on importe le jeu de données dans notre projet
head(mtcars)  # la colonne am indique le type de ransmission
              # automatiques (am=1) ou manuelles (am=0)

#Question: Comparez l'efficience des voitures (mpg) en fonction du type de transmission.
#         La différence est-elle significative? Donnez la valeur p et l'invervale de confiance 
#         à 95%.

#1ère méthode: avec deux vecteurs

# 1) On sélectionne les voitures automatiques
est.automatique <- mtcars$am == 1
head(est.automatique) #ok j'ai un vecteur booléen
automatiques <- mtcars[est.automatique, "mpg"]
head(automatiques) #ça marche, j'ai sélectionné juste le mpg des voitures automatiques

#2) On sélectionne les voitures manuelles
manuelles <- mtcars[!est.automatique, "mpg"] #j'utilise ! pour faire la négation de mon vecteur

#3) On fait le test t
t.test(automatiques, manuelles)
#moyenne des automatiques: 24.39 mpg
#moyenne des manuelleS: 17.15 mpg
#intervale de confiance: la différence entre les deux est située entre 3.21 et 11.28 mpg

#2e méthode pour le test t: notation par formule
t.test(data=mtcars, mpg ~ am) #une seule ligne! :)
  #group 0 = manuelle; group 1 = automatique

#On remarque trois choses au passage:
#  1) L'intervale de confiance n'inclut pas la valeur 0. Ça veut dire que peu importe
#     la valeur réelle de la différence entre les moyennes, celle-ci n'est pas de 0.
#  2) La valeur p est d'environ 0.0014 ce qui indique habituellement un lien significatif.
#  3) Cela ne signifie pas que l'efficience accrue est causée par la transmission automatique!

#vérification du postulat d'équivalence des variances
  #t.test a un parametre var.equal qui permet d'indiquer si les variances sont égales 
  #mais le sont-elles?
  #nous allons utiliser un test de Bartlett pour le savoir
  #voir https://www.itl.nist.gov/div898/handbook/eda/section3/eda357.htm
bartlett.test(mtcars$mpg ~ mtcars$am) #en séparant par type de transmission
  #H0: les groupes ont une variance équivalente
  #H1: au moins un groupe a une variance différente
  #Bartlett's K-squared = 3.2259, df = 1, p-value = 0.07248
  #Donc les variances ne sont pas équivalentes.
  #Par défaut, la fonction t.test considère que les variances ne sont *pas* équivalentes.
  #Ouf, La police des postulats ne viendra pas nous arrêter!
  
  #Si on veut indiquer que la variance est équivalente:
  #   t.test(data=mes.donnees, vardep ~ varindep, var.equal = TRUE)

#  Question: les véhicules ayant un moteur en "V" (variable vs=0) sont-ils plus efficients?
#            Répondez à l'aide d'un test t après avoir vérifié l'équivalence des variances
#            avec un test de Bartlett.

bartlett.test(mtcars$mpg ~ mtcars$vs)
t.test(data=mtcars, mpg ~ vs) #par défaut, t.test considère que les variances diffèrent
t.test(data=mtcars, mpg ~ vs, var.equal = TRUE) #variance équivalente

# Et le test de Levene? Utilisez la fonction leveneTest() dans le package car
  
#---- ANOVA ------------------------------------------------------------------------------------

#L'ANOVA se fait habituellement avec la fonction aov().
#C'est mélangeant car il existe aussi une fonction anova() mais elle sert à comparer
#des modèles.

## Application de l'ANOVA à 1 et plusieurs variables indépendantes.
# Comme dans le livre de Howell, on va utiliser le dataset d'Eysenk (1994) 
# voir http://www.statsci.org/data/general/eysenck.html

eysenck <- read.delim("http://www.statsci.org/data/general/eysenck.txt")
head(eysenck)
table(eysenck$Process) #5 stratégies, 20 individus dans chaque

#Question: Les stratégies sont-elles équivalentes?
mon.anova.1 <- aov(eysenck$Words ~ eysenck$Process)
summary(mon.anova.1)
# >           Df Sum Sq Mean Sq  F value  Pr(>F)    
# > Process      4   1515   378.7    31.21  <2e-16 ***
# > Residuals   95   1153    12.1                   

#Pour plusieurs variables indépendantes, on utilise la notation en formule.
# aov(variable.dep ~ variable.indep1 + variable.indep2)
#   ou pour ajouter l'interaction entre les variables indépendantes:
# aov(variable.dep ~ variable.indep1 * variable.indep2) 

#Question: les groupes d'âges et les stratégies employées sont-ils équivalents?
#           Y a-t-il une interaction entre la stratégie et l'âge?

mon.anova.2 <- aov(eysenck$Words ~ eysenck$Process * eysenck$Age)
summary(mon.anova.2)
# >            Df Sum Sq Mean Sq  F value    Pr(>F)    
# > Process      4 1514.9   378.7   47.191   < 2e-16 ***
# > Age          1  240.3   240.3   29.936  3.98e-07 ***
# > Process:Age  4  190.3    47.6    5.928  0.000279 ***
# > Residuals   90  722.3     8.0                     
# Donc les âges ne sont pas équivalents, ni les stratégies employées.
# On observe aussi une interaction entre l'âge et la stratégie.

#Bonus: production de boites à moustache
plot(eysenck$Words ~ eysenck$Process,
     main="Comparaison de stratégies de mémorisation",
     xlab="Stratégies",
     ylab="Nb de mots retenus")

##Comparaison de deux ANOVA
#voir http://vislab-ccom.unh.edu/~schwehr/rt/25-R-lab3-ANOVA.pdf
#on va utiliser le dataset "crabs" tiré du package MASS
library(MASS) #un package à découvrir... avec plein de bonnes choses à l'intérieur :)
data(crabs)
head(crabs) #coup d'oeil aux données
help(crabs) #pour en savoir plus sur les crabes, nos amis aquatiques à pinces
table(crabs$sp, crabs$sex) #ça alors, ce design expérimental est parfaitement balancé! :)

#Pour comparer deux ANOVAs, nous allons utiliser la fonction anova()
# syntaxe: anova(modele.1, modele.2)

# Question: soit un modèle de type ANOVA utilisant l'espèce (sp) et le sexe (sex) pour
#           expliquer la taille de la carapace (BD). Y a-t-il une différence
#           entre un modèle avec et sans interaction? Si oui, lequel est le "meilleur" ?

#on produit d'abord nos ANOVAs
m2i <- aov(data=crabs, BD ~ sex * sp) #analyse de variance avec interaction
m2n <- aov(data=crabs, BD ~ sex + sp) #analyse de variance sans interaction

#lecture des résultats
summary(m2i)
summary(m2n)
coef(m2i) 
coef(m2n)

#on peut aussi tester quelques postulats
plot(m2i)
plot(m2n)
shapiro.test(m2i$residuals) #normal
shapiro.test(m2n$residuals) #normal
  #test de Bartlett sur l'égalité des variances
bartlett.test(crabs$BD ~ crabs$sex) #en séparant par la variable sex
bartlett.test(crabs$BD ~ crabs$sp)  #en séparant par la variable sp

#comparaison des modèles
compar.anova <- anova(m2i, m2n)
compar.anova
  #il y a une différence légère (p=0.03547)
  #Le modèle avec interaction est légèrement meilleur.

