# intro_glm.R
#
# régression multivariée, régressions non-linéaires
# Par G Loignon
# guillaume.loignon@umontreal.ca
# Dernière révision: février 2019

library("data.table")
library(pscl) #pour le pseudo-R2

#---- Régression linéaire  ----

# Syntaxe de base:
# lm(data=mes.donnees, var.dep ~ var.indep.1 + var.indep.2 * var.indep.3 etc etc etc )

load("data/dt_synth2_large.rda")
# Exemple: je veux prédire la noteMax en philo 3 en fonction de deux autres cours de philosophie et du programme
modele1 <- lm( data=dt.cegep.large, philo103 ~ philo102 * philo101 )
summary(modele1) #et on affiche les résultats
plot(modele1) #sorties graphiques fréquemment employées pour diagnostiquer le modèle de régression

# Consigne: faites un modèle multivarié visant à expliquer la note en philo 101.
# Vous pouvez utiliser tout ce qui est dans le jeu de données




# Solution plus bas...
























modele2 <- lm( data=dt.cegep.large, philo101 ~ MGS * moyNoteSpec)
summary(modele2) #et on affiche les résultats

# Ça fonctionne même pour les variables catégorielles!
modele3 <- lm( data=dt.cegep.large, philo101 ~ MGS * Programme )
summary(modele3) #R a fait une régression pour chaque niveau de la variable catégorielle Programmes
plot(modele3)

#---- Prédiction avec un modèle linéaire ----
roger <- data.table ( #On invente Roger Tremblay, on nouvel étudiant
  nom="Tremblay, Roger",
  Programme="Tech. humaines",
  MGS=60)
roger #il manque des données car Roger vient juste d'arriver, il n'a pas encore fait de cours! :)

predict.lm(modele3, newdata=roger, type="response" ) #va afficher la note prédite en philo101
predict.lm(modele3, newdata=roger, type="terms" ) #va afficher les termes du modèle

# À VOTRE TOUR:
#
#   - Quelle serait la note prédite en philo 101 pour Roger s'il était plutôt inscrit au programme de
#     "Sciences informatiques et mathématiques" ?  
#
#   - Ursula, la soeur de Roger, hésitait quant à son choix de carrière. Elle a seulement fait des cours de
#     base à l'automne dernier et a obtenu 72% en philo 101. Au secondaire, elle a maintenu une
#     moyenne de 76%. Elle voudrait maintenant s'inscrire dans le programme "Tech. medicale". Va-t-elle
#     avoir de la difficulté si elle continue dans ce programme? Aidez Urusula en prédisant sa moyenne
#     en formation spécifique (colonne moyNoteSpec) à partir des données dont vous disposez. Cette
#     prédiction est-elle fiable?













# Solution plus bas...










modele4 <- lm( data=dt.cegep.large, moyNoteSpec ~ MGS + Programme + philo101 )
summary(modele4)

ursula <- data.table ( 
  nom="Tremblay, Usrula",
  Programme="Tech. medicale",
  MGS=76,
  philo101=72)

predict.lm(modele4, newdata=ursula, type="response" ) #va afficher la note prédite en philo101
predict.lm(modele4, newdata=ursula, type="terms" ) #va afficher les termes du modèle

#---- Régression logistique ----
  #Données de simulation concernant des élèves ayant suivi (ou pas) un cours de renforcement.
load("data/sim_renfo.rda")

#joli graphique, on constate que la variance diffère entre les groupes
ggplot(dt.sim.renfo, aes(x=NotePre, y=NotePost, color=condition)) +
  geom_point(size=1.5) +
  ggtitle("Scores des élèves au pré et post-test selon la condition")
  #pssst... nous allons voir ggplot2 après la pause :)

# À VOTRE TOUR:
#
#   - Décrivez le graphique que vous venez de produire (optionnel: améliorez le titre)
# 
#   - ggplot me donne un message d'avertissement dans la console, pourquoi? Que peut-on faire 
#     pour corriger la situation? (Indice: ce jeu de données NA pas d'allure!)
#
#   - La moyenne au pré-test est-elle équivalente entre les deux groupes? Utilisez un test
#     statistique pour répondre.












# Solution plus bas...













t.test(data=dt.sim.renfo, NotePost ~ condition) #on explore le lien entre la condition (renfo/témoin) 
                                            #et la NotePost obtenue par l'élève suite à l'intervention

modele.lin <-  lm(data=dt.sim.renfo, NotePost ~ NotePre * condition) #modèle linéaire
summary(modele.lin)

modele.log.1 <- glm(data=dt.sim.renfo, NotePost > 59 ~ NotePre * condition, family="binomial") #reg logistique
summary(modele.log.1)
coef(modele.log) #si on veut uniquement les coefficients


#---- Quelques tests pour évaluer rég. logistique ----

  # Voir aussi https://www.r-bloggers.com/evaluating-logistic-regression-models/

# Le modèle modele.log.1 a comme VD la note au pré-test et la condition (exp ou tém).
# Je vais faire un autre modèle qui se base uniquement sur la note au pré-test
modele.log.2 <- glm(data=dt.sim.renfo, NotePost > 59 ~ NotePre, family="binomial") #modele alternatif
anova(modele.log.1, modele.log.2, test="Chisq") #On compare les deux modèles

# Calcul du pseudo R2 (c'est la colonne McFadden)
  #voir https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/
pR2(modele.log.1)
pR2(modele.log.2)

# Attention avec la rég logistique: l'effet des VI ne s'exprime pas de manière constante (linéaire) 
# sur la variable dépendante. Dans cet exemple, la condition semble peu contribuer au score en
# en post-test quand on regarde les pseudo-R2, mais un examen plus approfondi pourrait montrer autre
# chose. 
# J'ai fait une démo pour illustrer ce principe, elle est dans ./script/demo/demo_pred.logit.R

# Pour les "best practices" en régression logistique, voir ce livre:
# https://us.sagepub.com/en-us/nam/best-practices-in-logistic-regression/book239211
#  (On a accès aux chapitres via la bibliothèque de l'U de M)