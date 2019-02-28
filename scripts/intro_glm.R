# intro_glm.R
#
# régression multivariée, régressions non-linéaires
# Par G Loignon
# guillaume.loignon@umontreal.ca
# Dernière révision: février 2019

library("data.table")
library(lmtest) #pour le lrtest()
library(pscl) #pour le pseudo-R2
library(caret) #pour confusionMatrix()
library(e1071) #confusionMatrix en a besoin

#---- régression multivariée ----

# Syntaxe de base:
# lm(data=mes.donnees, var.dep ~ var.indep.1 + var.indep.2 * var.indep.3 etc etc etc )

load("data/dt_synth2_large.rda")
# Exemple: je veux prédire la noteMax en philo 3 en fonction de deux autres cours de philosophie et du programme
modele1 <- lm( data=dt.cegep.large, philo103 ~ philo102 * philo101 )
summary(modele1) #et on affiche les résultats
plot(modele1) #sorties graphiques fréquemment employées pour diagnostiquer le modèle de régression

# Consigne: faites un modèle multivarié visant à expliquer la note en philo 101.

modele2 <- lm( data=dt.cegep.large, philo101 ~ MGS * moyNoteSpec)
summary(modele2) #et on affiche les résultats

# Ça fonctionne même pour les variables catégorielles!
modele3 <- lm( data=dt.cegep.large, philo101 ~ MGS * Programme )
summary(modele3) #R a fait une régression pour chaque niveau de la variable catégorielle Programmes
plot(modele3)

# Prédiction avec un modèle linéaire
roger <- data.table ( #On invente Roger Tremblay, on nouvel étudiant
  nom="Tremblay, Roger",
  Programme="Tech. humaines",
  MGS=60)
roger #il manque des données car Roger vient juste d'arriver, il n'a pas encore fait de cours! :)

predict.lm(modele3, newdata=roger, type="response" ) #va afficher la note prédite en philo101
predict.lm(modele3, newdata=roger, type="terms" ) #va afficher les termes du modèle

#   Question: quelle serait la note prédite pour Roger s'il était plutôt inscrit
#             au programme "Sciences informatiques et mathématiques" ?  

#---- régression logistique ----
  #Données de simulation concernant des élèves ayant suivi (ou pas) un cours de renforcement.
load("data/sim_renfo.rda")

#Je sais que plusieurs élèves du groupe expérimental n'ont pas complété le programme
table(is.na(dt.sim.renfo$NotePost), dt.sim.renfo$condition) 

#joli graphique, on constate que la variance diffère entre les groupes
ggplot(dt.sim.renfo, aes(x=NotePre, y=NotePost, color=condition)) +
  geom_point(size=2) 

t.test(data=dt.sim.renfo, NotePost ~ condition) #on explore le lien entre la condition (renfo/témoin) 
                                            #et la NotePost obtenue par l'élève suite à l'intervention
chisq.test(y=dt.sim.renfo$condition=="RENFO", x=dt.sim.renfo$NotePost > 59) #lien avec réussite

modele.lin <-  lm(data=dt.sim.renfo, NotePost ~ NotePre * condition) #modèle linéaire
summary(modele.lin)
modele.log <- glm(data=dt.sim.renfo, NotePost > 59 ~ NotePre * condition, family="binomial") #reg logistique
summary(modele.log)
coef(modele.log) #si on veut uniquement les coefficients
  
#---- tests pour évaluer rég. logistique ----
#Voir https://www.r-bloggers.com/evaluating-logistic-regression-models/

#je fais un autre modèle qui se base uniquement sur la note au pré-test
modele.log.2 <- glm(data=dt.sim.renfo, NotePost > 59 ~ NotePre, family="binomial") #modele alternatif
anova(modele.log, modele.log.2, test ="Chisq")
lrtest(modele.log, modele.log.2)

#---- matrice de confusion ----
# On cherche ici à savoir si le modèle de rég. logistique permet de prédire adéquatement l'issue du post-test.
# Cette méthode peut aussi servir à utiliser le modèle comme outil de classification.

dt.cas.complets <- dt.sim.renfo[complete.cases(dt.sim.renfo)] #on va chercher les obs complètes uniquement
pred.log.r <- predict.glm(modele.log, newdata=dt.cas.complets, type="response" ) #on va chercher la prédiction
pred.log.t <- predict.glm(modele.log, newdata=dt.cas.complets, type="terms" ) #pour ajouter les termes du modèle
colnames(pred.log.t) <- paste("term", colnames(pred.log.t), sep="_") #j'ajoute un préfixe aux termes
head(pred.log.t) #tableau des termes de la rég logistique (remarquez les préfixes)
pred.sommaire <- cbind(dt.cas.complets, pred.log.t)
pred.sommaire <- cbind(pred.sommaire, prob.pred.log = pred.log.r)

  #ajout de variables dichotomiques pour la matrice de confusion
pred.sommaire[, prediction := factor(prob.pred.log < 0.5)] #pour bel affichage des diagnostics
pred.sommaire[, issue := factor(NotePost < 60)]
levels(pred.sommaire$prediction) <- c("REUSSITE", "ECHEC")
levels(pred.sommaire$issue) <- c("REUSSITE", "ECHEC")


  #méthode 1: fonction confusionMatrix() dans le package caret
    #cette méthode est intéressante car elle produit plusieurs statistiques
    #utiles, en plus d'avoir un affichage plus clair de la matrice
    #voir ?confusionMatrix pour voir comment les statistiques sont calculées.
confusionMatrix(data=pred.sommaire$prediction, reference=pred.sommaire$issue,
                dnn = c("Prédiction", "Issue"), positive="REUSSITE")
    # data est un vecteur contenant les données de prédiction
    # reference contient les données avec l'issue réelle
    # dnn est un param optionnel pour les étiquettes
    # positive indique quel est l'issue désirée, ici la réussite
    
  #méthode 2: r de base
table(pred.sommaire$prediction, pred.sommaire$issue,
      dnn = c("Prédiction", "Issue"))

#   Question: combient ai-je de faux positifs? de faux négatifs?

#   Question: Quels sont les paramètres de cet algorithme de classification? Comment
#             pourrait-on l'améliorer?

# Et un graphique de tout ça pour montrer à nos ami-es!
ggplot(pred.sommaire, aes(x=NotePre, y=prob.pred.log, color=condition)) +
  geom_point(size=1) +
  geom_smooth(method = "glm", 
              method.args = list(family = "quasibinomial"), 
              se = FALSE)
    #pourquoi quasibinomial? car j'ai une probabilité entre 0 et 1, 
    # plutôt qu'une variable dichotomique qui serait soit 0 soit 1.

## Simulation 2: Analyse de l'effet d'une intervention en fonction d'une autre variable
# On cherche à estimer comment l'intervention fait varier la probabilité de réussir le
# post-test, et comment cet effet varie selon l'habileté de l'élève telle qu'indiquée
# par son score au pré-test.

test.data <- data.table( #va produire un petit tableau avec des élèves simulés
  condition=c(rep(c("TEM"), 40), rep(c("RENFO"), 40)),
  NotePre=rep(c(51:90), 2)
)
#rouler le code commenté ci-dessous pour avoir seulement les quintiles de PreTest
#summary(dt.sim.renfo$NotePre) #pour voir les quintiles
#test.data <- data.table( #va produire un petit tableau avec des élèves simulés
#  condition=c(rep(c("TEM"), 5), rep(c("RENFO"), 5)),
#  NotePre=rep(c(50, 62, 72, 83, 100), 2)
#)

head(test.data)

pred.lin.r <- predict.lm(modele.lin, newdata=test.data, type="response" )
pred.lin.sommaire <- cbind(test.data, note.pred.lin = pred.lin.r)
pred.log.r <- predict.glm(modele.log, newdata=test.data, type="response" )
pred.sommaire <- cbind(pred.lin.sommaire, prob.pred.log = pred.log.r)
head(pred.sommaire)

#tiens, faisons-en un tableau large afin d'analyser l'effet
pred.large <- dcast(data = pred.sommaire, 
                        formula = NotePre ~ condition, 
                        value.var = "prob.pred.log",
                        fun.aggregate = max)
pred.large[, effetProb := RENFO - TEM]
head(pred.large)

mean(pred.large$effetProb) #l'effet moyen est négatif!
median(pred.large$effetProb) # tout comme l'effet médian

# Un graphique pour y voir plus clair
ggplot(pred.large, aes(x=NotePre, y=effetProb)) +
  geom_point(size=2) +
  geom_line(linetype="dotted") +
  ggtitle("Effet de l'intervention sur la prob. de réussite en fonction du score au pré-test")

  # On remarque que le renforcement fonctionne beaucoup mieux quand l'élève a moins bien performé 
  # au pré-test. Le principe des rendements décroissants (diminishing returns) s'applique bien ici!
  # Le point charnière semble être autour de 72 au pré-test. On pourrait donc recommander le
  # reforcement pour tous les élèves ayant obtenu une note sous ce seuil.

#Morale(s) de cette histoire: 
  #  rapporter l'effet moyen n'a parfois pas de sens.
  #  rapporter uniquement les coefficients de la régression logistique est potentiellement trompeur
  #  ne pas décrire les résultats d'une rég. logistique comme s'il s'agissait d'une relation linéaire
  #  Voir ce livre! https://us.sagepub.com/en-us/nam/best-practices-in-logistic-regression/book239211