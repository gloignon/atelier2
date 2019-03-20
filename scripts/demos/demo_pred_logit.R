#demo_pred.logit.R
#
# Démonstration: on fait des prédictions à partir d'un modèle logistique.
#
# On démontre aussi qu'une intervention qui semble avoir un effet négatif
# peut en fait être bénéfique sur la partie de la population pour laquelle
# elle a été conçue.
#
# Et on retient qu'il faut se méfier des articles qui décrivent une régression
# logistique comme si c'était une régression linégaire.

library(data.table)
library(effsize) #pour le d de Cohen

load("/cloud/project/data/sim_renfo.rda")

# Production de modèles 
modele.lin <-  lm(data=dt.sim.renfo, NotePost ~ NotePre * condition)  # rég linéaire
modele.log <- glm(data=dt.sim.renfo, NotePost > 59 ~ NotePre * condition, family="binomial")  # rég logistique

# On simule des élèves
test.data <- data.table( 
  condition=c(rep(c("TEM"), 40), rep(c("RENFO"), 40)),
  NotePre=rep(c(51:90), 2)
)
  # Pour chaque score de l'intervale qui nous intéresse, on a simulé un élève du groupe témoin et un élève renfo

#on prédit des scores à partir des modèles
pred.lin.r <- predict.lm(modele.lin, newdata=test.data, type="response" )
pred.lin.sommaire <- cbind(test.data, note.pred.lin = pred.lin.r)
pred.log.r <- predict.glm(modele.log, newdata=test.data, type="response" )
pred.sommaire <- cbind(pred.lin.sommaire, prob.pred.log = pred.log.r)
head(pred.sommaire)

#tiens, faisons-en un tableau large afin de mieux analyser l'effet
pred.large <- dcast(data = pred.sommaire, 
                    formula = NotePre ~ condition, 
                    value.var = "prob.pred.log",
                    fun.aggregate = max)

# On ajoute la variable effetProb, qui indique la différence de probabilité de réussite
# entre le groupe renfo et le groupe témoin.
pred.large[, effetProb := RENFO - TEM]
head(pred.large)
  # Ce qu'on voit aussi: la probabilité de réussite de l'élève RENFO est comparée à celle
  # du même élève hypothétique, mais qui aurait été dans le groupe témoin (TEM) à la place.
  # La colonne effetProb indique la différence entre les deux probabilités de réussite.

mean(pred.large$effetProb)  # l'effet moyen est négligeable, voire négatif!
median(pred.large$effetProb)  # tout comme l'effet médian
  
# On peut même se risquer à calculer un d de Cohen
cohen.d(pred.large$RENFO, pred.large$TEM)  # ça confirme: effet négligeable et négatif

# Un graphique pour y voir plus clair
ggplot(pred.large, aes(x=NotePre, y=effetProb)) +
  geom_point(size=2) +
  geom_line(linetype="dotted") +
  ggtitle("Effet de l'intervention sur la probabilité de réussite en fonction du score au pré-test")

# Hypothèse: l'intervention serait bénéfique chez la minorité d'élèves en difficulté
nrow(dt.sim.renfo[NotePre < 68]) / nrow(dt.sim.renfo)
  # l'intervention aiderait donc seulement le 25% des élèves les plus faibles, ce qui
  # expliquerait l'effet moyen qui semble nul

# On si on faisant deux modèles linéaires?
modele.lin.diffic <- lm(data=dt.sim.renfo[NotePre < 68], NotePost > 59 ~ NotePre + condition) 
summary(modele.lin.diffic)  # effet positif!
modele.lin.reste <- lm(data=dt.sim.renfo[NotePre > 72], NotePost > 59 ~ NotePre + condition)
summary(modele.lin.reste)  # effet négatif!

# Les résultats semblent indiquer que, bien ciblée, cette intervention peut fonctionner, mais qu'elle
# a un effet négatif chez les élèves qui n'en ont pas besoin (pourquoi? ça reste à voir!)