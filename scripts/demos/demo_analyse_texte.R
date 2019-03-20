library(readr) #pour ouvrir les txt
library(udpipe)
library("data.table")
library(tidyverse) #toujours pratique, ici on utilise son package stringr
library(genius) #pour les paroles de chanson
library(wordcloud) #pour le nuage de concept 

content_categories <- c("VERB", "NOUN", "PROPN", "ADJ") #utilisé quand on filtre les lexiques

#lit tous les fichiers txt d'un dossier et les retourne dans un format prêt à
#être analysé par udpipe
lireToutTxt <- function (dossier) {
  chemins <- list.files(path=dossier, full.names = TRUE) #liste des fichiers avec le "path"
  n <- length(chemins) #le nombre de textes dans le dossier 
  mon.corpus <- data.table ( #initialisation
    doc_id = basename(chemins),
    text = ""
  )
  for (i in (1:n)) {
    contenu <- read_file(chemins[i])
    contenu <- str_replace_all(contenu, "’", "'") #correction pour les apostrophes
      #note: str_replace_all est dans le package stringr, qu'on retrouve déjà dans le tidyverse
    mon.corpus[i, "text"] <- contenu
  }
  return (mon.corpus)
}

#analyse de cooccurrence (simplifié)
analyseCooc <- function(dt.lexique) {
  setDT(dt.lexique) #et on le transforme en data.table
  dt.lexique.parent <- cbind_dependencies(dt.lexique) #on ajoute à chaque lemme les infos de son parent
  
  #on fait un sommaire des coocurrences
  dt.cooc <- unique(dt.lexique.parent[!is.na(lemma) 
                                      & !is.na(lemma_parent) 
                                      & upos %in% content_categories, 
                                      .(
                                        .N
                                      ), by=c("lemma", "lemma_parent")])[order(-N)]
  dt.cooc <- dt.cooc[lemma != lemma_parent] #on retire les cooc qui ont le même lemma et parent
                                             
  
  return(dt.cooc)
}

#--- démonstration 1 ----
dossier = "./data/corpus/martineau"
dt.corpus <- lireToutTxt(dossier) #récupération des textes
fr <- udpipe_load_model("./data/modeles_ling/french-gsd-ud-2.3-181115.udpipe")
dt.lexique.richard <- udpipe(x=dt.corpus, object=fr, 
                             trace = TRUE) #analyse lexicale
setDT(dt.lexique.richard)

#analyse de cooccurrence
dt.cooc <- analyseCooc(dt.lexique.richard)
head(dt.cooc)

#nuage de mots
sommaire.mots.richard <- dt.lexique.richard[upos %in% content_categories, .(
  freq=.N
), by=lemma][order(-freq)]
head(sommaire.mots.richard)

set.seed(123) #détermine la suite de chiffre pseudo-aléatoire que R va utiliser
wordcloud(words = sommaire.mots.richard$lemma, freq = sommaire.mots.richard$freq, 
          min.freq = 4, max.words=100, random.order=FALSE, rot.per=0.33, 
          fixed.asp = TRUE, use.r.layout = TRUE,
          colors=brewer.pal(8, "Dark2"))


#---- Démonstration 2 ----
#en <- udpipe_load_model("./data/modeles_ling/english-ewt-ud-2.3-181115.udpipe")
#load("./data/corpus/songdata.rda")
#dt.songdata <- data.table(songdata)
#liste_disco <- c("ABBA", "Aretha Franklin", "Diana Ross", "Pointer Sisters", 
#                 "Bee Gees", "Gloria Gaynor", "Village People")
#dt.disco <- dt.songdata[artist %in% liste_disco, .(
#  doc_id = song,
#  text
#)]

#dt.lexique.disco <- udpipe(x=dt.disco, object=en, trace = TRUE) #analyse lexicale

#note: le code commenté ci-dessus permet de recréer le dataset de lexique disco
load("./corpus/dt_lexique_disco.rda")
dt.cooc <- analyseCooc(dt.lexique.disco)
head(dt.cooc, 20)

#nuage de mots
sommaire.mots.disco <- dt.lexique.disco[upos %in% content_categories, .(
  freq=.N
), by=lemma][order(-freq)]

set.seed(125) #détermine la suite de chiffre pseudo-aléatoire que R va utiliser
wordcloud( words = sommaire.mots.disco$lemma, freq = sommaire.mots.disco$freq, 
           min.freq = 10, max.words=120, random.order=FALSE, rot.per=0.33, 
           colors=brewer.pal(9, "Set1"))
  #pour en savoir plus sur les palettes de couleur en R:
  # https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf