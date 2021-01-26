#' ---
#' title: "TP 2 estimation des effectifs en populations fermées"
#' output:
#'   pdf_document: default
#'   html_document: default
#' ---
#' 
## ----setup, include=FALSE-----------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE)

#' 
#' On charge le package `RMark` qui appelle le logiciel Mark depuis `R`. On charge aussi le package `secr` qui permet d'implémenter le test de `closure`.
## -----------------------------------
library(RMark)
library(secr)

#' 
#' # Exercice 1 : souris sylvestre
#' 
#' ## Lecture et formatage des données
#' 
#' On commence par lire les données qui se trouvent dans le répertoire dat/
## -----------------------------------
mouse <- convert.inp("dat/deer-mouse-nogroup.inp",
                    group.df = NULL,
                    covariates = NULL)

#' 
#' On regarde les 10 premières lignes du fichier. 
## -----------------------------------
head(mouse)

#' 
#' Les 10 dernières lignes.
## -----------------------------------
tail(mouse)

#' 
#' On fait les tests de fermeture. Pour cela, il nous faut d'abord convertir les données au format requis pour utiliser le package secr qui fait ces tests. Le formatage consiste à mettre un espace entre les colonnes de capture. 
## -----------------------------------
mouse_secr <- unRMarkInput(mouse)

#' 
#' On peut utiliser la fonction summary de R pour obtenir un résumé des données. 
## -----------------------------------
summary(mouse_secr)

#' 
#' ## Test de l'hypothèse de fermeture
#' 
#' On fait enfin les tests. Par défaut, seul le test d'Otis est fait. En rajoutant l'option "SB = TRUE", on fait aussi le test de Stanley et Burnham. 
## -----------------------------------
closure.test(mouse_secr, SB = TRUE)

#' 
#' ## Une première série de modèles
#' 
#' Pour utiliser RMark, on passe par 3 étapes : la préparation des données, la définition des modèles et l'ajustement à proprement parler. 
#' 
#' On commence par préparer les données. 
## -----------------------------------
mouse.proc <- process.data(mouse, 
                           begin.time = 1, 
                           model = "FullHet")
mouse.ddl <- make.design.data(mouse.proc)

#' 
#' On définit les modèles que l'on souhaite ajuster grâce à une fonction R qui fait 3 choses : spéficication des effets, création d'une liste des modèles à ajuster et préparation pour envoi à Mark. Par défaut, Mark considère un effet comportement et distingue une probabilité de capture c et une autre de recapture p. On utilise "share = TRUE" pour fusionner ces deux paramètres en une seule probabilité de capture. 
## -----------------------------------
run.mouse <- function() {

## On specifie les effets
  
  # M0 : p constant dans le temps
  p.dot <- list(formula =  ~ 1, share = TRUE)
  # Mb : p (recapture) different de c (premiere capture) et constants dans le temps 
  p.dot.behav <- list(formula =  ~ 1)
  # Mt : p varie selon la session (dans le temps)
  p.time <- list(formula =  ~ time, share = TRUE)
  # Mh : p est heterogene entre individu
  p.h <- list(formula = ~ mixture, share = TRUE)
  # Mtb
  p.time.behav <- list(formula =  ~ time)
  # Mbh
  p.h.behav <- list(formula =  ~ mixture)
  # Mth
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  # Mtbh
  p.h.time.behav <- list(formula =  ~ mixture + time)

## On construit la liste des modeles
  mouse.model.list <- create.model.list("FullHet")
  
## On prépare le tout pour envoi a Mark
  mouse.results <- mark.wrapper(mouse.model.list,
                              data = mouse.proc, 
                              ddl = mouse.ddl)
                              
## On retourne les resultats
  return(mouse.results)
}

#' 
#' On fait tourner tous les modèles d'un coup. 
## -----------------------------------
mouse.results <- run.mouse()

#' 
#' On examine les résultats. 
## -----------------------------------
mouse.results

#' 
#' Le nom des modèles n'est pas limpide. On fait le lien entre la première colonne qui donne le numéro du modèle, et la liste des modèles qu'on a définie au-dessus. 
## -----------------------------------
names(mouse.results)

#' 
#' Par exemple, si l'on veut afficher les résultats du modèle $M_0$, il s'agit du modèle 1 "p.dot". On peut afficher la probabilité de détection avec l'intervalle de confiance associé. 
## -----------------------------------
mouse.results$p.dot$results$real

#' 
#' On obtient aussi une estimation de l'effectif. 
## -----------------------------------
mouse.results$p.dot$results$derived

#' 
#' Le meilleur modèle selon l'AIC est le modèle numéroté 4 qui correspond à "p.h.behav". On affiche les résults pour ce modèle. 
## -----------------------------------
mouse.results$p.h.behav$results$real
mouse.results$p.h.behav$results$derived

#' 
#' ## Analyses séparées, mâles vs. femelles
#' 
#' Ici on sépare mâles et femelles et on reproduit l'analyse ci-dessus.On commence par lire les données. On spécifie le groupe, ici les mâles d'abord, puis les femelles. 
## -----------------------------------
mouse <- convert.inp("dat/deer-mouse-sex2G-MF.inp",
                    group.df = data.frame(sex = c("M","F")),
                    covariates = NULL)

#' 
#' On inspecte les données. 
## -----------------------------------
head(mouse)
tail(mouse)

#' 
#' On sépare mâles et femelles en deux jeux de données. 
## -----------------------------------
mouseM <- mouse[mouse$sex == "M", ]
mouseF <- mouse[mouse$sex == "F", ]

#' 
#' On formate les données pour effectuer les tests de l'hypothèse de fermeture. 
## -----------------------------------
mouseM_secr <- unRMarkInput(mouseM) # on convertit au bon format
mouseF_secr <- unRMarkInput(mouseF) # on convertit au bon format

#' 
#' 
#' On fait les tests de fermeture, les mâles d'abord. 
## -----------------------------------
closure.test(mouseM_secr, SB = TRUE)

#' 
#' Les femelles ensuite.
## -----------------------------------
closure.test(mouseF_secr, SB = TRUE)

#' 
#' Les modèles maintenant. Commençons par les mâles. 
#' 
## -----------------------------------
mouse.proc <- process.data(mouseM, 
                           begin.time = 1, 
                           model = "FullHet")
mouse.ddl <- make.design.data(mouse.proc)

#' 
#' La liste des modèles.
## -----------------------------------
run.mouse <- function() {

  # sans l'effet sexe
  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ mixture + time)

  mouse.model.list <- create.model.list("FullHet")
  
  mouse.results <- mark.wrapper(mouse.model.list,
                                data = mouse.proc, 
                                ddl = mouse.ddl)
                              
  return(mouse.results)
}

#' 
#' On lance Mark. 
## -----------------------------------
mouse.results <- run.mouse()

#' 
#' Et on inspecte les résultats.
## -----------------------------------
mouse.results

#' 
#' Les noms des modèles. 
## -----------------------------------
names(mouse.results)

#' 
#' On examine les résultats obtenus selon le meilleur modèle (#5). 
## -----------------------------------
mouse.results$p.h.time$results$real
mouse.results$p.h.time$results$derived

#' 
#' On procède de même pour les femelles. 
#' 
## -----------------------------------
mouse.proc <- process.data(mouseF, 
                           begin.time = 1, 
                           model = "FullHet")
mouse.ddl <- make.design.data(mouse.proc)

#' 
#' La liste des modèles.
## -----------------------------------
run.mouse <- function() {

  # sans l'effet sexe
  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ mixture + time)

  mouse.model.list <- create.model.list("FullHet")
  
  mouse.results <- mark.wrapper(mouse.model.list,
                                data = mouse.proc, 
                                ddl = mouse.ddl)
                              
  return(mouse.results)
}

#' 
#' On lance Mark. 
## -----------------------------------
mouse.results <- run.mouse()

#' 
#' Et on inspecte les résultats.
## -----------------------------------
mouse.results

#' 
#' Les noms des modèles. 
## -----------------------------------
names(mouse.results)

#' 
#' On examine les résultats obtenus selon le meilleur modèle (#1). 
## -----------------------------------
mouse.results$p.dot$results$real
mouse.results$p.dot$results$derived

#' 
#' ## Analyse avec un effet sexe
#' 
#' Il est un peu dommage de séparer mâles et femelles en deux analyses séparées. En effet, on pourrait vouloir tester un effet sexe sur la probabilité de détection. On reprend l'analyse en considérant le jeu de données dans son entier. 
## -----------------------------------
mouse <- convert.inp("dat/deer-mouse-sex2G-MF.inp",
                    group.df = data.frame(sex = c("M","F")),
                    covariates = NULL)
head(mouse)
tail(mouse)

#' 
#' On passe à la définition des modèles maintenant. On commence par préparer les données. On utilise l'option "groups = "sex"" pour préciser qu'on va considérer des modèles avec l'effet sexe. 
## -----------------------------------
mouse.proc <- process.data(mouse, 
                           begin.time = 1, 
                           model = "FullHet", 
                           groups = "sex")
mouse.ddl <- make.design.data(mouse.proc)

#' 
#' La liste des modèles. Ce sont les mêmes qu'au-dessus, auxquels on a ajouté d'autres modèles avec l'effet sexe.  
## -----------------------------------
run.mouse <- function() {

  # sans l'effet sexe
  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ mixture + time)

  # avec l'effet sexe
  p.sex <- list(formula =  ~ sex, share = TRUE)
  p.sex.behav <- list(formula =  ~ sex)
  p.time.sex <- list(formula =  ~ time + sex, share = TRUE)
  p.time.behav.sex <- list(formula =  ~ sex + time)
  p.h.sex <- list(formula = ~ mixture + sex, share = TRUE)
  p.h.behav.sex <- list(formula =  ~ sex + mixture)
  p.h.time.sex <- list(formula = ~ time + mixture + sex, share = TRUE)
  p.h.time.behav.sex <- list(formula =  ~ sex + mixture + time)

  mouse.model.list <- create.model.list("FullHet")
  
  mouse.results <- mark.wrapper(mouse.model.list,
                                data = mouse.proc, 
                                ddl = mouse.ddl)
                              
  return(mouse.results)
}

#' 
#' On fait tourner tous ces modèles, et on inspecte le classement.
## -----------------------------------
mouse.results <- run.mouse()
mouse.results

#' 
#' Les noms des modèles.
## -----------------------------------
names(mouse.results)

#' 
#' On examine le meilleur modèle selon l'AIC (#9).
## -----------------------------------
mouse.results$p.h.time.behav.sex$results$real
mouse.results$p.h.time.behav.sex$results$derived

#' 
#' Et un autre modèle, le modèle #2 classé 13ème. 
## -----------------------------------
mouse.results$p.dot.behav$results$real
mouse.results$p.dot.behav$results$derived

#' 
#' 
#' # Nettoyage
#' 
#' On supprime les fichiers temporaires. 
#' 
## -----------------------------------
rm(list = ls(all = TRUE))
cleanup(ask = FALSE)

#' 
