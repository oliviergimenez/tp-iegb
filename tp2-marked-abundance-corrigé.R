#' ---
#' title: "TP 2 estimation des effectifs en populations fermées"
#' output:
#'   pdf_document: default
#'   html_document: default
#' ---
#' 
## ----setup, include = FALSE--------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE)

#' 
#' On charge le package `RMark` qui appelle le logiciel Mark depuis `R`. On charge aussi le package `secr` qui permet d'implémenter le test de `closure`.
## ----------------------------------------------------------------------------------
library(RMark)
library(secr)

#' 
#' # Exercice 1 : souris sylvestre
#' 
#' ## Lecture et formatage des données
#' 
#' On commence par lire les données qui se trouvent dans le répertoire dat/
## ----------------------------------------------------------------------------------
mouse <- convert.inp("dat/deer-mouse-nogroup.inp",
                    group.df = NULL,
                    covariates = NULL)

#' 
#' On regarde les 10 premières lignes du fichier. 
## ----------------------------------------------------------------------------------
head(mouse)

#' 
#' Les 10 dernières lignes.
## ----------------------------------------------------------------------------------
tail(mouse)

#' 
#' On fait les tests de fermeture. Pour cela, il nous faut d'abord convertir les données au format requis pour utiliser le package secr qui fait ces tests. Le formatage consiste à mettre un espace entre les colonnes de capture. 
## ----------------------------------------------------------------------------------
mouse_secr <- unRMarkInput(mouse)

#' 
#' On peut utiliser la fonction summary de R pour obtenir un résumé des données. 
## ----------------------------------------------------------------------------------
summary(mouse_secr)

#' 
#' ## Test de l'hypothèse de fermeture
#' 
#' On fait enfin les tests. Par défaut, seul le test d'Otis est fait. En rajoutant l'option "SB = TRUE", on fait aussi le test de Stanley et Burnham. 
## ----------------------------------------------------------------------------------
closure.test(mouse_secr, SB = TRUE)

#' 
#' ## Une première série de modèles
#' 
#' Pour utiliser RMark, on passe par 3 étapes : la préparation des données, la définition des modèles et l'ajustement à proprement parler. 
#' 
#' On commence par préparer les données. 
## ----------------------------------------------------------------------------------
mouse.proc <- process.data(mouse, 
                           begin.time = 1, 
                           model = "FullHet")
mouse.ddl <- make.design.data(mouse.proc)

#' 
#' On définit les modèles que l'on souhaite ajuster grâce à une fonction R qui fait 3 choses : spéficication des effets, création d'une liste des modèles à ajuster et préparation pour envoi à Mark. Par défaut, Mark considère un effet comportement et distingue une probabilité de capture c et une autre de recapture p. On utilise "share = TRUE" pour fusionner ces deux paramètres en une seule probabilité de capture. 
## ----------------------------------------------------------------------------------
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
## ----------------------------------------------------------------------------------
mouse.results <- run.mouse()

#' 
#' On examine les résultats. 
## ----------------------------------------------------------------------------------
mouse.results

#' 
#' Le nom des modèles n'est pas limpide. On fait le lien entre la première colonne qui donne le numéro du modèle, et la liste des modèles qu'on a définie au-dessus. 
## ----------------------------------------------------------------------------------
names(mouse.results)

#' 
#' Par exemple, si l'on veut afficher les résultats du modèle $M_0$, il s'agit du modèle 1 "p.dot". On peut afficher la probabilité de détection avec l'intervalle de confiance associé. 
## ----------------------------------------------------------------------------------
mouse.results$p.dot$results$real

#' 
#' On obtient aussi une estimation de l'effectif. 
## ----------------------------------------------------------------------------------
mouse.results$p.dot$results$derived

#' 
#' Le meilleur modèle selon l'AIC est le modèle numéroté 4 qui correspond à "p.h.behav". On affiche les résults pour ce modèle. 
## ----------------------------------------------------------------------------------
mouse.results$p.h.behav$results$real
mouse.results$p.h.behav$results$derived

#' 
#' ## Analyses séparées, mâles vs. femelles
#' 
#' Ici on sépare mâles et femelles et on reproduit l'analyse ci-dessus.On commence par lire les données. On spécifie le groupe, ici les mâles d'abord, puis les femelles. 
## ----------------------------------------------------------------------------------
mouse <- convert.inp("dat/deer-mouse-sex2G-MF.inp",
                    group.df = data.frame(sex = c("M","F")),
                    covariates = NULL)

#' 
#' On inspecte les données. 
## ----------------------------------------------------------------------------------
head(mouse)
tail(mouse)

#' 
#' On sépare mâles et femelles en deux jeux de données. 
## ----------------------------------------------------------------------------------
mouseM <- mouse[mouse$sex == "M", ]
mouseF <- mouse[mouse$sex == "F", ]

#' 
#' On formate les données pour effectuer les tests de l'hypothèse de fermeture. 
## ----------------------------------------------------------------------------------
mouseM_secr <- unRMarkInput(mouseM) # on convertit au bon format
mouseF_secr <- unRMarkInput(mouseF) # on convertit au bon format

#' 
#' 
#' On fait les tests de fermeture, les mâles d'abord. 
## ----------------------------------------------------------------------------------
closure.test(mouseM_secr, SB = TRUE)

#' 
#' Les femelles ensuite.
## ----------------------------------------------------------------------------------
closure.test(mouseF_secr, SB = TRUE)

#' 
#' Les modèles maintenant. Commençons par les mâles. 
#' 
## ----------------------------------------------------------------------------------
mouse.proc <- process.data(mouseM, 
                           begin.time = 1, 
                           model = "FullHet")
mouse.ddl <- make.design.data(mouse.proc)

#' 
#' La liste des modèles.
## ----------------------------------------------------------------------------------
run.mouse <- function() {

  # sans l'effet sexe
  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ time + mixture)

  mouse.model.list <- create.model.list("FullHet")
  
  mouse.results <- mark.wrapper(mouse.model.list,
                                data = mouse.proc, 
                                ddl = mouse.ddl)
                              
  return(mouse.results)
}

#' 
#' On lance Mark. 
## ----------------------------------------------------------------------------------
mouse.results <- run.mouse()

#' 
#' Et on inspecte les résultats.
## ----------------------------------------------------------------------------------
mouse.results

#' 
#' Les noms des modèles. 
## ----------------------------------------------------------------------------------
names(mouse.results)

#' 
#' On examine les résultats obtenus selon le meilleur modèle (#5). 
## ----------------------------------------------------------------------------------
mouse.results$p.h.time$results$real
mouse.results$p.h.time$results$derived

#' 
#' On procède de même pour les femelles. 
#' 
## ----------------------------------------------------------------------------------
mouse.proc <- process.data(mouseF, 
                           begin.time = 1, 
                           model = "FullHet")
mouse.ddl <- make.design.data(mouse.proc)

#' 
#' La liste des modèles.
## ----------------------------------------------------------------------------------
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
## ----------------------------------------------------------------------------------
mouse.results <- run.mouse()

#' 
#' Et on inspecte les résultats.
## ----------------------------------------------------------------------------------
mouse.results

#' 
#' Les noms des modèles. 
## ----------------------------------------------------------------------------------
names(mouse.results)

#' 
#' On examine les résultats obtenus selon le meilleur modèle (#1). 
## ----------------------------------------------------------------------------------
mouse.results$p.dot$results$real
mouse.results$p.dot$results$derived

#' 
#' ## Analyse avec un effet sexe
#' 
#' Il est un peu dommage de séparer mâles et femelles en deux analyses séparées. En effet, on pourrait vouloir tester un effet sexe sur la probabilité de détection. On reprend l'analyse en considérant le jeu de données dans son entier. 
## ----------------------------------------------------------------------------------
mouse <- convert.inp("dat/deer-mouse-sex2G-MF.inp",
                    group.df = data.frame(sex = c("M","F")),
                    covariates = NULL)
head(mouse)
tail(mouse)

#' 
#' On passe à la définition des modèles maintenant. On commence par préparer les données. On utilise l'option "groups = "sex"" pour préciser qu'on va considérer des modèles avec l'effet sexe. 
## ----------------------------------------------------------------------------------
mouse.proc <- process.data(mouse, 
                           begin.time = 1, 
                           model = "FullHet", 
                           groups = "sex")
mouse.ddl <- make.design.data(mouse.proc)

#' 
#' La liste des modèles. Ce sont les mêmes qu'au-dessus, auxquels on a ajouté d'autres modèles avec l'effet sexe.  
## ----------------------------------------------------------------------------------
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
  p.h.time.behav.sex <- list(formula =  ~ sex + time + mixture)

  mouse.model.list <- create.model.list("FullHet")
  
  mouse.results <- mark.wrapper(mouse.model.list,
                                data = mouse.proc, 
                                ddl = mouse.ddl)
                              
  return(mouse.results)
}

#' 
#' On fait tourner tous ces modèles, et on inspecte le classement.
## ----------------------------------------------------------------------------------
mouse.results <- run.mouse()
mouse.results

#' 
#' Les noms des modèles.
## ----------------------------------------------------------------------------------
names(mouse.results)

#' 
#' On examine le meilleur modèle selon l'AIC (#9).
## ----------------------------------------------------------------------------------
mouse.results$p.h.time.behav.sex$results$real
mouse.results$p.h.time.behav.sex$results$derived

#' 
#' Et un autre modèle, le modèle #2 classé 13ème. 
## ----------------------------------------------------------------------------------
mouse.results$p.dot.behav$results$real
mouse.results$p.dot.behav$results$derived

#' 
#' 
#' # Exercice 2 : cigognes
#' 
#' Les données.
## ----------------------------------------------------------------------------------
cigogne <- convert.inp("dat/cigognes-2002-3G.inp",
                    group.df = data.frame(bagues = c("metal","couleur", "darvic")),
                    covariates = NULL)
head(cigogne)
tail(cigogne)

#' 
#' On formate les données. 
## ----------------------------------------------------------------------------------
cigogne_secr <- unRMarkInput(cigogne) # on convertit au bon format

#' 
#' On fait les tests de fermeture. 
## ----------------------------------------------------------------------------------
closure.test(cigogne_secr, SB = TRUE)

#' 
#' Les modèles maintenant. On sépare selon le type de bagues.
#' 
#' Couleur d'abord.
## ----------------------------------------------------------------------------------
cigogne_bague <- cigogne[cigogne$bagues=="couleur",]
cigogne.proc <- process.data(cigogne_bague, begin.time = 1, model = "FullHet")
cigogne.ddl <- make.design.data(cigogne.proc)

#' 
#' Liste des modèles (pas d'effet comportement).
## ----------------------------------------------------------------------------------
run.cigogne <- function() {

  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  cigogne.model.list <- create.model.list("FullHet")
  cigogne.results <- mark.wrapper(cigogne.model.list,
                                  data = cigogne.proc, 
                                  ddl = cigogne.ddl)
  return(cigogne.results)
}

#' 
#' On fait tourner. 
## ----------------------------------------------------------------------------------
cigogne.results <- run.cigogne()

#' 
#' Le classement des modèles.
## ----------------------------------------------------------------------------------
cigogne.results

#' 
#' Les noms. 
## ----------------------------------------------------------------------------------
names(cigogne.results)

#' 
#' Les résultats selon le meilleur modèle. 
## ----------------------------------------------------------------------------------
(pcouleur <- cigogne.results$p.time$results$real)
(Ncouleur <- cigogne.results$p.time$results$derived)

#' 
#' Darvic ensuite.
## ----------------------------------------------------------------------------------
cigogne_bague <- cigogne[cigogne$bagues=="darvic",]
cigogne.proc <- process.data(cigogne_bague, begin.time = 1, model = "FullHet")
cigogne.ddl <- make.design.data(cigogne.proc)

#' 
#' On appelle Mark.
## ----------------------------------------------------------------------------------
cigogne.results <- run.cigogne()

#' 
#' Les résultats. 
## ----------------------------------------------------------------------------------
cigogne.results

#' 
#' Les noms.
## ----------------------------------------------------------------------------------
names(cigogne.results)

#' 
#' Les résultats selon le meilleur modèle.
## ----------------------------------------------------------------------------------
(pdarvic <- cigogne.results$p.h.time$results$real)
(Ndarvic <- cigogne.results$p.h.time$results$derived)

#' 
#' Metal enfin.
## ----------------------------------------------------------------------------------
cigogne_bague <- cigogne[cigogne$bagues=="metal",]
cigogne.proc <- process.data(cigogne_bague, begin.time = 1, model = "FullHet")
cigogne.ddl <- make.design.data(cigogne.proc)

#' 
## ----------------------------------------------------------------------------------
cigogne.results <- run.cigogne()

#' 
## ----------------------------------------------------------------------------------
cigogne.results

#' 
## ----------------------------------------------------------------------------------
names(cigogne.results)

#' 
## ----------------------------------------------------------------------------------
(pmetal <- cigogne.results$p.time$results$real)
(Nmetal <- cigogne.results$p.time$results$derived)

#' 
#' On visualise les probabilités de détection.
## ----------------------------------------------------------------------------------
p.estim <- data.frame(couleur = pcouleur[-c(1,16),1],
                      darvic = pdarvic[-c(1,30),1], 
                      metal = pmetal[-c(1,16),1],
                      mixture = c(rep("M1", 14), rep("M2", 14)),
                      occ = c(1:14, 1:14))
# pivote les données
library(tidyr)
p.estim <- pivot_longer(p.estim, 
                               cols = couleur:metal,
                               names_to = "type_bague", 
                               values_to = "p_estim")
# visualise
library(ggplot2)
ggplot(data = p.estim,
       aes(x = occ, y = p_estim, color = mixture)) +
  geom_line() +
  facet_wrap(~type_bague) + 
  theme_light() +
  labs(x = "occasions de capture",
       y = "probabilité estimée de capture",
       color = "classe hétérogénéité")

#' 
#' # Exercice 3 : cistudes
#' 
#' Les données.
## ----------------------------------------------------------------------------------
library(readr)
dat <- read_csv2("dat/BDD-CMR-Cistudes-Vigueirat.csv")
library(janitor)
dat <- clean_names(dat)

#' 
#' Quelles sont les années avec le plus de marquages et recaptures?
## ----------------------------------------------------------------------------------
library(dplyr)
dat %>% 
  count(action, mois, annee, sort = TRUE)
dat <- dat %>% select(id_ind, jour, mois, annee)

#' 
#' On extrait les mois de juin des années 1997 et 2007.
## ----------------------------------------------------------------------------------
library(tibble)
dat1997 <- dat %>% 
  filter(mois == 6, annee == 1997) %>% 
  select(id_ind, jour) %>% 
  add_column(det = 1) %>% 
  arrange(id_ind)
dat2007 <- dat %>% 
  filter(mois == 6, annee == 2007) %>% 
  select(id_ind, jour) %>% 
  add_column(det = 1) %>% 
  arrange(id_ind)

#' 
#' On fait les histoires pour 1997.
## ----------------------------------------------------------------------------------
histories1997 <- dat1997 %>% 
  group_by(id_ind) %>%
  mutate(id2 = row_number()) %>%
  pivot_wider(values_from = det,
              names_from = jour) %>% # les jours en colonnes
  select(-id2) %>%
  group_by(id_ind) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>% # on rassemble les evenements pour chaque ind
  select(-id_ind)
histories1997[is.na(histories1997)] <- 0 # les Na sont des non-détections = 0   
histories1997[histories1997 > 1] <- 1 # les observations mens multiples = 1
(histories1997 <- as.matrix(histories1997))

#' 
#' Et pour 2007.
## ----------------------------------------------------------------------------------
histories2007 <- dat2007 %>% 
  group_by(id_ind) %>%
  mutate(id2 = row_number()) %>%
  pivot_wider(values_from = det,
              names_from = jour) %>% # les jours en colonnes
  select(-id2) %>%
  group_by(id_ind) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>% # on rassemble les evenements pour chaque ind
  select(-id_ind)
histories2007[is.na(histories2007)] <- 0 # les Na sont des non-détections = 0   
histories2007[histories2007 > 1] <- 1 # les observations mens multiples = 1
(histories2007 <- as.matrix(histories2007))

#' 
#' On fait les tests et l'ajustement pour 1997.
## ----------------------------------------------------------------------------------
cistude <- data.frame(ch = collapseCH(histories1997), 
                      freq = rep(1, nrow(histories1997)))
head(cistude)
tail(cistude)

#' 
#' On fait les tests de fermeture.
## ----------------------------------------------------------------------------------
cistude_secr <- unRMarkInput(cistude) # on convertit au bon format
summary(cistude_secr) # resumes
closure.test(cistude_secr, SB = TRUE)

#' 
#' On passe à l'ajustement des modèles.
#' 
## ----------------------------------------------------------------------------------
cistude.proc <- process.data(cistude, 
                             begin.time = 1, 
                             model = "FullHet")
cistude.ddl <- make.design.data(cistude.proc)

#' 
## ----------------------------------------------------------------------------------
run.cistude <- function() {

  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ mixture + time)

  cistude.model.list <- create.model.list("FullHet")
  
  cistude.results <- mark.wrapper(cistude.model.list,
                              data = cistude.proc, 
                              ddl = cistude.ddl)
                              
  return(cistude.results)
}

#' 
## ----------------------------------------------------------------------------------
cistude.results <- run.cistude()

#' 
## ----------------------------------------------------------------------------------
cistude.results

#' 
## ----------------------------------------------------------------------------------
names(cistude.results)

#' 
## ----------------------------------------------------------------------------------
cistude.results$p.dot.behav$results$real
cistude.results$p.dot.behav$results$derived

#' 
#' Idem avec 2007.
#' 
## ----------------------------------------------------------------------------------
cistude <- data.frame(ch = collapseCH(histories2007), freq = rep(1, nrow(histories2007)))
head(cistude)
tail(cistude)

#' 
## ----------------------------------------------------------------------------------
cistude_secr <- unRMarkInput(cistude) # on convertit au bon format
summary(cistude_secr) # resumes
closure.test(cistude_secr, SB = TRUE)

#' 
## ----------------------------------------------------------------------------------
cistude.proc <- process.data(cistude, begin.time = 1, model = "FullHet")
cistude.ddl <- make.design.data(cistude.proc)

#' 
## ----------------------------------------------------------------------------------
run.cistude <- function() {

  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ mixture + time)

  cistude.model.list <- create.model.list("FullHet")
  
  cistude.results <- mark.wrapper(cistude.model.list,
                              data = cistude.proc, 
                              ddl = cistude.ddl)
                              
  return(cistude.results)
}

#' 
## ----------------------------------------------------------------------------------
cistude.results <- run.cistude()

#' 
## ----------------------------------------------------------------------------------
cistude.results

#' 
## ----------------------------------------------------------------------------------
names(cistude.results)

#' 
## ----------------------------------------------------------------------------------
cistude.results$p.h$results$real
cistude.results$p.h$results$derived

#' 
#' 
#' # Exercice 4 : iguanes
#' 
#' ## Données 2006
#' 
#' Les données.
## ----------------------------------------------------------------------------------
iguane <- convert.inp("dat/iguanes-2006-2sexes-FM.inp",
                    group.df = data.frame(sex = c("F","M")),
                    covariates = NULL)
head(iguane)
tail(iguane)

#' 
#' On sépare mâles et femelles.
## ----------------------------------------------------------------------------------
iguaneM <- iguane[iguane$sex == "M", ]
iguaneF <- iguane[iguane$sex == "F", ]

#' 
#' On formate les données. 
## ----------------------------------------------------------------------------------
iguane_secr <- unRMarkInput(iguane) # on convertit au bon format
iguaneM_secr <- unRMarkInput(iguaneM) # on convertit au bon format
iguaneF_secr <- unRMarkInput(iguaneF) # on convertit au bon format
summary(iguane_secr) # resumes
summary(iguaneM_secr) # resumes
summary(iguaneF_secr) # resumes

#' 
#' 
#' Les deux sexes ensemble.
## ----------------------------------------------------------------------------------
closure.test(iguane_secr, SB = TRUE)

#' 
#' On fait les tests de fermeture, mâles d'abord. 
## ----------------------------------------------------------------------------------
closure.test(iguaneM_secr, SB = TRUE)

#' 
#' Femelles ensuite.
## ----------------------------------------------------------------------------------
closure.test(iguaneF_secr, SB = TRUE)

#' 
#' Les modèles maintenant. On commence par le jeu de données avec les deux sexes ensemble.
#' 
## ----------------------------------------------------------------------------------
iguane.proc <- process.data(iguane, 
                            begin.time = 1, 
                            model = "FullHet")
iguane.ddl <- make.design.data(iguane.proc)

#' 
#' Liste des modèles.
## ----------------------------------------------------------------------------------
run.iguane <- function() {

  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ mixture + time)

  iguane.model.list <- create.model.list("FullHet")
  
  iguane.results <- mark.wrapper(iguane.model.list,
                              data = iguane.proc, 
                              ddl = iguane.ddl)
                              
  return(iguane.results)
}

#' 
## ----------------------------------------------------------------------------------
iguane.results <- run.iguane()

#' 
## ----------------------------------------------------------------------------------
iguane.results

#' 
## ----------------------------------------------------------------------------------
names(iguane.results)

#' 
#' examine the output from top-ranked model (#8)
## ----------------------------------------------------------------------------------
iguane.results$p.time$results$real
iguane.results$p.time$results$derived

#' 
#' En séparant les sexes. Femelles, puis mâles. 
#' 
## ----------------------------------------------------------------------------------
iguane.proc <- process.data(iguaneF, 
                            begin.time = 1, 
                            model = "FullHet")
iguane.ddl <- make.design.data(iguane.proc)

#' 
#' Liste des modèles.
## ----------------------------------------------------------------------------------
run.iguane <- function() {

  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ mixture + time)

  iguane.model.list <- create.model.list("FullHet")
  
  iguane.results <- mark.wrapper(iguane.model.list,
                              data = iguane.proc, 
                              ddl = iguane.ddl)
                              
  return(iguane.results)
}

#' 
## ----------------------------------------------------------------------------------
iguane.results <- run.iguane()

#' 
## ----------------------------------------------------------------------------------
iguane.results

#' 
## ----------------------------------------------------------------------------------
names(iguane.results)

#' 
## ----------------------------------------------------------------------------------
iguane.results$p.h.time$results$real
iguane.results$p.h.time$results$derived

#' 
## ----------------------------------------------------------------------------------
iguane.results$p.time$results$real
iguane.results$p.time$results$derived

#' 
#' Les mâles maintenant. 
#' 
## ----------------------------------------------------------------------------------
iguane.proc <- process.data(iguaneM, begin.time = 1, model = "FullHet")
iguane.ddl <- make.design.data(iguane.proc)

#' 
#' Liste des modèles.
## ----------------------------------------------------------------------------------
run.iguane <- function() {

  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ mixture + time)

  iguane.model.list <- create.model.list("FullHet")
  
  iguane.results <- mark.wrapper(iguane.model.list,
                              data = iguane.proc, 
                              ddl = iguane.ddl)
                              
  return(iguane.results)
}

#' 
## ----------------------------------------------------------------------------------
iguane.results <- run.iguane()

#' 
## ----------------------------------------------------------------------------------
iguane.results

#' 
## ----------------------------------------------------------------------------------
names(iguane.results)

#' 
## ----------------------------------------------------------------------------------
iguane.results$p.time$results$real
iguane.results$p.time$results$derived

#' 
#' 
#' ## Données 2010
#' 
#' Les données
## ----------------------------------------------------------------------------------
iguane <- convert.inp("dat/iguanes-2010-2sexes-FM.inp",
                    group.df = data.frame(sex = c("F","M")),
                    covariates = NULL)
head(iguane)
tail(iguane)

#' 
#' On sépare mâles et femelles.
## ----------------------------------------------------------------------------------
iguaneM <- iguane[iguane$sex == "M", ]
iguaneF <- iguane[iguane$sex == "F", ]

#' 
#' On formate les données. 
## ----------------------------------------------------------------------------------
iguane_secr <- unRMarkInput(iguane) # on convertit au bon format
summary(iguane_secr) # resumes

#' 
#' Les deux sexes ensemble.
## ----------------------------------------------------------------------------------
closure.test(iguane_secr, SB = TRUE)

#' 
#' Les modèles maintenant. On commence par le jeu de données avec les deux sexes ensemble.
#' 
## ----------------------------------------------------------------------------------
iguane.proc <- process.data(iguane, 
                            begin.time = 1, 
                            model = "FullHet")
iguane.ddl <- make.design.data(iguane.proc)

#' 
#' Liste des modèles.
## ----------------------------------------------------------------------------------
run.iguane <- function() {

  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ mixture + time)

  iguane.model.list <- create.model.list("FullHet")
  
  iguane.results <- mark.wrapper(iguane.model.list,
                              data = iguane.proc, 
                              ddl = iguane.ddl)
                              
  return(iguane.results)
}

#' 
## ----------------------------------------------------------------------------------
iguane.results <- run.iguane()

#' 
## ----------------------------------------------------------------------------------
iguane.results

#' 
## ----------------------------------------------------------------------------------
names(iguane.results)

#' 
## ----------------------------------------------------------------------------------
iguane.results$p.dot$results$real
iguane.results$p.dot$results$derived

#' 
#' En séparant les sexes. Femelles, puis mâles. 
#' 
## ----------------------------------------------------------------------------------
iguane.proc <- process.data(iguaneF, begin.time = 1, model = "FullHet")
iguane.ddl <- make.design.data(iguane.proc)

#' 
#' Liste des modèles.
## ----------------------------------------------------------------------------------
run.iguane <- function() {

  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ mixture + time)

  iguane.model.list <- create.model.list("FullHet")
  
  iguane.results <- mark.wrapper(iguane.model.list,
                              data = iguane.proc, 
                              ddl = iguane.ddl)
                              
  return(iguane.results)
}

#' 
## ----------------------------------------------------------------------------------
iguane.results <- run.iguane()

#' 
## ----------------------------------------------------------------------------------
iguane.results

#' 
## ----------------------------------------------------------------------------------
names(iguane.results)

#' 
## ----------------------------------------------------------------------------------
iguane.results$p.dot$results$real
iguane.results$p.dot$results$derived

#' 
#' Les mâles maintenant. 
#' 
## ----------------------------------------------------------------------------------
iguane.proc <- process.data(iguaneM, begin.time = 1, model = "FullHet")
iguane.ddl <- make.design.data(iguane.proc)

#' 
#' Liste des modèles.
## ----------------------------------------------------------------------------------
run.iguane <- function() {

  p.dot <- list(formula =  ~ 1, share = TRUE)
  p.dot.behav <- list(formula =  ~ 1)
  p.time <- list(formula =  ~ time, share = TRUE)
  p.h <- list(formula = ~ mixture, share = TRUE)
  p.time.behav <- list(formula =  ~ time)
  p.h.behav <- list(formula =  ~ mixture)
  p.h.time <- list(formula = ~ time + mixture, share = TRUE)
  p.h.time.behav <- list(formula =  ~ mixture + time)


  iguane.model.list <- create.model.list("FullHet")
  
  iguane.results <- mark.wrapper(iguane.model.list,
                              data = iguane.proc, 
                              ddl = iguane.ddl)
                              
  return(iguane.results)
}

#' 
## ----------------------------------------------------------------------------------
iguane.results <- run.iguane()

#' 
## ----------------------------------------------------------------------------------
iguane.results

#' 
## ----------------------------------------------------------------------------------
names(iguane.results)

#' 
## ----------------------------------------------------------------------------------
iguane.results$p.dot$results$real
iguane.results$p.dot$results$derived

#' 
#' 
#' # Nettoyage
#' 
#' On supprime les fichiers temporaires. 
#' 
## ----------------------------------------------------------------------------------
rm(list = ls(all = TRUE))
cleanup(ask = FALSE)

#' 
