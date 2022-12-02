#vider la mémoire

rm(list = ls())


#Library
library("DescTools")# Pour la visualisation des données manquantes
library(dplyr)
library(tidyverse) #Pour ggplot
library(nnet)
library('Hmisc') #pour describe

library(MASS)




# chemin vers le répertoire courant où est placé le fichier de données
setwd("C:/Users/ousma/Desktop/M2SEP/Gestion de Projets digitalisés/Projet CO2")


## Importation du fichier
CO2 <- read.csv2("bdd CO2.csv", stringsAsFactors=TRUE)




#vérifications
str(CO2)
View(CO2)
nrow(CO2)
ncol(CO2)
#quelques statistiques
summary(CO2)




# calculating the product of dimensions of dataframe 
totalcells_base = prod(dim(CO2)) #nombre total de cellule est de 1431144

# calculating the number of cells with na
missingcells = sum(is.na(CO2)) #nombre de cellule na est de 58307

# calculating percentage of missing values
(missingcells * 100 )/(totalcells_base) #pourcentage de na est de 4.07

csdcnj sd,sd,



####################################################################################################################################
#### Correction de la base####
####################################################################################################################################

#Faute de frappe COMBISPACE
# unique(CO2$Carrosserie)
# summary(CO2$Carrosserie)
CO2$Carrosserie[CO2$Carrosserie == "COMBISPCACE"] <- "COMBISPACE"
CO2 %>% 
  filter(!(Carrosserie == "COMBISPCACE")) -> CO2


#NA entre hc nox et hcnox
CO2 %>% 
  mutate(hc_nox = ifelse(!is.na(hc) & !is.na(nox),hc + nox,hcnox)) %>% 
  dplyr::select(-hcnox, -hc, -nox) -> CO2


#Correction date ???????
str(CO2)




# calculating the product of dimensions of dataframe 
totalcells = prod(dim(CO2)) #nombre total de cellule est de 1321056

# calculating the number of cells with na
missingcells = sum(is.na(CO2)) #nombre de cellule na est de 3148

# calculating percentage of missing values
(missingcells * 100 )/(totalcells) #pourcentage de na est de 0.24

#####MOdif sur la base#####
CO2 %>% 
  mutate(var_co2 = ifelse(co2 < 140,"faible",ifelse(co2 < 200, "moyenne","forte"))) %>% #voir critère
  dplyr::select(-co2, -dscom, -hybride,-date_maj, -conso_mixte, -champ_v9, -lib_mod_doss, -lib_mod, -cnit, -tvv) -> CO2_mod


str(CO2_mod)
colnames(CO2_mod)
miss <- data.frame(CO2_mod)
colnames(miss) <- c("la marque", "le type de carburant",
                    "la puissance administrative","la puissance maximale (en kW)",
                    "le type de boîte de vitesse et le nombre de rapports",
                    "consommation urbaine de carburant (en l/100km)",
                    "consommation extra urbaine de carburant (en l/100km)",
                    "le résultat d’essai de CO type I",
                    "les résultats d’essai HC+NOX",
                    "le résultat d’essai de particules",
                    "la masse en ordre de marche mini",
                    "la masse en ordre de marche maxi",
                    "Carrosserie","gamme",
                    "Statut de pollution par émission de CO2 (en g/km)")
# View(miss)

View(miss)

####Données manquantes :
length(which(is.na(miss)))
# PlotMiss(miss, main = "Données manquantes")
#en baton rouge c'est une données manquantes

na.omit(CO2_mod) -> data_co2  #On retire toutes les données manquantes !
as.factor(data_co2$var_co2) -> data_co2$var_co2
# colnames(data_co2)
ncol(data_co2)
ncol(CO2)
# View(data_co2)
# describe(data_co2)
nrow(data_co2)
nrow(CO2)

totalcells_mod = prod(dim(data_co2)) #nombre total de cellule est de 993322
((totalcells_base -totalcells_mod )*100)/ totalcells_base #on a retiré 45.1% de la base initial

totalcells = prod(dim(CO2_mod)) #nombre total de cellule est de 993322
((totalcells -totalcells_mod )*100)/ totalcells #on a retiré 4.9% par sélection de lignes sans données manquantes sur la base conennant certaines colonnes

###Echantillonnage sur les moyennes et fortes####

set.seed(12345) #Fixer le générateur

forte <- sample_n(data.frame(subset(data_co2, var_co2 == "forte")), 3613)
forte
nrow(forte)
moyenne <- sample_n(data.frame(subset(data_co2, var_co2 == "moyenne")), 3613)
moyenne
nrow(moyenne)
faible <- data.frame(subset(data_co2, var_co2 == "faible"))
faible
nrow(faible)

Co2data <- rbind(forte,moyenne,faible)

Co2data1 <- Co2data[sample(nrow(Co2data)),]
view(Co2data1) 


summary(Co2data1)
str(Co2data1)





#### Forêts aléatoires ####
set.seed(5678)
perm <- sample(10839,8000)
app <- Co2data1[perm,] #échantillon d'apprentissage
valid <- Co2data1[-perm,] #échantillon de validation 

#Exporter les bases pour python

write.table(app,"Apprentissage.csv",sep=";",row.names=FALSE)
write.table(valid,"Validation.csv",sep=";",row.names=FALSE)
write.table(Co2data1,"Complete.csv",sep=";",row.names=FALSE)

## 2. Construire et analyser une forêt aléatoire
library(randomForest)
set.seed(1234)
foret <- randomForest(var_co2~., data=app) 
foret  #ntree = 500 par défaut (nombre d'arbres, ou des échantillon bootstrap)
#mtry = 7 (par défaut = partie entière de la racine carré du nombre de variables explicatives, en classif.) 
#erreur de classif estimée par la méthode OOB (Out Of Bag)

## 3. Sélectionner les paramètres de la forêt
plot(foret)
tail(foret$err.rate) # les 6 dernieres erreurs de classif, peut de différence entre ces valeurs, donc le choix ntree = 500 est suffisant. 
grille.mtry <- data.frame(mtry=seq(from=1,to=15, by = 3)) #grille des choix du paramètre mtry
library(caret)
ctrl <- trainControl(method="oob") #method oob : Out Of Bag
set.seed(1234)
sel.mtry <- train(var_co2~., data=app, method="rf", trControl=ctrl, tuneGrid=grille.mtry)
sel.mtry
sel.mtry$bestTune
plot(sel.mtry)
####La forêt Optimale

set.seed(1234)
foret_opt <- randomForest(var_co2~., data=app, mtry=as.numeric(sel.mtry$bestTune)) 
foret_opt
plot(foret_opt)

tail(foret_opt$err.rate)


## 4. Faire de la prévision

set.seed(1234)
foret_opt <- randomForest(var_co2~., data=app, mtry=as.numeric(sel.mtry$bestTune))

prev.valid <- predict(foret_opt, newdata=valid)
prev.valid[1:30]

prob.valid <- predict(foret_opt, newdata=valid, type="prob")

prob.valid[1:30,]

## 5. Estimer les performances de la forêt
#on compare les deux forêts mtry = 3 (la foret par defaut) et mtry = 13 (la foret selectionnée ci-dessus) 
set.seed(1234)
foret <- randomForest(var_co2~., data=app, xtest=valid[,-15], ytest=valid[,15], keep.forest=TRUE) #mtry = 3
set.seed(1234)
foret_opt <- randomForest(var_co2~., data=app, mtry=as.numeric(sel.mtry$bestTune), 
                          xtest=valid[,-15], ytest=valid[,15], keep.forest=TRUE)
foret
foret_opt


nrow(valid)

###Performances
valid$predicted <- predict(foret_opt, valid)
View(valid)

actual <- valid$var_co2
pred <- valid$predi

library(caret)
#F score
table(actual, pred)
confusionMatrix(pred, actual, mode = "everything", positive="1")


1-0.9954 #erreur de classif

## 6. Interpréter la forêt aléatoire

#forêt par défaut
var.imp <- foret$importance

ord <- order(var.imp, decreasing=TRUE)
barplot(sort(var.imp, decreasing = TRUE)[1:10], names.arg=rownames(var.imp)[ord][1:10], cex.names=0.6)

#forêt optimale

var.imp <- foret_opt$importance
ord <- order(var.imp, decreasing=TRUE)

barplot(sort(var.imp, decreasing = TRUE)[1:10],
        names.arg=rownames(var.imp)[ord][1:10], col = 'blue',
        xlab= "Variables" ,
        ylab= "Importance",
       cex.names=0.6)

## 2. L’algorithme des plus proches voisins (kNN)
valid[,-c(1,2,5,12,13,15,16)] <- scale(valid[,-c(1,2,5,12,13,15,16)])
app[,-c(1,2,5,12,13,15,16)] <- scale(app[,-c(1,2,5,12,13,15,16)]) #on normalise le tableau des variables explicatives

#Test d'un modèle par défaut

library(class)
reg3ppv <- knn(train=app[,-c(1,2,5,12,13,15,16,17)], test=valid[,-c(1,2,5,12,13,15,16,17)], cl=app$var_co2, k=3) #modele kNN avec k=1
mean(reg3ppv!=valid$var_co2) #l'erreur de classification du 3-NN
## 3. Calibrer le paramètre k avec la fonction train() du package caret
grille.k <- data.frame(k=seq(from=1, to=30, by=1))
library(caret)
ctrl1 <- trainControl(method="LGOCV", number=1, index=list(1:8000)) #estimation par validation hold out
sel.k1 <- train(var_co2~., data=Co2data, method="knn", trControl=ctrl1, tuneGrid=grille.k)
sel.k1
sel.k1$bestTune
plot(sel.k1)
#on peut répéter le processus précédent plusieurs fois et faire une moyenne
M <- 10 #on répète la méthode LGOCV M fois
ctrl <- trainControl(method="LGOCV", number=M)  #évaluation de l'erreur par la méthode apprentissage/validation (75%)
ctrl
#l'échantillonnage est fait de manière aléatoire
#la méthode est répétée M fois
sel.k <- train(var_co2~., data=Co2data1, method="knn", trControl=ctrl, tuneGrid=grille.k)
sel.k$bestTune
plot(sel.k)
getTrainPerf(sel.k)
#la méthode K-fold CV (K=10)
ctrl2 <- trainControl(method="cv", number=10)
sel.k2 <- train(var_co2~., data=Co2data, method="knn", trControl=ctrl2, tuneGrid=grille.k)
sel.k2
sel.k2$bestTune
plot(sel.k2)
#la méthode K-fold CV répétée M fois
M <- 2
ctrl3 <- trainControl(method="repeatedcv", number=10, repeats=M)
sel.k3 <- train(var_co2~., data=Co2data, method="knn", trControl=ctrl3, tuneGrid=grille.k)
sel.k3
sel.k3$bestTune
plot(sel.k3)

#Le 1-NN

Voisin <- knn(train=app[,-c(1,2,5,12,13,15,16)], test=valid[,-c(1,2,5,12,13,15,16)], cl=app$var_co2, k=1)
mean(Voisin!=valid$var_co2) #l'erreur de classification du 1-NN
### Performances du K-NN
valid$Vois <- Voisin
View(valid)
actual <- valid$var_co2
pred2 <- valid$Vois
pred2
library(caret)
#F score
table(actual, pred2)
confusionMatrix(pred2, actual, mode = "everything", positive="1")






