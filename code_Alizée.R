library(readxl)
data <- read_excel("data.xls")
View(data)

data1 <- data[,c(-1,-2)]

#####################################################
# Methode ascendante (forward selection, version 1) #
#####################################################
library(leaps)                 # utiliser fonction regsubset 
                               #fonction regsubsets() : sélection de variables explicatives numériques, fonction qui permet de comparer tous les modèles
# transformation des variables quali en quanti 
XX <- model.matrix( Evol_trimestriel ~ . , data = data1)  # Matrice de design
p <- ncol(XX)-1                                            # Nombre de variables numériques explicatives dans le modèle de RLM complet

#Forward selection, version 1 : fonction regsubsets()                   # pour selectionner modele opti 
select.mod.for <- regsubsets( Evol_trimestriel ~  . , data = data1, 
                             nbest = 1, nvmax = p, method = "forward")


summary(select.mod.for)$rsq       
summary(select.mod.for)$adjr2     # les r² ajusté des dfférents modèles
summary(select.mod.for)

# graphiques des différents critères de selction
par(mfrow = c(1,3))
plot(select.mod.for, scale = "bic", main = "Forward selection")   #critère BIC
plot(select.mod.for, scale = "Cp", main = "Forward selection")    #critère CP de Mallow
plot(select.mod.for, scale = "adjr2", main = "Forward selection") # critère du r² ajusté
plot(select.mod.for, scale = "r2", main = "Forward selection")    # critère du r²

# le meilleur modèle selon le BIC : tout sauf variable construction
# le meilleur modele pour le reste : toutes les variables


###################################################
##Forward selection, version 2 : fonction step()
###################################################
modele.trivial <- lm( Evol_trimestriel ~ 1, data = data1)      # avec seulement l'intercept
modele.complet <- lm( Evol_trimestriel ~ ., data = data1)      # avec toutes les variables explicatives dans le modele 
#selon AIC (k = 2)  
res.select.AIC.for <- step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = data1, direction = "forward", k = 2)      
# le modele opti pour le critere AIC est le modele complet 

#selon BIC (k = log(n))

n <- nrow(data1)
res.select.BIC.for <- step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = data1, direction = "forward", k = log(n))                # pour dir que c'est bic mettre k=log(n)

 step(modele.complet, data=data1,direction="backward", k=log(n))
# le modèle optimatl pour le BIC est le modele complet

#selon le crit?re de la statistique de Fisher
n <- nrow(data1)
res.select.F.for <- step(modele.trivial, 
                         scope = list(lower = modele.trivial, upper = modele.complet),
                         data = data1, direction = "forward", test = "F")

# méthode ascendante bidirectionnnelle

modele.trivial <- lm(Evol_trimestriel  ~ 1, data = data1)
modele.complet <- lm(Evol_trimestriel  ~ ., data = data1)  #modele le plus large
res.select.AIC.for.both <- step(modele.trivial, scope = list(lower = modele.trivial,upper = modele.complet), data = data1, direction = "both", k = 2)  
res.select.BIC.for.both <- step(modele.trivial, scope = list(lower = modele.trivial,upper = modele.complet), data = data1, direction = "both", k = log(n))  
res.select.F.for.both <- step(modele.trivial, scope = list(lower = modele.trivial,upper = modele.complet), data = data1, direction = "both", test="F")  



###############
# GENETIQUE 

select.mod.gen <- glmulti(Evol_trimestriel ~ ., data = data1, level = 2, method = "g", 
                          fitfunction = lm, crit = 'aic', plotty = F)
aic.best.model <- summary(select.mod.gen)$bestmodel
aic.best.model

#selon BIC
select.mod.gen <- glmulti(Evol_trimestriel ~ ., data = data1, level = 2, method = "g", 
                          fitfunction = lm, crit = 'bic', plotty = F)
bic.best.model <- summary(select.mod.gen)$bestmodel
bic.best.model

#################################
# Meth ensemble validation
################################
x11()
n <- nrow(data1)
M <- 2000             # nombre d'itérations qu'on souhaite    
erreur_modele1 = NULL # initialiser le vecteur qui contiendra toutes les valeurs des erreurs prédites
erreur_modele2 = NULL 
erreur_modele3 = NULL 
for (i in 1:M)
{
  indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)   # tirer au hasard un certain nombre de données, tirage sans remise
  ensemble_apprentissage <- data1[indices, ]    # les données numéros de ligne tiré seront dans la base d'apprentissage, le reste sera dans la base de test 
  ensemble_validation <- data1[ - indices, ]   # exclure les observations qu'on a deja utilisé
  modele1 <- lm( `Evol_trimestriel` ~ . , data=ensemble_apprentissage)  # reg linéaire de variable expliquée Evolution tri en fonction de toutes les autres 
  #modele2 <- lm( `Evol_trimestriel` ~ `agriculture` + `industrie`+ `tertiaire_marchand`+`tertiaire_non_marchand`, data=ensemble_apprentissage)  # reg linéaire de variable expliquée Evolution tri en fonction de toutes les autres 
  modele2 <- lm( `Evol_trimestriel` ~ `agriculture` + `industrie`+ `tertiaire_marchand`+`tertiaire_non_marchand` + `construction`:`agriculture` + `tertiaire_marchand`:`industrie` + `tertiaire_non_marchand`:`tertiaire_marchand` , data=ensemble_apprentissage)  # reg linéaire de variable expliquée Evolution tri en fonction de toutes les autres 
  modele3 <- lm( `Evol_trimestriel` ~ `agriculture` + `industrie`+ `construction` +`tertiaire_marchand`+`tertiaire_non_marchand` + `construction`:`agriculture` + `tertiaire_marchand`:`industrie` + `tertiaire_marchand`:`construction` + `tertiaire_non_marchand`:`agriculture` + `tertiaire_non_marchand`:`tertiaire_marchand`, data=ensemble_apprentissage)  # reg linéaire de variable expliquée Evolution tri en fonction de toutes les autres 
   # prédiction des valeurs
  valeurs_predites1 <- predict(object = modele1, newdata = ensemble_validation)  # predire données sur validation, de Evolution tri en utilisant les valeurs des autres variables 
  valeurs_predites2 <- predict(object = modele2, newdata = ensemble_validation)  # predire données sur validation, de Evolution tri en utilisant les valeurs des autres variables 
  valeurs_predites3 <- predict(object = modele3, newdata = ensemble_validation)  # predire données sur validation, de Evolution tri en utilisant les valeurs des autres variables 
  # estimation des erreurs
  erreur_modele1[i] <- mean((ensemble_validation$`Evol_trimestriel` - valeurs_predites1)^2, na.rm=T)
  erreur_modele2[i] <- mean((ensemble_validation$`Evol_trimestriel` - valeurs_predites2)^2, na.rm=T)
  erreur_modele3[i] <- mean((ensemble_validation$`Evol_trimestriel` - valeurs_predites3)^2, na.rm=T)
  
}
Err1 = NULL
Err2 = NULL
Err3 = NULL
for (m in 1:M)
{Err1[m] = mean(erreur_modele1[1:m])      # la moyenne de toutes les erreurs 
Err2[m] = mean(erreur_modele2[1:m]) 
Err3[m] = mean(erreur_modele3[1:m]) 
}
x11()
par(mfrow=c(1,3))
Erreur_prevision_modele1 = Err1[M] #meilleur estimation de l'erreur de prévision du modele1, donc la derniere composante des suites des moy
plot(Err1, main=c('modèle 1',paste0("Meilleur approximation de l'erreur : ",Erreur_prevision_modele1)),type = 'l',col="#0174DF" , xlab="Nombre d'itérations de l'algorithme ", ylab="Estmation de l'Erreur") # la suite des moy des estimations, par la LGN on sit que ça conv
Erreur_prevision_modele2 = Err2[M] #meilleur estimation de l'erreur de prévision du modele1, donc la derniere composante des suites des moy
plot(Err2, main=c('modèle 2',paste0("Meilleur approximation de l'erreur : ",Erreur_prevision_modele2)),type = 'l',col="#0174DF" , xlab="Nombre d'itérations de l'algorithme ", ylab="Estmation de l'Erreur") # la suite des moy des estimations, par la LGN on sit que ça conv
Erreur_prevision_modele3 = Err3[M] #meilleur estimation de l'erreur de prévision du modele1, donc la derniere composante des suites des moy
plot(Err3, main=c('modèle 3',paste0("Meilleur approximation de l'erreur : ",Erreur_prevision_modele3)),type = 'l',col="#0174DF" , xlab="Nombre d'itérations de l'algorithme ", ylab="Estmation de l'Erreur") # la suite des moy des estimations, par la LGN on sit que ça conv












