library(readxl)
data<-read_excel("data.xls", sheet="DEP")
dataset<-data[,-c(1,2)]
p<-ncol(dataset)

require(glmulti)
#Backward elimination, version 1 : fonction regsubsets()
select.mod.bac <- regsubsets(Evol_trimestriel ~., data = dataset, 
                             nbest = 1, nvmax = p, method = "backward")
#Résultats pour le critère BIC des deux algorithmes
par(mfrow = c(1,2))
plot(select.mod.bac, scale = "bic", main = "Backward elimination")
#Résultas des deux algorithmes pour le critère Cp de Mallow 
par(mfrow = c(1,2))
plot(select.mod.bac, scale = "Cp", main = "Backward elimination")
#Résultats des deux algorithmes pour le critère du R2 ajusté 
par(mfrow = c(1,2))
plot(select.mod.bac, scale = "adjr2", main = "Backward elimination")

##Backward elimination, version 2 : fonction step()
modele.complet <- lm(Evol_trimestriel ~ ., data = dataset)
#selon AIC (k = 2)
res.select.AIC.bac <- step(modele.complet, data = dataset, direction = "backward", k = 2)
#selon BIC (k = log(n))
n <- nrow(dataset)
res.select.BIC.bac <- step(modele.complet, data = dataset, direction = "backward", k = log(n))
#selon le critère de Fisher
res.select.F.bac <- step(modele.complet, data = dataset, direction = "backward", test = "F")


#### Bootstrap ####
#modele 1
f_estimateurs_w <- function(data, index){return(coef(lm(formula = Evol_trimestriel ~ agriculture+industrie +tertiaire_marchand +tertiaire_non_marchand, data = data, 
                                                        subset = index)))}

f_estimateurs_w(data = dataset, index = 1:100)

f_estimateurs_w(data = dataset, index = sample(100, 100, replace = TRUE))

library(boot)
boot(data = dataset, statistic = f_estimateurs_w, R = 2000)

#comparaison avec les estimations des écarts-type données par la lm() 
modele <- lm(formula = Evol_trimestriel ~ agriculture+industrie +tertiaire_marchand +tertiaire_non_marchand, data = dataset)
summary(modele)

#modele 2
f_estimateurs_w <- function(data, index){return(coef(lm(formula = Evol_trimestriel~agriculture+industrie+tertiaire_marchand+tertiaire_non_marchand+construction:agriculture+tertiaire_marchand:industrie+tertiaire_non_marchand:tertiaire_marchand, data = data, 
                                                        subset = index)))}

f_estimateurs_w(data = dataset, index = 1:100)

f_estimateurs_w(data = dataset, index = sample(100, 100, replace = TRUE))

library(boot)
boot(data = dataset, statistic = f_estimateurs_w, R = 2000)

#comparaison avec les estimations des écarts-type données par la lm() 
modele <- lm(formula = Evol_trimestriel~agriculture+industrie+tertiaire_marchand+tertiaire_non_marchand+construction:agriculture+tertiaire_marchand:industrie+tertiaire_non_marchand:tertiaire_marchand, data = dataset)
summary(modele)

#Modele 3
f_estimateurs_w <- function(data, index){return(coef(lm(formula = Evol_trimestriel~agriculture + industrie + construction + tertiaire_marchand + tertiaire_non_marchand + construction:agriculture +  tertiaire_marchand:industrie + tertiaire_marchand:construction + tertiaire_non_marchand:agriculture + tertiaire_non_marchand:tertiaire_marchand, data = data, 
                                                        subset = index)))}

f_estimateurs_w(data = dataset, index = 1:100)

f_estimateurs_w(data = dataset, index = sample(100, 100, replace = TRUE))

library(boot)
boot(data = dataset, statistic = f_estimateurs_w, R = 2000)

#comparaison avec les estimations des écarts-type données par la lm() 
modele <- lm(formula = Evol_trimestriel~agriculture + industrie + construction + tertiaire_marchand + tertiaire_non_marchand + construction:agriculture +  tertiaire_marchand:industrie + tertiaire_marchand:construction + tertiaire_non_marchand:agriculture + tertiaire_non_marchand:tertiaire_marchand, data = dataset)
summary(modele)
