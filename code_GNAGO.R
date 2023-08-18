
#Projet methodes d'�chantillonnage

#Vider la mémoire
rm(list = ls())

# Importation des fichiers 
library(readxl)


data_proj<-  read_excel("~/SEP2/SEP0832 meth d'echantillonage/NEW_TD/data.xls", 
                        sheet = "DEP")
View(data_proj)
#Suppression des 2 premieres colonnes
data_proj<-data_proj[,c(-1)]
data_proj<-data_proj[,c(-1)]
View(data_proj)

names(data_proj)
str(data_proj)

edit(data_proj)

         ## Modèle de régression linéaire multiple

library(ISLR)
library(lmtest)
modele.RLM <- lm(formula = Evol_trimestriel  ~ ., data = data_proj)

attributes(modele.RLM)
summary(modele.RLM)
summary(modele.RLM)$coefficients

#la non-corrélation des erreurs
acf(modele.RLM$residuals)

#Vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives;
#graphiquement :
plot(modele.RLM, 1)

#Vérifier l'hypothèse d'homoscedasticité des erreurs;
#graphiquement :
plot(modele.RLM, 3)

#Tester la non-corrélation (d'ordre 1) des erreurs : test de Durbin-Watson
dwtest(modele.RLM, alternative = c("two.sided"))  #on obtient P_value = 0.2515, on accepte donc H_0, i.e., les erreurs sont non corrélées



#Test d'homoscedasticité e Breusch-Pagan
require(lmtest)
bptest(modele.RLM, studentize = FALSE) #on obtient P_value = 0.002576, on rejete donc H_0, 
#Le terme d'erreur n'est pas homoscedasticique

# vérifier la normalité des erreurs
  #normal Q-Q plot
plot(modele.RLM, 2)
  #histogramme versus densité normale
residus <- modele.RLM$residuals
hist(residus, freq = FALSE, ylim = c(0,6), 
     main = "Histogramme des résidus")
curve(dnorm(x, mean = mean(residus), sd = sd(residus)), 
      col = 2, lty = 2, lwd = 2, add = TRUE) #Il semble avoir une normalité des erreurs
    
#Test de Shapiro-Wilk pour tester l'hypothèse de normalité du terme d'erreur
shapiro.test(residuals(modele.RLM)) #on obtient une P_value = 0.9593, on accepte donc H_0, i.e., d'où le terme d'erreur est normale

#Ordonner les variables explicatives numériques selon les valeurs des p_values croissantes (du test de Student)
modele.RLM <- lm(formula = Evol_trimestriel  ~ ., data = data_proj)
summary(modele.RLM)
vect.pvalues.Student <- summary(modele.RLM)$coefficients[,"Pr(>|t|)"]
#On supprime la p_value de l'intercept 
vect.pvalues.Student <-vect.pvalues.Student[2:length(vect.pvalues.Student)] 
#Variables explicatives ordonnées, de la plus significative à la moins significative
sort(vect.pvalues.Student) 

#Ordonner les variables explicatives numériques selon les valeurs des P_values croissantes du test de Fisher
tests.Fisher <- anova(modele.RLM)
tests.Fisher #idéal car ici toutes les vaibles sont quanti
str(tests.Fisher)
m <- nrow(tests.Fisher)
vect.pvalues.Fisher <- tests.Fisher[1:m-1,"Pr(>F)"] #Extrait le vecteur des p_values
names(vect.pvalues.Fisher) <- rownames(tests.Fisher[1:m-1,])
sort(vect.pvalues.Fisher)
#tertiaire_marchand ,agriculture, tertiaire_non_marchand  ,industrie , construction 
# conclusion de l'Ordonnement les variables explicatives de la plus significative à la moins significative
sort(vect.pvalues.Student)
sort(vect.pvalues.Fisher) #idéal

    ##algorithmes de selection de modeles/variables en régression
XX <- model.matrix(Evol_trimestriel ~., data = data_proj) #Matrice de design(contenant que des variables quanti)
View(XX)
p <- ncol(XX)-1 
p #Nombre de variables numériques explicatives dans le modéle de RLM complet
library(leaps)
#sélection de variables explicatives numériques(Avec 1 variable explicatives) 
select.modeles_1 <- regsubsets(Evol_trimestriel ~ ., data = data_proj, 
                             nbest = 1, nvmax = p,method = "exhaustive") #on enleve la variable Nbre_demplois_salaries 
summary(select.modeles_1) #le meilleur modele parmi tous les modeles avec 1 variable  axplicatives contient la variable tertiaire_marchand
        #selon les critéres
par(mfrow=c(1,3))
plot(select.modeles_1, scale = "adjr2",main="R2 adjust�")#R2_adjusté
plot(select.modeles_1, scale = "bic",main="BIC")
plot(select.modeles_1, scale = "Cp",main="cp")

#sélection de variables explicatives numériques(Avec 2 variable explicatives) )
select.modeles_2 <- regsubsets(Evol_trimestriel ~ ., data = data_proj, 
                               nbest = 2, nvmax = p,method = "exhaustive")  
summary(select.modeles_2) #les meilleurs modeles parmi tous les modeles avec 2 variables axplicatives contient les variables (tertiaire_marchand et agriculture)
        #selon les critéres
par(mfrow=c(1,3))
plot(select.modeles_2, scale = "adjr2",main="R2 adjusté")#R2_adjusté
plot(select.modeles_2, scale = "bic",main="BIC")
plot(select.modeles_2, scale = "Cp",main="cp")


#Autre methode de sélection de variables explicatives numérique 
require(glmulti)
require(rJava)
#le MDR simple Evol_trimestriel = ax1 +bx2 +cx2 +.......  (level = 1)
select.modele.aic_1 <- glmulti(Evol_trimestriel  ~., data = data_proj, level = 1, 
                             fitfunction = lm, crit = "aic", plotty = FALSE,method = "h") #level= 1 considere le MDR simple sales= ax1 +bx2 +cx2 +.......
modele.opt.aic_1<- summary(select.modele.aic_1)$bestmodel
modele.opt.aic_1
anova(lm(modele.opt.aic_1, data = data_proj))

select.modele.bic_1 <- glmulti(Evol_trimestriel ~., data = data_proj, level = 1,
                             fitfunction = lm, crit = "bic", plotty = FALSE, method = "h")
modele.opt.bic_1 <- summary(select.modele.bic_1)$bestmodel
modele.opt.bic_1
anova(lm(modele.opt.bic_1, data = data_proj))
# conclusion: Evol_trimestriel ~ "1+ Nbre_demplois_salaries + agriculture + industrie + construction + tertiaire_marchand + tertiaire_non_marchand"

#le MDR avec des interactions par paire Evol_trimestriel = ax1.x2 +bx2.x3 +.......  (level = 2)
select.modele.aic_2 <- glmulti(Evol_trimestriel  ~., data = data_proj, level = 2, 
                               fitfunction = lm, crit = "aic", plotty = FALSE, method = "h") #level= 1 considere le MDR simple sales= ax1 +bx2 +cx2 +.......
modele.opt.aic_2<- summary(select.modele.aic_2)$bestmodel
modele.opt.aic_2
anova(lm(modele.opt.aic_2, data = data_proj))

select.modele.bic_2 <- glmulti(Evol_trimestriel ~., data = data_proj, level = 2,
                               fitfunction = lm, crit = "bic", plotty = FALSE, method = "h")
modele.opt.bic_2 <- summary(select.modele.bic_2)$bestmodel
modele.opt.bic_2
anova(lm(modele.opt.bic_2, data = data_proj))

             ## Méthode K-fold CV ##
modele1 <- glm(formula =Evol_trimestriel ~ agriculture+industrie +construction+tertiaire_marchand +tertiaire_non_marchand, data = data_proj)
modele2 <- glm(formula =Evol_trimestriel ~agriculture+industrie +tertiaire_marchand +tertiaire_non_marchand, data = data_proj)  
modele3  <- glm(formula =Evol_trimestriel ~agriculture+industrie+tertiaire_marchand+tertiaire_non_marchand+ construction:agriculture+tertiaire_marchand:industrie +tertiaire_non_marchand:tertiaire_marchand, data = data_proj)
library(boot)
n=nrow(data_proj)
estimation_erreur_modele1 <- cv.glm(data = data_proj, glmfit = modele1, K = 10)$delta[1] #estimation de l'erreur du modele1 
estimation_erreur_modele2 <- cv.glm(data = data_proj, glmfit =  modele2, K = 10)$delta[1] #estimation de l'erreur du modele2 
estimation_erreur_modele3 <- cv.glm(data = data_proj, glmfit = modele3, K = 10)$delta[1] #estimation de l'erreur du modele3 

print(c("R�sultats des estimations par 10-fold CV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation_erreur_modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2)),
        paste("Estimation de l'erreur du modele3 = ", as.character(estimation_erreur_modele3)))) 
