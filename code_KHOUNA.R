library(ISLR)
library(lmtest)
library(readxl)
library(boot) 


setwd("C:/Users/b/Documents/M1 S8/SEP0832 méthodes d'échantillonnage")
evol <- read_excel("evolution.xls")

#row.names(evol)<- data.frame(evol[,1])
evol <- evol[,-1]
evol <- evol[,-1]
View(evol)


modele_total <- lm(Evol_trimestriel ~ .,data=evol)
attributes(modele_total)
summary(modele_total)
summary(modele_total)$coefficients
summary(modele_total)$residuals


#la non-colinéarité des erreurs
#Graphiquement
acf(modele_total$residuals)
#Test de Durbin-Waston
dwtest(modele_total, alternative = c("two.sided"))
#
#
#Linéarité
#Graphiquement
plot(modele_total,1)
#
#
#homoscédicité
#Graphiquement
plot(modele_total,3)
#l'hypothèse n'est pas vérifiée. Pour remedier à ce problème:
evol$Evol_trimestriel <- sqrt(abs(evol$Evol_trimestriel))
mod_2 <- lm(Evol_trimestriel ~ .,data=evol)
plot(mod_2,3)
#Test de Breusch-Pagan
require(lmtest)
bptest(modele_total, studentize = FALSE)
#
#Normalité
#Graphiquement
plot(modele_total,2)
#histogramme versus densité normale
residus <- modele_total$residuals
hist(residus, freq = FALSE, 
     main = "Histogramme des résidus")
curve(dnorm(x, mean = mean(residus), sd = sd(residus)), 
      col = 2, lty = 2, lwd = 2, add = TRUE)
#Test de shapiro-Wilk
shapiro.test(residuals(modele_total))


#Classement des variables explicatives selon les valeurs des p-values croissantes du test de Fisher
tests.Fisher <- anova(modele_total)
tests.Fisher
str(tests.Fisher)
m <- nrow(tests.Fisher)
vect.pvalues.Fisher <- tests.Fisher[1:m-1,"Pr(>F)"] #Extrait le vecteur des p_values
names(vect.pvalues.Fisher) <- rownames(tests.Fisher[1:m-1,])
sort(vect.pvalues.Fisher)

#Points influents
par(mfrow=c(1,2))
plot(modele_total,4) #distance de Cook
abline(h=4/(100-5-1), col=c("blue"), lty = 2, lwd=3)
#Outliers et points leviers extrêmes
plot(modele_total,5)
abline(h=c(-3,3), v=2*(5+1)/100, col=c("blue","blue","green"),lty=c(2,2,2),lwd=c(3,3,3)) 


#Méthode de LOOCV: estimation de l'erreur
# ie, la K-fold CV avec K=n le nombre d'observations
modele1 <- glm(formula = Evol_trimestriel ~ agriculture+industrie +tertiaire_marchand +tertiaire_non_marchand, data = evol)
n=length(evol$Evol_trimestriel) #nombre d'observations
estimation_erreur_modele1 <- cv.glm(data = evol, glmfit = modele1, K = n)$delta[1]
estimation_erreur_modele1

modele2 <- glm(formula = Evol_trimestriel ~  agriculture + industrie + tertiaire_marchand + tertiaire_non_marchand + construction:agriculture + tertiaire_marchand:industrie + tertiaire_non_marchand:tertiaire_marchand , data = evol)
estimation_erreur_modele2 <- cv.glm(data = evol, glmfit = modele2, K = n)$delta[1]
estimation_erreur_modele2

modele3 <- glm(formula = Evol_trimestriel ~ 1 + agriculture + industrie + construction + tertiaire_marchand + tertiaire_non_marchand + construction:agriculture +  tertiaire_marchand:industrie + tertiaire_marchand:construction + tertiaire_non_marchand:agriculture + tertiaire_non_marchand:tertiaire_marchand , data = evol)
estimation_erreur_modele3 <- cv.glm(data = evol, glmfit = modele3, K = n)$delta[1]
estimation_erreur_modele3

print(c("Résultats des estimations par LOOCV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation_erreur_modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2)),
        paste("Estimation de l'erreur du modele3 = ", as.character(estimation_erreur_modele3))))
#l'erreur de prévision la plus faible est celle du modèle 1, donc c'est le modèle 1 qui prédit le mieux


