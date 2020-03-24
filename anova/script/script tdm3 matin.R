# debut du tdm 3

# Exercice 1


# importer le tableau
Vigne <- read.table("rendements_vigne.csv", 
                    sep = ";", header = TRUE, dec = ".")

summary(Vigne)
str(Vigne)


#representation graphique

boxplot(Vigne[,2] ~ Vigne[,3])
boxplot(Vigne$Rendement ~ Vigne$Pesticide)
boxplot(Vigne[,"Rendement"] ~ Vigne[,"Pesticide"])

# principe de l'anova

## en plus
stripchart(Vigne$Rendement ~ Vigne$Pesticide, vertical = TRUE, pch = 20, xlim = c(0,5), col = "black",
           xlab = "Traitement", ylab = "Rendement")

#moyenne générale
abline(h= mean(Vigne$Rendement), col = "red", lwd = 3)

#moyenne des groupes
tapply(Vigne$Rendement , Vigne$Pesticide, mean)
segments(x0= c(0.5,1.5,2.5,3.5),
         x1 = c(1.5,2.5,3.5, 4.5), 
         y0=tapply(Vigne$Rendement , Vigne$Pesticide, mean),
         y1=tapply(Vigne$Rendement , Vigne$Pesticide, mean),
         col = c("blue", "green", "orange", "purple"), lwd = 3)


# TEST 

##1/ Qestion bio: Question existe-t-il une différence significative de
##rendement moyen de la vigne selon les différents traitements ?


##2/ Hypothèses :
## H0:tous les rendements moyens sont identiques (qqsoit le ttmt)
## H1: au moins une des moyennes est différente des autres

##3/ choix du test : test param -> ANOVA
# (1 var qualitative à 4 modalités et une variable quantitative)

##4/ realsitation du test et validation du modele

  ##/ a/ ecriture du modele fonction aov()

lm1 <- aov(Rendement ~ Pesticide, data = Vigne)

  ##b/ validation du modele

lm1$coefficients
lm1$fitted.values
lm1$residuals
    ## normalite des residus

hist(lm1$residuals)

qqnorm(lm1$residuals)
qqline(lm1$residuals)

#test de shapiro
#H0' : loi N
#H1' : ne suit pas une loi N

shapiro.test(lm1$residuals) # rsidus suivent une loi normale

      ## homoscedasticité des résidus

par(mfrow = c(2,2))
plot(lm1)


    ## resultat du test

anova(lm1)




##6/ interpretation du test


## bonus comprendre le taleau de l'anova
## SS 

MuVigne <- mean(Vigne$Rendement) ; MuVigne
n <- nrow(Vigne) ; n
k <- nlevels(Vigne$Pesticide) ; k

## SCT somme des carres totaux

# totaux
Vigne$Rendement - MuVigne
sum(Vigne$Rendement - MuVigne)

(Vigne$Rendement - MuVigne)^2
SCT <- sum((Vigne$Rendement - MuVigne)^2) ; SCT

#somme des carres entre groupes
MuPest<-tapply(Vigne$Rendement , Vigne$Pesticide, mean); MuPest
SCE <- sum(table(Vigne$Pesticide)*(MuPest - MuVigne)^2) ; SCE

#somme des carres intra groupes (residus)
SCI <- SCT - SCE ; SCI

anova(lm1)


## mean square

4.9118/ 3
5.568/116

1.6373 /0.048


##7/ test post hoc


PostHoc <-TukeyHSD( lm1)
PostHoc


par(mfrow = c(1,1))
plot(PostHoc, las = 1, cex.axis = 0.75)



















