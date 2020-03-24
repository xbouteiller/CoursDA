#TDM 3 Anova


## Ex1 : rendement vigne

## tableau rendement vigne.csv
## ouvrir le tableau csv : 3 colonnes, 1: index, 2 var quanti continue, 3 : var quali nominale (facteur) 4 modalités



## Charger les données

Vigne <-read.table(
"rendements_vigne.csv", 
sep = ";", dec = ".", 
header = TRUE)

head(Vigne)
str(Vigne)
summary(Vigne)


## on s'interesse au rendement en fonction du ttmt
## Visualiser les données

boxplot(Vigne[,2] ~ Vigne[,3])
boxplot(Vigne$Rendement ~ Vigne$Pesticide)
boxplot(Vigne[,"Rendement"] ~ Vigne[,"Pesticide"])


## en plus
stripchart(Vigne$Rendement ~ Vigne$Pesticide, vertical = TRUE, 
		   pch = 20, xlim = c(0,5), col = "black",
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
         



##1/ Qestion bio: Question existe-t-il une différence significative de
##rendement moyen de la vigne selon les différents traitements ?


##/2 hypothèses
##H0 : toutes les moyennes sont identiques 
##H1 : au moisn une des moyennes testées est différentes significativement


##/3 Choix du test :
## variable quanti continu et var quali nominale (facteur) à 4 modalités : ANOVA
#ANOVA

##4/ Réalistation du test et validation des conditions d'utilisation

  ##a- ectiture du modele
  #modele lineaire sur R
  #aov() ou lm()
lm1 <- aov(Rendement ~ Pesticide, 
		  data = Vigne)
1.1354242-0.3165893

  ##b- validation du modele : normalité des residus
lm1$coefficients
lm1$fitted.values
lm1$residuals
Vigne$Rendement-(lm1$fitted.values+
                 lm1$residuals)
				 
    #Methode graphique
    hist(lm1$residuals)
    
    qqnorm(lm1$residuals)
    qqline(lm1$residuals)
    
    #test: H0' Normalité et H1' : non normalité
    shapiro.test(lm1$residuals) # conclusion


  #variance des résidus : varaince homogène H0'' et H1'' uniquement méthode graphique
  par(mfrow = c(2,2))
  plot(lm1)
  #modele valide
  #normalite des residus
  #homogeneite des variances

  ##c- test du modele
anova(lm1) # equivalent de aov(Rendement ~ Pesticide, data = Vigne)
summary(lm1)

##5/ interpretation du test
#on rejette H0
#p<0.05 
##6/ conclusion biologique
# une moyenne au moins est differente
# des autres

##/ que signifient les différentes choses dans le tableau de sortie de l'anova ?
## df
4 - 1
120-4

## SS 

MuVigne <- mean(Vigne$Rendement) ; MuVigne

n <- nrow(Vigne) ; n
k <- nlevels(Vigne$Pesticide) ; k

## SS et MS

## SCT = SCE + SCI

# totaux
(Vigne$Rendement - MuVigne)
(Vigne$Rendement - MuVigne)^2
SCT <- sum((Vigne$Rendement - MuVigne)^2) ; SCT
MSCT <- SCT  / (n-1) ; MSCT


#entre groupes
MuPest<-tapply(Vigne$Rendement , Vigne$Pesticide, mean); MuPest
table(Vigne$Pesticide)
SCE <- sum(table(Vigne$Pesticide)*(MuPest - MuVigne)^2) ; SCE


MSCE <- SCE / (k - 1) ; MSCE

# intra groupe

SCI <- SCT - SCE ; SCI
MSCI <- SCI / (n -k) ; MSCI

## F value

F <- MSCE  / MSCI ; F

## MS
4.9118 / 3
5.5680 / 116

## F value
1.6373 / 0.0480
##p < 0.05
## il existe au moins une des moyennes diff


## Graphique
par(mfrow = c(1,1))
AxeX <- seq(0, 50, by = 0.1 )
AxeY <- df(AxeX, k-1, n-k) # loi de Fsiher

Fcritique <- 3.07

plot(AxeY ~ AxeX, ty = "l", lwd = 2)

abline(v = Fcritique, lwd = 2, col = 'red')
abline(v = F, lwd = 2, col = 'green')


  ##7/ test post hoc


PostHoc <-TukeyHSD( lm1)

par(mfrow = c(1,1))
plot(PostHoc, las = 1, cex.axis = 0.75)

## exercice 2 
## exercice 3
## exercice 4

