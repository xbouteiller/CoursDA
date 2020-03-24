# Script pour le tdm 4


# data DATAcater

# Les donnees du fichier DATAcater.csv presentent les resultats d'une experience mesurant la croissance de
# la tordeuse du ch^ene (Lymantria dispar) selon la concentration en Tanin de la feuille de ch^ene sur
# laquelle l'individu s'est nourri.
# Le protocole consistait a mesurer la taille des chenilles apres 10 jours d'elevage. La source de nourriture
# des chenilles provenait de feuille de ch^ene. Le lot de feuille pour une chenille donnee provenait d'un seul
# arbre. La concentration en tanin des feuilles a ete estimee a partir d'un prelevement sur 5 feuilles de chaque lot


# Chargez le jeu de donnees.

DATAs0 <- read.table("DATAcater.csv", header=T, sep=";", dec=",")
str(DATAs0)

# exploration des donnees graphiques

plot(DATAs0$CROISSANCE~DATAs0$TANIN,
      bty = "l", # type de cadre
      pch= 20, # type de point
      col="darkblue", # couleur des points
      xlab="Concentration en tanin des feuilles", # titre de x
      ylab="Taille des chenilles") # titre de y

abline'me'

# Demarche statistique

  #1/ Question bio: existe-t-il une relation entre la taille des chenilles et la concentration en tanin des feuilles
  
  #2/ Hypothèses:
#H0: pas de relation linéaire entre X et Y
#H1: il existe une relation entre X et Y
  
  
  #3/ modele de regression lineaire
  # 2 variables quantitatives
  
  #modele
  
  res <- lm(CROISSANCE~TANIN, data = DATAs0) # construction du modele
  
  #validation du modele
  hist(res$residuals)
  shapiro.test(res$residuals)
  
  par(mfrow = c(2,2))
  plot(res)
  # normalite des residus : qqplot ok
  #homoscedasticité : satisfaisant
  # les résidus doivent:
  #avoir la même variance
  #ils sont indépendants 
  #issus d'une loi normale
  
  # Le premier graphique (haut-gauche) montre les residus sur l'axe y en fonction des valeurs predites du modele. 
  # Cela demande une certaine experience et une certaine pratique pour interpreter ses graphiques 
  # mais ce qu'il ne faut pas voir est une structure particuliere, un arrangement particulier,
  # des points sur le graphique. La distribution des points doit apparaitre aleatoire autour 
  # d'une courbe de pente nulle, un peu comme un ciel etoile lors d'une nuit clair. 
  # Le second graphique (haut-droite) montre le qqplot qui doit ^etre une ligne droite si les residus sont 
  # normalement distribues. Si la distribution des points suit une courbe en S ou a une forme de U alors 
  # il sera necessaire d'ajuster un modele different. Le troisieme graphique (bas-gauche) est une variante du premier, 
  # mais a une autre echelle. Si il y a de l'heteroscedasticite dans le modele, comme une variance qui augmente avec la moyenne,
  # alors la distribution des points prendra la forme d'un triangle. Le quatrieme graphique (bas-droite)
  # est utile pour reperer les points qui ont le plus d'inuence sur l'estimation des parametres de la regression
  # (le nombre associe a ses points correspond a leur position dans le vecteur de valeur et donc a leur numero de ligne dans le jeu 
  # de donnees). Ils s'agit alors d'engager une reection sur la facon de considerer les valeurs extremes, 
  # les valeurs aberantes ou encore nommees en anglais les outliers. 
  # 
  
  #interprétation du modèle
  #exploration du modele
  res$coefficients
  res$fitted.values
  res$residuals
  
  par(mfrow = c(1,1))
  plot(DATAs0$CROISSANCE~DATAs0$TANIN,
      bty = "l", # type de cadre
      pch= 20, # type de point
      col="darkblue", # couleur des points
      xlab="Concentration en tanin des feuilles", # titre de x
      ylab="Taille des chenilles") # titre de y
  lines(res$fitted.values ~ DATAs0$TANIN, col = 'red', lwd = 2)
  X = 0:15; X
  b = res$coefficients[1]
  a = res$coefficients[2]; b
  Y = a*X+b
  lines(Y ~ X, col = 'green', lwd = 4)
  points(Y ~ X, col = 'red', pch = 20)

  
  res
  anova(res)
  summary(res)
  
  summary(res)$r.squared
  attributes(summary(res))
  #conclusion statistique
  #p < 0.05
  #rejet d'H0
  
  #repondse a la question bio
  
  
  #test de correlation
  
  #verifier normalite des donnees
  #ici ok (qqplot)
  
  #test de correlation
  #H0 r = 0
  #H1 r != 0
  
  ?cor.test()
  cor.test(DATAs0$CROISSANCE,DATAs0$TANIN, method = 'pearson')
  (-0.4691613)^2
  
  
  

  
  
  
  
  #4/ modele validé conclusion du test
    #4A/ retrouvons "manuellement le résultat du test"
    # Calculons les différentes sources de variations
par(mfrow = c(1,1))
  
plot(DATAs0$CROISSANCE~DATAs0$TANIN,
       bty = "l", # type de cadre
       pch= 20, # type de point
       col="darkblue", # couleur des points
       xlab="Concentration en tanin des feuilles", # titre de x
       ylab="Taille des chenilles") # titre de y
  
#moyenne generale = modele nul
abline(h=mean(DATAs0$CROISSANCE), col = 'red', lwd = 2)

#droite ajustee
abline(res, col = 'green', lwd = 2)

# pour une point (ex le point 15)

point_to_print= 15
point_to_print= c(15,20,25,30,35,40)

#var totale
segments(x0= DATAs0$TANIN[point_to_print]-0.1 , y0= DATAs0$CROISSANCE[point_to_print],
         x1= DATAs0$TANIN[point_to_print]-0.1 , y1 = mean(DATAs0$CROISSANCE) , lwd = 2,  col = 'red')


#var expliquée
segments(x0= DATAs0$TANIN[point_to_print]+0.1 , y0= res$fitted.values[point_to_print],
         x1= DATAs0$TANIN[point_to_print]+0.1 , y1 = mean(DATAs0$CROISSANCE) , lwd = 2,  col = 'green')

#var résiduelle
segments(x0= DATAs0$TANIN[point_to_print]+0.1 , y0=  DATAs0$CROISSANCE[point_to_print],
         x1= DATAs0$TANIN[point_to_print]+0.1 , y1 = res$fitted.values[point_to_print] , lwd = 2,  col = 'cyan')


  
    #variation totale (ici en rouge)

SCET <- sum((DATAs0$CROISSANCE - mean(DATAs0$CROISSANCE))^2) ; SCET

  #variation expliquée
SCER <- sum((res$fitted.values - mean(DATAs0$CROISSANCE))^2) ; SCER


  #Variation residuelle
SCEE <- sum((res$fitted.values - DATAs0$CROISSANCE)^2);  SCEE

  #statistique du test

Fcalc = (SCER/1) / (SCEE /(nrow(DATAs0)-2)) ; Fcalc

  #comparer Fcalc à Fcritique (table) et conclure sur le test

Fcritique = 3.96
Fcalc > Fcritique

#on rejette H0 et on conclue qu'il existe un lien entre les deux variables


    #4B retrouvons les conclusions du test graca à R

anova(res) # retouver dans le tableau les elements que nous avons calculé manuellement

summary(res) # pour aller plus loin dans la compréhension du modèle significativité des coeff
#H0 le coeff est nul
#h1 le coeff est diff de 0

a = -0.9328 # ou res$coefficients[2]
b = 61.1303 # ou res$coefficients[1]

x = seq(0,15, by = 0.1)

lines(x , a*x + b, col = "orange" , lwd = 3)






# exercice 2 comprendre la significativité des coefficients et l'intensité d'une relation


X = seq (0,100, by = 1)
Y1 = X
Y2 = X + rnorm(n = length(X), mean = 1 , sd = 2)
Y3 = X + rnorm(n = length(X), mean = 1 , sd = 10)
Y4 = rnorm(n =length(X), mean = 1 , sd = 2)

par(mfrow= c(2,2))
plot(Y1 ~ X, pch= 20,col="darkblue")
plot(Y2 ~ X, pch= 20, col="darkblue")
plot(Y3 ~ X, pch= 20, col="darkblue")
plot(Y4 ~ X, pch= 20, col="darkblue")

par(mfrow= c(1,1))

lm1 <- lm(Y1 ~ X)
lm2 <- lm(Y2 ~ X)
lm3 <- lm(Y3 ~ X)
lm4 <- lm(Y4 ~ X)

anova(lm1); summary(lm1) 
anova(lm2); summary(lm2) 
anova(lm3); summary(lm3) 
anova(lm4); summary(lm4) 

names(lm1)
summary(lm1)$r.squared


par(mfrow= c(2,2))
plot(Y1 ~ X, pch= 20, col="darkblue") ; legend("topleft", legend = paste("R2 = " , signif(summary(lm1)$r.squared,3) ),bty = "n", cex = 2)
                                               
plot(Y2 ~ X, pch= 20, col="darkblue") ; legend("topleft", legend = paste("R2 = " , signif(summary(lm2)$r.squared,3) ),bty = "n", cex = 2)
                                                                     
plot(Y3 ~ X, pch= 20, col="darkblue") ; legend("topleft", legend = paste("R2 = " , signif(summary(lm3)$r.squared,3) ),bty = "n", cex = 2)
                                                                     
plot(Y4 ~ X, pch= 20, col="darkblue") ; legend("topleft", legend = paste("R2 = " , signif(summary(lm4)$r.squared,3) ),bty = "n", cex = 2)

# exercices suivants : à vous de jouer


# DF meteo

#import
meteo <- read.table("meteo.csv", header = TRUE, sep = ";", dec = ".")
str(meteo)
                           
#figure
plot(Precipitations ~ Latitude  , data = meteo)

#modele
lm_met <- lm(Precipitations ~ Latitude  , data = meteo)

#residus
par(mfrow = c(2,2))
plot(lm_met)

# test
anova(lm_met)
summary(lm_met)


# question 2

#figure
plot(Temperature_an ~ Latitude, data = meteo)

#modele
lm_met <- lm(Temperature_an ~ Latitude, data = meteo)

#residus
par(mfrow = c(2,2))
plot(lm_met)

# test
anova(lm_met)
summary(lm_met)

# QUESTION 3


plot(Temperature_an ~ Geographie, data = meteo)

lm_met2 <-aov(Temperature_an ~ Geographie, data = meteo)

#residus
par(mfrow = c(2,2))
plot(lm_met2)

# test
anova(lm_met2)
summary(lm_met2)
par(mfrow = c(1,1))
plot(TukeyHSD(lm_met2))



## exercice 2


#1: importation

crime <- read.table("crime.csv", 
					header = TRUE, 
					sep = ";", 
					dec = ".")
str(crime)
summary(crime)

# attention il faudra recoder les variables qualit en facteurs

#2 : question
# Question 1 :
# -	A) existe-il une relation entre le taux de 
# criminalité (CrimeRate, variable dépendante) 
# et le taux d’homme 
# (nombre d’homme pour 1000 femmes) 
# (Males, variable indépendante) ?

#3/ visualisation
# 2 variables quantitatives

crime[1:10,3:5]
crime[crime[,'CrimeRate']>60,4:6]

summary(crime$CrimeRate)
summary(crime[,'CrimeRate'])

summary(crime$Males)
summary(crime[,'Males'])

hist(crime[,'CrimeRate'], col = 'cyan')
hist(crime[,'Males'], col = 'red')

plot(crime[,'CrimeRate']~crime[,'Males'],
	pch = 19,
	col = 'red',
	main = 'mon titre',
	xlab = 'tx d\'homme',
	ylab = 'tx de criminalite',
	xlim = c(900, 1100))
	
# Demarche statistique
#a: question bio
#b: choix du test
#- test de correlation
#- regression lineaire

#c: hypotheses
#H0 pas de lien
#H1 il y a un lien 

#d: appliquer le modele
lm1 = lm(CrimeRate ~ Males, data = crime)

#e: valider le modele
par(mfrow = c(2,2))
plot(lm1)
par(mfrow = c(1,1))

# normalite des residus : qqplot ici ok
# homoscedasticité : ici a l'air ok

#f: intepreter le modele
lm1$coefficients

plot(crime[,'CrimeRate']~crime[,'Males'],
	pch = 19,
	col = 'red',
	main = 'mon titre',
	xlab = 'tx d\'homme',
	ylab = 'tx de criminalite')
	
x = 900:1100
alpha = lm1$coefficients[1];alpha
beta = lm1$coefficients[2];beta
y = alpha + beta*x

lines(y~x, lwd = 2, col = 'blue')

lm1$fitted.values
lm1$residuals

anova(lm1)
summary(lm1)


#H0: r=0
#H1: r !=0

cor.test(crime$CrimeRate, crime$Males, method = 'pearson')
0.1571129 *0.1571129 

# on ne rejette pas H0 (p>0.05)

#h. il n'existe pas de lien entre les 2 variables


# test de correlation
#H0 r = 0
#H1 r != 0
cor.test(crime[,'CrimeRate'],crime[,'Males'],
		method = 'pearson')
		
r = 0.1571129
r^2



























crime$Southern <- as.factor(crime$Southern)


plot(CrimeRate ~ Southern, data = crime)
t.test(CrimeRate ~ Southern, data = crime)


plot(CrimeRate ~ Wage, data = crime)
lmc <- lm(CrimeRate ~ Wage, data = crime)
par(mfrow = c(2,2))
plot(lmc)

anova(lmc)
summary(lmc)


t.test(crime$CrimeRate, crime$CrimeRate10, paired = TRUE)
