###Importation BdD et visualisation
setwd("C:/Nadege/ATERBioGeCo/Enseignement/Biostats/L3/TDM4/BdD")
meteo<-read.csv("meteo.csv", header=TRUE, sep=";",dec=".")
str(meteo)
plot(Latitude,Temperature_an, pch=16,cex=1.5, meteo)
###################L'objectif de l'�tude est d'�tudier la relation entre la variable r�ponse y (Temperature annuelle), en fonction de la variable x (la Latitude)


# Construction du modele de regression lineaire 
#Hypoth�se :
#H0 : La variable Temp�rature est lin�airement ind�pendante de la variable Latitu
#H1 : La variable Temp�rature est expliqu�e lin�airement par la variable Latitude
##### Test: Conditions d'application : aide � la d�cision avec des tests 

###Normalit� 


# Cr�ation du mod�le pour expliquer une variable quantitative par une variable quantitative (Regression lineaire)
mod<-lm(Temperature_an~Latitude, meteo)
res<-mod$residuals

#H0 : La distribution des r�sidus suit une loi normale
#H1 : La distribution des r�sidus ne suit pas une loi normale
#Risque alpha : 5% - 0.05

# Methode graphique
hist(res) 
qqnorm(res)
qqline(res)
# Tests pour aider � la decision
shapiro.test(res) 
mod$residuals
mod$fitted.values
###Homoc�dasticit� 
# Methode graphique
par(mfrow=c(2,2))
plot(mod)
#??? Choix du test, re-formulation des hypoth�ses et choix du risque alpha 

#Hypoth�se : 
#H0 : La variable Temp�rature est lin�airement ind�pendante de la variable Latitude
###Selon Fisher : coefficient de d�termination R�=0 -> fonction anova( ) et summary( )
###Selon Student : a = 0, la pente de la r�gression est nulle -> fonction summary( )
#H1 : La variable Temp�rature est expliqu�e lin�airement par la variable Latitude
###Selon Fisher : coefficient de d�termination R�???0 -> fonction anova( ) et summary( )
###Selon Student : a ??? 0, la pente de la r�gression n'est pas nulle -> fonction summary( )
#Risque ?? : 5% - 0.05 

#??? Calcul de la statistique associ�e et ??? Estimer la valeur critique de la statistique
#Calcul SCT et MSCT
X<-mean(meteo$Temperature_an)
X
SCT<-sum((X-meteo$Temperature_an)^2)
SCT

n<-length(meteo$Temperature_an)
n
MSCT<-SCT/(n-1)
MSCT

#Calcul SCR et MSCR
SCR <- sum((X - mod$fitted.values)^2)
SCR
MSCR<-SCR/1
MSCR
#Calcul SCE et MSCE
SCE <- sum((meteo$Temperature_an - mod$fitted.values)^2)
SCE
MSCE<-SCE/(n-2)
MSCE

#Calcul F
F=MSCR/MSCE
F

# Calcul p-value(F) 
PrF <- 1 - pf(F, df1=1, df2 = n-2)
PrF

#Rendu graphique
abscisse <- seq(0,130,l=1000)
ordonnee <- df(abscisse,1, n-2)
plot(x = abscisse, y=ordonnee, type="l")
abline(v=c(4.20),col="red",lwd=2)
abline(v=F,col="green",lwd=2)

#Regression lineaire pour pr�diction de valeurs � partir d'une variable qualitative 
############################################################
# M�thode informatique gr�ce � la fonction anova( )
anova(mod)

# M�thode informatique gr�ce � la fonction summary( ) 
summary(mod)

###Graphique
x11()
plot(meteo$Latitude, meteo$Temperature_an, pch=16,cex=1.5,
 xlab="Latitude", ylab="Temp�rature annuelle",ylim=c(0,25))
abline(mod, col="red", lwd=2,lty=2)
text(44, 0,substitute(Temperature_an==a*Latitude+b,
list(a=round(mod$coef[2],2),b=round(mod$coef[1],2))))

#Exercice 

# Test de la corr�lation de Bravais-Pearson
cor.test(meteo$Latitude,meteo$Temperature_an, method = "pearson") 
cor.test(meteo$BAC, meteo$Temperature_an, method = "pearson") 
plot(meteo$Latitude, meteo$Temperature_an,pch=16,cex=1.5)
plot(meteo$Temperature_an, meteo$BAC,pch=16,cex=1.5)

#R�vision