# Correction exercice Regime

reg = read.table("Diet.csv", sep = ";", header = TRUE)
str(reg)

#pb : la variable Diet est considérée comme un entier 'integer'
#il faut la transformer en facteur

reg[,'Diet'] = as.factor(reg[,'Diet'])
str(reg)
#ok

#creation d'une variable différence de poids
reg[,'diff'] = reg[,'weight6weeks']-reg[,'pre.weight']

#visualisation des données
boxplot(reg[,'diff'] ~ reg[,'Diet'])


#question bio : les régimes sont ils efficaces ? lequel est le meilleur ?


# hypotheses
#H0 : quelque soit le regime, la diff moyenne de poids est la meme
#H1 : au moins une des diff moyennes est differentes des autres


#choix du test stat
# anova : var quanti en fonction de var quali a 3 modalites

#anova
lm1 = lm(diff ~ Diet, data = reg) # TukeyHSD ne fonctionne pas avec lm
#ou
lm1 = aov(diff ~ Diet, data = reg)

#le modele est il valide ? pour pouvoir l'interpreter il faut qu'il soit valide
# -> analyse des residus

hist(lm1$residuals)
par(mfrow = c(2,2))
plot(lm1)

#qqplot : normalité des residus ok
# standardized vs fitted : homogeneite des variances a l'air ok

#le modele est valide, on peut l'interpreter
anova(lm1)
#p = 0.001 < 0.05 : on rejette H0 au moins une des diff moyennes est differente

summary(lm1)

#TukeyHSD
TukeyHSD(lm1)
#ou
plot(TukeyHSD(lm1))

#modalite 3/2 et 3/1 differente

#reponse a la question biologique
#il existe une difference d'efficacite des regimes, le 3 est le meilleur (perte de poids plsu importante)


tom = read.table('Tomato1.csv', sep=",")
str(tom)