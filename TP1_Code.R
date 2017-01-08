library(DMwR)
head(algae)

#################### 4.4 RESUMER ET VISUALISER #######################
summary(algae)
#There are NAs in the chemical variables

#af???cher l'histogramme, l'estimateur à noyau de la densité et le QQ-plot 
library(car) 
op = par(mfrow=c(1,2)) 
hist(algae$mxPH, prob=T, xlab="", main="Histogram of maximum pH value",ylim=0:1) 
lines(density(algae$mxPH,na.rm=T)) 
rug(jitter(algae$mxPH)) 
qqnorm(algae$mxPH,main="Normal QQ plot of maximum pH") 
par(op)

# tracer des boxplot conditionnelles où le conditionnement est fait par rapport à une variable catégorielle (ici size) 
library(lattice) 
bwplot(size ~ a1, data=algae, ylab="River Size",xlab="Algal A1")

#################### 4.5 DONNEES MANQUANTES #######################
algae[!complete.cases(algae),]
nrow(algae[!complete.cases(algae),]) #16 observations (lignes) contiennent des NAs

#Methode 1 : Supprimer les observations avec des NA
algae1 = na.omit(algae) 
nrow(algae) 
nrow(algae1)

#Methode 2 : Remplir avec les valeurs les plus fréquentes
algae2 = centralImputation(algae)  #This function fills in any NA value in all columns of a data frame with the statistic of centrality (given by the function centralvalue()) of the respective column. 
#ie : If the variable is numeric it returns de median of the given sample, if it is a factor it returns the mode. In other cases it tries to convert to a factor and then returns the mode.
nrow(algae[!complete.cases(algae),]) 
nrow(algae2[!complete.cases(algae2),])

#Methode 3 : Explorer la similitude entre les observations
algae3 = knnImputation(algae, k =10, meth = "median")  #PB : il faut omettre les variables à prédire !!
nrow(algae[!complete.cases(algae),]) 
nrow(algae3[!complete.cases(algae3),])

algae[,1:11] = knnImputation(algae[,1:11], k =10, meth = "median")

#NB: sur la base test
#Paramètre "distData" : Optionally you may sepecify here a data frame containing the data set that should be used to ???nd the neighbours. This is usefull when ???lling in NA values on a test set, where you should use only information from the training set. This defaults to NULL, which means that the neighbours will be searched in data
#It takes the whole data frame as the argument and you don't even have to specify which variabe you want to impute. But be cautious not to include the response variable while imputing, because, when imputing in test/production environment, if your data contains missing values, you won't be able to use the unknown response variable at that time.
#Que fait-il des var explicatives qualitatives ? Il doit probablement mettre une dist = 1 si c'est la mêm catégorie et 0 sinon (vu que la distance doit être normée)

#################### 4.6 MODELE PREDICTIF #######################

## Methode 1 : Regression linéaire multiple

#Calcul des coeffs
algae = knnImputation(algae, k =10, meth = "median") 
lm.a1 <- lm(a1 ~ ., data = algae[, 1:12]) 
summary(lm.a1)

#Choix des variables pertinentes
anova(lm.a1) 
#Un peu inutile: sens bizarre de la table ANOVA qui introduit un ordre entre les variables
#D'où le fait que ce n'est pas cohérent avec le test de Student

#Déterminer un sous modèle qui ne contient pas de variable inutile
final.lm = step(lm.a1)#. If the scope argument is missing the default for direction is "backward". 
summary(final.lm)

##Methode 2 : Arbre de décision

#Calcul
algae = knnImputation(algae, k =10, meth = "median") 
library(rpart) 
rt.a1 = rpart(a1 ~ ., data = algae[, 1:12]) 
rt.a1 

#Afficher l'arbre
par(lwd=2, col="red") 
plot(rt.a1, compress=TRUE) 
text(rt.a1, use.n=TRUE,col="blue") 

#Autre viz
par(lwd=2, bg="lemonchiffon3") 
prettyTree(rt.a1,col="navy",bg="lemonchiffon") 

#Evaluer la qualité de prévision
lm.predictions.a1 = predict(final.lm, algae) 
rt.predictions.a1 = predict(rt.a1, algae) 
print(regr.eval(algae[, "a1"], rt.predictions.a1, train.y = algae[,"a1"]))
regr.eval(algae[, "a1"], lm.predictions.a1, train.y = algae[,"a1"]) 

#Afficher les erreurs
par(mfrow = c(1, 2), col="navy", bg="lemonchiffon1") 
plot(lm.predictions.a1, algae[, "a1"], main = "Linear Model", xlab = "Predictions", ylab = "True Values", xlim=c(-15,62)) 
abline(0, 1, lty = 2) 
plot(rt.predictions.a1, algae[, "a1"], main = "Regression Tree", xlab = "Predictions", ylab = "True Values", xlim=c(-15,62)) 
abline(0, 1, lty = 2) 

#Calcul des prévisions sur la base de test
summary(test.algae) 
test.algae = knnImputation(test.algae, k =10, meth = "median")#Nettoyer les données manquantes de la même facon que train
lm.predictions.a1 = predict(final.lm, test.algae)
rt.predictions.a1 = predict(rt.a1, test.algae) 

#Evaluation des prédictions sur la base test
#NB : train.y => l. The baseline used in our implementation is a constant model that always predicts the average target variable value
regr.eval(algae.sols[, "a1"], lm.predictions.a1, train.y = algae[,"a1"]) 
regr.eval(algae.sols[, "a1"], rt.predictions.a1, train.y = algae[,"a1"]) 

#Même processus pour les autres variables
#NB: les NA ont déjà été remplacés dans les bases train et test
for (i in 2:7){
  #Entraîner les modèles
  algae_i = algae[,c(1:11,11+i)]
  y <- paste("a", i, sep ='')
  x <- names(algae_i)[!names(algae_i) %in% y]
  formula = as.formula(paste(y, paste(x, collapse="+"), sep="~"))
  lm.ai <- lm(formula, data = algae_i)
  final.lm.ai = step(lm.ai)
  rt.ai = rpart(formula, data = algae_i) 
  
  #Qualité de prévision sur la base train
  lm.predictions.ai = predict(final.lm.ai, algae_i) 
  rt.predictions.ai = predict(rt.ai, algae_i) 
  
  #Qualité sur la base test
  lm.predictions.ai_test = predict(final.lm.ai, test.algae)
  rt.predictions.ai_test = predict(rt.ai, test.algae) 
  
  sink(paste("C:/Users/roman/Desktop/ENSAE 3A/Apprentissage statistique/output", paste(i, ".txt", sep=''), sep =''))
  print('Resultats Arbre de decision sur base train')
  print(regr.eval(algae_i[[y]], rt.predictions.ai, train.y = algae_i[[y]]))
  print('Resultats Regression lineaire sur base train')
  print(regr.eval(algae_i[[y]], lm.predictions.ai, train.y = algae_i[[y]]))
  print('Resultats Arbre de decision sur base test')
  print(regr.eval(algae.sols[[y]], rt.predictions.ai_test, train.y = algae_i[[y]]))
  print('Resultats Regression lineaire sur base test')
  print(regr.eval(algae.sols[[y]], lm.predictions.ai_test, train.y = algae_i[[y]]))
  sink()
  
  par(mfrow = c(1, 2), col="navy", bg="lemonchiffon1") 
  plot(lm.predictions.ai, algae[[y]], main = "Linear Model", xlab = "Predictions", ylab = "True Values", xlim=c(-15,62)) 
  abline(0, 1, lty = 2) 
  plot(rt.predictions.ai, algae[[y]], main = "Regression Tree", xlab = "Predictions", ylab = "True Values", xlim=c(-15,62)) 
  abline(0, 1, lty = 2) 
  
  rm(algae_i, lm.predictions.ai,rt.predictions.ai, lm.predictions.ai_test,rt.predictions.ai_test)
}
