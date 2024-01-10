################# Projet Statistiques #################


######## Importation et chargement des bibliothèques: ########
install.packages("naniar")
library(naniar) 
install.packages("funModeling")
library(funModeling)
install.packages("Hmisc")
library(Hmisc)
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

######## Tache 1: Importation des données ######## 
data<- read.csv(file ='C:/Users/Abir/Desktop/4ème DS/Projet stat/seed.csv',header=TRUE, na.strings=c(""," ","NA"))
View(data)
head(data)

# dimension de la data
dim(data)

# résumé statistique de la data 
summary(data)


######## Tache 2: Pré-traitement des données ######## 

###### Valeurs abérantes ######

#il exsiste 2 valeurs abérantes dans la variable Ac: 
boxplot(data$Ac) 

#Aucune valeur abérante détectée pour les autres variables: 
boxplot(data$P)
boxplot(data$A)
boxplot(data$C)
boxplot(data$Lkg)
boxplot(data$Wk)
boxplot(data$Lk)

#Chercher les 2 valeurs abérantes en utilisant la boite à moustaches: 
Qnl1=quantile(data$Ac, prob = 0.25)
Qnl3=quantile(data$Ac, prob = 0.75)
Qnl1
Qnl3
IRQ1=Qnl3-Qnl1
IRQ1
Minbp=Qnl1 - 1.5*IRQ1
Maxbp=Qnl3+ 1.5*IRQ1
Minbp
Maxbp

## l'indice de la valeur abérante ayant pr indice=179 et qui dépasse la valeur maximale du boxplot est:
## l'indice de la valeur abérante ayant pr indice=183 et qui dépasse la valeur maximale du boxplot est:
which(data$Ac > Maxbp )

# indice des valeurs abérantes:
which(data$Ac==8.456)
which(data$Ac==8.315)

# Affecter la valeur NA aux valeurs abérantes qu'on va traiter par la suite dans la partie de traitement des valeurs manquantes:
data$Ac[179]=NA
data$Ac[183]=NA

###### Valeurs manquantes ######


### question 1 : Pourcentage des valeurs manquantes: ###
sum(is.na(data))/prod(dim(data))*100

# Vérifier l'existence des valeurs manquantes
any(is.na(data))

# pour la visualisation des valeurs manquantes on utilise cette fonction:
gg_miss_var(data)
vis_miss(data)
df_status(data)

### Traiter les valeurs manquantes : 

#Variable C:Compactness :
which(is.na(data$C))

# les indices des valeurs manquantes:
vm=c(14,27,41,106,153)

#Formule de la variable C en fonction de A et P:
for (i in  vm )
{
  data$C[i]=(4*pi*data$A[i])/(data$P[i]*data$P[i])
}


#Variable length of kernel : LK :

# les indices des valeurs manquantes:
which(is.na(data$Lk))

# => nous avons détecté une seule valeur manquantes d'indice 130.

# Imputation: méthode mean 
dat.moy=impute(data$Lk,fun=mean)
data$Lk=dat.moy


# Variable Asymmetry coefficient Ac: 

# Imputation: méthode mean 
dat.moy=impute(data$Ac,fun=mean)
data$Ac=dat.moy

#La variable qualitative Varietie: 

# on a utilisé la méthode getmode() pour les variables qualitatives:
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# result sert à extraire la valeur la plus fréquente
result <- getmode(data$varietie)

# indices des valeurs manquantes
which(is.na(data$varietie))

# nous avons obtenu : 
vm=c(34,35,83,109,150,153)

# on affectera dans chaque indice la valeur de result:
for (i in  vm )
{
  data$varietie[i]=result
}

# Vérification: 
which(is.na(data$varietie))



######## Tache 3: Analyse univariée ######## 

##question 1 : etude de la normalité des variable quantitatives: 

# A : p-value = 9.696e-09 < 0.05 : on accepte H1 ( ne suit pas la loi normale) 
shapiro.test(data$A)

#P: p-value =  2.108e-08 < 0.05 : on accepte H1 ( ne suit pas la loi normale) 
shapiro.test(data$P)

#C :p-value = 0.001319 < 0.05 : on accepte H1 ( ne suit pas la loi normale) 
shapiro.test(data$C)

# LK : p-value = 1.242e-07 < 0.05 : on accepte H1 ( ne suit pas la loi normale) 
shapiro.test(data$Lk)

# Wk : p-value = 8.563e-06 < 0.05: on accepte H1 ( ne suit pas la loi normale) 
shapiro.test(data$Wk)

# Ac :  p-value = 0.06584 > 0.05 : on accepte H0 (  suit  la loi normale) 
shapiro.test(data$Ac)

# LKG :p-value = 3.433e-09< 0.05 : on accept H1 ( ne suit pas la loi normale) 
shapiro.test(data$Lkg)

##question 2 : Modalités des variables qualitatives:
table(data$varietie)
#### conclusion: => 3 modalités: Canadian,rosa et kama 



######## Tache 4: Analyse bivariée ######## 

#Pour A,P,C,LK,WK qui ne suivent pas la loi normale, on utilisera dans le reste la méthode SPEARMAN
attach(data)

#  p-value = 2.2e-16 < 0.005 : on accepte H1 : il y a une dépendance entre A et lkg 
cor.test(A,Lkg,method= 'spearman') 

#p-value = 2.2e-16 <0.05 on accepte H1 : il y a une dépendance entre P et lkg 
cor.test(P,Lkg,method= 'spearman')


#p-value = 0.05873 > 0.05 on accepte H0 : peut conclure une faible liaison entre les deux variables.
cor.test(C,Lkg,method= 'spearman')

#p-value = 2.2e-16 <0.05 , on accepte H1 : il y a une dependance entre Lk et lkg 
cor.test(Lk,Lkg,method= 'spearman')

#p-value = 2.2e-16 <0.05  on accepte H1 : il y a une dependance entre Wk et lkg 
cor.test(Wk,Lkg,method= 'spearman')

## Ac suit la loi normale , on peut utiliser la methode de PEARSON: 

#p-value = 0.6499 > 0.05 , on accepte H0 ,On peut conclure qu'il existe une faible liaison entre les deux variables.
cor.test(Ac,Lkg,method= 'pearson')


##### Variable qualitative : Varietie 
## Variable A:

#methode 1: graphiquement
boxplot(A~varietie)

# on étudie la normalité en premier lieu: 
tapply(A,varietie,shapiro.test)

#  Canadian wheat : p-value = 0.944 > 0.05 , on accepte H0 , on a la normalite pour le groupe Canadian wheat
#  Kama wheat :  p-value = 0.001609 < 0.05 , on accepte H1 ,  la normalite n'est pas assuree pour le groupe  Kama wheat
#  Rosa wheat  p-value = 2.532e-08 on accepte H1 ,  la normalite n'est pas assuree pour le groupe Rosa wheat

# On étudie la variance:
bartlett.test(A~varietie) # p-value = 0.2174 > 0.05 , on accepte H0 ,  ils ont la meme variance 

#### => On a n'a pas la normalite donc on ne peut pas appliquer ANOVA et 
# puisque notre variable cible possede 3 modalites alors on utilisie KRUSKAL

kruskal.test(A~varietie) # p-value < 2.2e-16 on accepte H1 , il y a  un effet 


## Variable P:

boxplot(P~varietie)
tapply(P,varietie,shapiro.test)
#  Canadian wheat : p-value = 0.8117 > 0.05 , on accepte H0 , on a la normalite pour le groupe Canadian wheat
#  Kama wheat :  p-value = 0.02313 < 0.05 , on accepte H1 ,  la normalite n'est pas assuree pour le groupe  Kama wheat
#Rosa wheat  p-value = 8.681e-08  < 0.05 on accepte H1 ,  la normalite n'est pas assuree pour le groupe Rosa wheat

bartlett.test(P~varietie) # p-value = 0.4404 > 0.05 , on accepte H0 ,  ils ont la meme variance 

# On a n' a pas la normalite donc  on ne peut pas appliquer ANOVA   
# et puisque notre variable cible possede 3 modalites alors on utilisie Kruskal

kruskal.test(P~varietie) # p-value < 2.2e-16 on accepte H1 , il y a  un effet 


## Variable Ac:

boxplot(Ac~varietie)
tapply(Ac,varietie,shapiro.test)
#  Canadian wheat : p-value = 0.03998  < 0.05 , on accepte H1 , la normalite n'est pas assuree pour le groupe Canadian wheat
#  Kama wheat :  p-value = 0.05606 > 0.05 , on accepte H0 ,  la normalite est  assuree pour le groupe  Kama wheat
#Rosa wheat  p-value = 0.7213  > 0.05 on accepte H0 ,  la normalite est   assuree pour le groupe Rosa wheat

bartlett.test(Ac~varietie) # p-value = 0.8453 > 0.05 , on accepte H0 ,  ils ont la meme variance 


# On a n' a pas la normalite donc  on ne peut pas appliquer ANOVA   
# et puisque notre variable cible possede 3 modalites alors on utilisie Kruskal

kruskal.test(Ac~varietie) # p-value = 7.37e-14 < 0.005 s on accepte H1 , il y a  un effet 



## Variable Lk: 

boxplot(Lk~varietie)
tapply(Lk,varietie,shapiro.test)
#  Canadian wheat : p-value = 0.9336 > 0.05 , on accepte H0 , la normalite est asuree pour le groupe Canadian wheat
#  Kama wheat :  p-value = 0.4345 > 0.05 , on accepte H0 ,  la normalite est  assuree pour le groupe  Kama wheat
#Rosa wheat  p-value = 0.0002585  <  0.05 on accepte H1 ,  la normalite n'est  pas  assuree pour le groupe Rosa wheat


bartlett.test(Lk~varietie) # p-value = 0.01196 < 0.05 , on accepte H1 ,  ils n'ont pas  la meme variance 


# On a n' a pas la normalite donc  on ne peut pas appliquer ANOVA   
# et puisque notre variable cible possede 3 modalites alors on utilisie Kruskal

kruskal.test(Lk~varietie) # p-value =< 2.2e-16  < 0.05 , on accepte H1 , il y a  un effet 
  



## Variable C:

boxplot(C~varietie)
tapply(C,varietie,shapiro.test)
#  Canadian wheat :  p-value = 0.9064 > 0.05 , on accepte H0 , la normalite est asuree pour le groupe Canadian wheat
#  Kama wheat : p-value = 0.113 > 0.05 , on accepte H0 ,  la normalite est  assuree pour le groupe  Kama wheat
#Rosa wheat  p-value = 0.5732 >  0.05 on accepte H0 ,  la normalite  est    assuree pour le groupe Rosa wheat


bartlett.test(C~varietie) # p-value = 0.009186  < 0.05 , on accepte H1 ,  ils n'ont pas  la meme variance 


# On a n' a pas la normalite donc  on ne peut pas appliquer ANOVA   
# et puisque notre variable cible possede 3 modalites alors on utilisie Kruskal

kruskal.test(C~varietie) # p-value < 2.2e-16  < 0.05 , on accepte H1 , il y a  un effet 



## Variable Wk:

boxplot(Wk~varietie)
tapply(Wk,varietie,shapiro.test)
#  Canadian wheat :  p-value =  p-value = 0.8634 > 0.05 , on accepte H0 , la normalite est asuree pour le groupe Canadian wheat
#  Kama wheat : 0.09265 > 0.05 , on accepte H0 ,  la normalite est  assuree pour le groupe  Kama wheat
#Rosa wheat  p-value = 0.0001077 <  0.05 on accepte H1 ,  la normalite  n'est pas   assuree pour le groupe Rosa wheat


bartlett.test(Wk~varietie) # p-value = 0.9967  > 0.05 , on accepte H0,   ils ont   la meme variance 


# On a n' a pas la normalite donc  on ne peut pas appliquer ANOVA   
# et puisque notre variable cible possede 3 modalites alors on utilisie Kruskal

kruskal.test(Wk~varietie) # p-value < 2.2e-16  < 0.05 , on accepte H1 , il y a  un effet 



######## Tache 5: Régression linéaire ######## 

## Question1 : Regression lineaire multiple 

#Modele1
attach(data)
y=data$Lkg
modele1=lm(y~A+Ac+Lk+P+Wk+C)
coef(modele1)
summary(modele1)


## Question2: stratégie détaillée 

#R²= 0.9119 : ce modele est un bon modele 
#Le parametre relatif  à la variable wk presente la valuerp=0.0178 la plus importante, 
# alors la variable Wk est de variabilite minimale. On 
#elimine la variable wk et on definit le nouveau modele :


#Modele2
y=data$Lkg
modele2=lm(y~A+Ac+Lk+P+C)
coef(modele2)
summary(modele2)

#R²=  0.9091 : ce modele est un bon modele 
#Le parametre relatif  à la variable Ac presente la valuerp= 0.000642 la plus importante,
# alors la variable Ac est de variabilite minimale. On 
#elimine la variable Ac et on definit le nouveau modele :


# Comparaison de deux modèle par la méthode de Critère d'information akaike:

AIC(modele1)
#-179.9419

AIC(modele2)
#-176.0896

#interprétation:
# On peut remarquer que l’AIC du premier modele  est  le plus faible alors ce modele est le meilleur entre les deux


## Question3 

# Visualisation : 

PCADATA=data[,1:6]

res.pca <- PCA(PCAdata, graph = FALSE)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)


#Pour Determiner les composants les plus importants:
# ****** 1ère méthode: ****** #
  
#On remarque la valeur de variance la plus élevée pour PC1= 72% et PC2= 17% 
pca<-prcomp(data[,1:6],center = TRUE,scale. = TRUE)
summary(pca)


# ****** 2ème méthode: ******#

install.packages("ggplot2")
screeplot(x=pca , type='line' ,main='PCA en fonction de la variance ')

# ****** 3ème méthode: ******#

#PC1 : 20.9 , PC2 : 1.01
pca$sdev


#Appliquer la regression linéaire multiple sur les nouveaux composants en PC1 et PC2 :


components <- cbind(Lkg = data[, "Lkg"], pca$x[, 1:2]) %>%as.data.frame()
lmpca<- lm(Lkg ~ ., data = components)
summary(lmpca)
summary(modele1)$adj.r.squared
summary(lmpca)$adj.r.squared


######## Tache 6: Régression linéaire généralisée ######## 

#Notre variable cible Lkg 

modglm<-glm(Lkg~Ac+P+C+A+Wk+Lk,family = Gamma(link="inverse"))
summary(modglm)
plot(modglm)














