cat("\014")


#####(ETAPE 0) CONSTRUCTION DU MODELE LINAIRE EN FONCTION DU DATASET AVEC SON SUMMARY
## On fait les lignes suivantes, dans le but de réduire notre modèle pour effectuer un travail 
#utile ( sinon R² =1)

summary(cereal)
cereal = cereal[,-c(1,5,6,7,9,11)]
print(cereal)


pairs(cereal, col="red",main="Le nuage des points deux à deux variables") #Affichage de nuage de point des variables deux à deux
modele = lm(rating~calories+protein+fat+sugars+vitamins+weight+cups, data = cereal)
print(modele) # coefficient du modele linéaire
smModele = summary(modele) 
print(smModele) # coefficients, R^2, p-value, statistic de student, F-statistic
#plot(modele)
#####(ETAPE 1) VERIFICATION DES VALEURS ATYPIQUES 

#résidu studentisé permet de detecter les valeurs aberrantes
res.student = rstudent(modele) 
print(res.student) #residu studentise (centrée et réduire toutes les erreurs de chaque variables expliquées)

#Risque alpha = 0.05
alpha = 0.05

#calcul du seuil à partir de la loi de Student à (n-p-2) ddl (seuil à partir duquel on compare chaque valeur pour detecter si elle est atypique)
seuil.student = qt(1-alpha/2,67-6-2) #ddl=67 d'apres le summary(modele), fonction permettant de lire sur la table de student avec alpha=5% et 61ddl
print(seuil.student) #seuil critique, si les valeurs des erreurs sont supérieur à seuil.student alors = valeur atypiques 


#Quelles sont les observations atypiques au sens de ce seuil 
atypiques.rstudent=(res.student < -seuil.student | res.student > +seuil.student)# dans le sens positif ou le sens négatif
ab.student = cereal[atypiques.rstudent,] #tableau des valeurs atypiques
print(ab.student)


#####(ETAPE 2) VERIFICATION DES 3 HYPOTHESES DE VALIDITE D'UN MODELE LINEAIRE  

##esperance des résidus est nulle
smModeleResiduals = smModele$residuals
print(smModeleResiduals) #Affichage des résidus
print(mean(smModeleResiduals)) #Affichage de la moyenne des résidus
#espérance des résidus presque nulle = 0.0000000000000000000000000000001 donc R doit être = approximtivement à 1 mais pas totalement donc R arrondis

##homocedasticité des résidus => variance des résidu est une constante (interprétation graphique)
plot(cereal_2$rating,smModeleResiduals,ylab="Résidu",xlab="Y obs")#Graphique des résidus en fonction des valeurs observées par le modèle 
plot(modele$fitted.values,smModeleResiduals,ylab="résidu",xlab="Y predit")#Graphique des résidus en fonction des valeurs prédites par le modele 
#pas de corélation particulière pour Y observés et Y prédits donc homocedasticité des résidus

##normalité des résidus (vérifier que les résidus suivent bien une loi normale)
qqnorm(smModeleResiduals)#Graphique des résidus 
#quantiles des résidu linaire par rapport au quantile d'une loi normale donc résidu ~ loi normale

#####(ETAPE3) SIGNIFICATIVITE AVEC TEST DE STUDENT ET DE FISHER

#TEST STUDENT (SIGNIFICATIVITE DE CHAQUE VARIABLE)
print(smModele)
# on regarde si P(T>|t|) < seuil = 0.05
# si P(T>|t|) d'un coef > 0.05 alors on rejette H1 => le coefficient n'est pas significatif
# si P(T>|t|) d'un coef < 0.05 alors on rejette H0 => le coefficient est significatif
# Dans notre cas les 3 derniers coeffcients sont au dessus du seuil donc non significatif

#TEST FISHER (SIGNIFICATIVITE GLOBALE DU MODELE)
# on regarde la p-value (=P(F>|f|)) que l'on compare au seuil
# si p-value < seuil => on rejette H0 => les coefficient ne sont pas nulle simultanément
# si p-value > seuil => on rejette H1 => les coefficient sont nulle simultanément
# Dans notre exemple p-value largement inférieur au seuil d'ou les coefficients ne sont pas simultanément nulle

#supprimer la variable vitamins car son coefficients n'est pas significatifs
cereal_ajustée = cereal_2
print(cereal_ajustée)
cereal_ajustée =cereal_ajustée[,-5]

print(cereal_ajustée)

#R^2 ajuste est déjà dans la commande summary du premier modele linaire mais juste pour refaire on fait ça
modele_ajustee = lm(rating~calories+protein+fat+sugars+cups, data=cereal_2)
smModeleAjustee = summary(modele_ajustee)
print(smModeleAjustee)


#####(ETAPE4) ETUDE DE COLINEARITE DE VARIABLE AINSI QUE SELECTION DE VARIABLE
 
#Graphique du nuage des points en croisant deux à deux les variables
pairs(cereal_2, col="red",main="Le nuage des points deux à deux variables")
#On constate que certaine varibles semblent colinaires du fait de l'aligenement des points sous formes linéaire ou dans un ordre précis
#On constate que des graphiques ont une disposition de points lineaire ce qui signifie qu'il y'a corrélation (pour avoir une idée visuelle)

#Detection de colinearité
#TEST DE KLEIN (détection de corrélations croisées, colinéarités bivarriées)


#Cohérence des signes entre âj et r[y,xj] (présomption de colinéarité de la variable explique Xj avec la variable cible)
for (i in 1:6){
  if (smModeleAjustee$coefficients[,1][i]*cor(cereal_ajustée[,i],cereal_ajustée$rating, method="pearson")<0){ 
    #â*r(xk,y)<0 signes différents
    print("////////")
    print(cor(cereal_ajustée[,i],cereal_ajustée$rating, method="pearson"))
    print("Présomption de colinéarité simple")
  }else{
    print("////////")
    print(cor(cereal_ajustée[,i],cereal_ajustée$rating, method="pearson"))
    print("C'est good")
  }
}

#VIF facteur d'inflation de la variance (détection de corrélations multivariées, multicolinéarité)
#install.packages("car")
library("car")
VIF = vif(modele_ajustee)
print(VIF)

for (i in VIF){
  if (i>4){
    print("////////")
    print(i)
    print("Présomption de multicolinéarité")
  } else {
    print("////////")
    print(i)
    print("C'est good")
  }
}

#Sélection de variables (choisir une des deux méthodes si dessous)
#Critère AIC (convergence du critére AIC du modele à p-k+1 variables(=q variables))
#(A effectuer tant que le critère AIC diminue)

mod0=lm(lpsa~1,data=cereal)

modselect_f=stepAIC(mod0,lpsa~lcavol+lweight
                    +age+lbph+svi+lcp+gleason+pgg45,data=
                      Prostate.app,trace=TRUE,direction=c("forward"))
summary(modselect_f)




 