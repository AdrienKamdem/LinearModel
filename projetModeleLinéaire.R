cat("\014")


#####(ETAPE 0) CONSTRUCTION DU MODELE LINAIRE EN FONCTION DU DATASET AVEC SON SUMMARY
## On fait les lignes suivantes, dans le but de r�duire notre mod�le pour effectuer un travail 
#utile ( sinon R� =1)

summary(cereal)
cereal = cereal[,-c(1,5,6,7,9,11)]
print(cereal)


pairs(cereal, col="red",main="Le nuage des points deux � deux variables") #Affichage de nuage de point des variables deux � deux
modele = lm(rating~calories+protein+fat+sugars+vitamins+weight+cups, data = cereal)
print(modele) # coefficient du modele lin�aire
smModele = summary(modele) 
print(smModele) # coefficients, R^2, p-value, statistic de student, F-statistic
#plot(modele)
#####(ETAPE 1) VERIFICATION DES VALEURS ATYPIQUES 

#r�sidu studentis� permet de detecter les valeurs aberrantes
res.student = rstudent(modele) 
print(res.student) #residu studentise (centr�e et r�duire toutes les erreurs de chaque variables expliqu�es)

#Risque alpha = 0.05
alpha = 0.05

#calcul du seuil � partir de la loi de Student � (n-p-2) ddl (seuil � partir duquel on compare chaque valeur pour detecter si elle est atypique)
seuil.student = qt(1-alpha/2,67-6-2) #ddl=67 d'apres le summary(modele), fonction permettant de lire sur la table de student avec alpha=5% et 61ddl
print(seuil.student) #seuil critique, si les valeurs des erreurs sont sup�rieur � seuil.student alors = valeur atypiques 


#Quelles sont les observations atypiques au sens de ce seuil 
atypiques.rstudent=(res.student < -seuil.student | res.student > +seuil.student)# dans le sens positif ou le sens n�gatif
ab.student = cereal[atypiques.rstudent,] #tableau des valeurs atypiques
print(ab.student)


#####(ETAPE 2) VERIFICATION DES 3 HYPOTHESES DE VALIDITE D'UN MODELE LINEAIRE  

##esperance des r�sidus est nulle
smModeleResiduals = smModele$residuals
print(smModeleResiduals) #Affichage des r�sidus
print(mean(smModeleResiduals)) #Affichage de la moyenne des r�sidus
#esp�rance des r�sidus presque nulle = 0.0000000000000000000000000000001 donc R doit �tre = approximtivement � 1 mais pas totalement donc R arrondis

##homocedasticit� des r�sidus => variance des r�sidu est une constante (interpr�tation graphique)
plot(cereal_2$rating,smModeleResiduals,ylab="R�sidu",xlab="Y obs")#Graphique des r�sidus en fonction des valeurs observ�es par le mod�le 
plot(modele$fitted.values,smModeleResiduals,ylab="r�sidu",xlab="Y predit")#Graphique des r�sidus en fonction des valeurs pr�dites par le modele 
#pas de cor�lation particuli�re pour Y observ�s et Y pr�dits donc homocedasticit� des r�sidus

##normalit� des r�sidus (v�rifier que les r�sidus suivent bien une loi normale)
qqnorm(smModeleResiduals)#Graphique des r�sidus 
#quantiles des r�sidu linaire par rapport au quantile d'une loi normale donc r�sidu ~ loi normale

#####(ETAPE3) SIGNIFICATIVITE AVEC TEST DE STUDENT ET DE FISHER

#TEST STUDENT (SIGNIFICATIVITE DE CHAQUE VARIABLE)
print(smModele)
# on regarde si P(T>|t|) < seuil = 0.05
# si P(T>|t|) d'un coef > 0.05 alors on rejette H1 => le coefficient n'est pas significatif
# si P(T>|t|) d'un coef < 0.05 alors on rejette H0 => le coefficient est significatif
# Dans notre cas les 3 derniers coeffcients sont au dessus du seuil donc non significatif

#TEST FISHER (SIGNIFICATIVITE GLOBALE DU MODELE)
# on regarde la p-value (=P(F>|f|)) que l'on compare au seuil
# si p-value < seuil => on rejette H0 => les coefficient ne sont pas nulle simultan�ment
# si p-value > seuil => on rejette H1 => les coefficient sont nulle simultan�ment
# Dans notre exemple p-value largement inf�rieur au seuil d'ou les coefficients ne sont pas simultan�ment nulle

#supprimer la variable vitamins car son coefficients n'est pas significatifs
cereal_ajust�e = cereal_2
print(cereal_ajust�e)
cereal_ajust�e =cereal_ajust�e[,-5]

print(cereal_ajust�e)

#R^2 ajuste est d�j� dans la commande summary du premier modele linaire mais juste pour refaire on fait �a
modele_ajustee = lm(rating~calories+protein+fat+sugars+cups, data=cereal_2)
smModeleAjustee = summary(modele_ajustee)
print(smModeleAjustee)


#####(ETAPE4) ETUDE DE COLINEARITE DE VARIABLE AINSI QUE SELECTION DE VARIABLE
 
#Graphique du nuage des points en croisant deux � deux les variables
pairs(cereal_2, col="red",main="Le nuage des points deux � deux variables")
#On constate que certaine varibles semblent colinaires du fait de l'aligenement des points sous formes lin�aire ou dans un ordre pr�cis
#On constate que des graphiques ont une disposition de points lineaire ce qui signifie qu'il y'a corr�lation (pour avoir une id�e visuelle)

#Detection de colinearit�
#TEST DE KLEIN (d�tection de corr�lations crois�es, colin�arit�s bivarri�es)


#Coh�rence des signes entre �j et r[y,xj] (pr�somption de colin�arit� de la variable explique Xj avec la variable cible)
for (i in 1:6){
  if (smModeleAjustee$coefficients[,1][i]*cor(cereal_ajust�e[,i],cereal_ajust�e$rating, method="pearson")<0){ 
    #�*r(xk,y)<0 signes diff�rents
    print("////////")
    print(cor(cereal_ajust�e[,i],cereal_ajust�e$rating, method="pearson"))
    print("Pr�somption de colin�arit� simple")
  }else{
    print("////////")
    print(cor(cereal_ajust�e[,i],cereal_ajust�e$rating, method="pearson"))
    print("C'est good")
  }
}

#VIF facteur d'inflation de la variance (d�tection de corr�lations multivari�es, multicolin�arit�)
#install.packages("car")
library("car")
VIF = vif(modele_ajustee)
print(VIF)

for (i in VIF){
  if (i>4){
    print("////////")
    print(i)
    print("Pr�somption de multicolin�arit�")
  } else {
    print("////////")
    print(i)
    print("C'est good")
  }
}

#S�lection de variables (choisir une des deux m�thodes si dessous)
#Crit�re AIC (convergence du crit�re AIC du modele � p-k+1 variables(=q variables))
#(A effectuer tant que le crit�re AIC diminue)

mod0=lm(lpsa~1,data=cereal)

modselect_f=stepAIC(mod0,lpsa~lcavol+lweight
                    +age+lbph+svi+lcp+gleason+pgg45,data=
                      Prostate.app,trace=TRUE,direction=c("forward"))
summary(modselect_f)




 