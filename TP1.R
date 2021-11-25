########################### TD1 Partie 2

# va dans file -> import dataset -> import ton excel
# ensuite utilise directement ton excel dans les commandes car il est désormais enregistré dans Rstudio
cigarettes=cigarettes[,-1] # pour pouvoir afficher le nuage de point à faire une seule fois pour supprimer la première colonne du dataset importer

#cat("\014") => effacer le terminal de rstudio 
summary(cigarettes[,1]) #X1
summary(cigarettes[,2]) #X2
summary(cigarettes[,3]) #X3
summary(cigarettes[,4]) #X4

pairs(cigarettes) # plot nuage de points
summary(cigarettes)
pairs(cigarettes[,2 :4], col="red",main="Le nuage des points deux à deux variables")

#regression multiple pour exprimer CO en fonction de NICOTINE POIDS GOUDRON
modele <- lm(CO ~GOUDRON+NICOTINE+POIDS, data = cigarettes) # regression lineaire
print(modele) # affiche les coefficient de la regression lineaire
sm = summary(modele) #resumé du modele lineaire
print(sm$coefficients) # uniquement les coefficient de la regression multiple
print(sm$coefficients[,2]) # uniquement les ecart-type

e = modele$residuals #recuperer les résidus
print(e) # afficher les résidus
print(mean(e)) #afficherla moyenne des résidus pas égale à 0 car on estime
#égale à 0 pour le modéle théorique

#Graphique des résidus
plot(cigarettes$CO,e,ylab="Résidu",xlab="CO") 
abline(h=0) #droite horizontale ou verticale ou de regression à rajouter sur le plot

#résidu suit la loi de student
res.student = rstudent(modele) 
print(res.student) #residu studentise
print(e) #residu normale

#Seuil critique = ??
#Risque alpha = 0.1
alpha = 0.1
#calcul du seuil à partir de la loi de Student à (n-p-2) ddl
seuil.student = qt(1-alpha/2,24-3-2)
print(seuil.student)

#Quelles sont les observations atypiques au sens de ce seuil 
atypiques.rstudent=(res.student < -seuil.student | res.student > +seuil.student)
ab.student =cigarettes[atypiques.rstudent,]
print(ab.student)

#graphique matérialisant ces seuils critiques
plot(cigarettes$CO,res.student,cex=0.75)
abline(h=-seuil.student)
abline(h=+seuil.student) 
abline(h=0) 
text(cigarettes$CO[atypiques.rstudent],res.student[atypiques.rstudent],rownames(cigarettes)[atypiques.rstudent])


#levier d'une observation 
#Levier
indicateurs = influence.measures(modele)
# Descripteurs disponibles
attributes(indicateurs)
res.hat = indicateurs$infmat[,"hat"]
print(res.hat)

#sens atypique au sens du levier
#le seuil est défini par 2(p+1)/n.
seuil.hat =print(seuil.hat)
#Les points atypiques au sens du levier
atypiques.levier = (res.hat > seuil.hat)
ab.hat = rats[atypiques.levier,]
print(ab.hat)


