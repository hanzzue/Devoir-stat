library(dplyr)
library("ggpubr")
library(rstatix)
library(questionr)
dat <- read.csv("C://Users//abdel//OneDrive//Documents//pulse.txt",
                header = TRUE,
                sep = " ",
                dec = ".")
summary(donnée)
boxplot(dat$Poids~  dat$Fumeur)
###########outliner visualisation ###############"
Q1 <- quantile(dat$Poids, .25)
Q3 <- quantile(dat$Poids, .75)
IQR <- IQR(dat$Poids)
dat <- subset(dat, dat$Poids> (Q1 - 1.5*IQR) )
boxplot(dat$Poids~dat$Pouls_avant)

######################################################Q1)
ouifumeur=filter(dat,Fumeur=='TRUE')
nonfumeur=filter(dat,Fumeur=='FALSE')
mean(ouifumeur$Poids)
mean(nonfumeur$Poids)
t.test(dat$Poids~dat$Fumeur,var.equal=TRUE,methods='Pearson') # pval>0,05 on ne rejete pas l'hypothese nul 
hist(ouifumeur$Poids) #normalité donnée 
hist(nonfumeur$Poids)
shapiro.test(ouifumeur$Poids)#pval>0,05 H0= la distribution est normale
shapiro.test(nonfumeur$Poids)# pval< 0,05 # la distibution est non normale
ggqqplot(ouifumeur$Poids) # test normalité
ggqqplot(nonfumeur$Poids) # test normalité
var.test(dat$Poids~ dat$Fumeur) #les variances sont égales (pval>0,05) H0= variances égales homoscédaisicté respecté
var(ouifumeur$Poids)
var(nonfumeur$Poids)
 #rejet de la normalité ==> test non paramétrique 
wilcox.test(dat$Poids ~ dat$Fumeur) # pval > 0,05 , on ne rejete pas H0  , il 'ya pas de relation entre fumer et le poids


########exo2###############
power.t.test(n=11,sd=sqrt(228.4855),delta=2,sig.level = 0.05) #puissance=0,04 ==> soit 4% de chance de rejeter H0 quand H0 est faux ( accepte H1 quand H1 est vrai)
#####exo3######################
mean(dat$Pouls_avant)
shapiro.test(dat$Pouls_avant) #pval< 0,05 ==> on rejete l'hypothese nul et on accepte H1 , les données ne suivent pas une loi normale
ggqqplot(dat$Pouls_avant)
#var.test(dat$Pouls_avant)
##non paramétrique###
wilcox.test(dat$Pouls_avant,mu = 70) #pval< 0,05 ==> on rejete l'hypothese nul on accepte H1 , les moyennes sont différentes 
#########exo4#########
ouisportif=filter(dat,Sportif=='Intensif'|Sportif=='Moyen')
nonsportif=filter(dat,Sportif=='Peu')
boxplot(dat$Pouls_avant~dat$Sportif)
dat$Binairesport <- paste(dat$Sportif=='Intensif'|dat$Sportif=='Moyen') ## crée nouvelle colonne avec sport et non sport
boxplot(dat$Pouls_avant ~ dat$Binairesport,xlab = "sportif",ylab = "Pouls repos",main='Pratique du sport en fonction du pouls au repos')
#condtion d'application test paramétrique comparaison donnée indépendantes
shapiro.test(ouisportif$Pouls_avant)#pval<0,05 H1 non normale 
shapiro.test(nonsportif$Pouls_avant) #pval>0,05 H0  normale 
ggqqplot(ouisportif$Pouls_avant)
ggqqplot(nonsportif$Pouls_avant)
hist(nonsportif$Pouls_avant)
hist(ouisportif$Pouls_avant)
var.test(dat$Pouls_avant~ dat$Binairesport) #pval = 0.1846 > 0,05 ==> les données sont de variances égales homoscédaicité respecté
####test paramétrique#######
t.test(dat$Pouls_avant ~dat$Binairesport,var.equal=T) # pval> 0.1346 ==> on accepte H0 pas de différence entre sport et poul avant
##"non paramétrique#######
wilcox.test(dat$Pouls_avant ~dat$Binairesport)#pval<0,05 => on rejet H0 et on accepte H1, il y'a une différence entre le valeur du pouls au repos pour sportif / non sportif
##exo5### 
z=table(dat$Binairesport,dat$Fumeur)

rownames(z)<-c(" non Sportif", "sportif")
colnames(z)<-c("non fumeur"," fumeur")

lprop(z) #pourcentage
prop.test(z)
#comparaison deux variable qualitatives test chi2 donnée quantitatives (comptages)
chisq.test(z) # pval> 0,05, on accepte H0, pas de diff entre fumer et sport
fisher.test(z)
restest<-chisq.test(z)
restest$expected
restest$residuals
#######genre/sport
y=table(dat$Binairesport,dat$Genre)
chisq.test(y)
#graphique representation variable quali
barplot(dat,dat$Fumeur)

########exo6 trouver corrélation 
plot(x=dat$Pouls_avant,y=dat$Poids,xlab='pouls avant',ylab='Poids', main="Pouls avant/ POIDS",las=1,pch=19)

shapiro.test(dat$Poids) # H1 non normale
shapiro.test(dat$Pouls_avant) ### H1 non normale
hist(dat$Poids) 
hist(dat$Pouls_avant)
ggqqplot(dat$Poids)
ggqqplot(dat$Pouls_avant)
cor.test(x=dat$Poids,y=dat$Pouls_avant,method = "pearson") #paramétrique ( respecte normalité)
###non paramétrique
cor.test(x=dat$Poids,y=dat$Pouls_avant,method = "spearm",alternative="less") #nonparamétrique Pval>0,05 il n'ya pas de corrélation et rho = -0.17 pas significatif abscence de relation linéaire entre les deux variables considérés
#coréllograme 
is_quant=select(dat,Poids,Pouls_avant,Pouls_apres,Taille)
library(corrplot)
create_corrplot <- function(data,
                            method = "spearm",
                            sig.level = 0.05,
                            order = "original",
                            diag = FALSE,
                            type = "upper",
                            tl.srt = 90,
                            number.font = 1,
                            number.cex = 1,
                            mar = c(0,0,0,0),
                            language.true = FALSE,
                            language = "french") {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  ifelse(language.true == TRUE, ifelse(language == "french",
                                       print(paste0("Les valeurs manquantes dans le jeu de données sont automatiquement supprimées. Ce graphique de corrélation est basé sur ", nrow(data), " observations, ce qui représente ", round(nrow(data)/nrow(data_incomplete)*100, digits = 2), "% de l'ensemble des données initiales.")),
                                       print(paste0("Missing values in the dataset are automatically removed. This correlation plot is based on ", nrow(data), " observations, which represent ", round(nrow(data)/nrow(data_incomplete)*100, digits = 2), "% of the initial dataset."))), "")
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat, method="color", col=col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type=type, order=order,
           addCoef.col = "black", # Ajout du coefficient de corrélation
           tl.col="black", tl.srt=tl.srt, #Rotation des etiquettes de textes
           # Combiner avec le niveau de significativité
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # Cacher les coefficients de corrélation sur la diagonale
           diag=diag
  )
}

sig.level <- 1

create_corrplot(data = is_quant,
                method = "spearm",
                sig.level = sig.level,
                order = "hclust",
                diag = FALSE,
                type = "upper",
                tl.srt = 90,
                number.font = 1,
                number.cex = 1,
                mar = c(0,0,0,0),
                language.true = FALSE,
                language = "english")

###entre homme et femme?
femme=filter(dat,Genre=='Femme')
homme=filter(dat,Genre=='Homme')
is_quant_femme=select(femme,Poids,Pouls_avant,Pouls_apres,Taille)
is_quant_homme=select(homme,Poids,Pouls_avant,Pouls_apres,Taille)
 ###remove false data 
is_quant_homme <- is_quant_homme[-19,]                             ###remove false data 
 ###remove false data 
shapiro.test(is_quant_femme$Pouls_avant)

ggqqplot(is_quant_homme$Poids)
hist(is_quant_homme$Poids)
ggqqplot(is_quant_femme$Poids)
cor.test(is_quant_femme$Poids,is_quant_femme$Pouls_avant,method ='pearson') ##corrélation négative significatif?
cor.test(is_quant_homme$Poids,is_quant_homme$Pouls_avant,method ='spearm')
#régréssion linéaire

###femme
reg <- lm(is_quant_femme$Poids ~ is_quant_femme$Pouls_avant) #intercept=77.0034  ,is_quant_femme$Pouls_avant = -0.2603
summary(reg) #  y = -0.2603x + 77.0034 
plot(is_quant_femme$Poids~ is_quant_femme$Pouls_avant , xlab = "Pouls avant (Pulsation/minutes)",ylab= "POIDS (KG)",main=" y = -0.2603x + 77.0034" )
abline(reg, col="blue")
abline(77.0034 , -0.2603, col = 'red')

#########QUESTION 7 ###########
etudiant_OUICOURU=filter(dat,Couru=='TRUE')
xxx=etudiant_OUICOURU[7:8]
#etudapres=unlist(etudapres, use.names=FALSE)


#########graph donnee apparieie
library('tidyverse')
xxx$numero_etudiant<- seq(1:45)
mydf_long <- xxx %>% 
  gather(time,value,-numero_etudiant)
mydf_long$time <- fct_relevel(mydf_long$time, c("Pouls_avant", "Pouls_apres"))
library(ggplot2)
ggplot(mydf_long,aes(y=value, x=time,group=numero_etudiant))+
  geom_point(show.legend = FALSE, size=3, colour="blue")+
  geom_line(show.legend = FALSE, size=1, colour="blue")+
  theme_bw() 
###Principe du test de Student apparié
xxx$difference=(xxx$Pouls_apres - xxx$Pouls_avant) ###add new columns differences des deux conditions
mean(xxx$Pouls_avant) 
mean(xxx$Pouls_apres) 
moy_d = mean(xxx$difference)
moy_d  ##" moyennne des differences
######conditions d'application donne apparié 
shapiro.test(xxx$difference) #normalité #t=statist p_val>0,05 
t.test(xxx$Pouls_avant,xxx$Pouls_apres,paired=T)  ## La p value du test est <0.05, 
#l'hypothèse nulle d'égalité des moyennes est rejetée. Le test de Student apparié met en évidence une différence 
#significative entre les deux moyennes, dans le sens d'une moyenne de poids plus faible après la mise en place du régime.
 




########question 8 ##########"
etudiant_NONCOURU=filter(dat,Couru=='FALSE')
yyy=etudiant_NONCOURU[7:8]
yyy$numero_etudiant<- seq(1:62)
graph <- yyy %>% 
  gather(time,value,-numero_etudiant)
graph$time <- fct_relevel(graph$time, c("Pouls_avant", "Pouls_apres"))
library(ggplot2)
ggplot(graph,aes(y=value, x=time,group=numero_etudiant))+
  geom_point(show.legend = FALSE, size=3, colour="blue")+
  geom_line(show.legend = FALSE, size=1, colour="blue")+
  theme_bw() 
###fonction abline#######regression lineaire
require(stats)
reg<-lm(yyy$Pouls_avant ~ yyy$Pouls_apres, data = yyy)
#coeff=coefficients(reg)
#eq = paste0("y =", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(yyy$Pouls_avant~yyy$Pouls_apres,xlab = "Pouls apres",ylab= "Pouls avant ",main=" y = 1.028x -1.074")
abline(reg, col="blue")




text(locator(), labels = c("ROUGE theorique", "BLEU observé"))

reg <- lm(is_quant_femme$Poids ~ is_quant_femme$Pouls_avant) #intercept=77.0034  ,is_quant_femme$Pouls_avant = -0.2603
summary(reg) #  y = -0.2603x + 77.0034 
plot(is_quant_femme$Poids~ is_quant_femme$Pouls_avant , xlab = "Pouls avant (Pulsation/minutes)",ylab= "POIDS (KG)",main=" y = -0.2603x + 77.0034" )
abline(reg, col="blue")
abline(1 ,1, col = 'red')

