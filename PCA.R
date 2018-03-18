#read the german csv-file:
soep<-read.table("C:/Users/Danny/soep.dat", header=TRUE, sep=";", dec=".")
soep40<-subset(soep,soep$Alter>39 &soep$Alter<61)

#covariane matrix calculate:
kovar<-cov(soep40)
kovar

#correlation matrix calculate:
korr<-cor(soep40)
korr

#eigenvalue and eigenvectors:
eig<-eigen(korr)
eig

#plot screeplot:
plot(c(1,2,3,4),eig$values,type="l", main = "Screeplot",ylab = "Eigenwerte ?? i",xlab = "i")
text(x=c(1,2,3,4),y= eig$values,labels = "X", col = "red")

#proportion of variance:
var.anteil<-eig$values/sum(eig$values)
var.anteil

#80% of variance:
var.anteil[1]
var.anteil[1] +var.anteil[2]
var.anteil[1] +var.anteil[2] +var.anteil[3]

#Kaiser Kriterium:
eigmean<-mean(eig$values)
eigmean
eig$values

#Barplots for interpretation of PCA:
barplot(eig$vectors[,1], names.arg = c("Alter","Bildung","Kinder","Bruttoeinkommen"),main = "1.Hauptkomponente")
