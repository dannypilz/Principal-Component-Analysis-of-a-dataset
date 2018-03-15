#Einlesen der Daten:
soep<-read.table("C:/Users/Danny/Documents/Uni/Wirtschaft/VM Statistik/soep.dat", header=TRUE, sep=";", dec=".")
soep40<-subset(soep,soep$Alter>39 &soep$Alter<61)

#Kovarianz Matrix berechnen:
kovar<-cov(soep40)
kovar

#Korrelations Matrix berechnen:
korr<-cor(soep40)
korr

#Eigenwerte und Eigenvektoren bestimmen:
eig<-eigen(korr)
eig

#Screeplot zeichnen:
plot(c(1,2,3,4),eig$values,type="l", main = "Screeplot",ylab = "Eigenwerte ?? i",xlab = "i")
text(x=c(1,2,3,4),y= eig$values,labels = "X", col = "red")

#Anteil der Varianz bestimmen:
var.anteil<-eig$values/sum(eig$values)
var.anteil

#80% der Varianz soll geklärt werden:
var.anteil[1]
var.anteil[1] +var.anteil[2]
var.anteil[1] +var.anteil[2] +var.anteil[3]

#Kaiser Kriterium:
eigmean<-mean(eig$values)
eigmean
eig$values

#Barplots zur Interpretation der Hauptkomponenten: (Gewicht der ursprünglichen Variablen)
barplot(eig$vectors[,1], names.arg = c("Alter","Bildung","Kinder","Bruttoeinkommen"),main = "1.Hauptkomponente")
