

library(factoextra)
library(corrplot)
library(psych)

# Cogemos datos
datos = read.csv('cancer.csv',header=TRUE,sep = ",", dec=".")
sum(is.na(datos))
matrizdatos = as.matrix(datos)
rownames(matrizdatos)<- matrizdatos[, 1]
matrizdatos = matrizdatos[,-1]

# All numeric

matrizdatos = matrizdatos[, -1]
clase = datos$Diagnosis
index_benign = clase=='B'
class(matrizdatos) = "numeric"
mode(matrizdatos)
sum(is.na(matrizdatos))


# Analisis de las variables del dataset

descripcionmatrizdatos = summary(matrizdatos[, 1])

for (i in 2:ncol(matrizdatos)) {
  descripcionmatrizdatos = cbind(descripcionmatrizdatos, as.matrix(summary(matrizdatos[, i])))
} 
colnames(descripcionmatrizdatos) = colnames(matrizdatos)
descripcionmatrizdatos

# Diagramas de Caja

for (i in 1:ncol(matrizdatos)){
  x11()
  boxplot(matrizdatos[, i], main = c("Diagrama de caja de", 
                                           colnames(matrizdatos)[i]), ylab="Tumors",col="lightblue")
}

# Histograms

for (i in 1:ncol(matrizdatos)){
  x11()
  hist(matrizdatos[, i],ylab = "Tumors",xlab = colnames(matrizdatos)[i],
       main = c("Histograma de", colnames(matrizdatos)[i]))
}

# Histogram with benign and malign variables

for (i in 1:ncol(matrizdatos)){
  x11()
  
  minim = descripcionmatrizdatos[, i][['Min.']]
  maxim = descripcionmatrizdatos[, i][['Max.']]
  
  hist(matrizdatos[, i][index_benign], ylab = "Tumors",xlab = colnames(matrizdatos)[i],
       main = c("Histograma de", colnames(matrizdatos)[i]), col='green', xlim=c(minim, maxim))
  
  hist(matrizdatos[, i][!index_benign], ylab = "Tumors",xlab = colnames(matrizdatos)[i],
       main = c("Histograma de", colnames(matrizdatos)[i]), col='red', add=T)
  
  legend('topleft', c('Benign', 'Malign'), col=c('green', 'red'), lwd=4)
}



# PCA

  # Scaling data

pca.cancer = prcomp(matrizdatos, scale=T)
View(pca.cancer)


fviz_pca_ind(pca.cancer, geom.ind="point", col.ind="#FC4E07", axes=c(1, 2), pointsize=1.5) 

fviz_pca_var(pca.cancer, col.var="cos2", geom.var="arrow", labelsize=1, repel=FALSE)

fviz_pca_var(pca.cancer, col.var="blue",repel=T)


fviz_pca_biplot(pca.cancer, col.var="cos2", geom.var="arrow", labelsize=2, repel=FALSE)



fviz_screeplot(pca.cancer, addlabels=TRUE, ylim=c(0, 50))

fviz_contrib(pca.cancer, choice="var", axes=1, top=10, ylim=c(0, 8))

pcavariables = get_pca_var(pca.cancer)
corrplot(pcavariables$cos2)


options(ggrepel.max.overlaps = 10)

# Correlations

correlaciones = round(cor(matrizdatos),2)
cor.plot(matrizdatos)


