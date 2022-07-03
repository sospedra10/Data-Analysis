
rm(list=ls()) 

getwd()
setwd('C:/Users/javie/OneDrive/Documentos/Coding/R')

#library(ggplot2)
library(factoextra)
#library(ggrepel)
#library(igraph)
#library(corrplot)
#library(psych)

# Cogemos datos
datos = read.csv('Datasets/cancer.csv',header=TRUE,sep = ",", dec=".")
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
datospca = matrizdatos
pca.cancer = prcomp(datospca, scale=T)
View(pca.cancer)


fviz_pca_ind(pca.cancer, geom.ind="point", col.ind="#FC4E07", axes=c(1, 2), pointsize=1.5) 

fviz_pca_var(pca.cancer, col.var="cos2", geom.var="arrow", labelsize=2, repel=FALSE)

fviz_pca_biplot(pca.cancer, col.var="cos2", geom.var="arrow", labelsize=2, repel=FALSE)


fviz_screeplot(pca.cancer, addlabels=TRUE, ylim=c(0, 50))
fviz_contrib(pca.cancer, choice="var", axes=1, top=7)

pcavariables = get_pca_var(pca.cancer)
corrplot(pcavariables$cos2, is.corr=FALSE)
options(ggrepel.max.overlaps = 10)
fviz_pca_var(pca.cancer, col.var="blue",repel=T)
pcavariables

# Correlaciones
corrplot.mixed(cor(matrizdatos))
correlaciones = round(cor(matrizdatos),2)
cor.plot(matrizdatos)
corrplot(correlaciones, method="pie",title="correlaciones", tl.pos="n", mar=c(2, 1, 3, 1),)


# Training

datos = read.csv('Datasets/cancer.csv',header=TRUE,sep = ",", dec=".")
matrizdatos = as.matrix(datos)

matrizdatos[matrizdatos=='M'] = 0
matrizdatos[matrizdatos=='B'] = 1

rownames(matrizdatos)<- matrizdatos[, 1]

# All numeric
matrizdatos = matrizdatos[, -1]
class(matrizdatos) = "numeric"

# Getting training and testing sets
set.seed(123)
samplesize = floor(0.7 * nrow(matrizdatos))
train_index = sample(seq_len(nrow(matrizdatos)), size = samplesize)

train = matrizdatos[train_index,]
head(train)

test = matrizdatos[-train_index,]
head(test)

# Preprocessing

Xtrain = scale(train[, -1])
ytrain = train[, 1]
dim(ytrain) = c(length(ytrain), 1) # adding extra dimension

Xtest = scale(test[, -1])
ytest = test[, 1]
dim(ytest) = c(length(ytest), 1) # adding extra dimension

dim(Xtrain)
dim(Xtest)
dim(ytrain)
dim(ytest)

ytrain[ytrain==0] = 'M'
ytrain[ytrain==1] = 'B'
ytest[ytest==0] = 'M'
ytest[ytest==1] = 'B'

Xtrain = as.matrix(Xtrain, byrow=TRUE)
ytrain = as.matrix(ytrain, byrow=TRUE)

Xtest = as.matrix(Xtest, byrow=TRUE)
ytest = as.matrix(ytest, byrow=TRUE)


# Create a Random Forest model with default parameters

library(randomForest)

# Important the factor for classification and not regression
model = randomForest(x = Xtrain, y = factor(ytrain[,1]),
                     xtest=Xtest, ytest=factor(ytest[,1]),
                     ntree = 300, type='classification', replace=T)
model


# Plotting model
plot(model)

# Importance plot
importance(model)


model

