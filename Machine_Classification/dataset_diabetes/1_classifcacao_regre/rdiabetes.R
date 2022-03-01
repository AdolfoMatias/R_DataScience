#carregando bibliotecas
#install.packages('MLmetrics')
library(rpart.plot)
library(rpart)

#chamando conjunto de dados
diabetes  <-  read.csv("diabetes.csv", sep=",", header= T, na.strings="", stringsAsFactors = T)
head(diabetes)
dim(diabetes)


#separando amostras em 0 e 1 por 70/30
amostras  = sample(c(0,1), 768, replace=T, prob=c(0.7,0.3))
arvotreino = diabetes[amostras ==0, -c(9)]
dataprev <- arvotreino[,-c(9)]
arvoteste = diabetes[amostras ==1, -c(9)]
dataprev

#gerando modelo de arvore no rpart
arvorec = rpart(arvotreino$Outcome ~ ., dataprev, method="class")
rpart.plot(arvorec, cex =.6)


#aplicando modelo de previsao
previsaoarvo = predict(arvorec, arvoteste)
head(previsaoarvo)


#comparando o modelo predito com o teste
compara = cbind(arvoteste, previsaoarvo)
compara

#Criando modelo para compara
compara["resultado"]= ifelse(compara$`0` >= 0.5, 0, 1)
head(compara)

#gerando madriz de confusão
matrizconf = table(compara$Outcome,compara$resultado)


#avaliando métricas
acura = ((matrizconf[1]+matrizconf[4])/sum(matrizconf))
acura
precio = (matrizconf[1]/(matrizconf[1]+matrizconf[2]))
precio
reca =  (matrizconf[1]/(matrizconf[1]+matrizconf[3]))
reca
f1scor = ((2*(precio*reca))/(precio+reca))
f1scor