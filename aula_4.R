install.packages(c("randomForest","caret", "ggplot2","randomForest","nnet", "e1071", "rpart", "dplyr", "kernlab"))
library(caret)
library(ggplot2)
library(randomForest)
library(nnet)
library(e1071)
library(rpart)
library(dplyr)
library(kernlab)
#Limpar ambiente de desenvolvimento
rm(list = ls())
## import do dado
dados <- read.csv('C:/Users/jeffe/Desktop/git/R/baseR/data/classificação.csv', stringsAsFactors = TRUE)

#-----------------------------------------------------------------
# Usando ggplot2
ggplot(dados, aes(Cor.Preferida)) +
  geom_bar(fill = "#0073C2FF")

#-----------------------------------------------------------------
# Transformar os Dados
#-----------------------------------------------------------------#Gerar números randômicos
set.seed(10)
head(dados)
#Transformação dos Dados
dados$Cor.Preferida <- as.numeric(dados$Cor.Preferida)
dados$Tipo.de.Música.Preferido <- as.numeric(dados$Tipo.de.Música.Preferido)
dados$Tipo.de.Comida.Preferido <- as.numeric(dados$Tipo.de.Comida.Preferido)
dados$Tipo.de.Refrigerante.Preferido <- as.numeric(dados$Tipo.de.Refrigerante.Preferido)
glimpse(dados)#-----------------------------------------------------------------
#Preparar Dados para Treinamento
#-----------------------------------------------------------------
#Separar Conjunto de Treinamento e de Teste
TreinamentoTamanho <- createDataPartition(dados$Gênero, p=0.8, list=FALSE)
TreinamentoDados <- dados[TreinamentoTamanho,]
TesteDados <- dados[-TreinamentoTamanho,]
#-----------------------------------------------------------------
#Preparar Dados para Treinamento: Caret - Classification and Regression Training
#Método Cross-Validation
#-----------------------------------------------------------------
#Usando caret
#Treinando o modelo
#https://cran.r-project.org/web/packages/caret/index.html
modeloC <- train(Gênero ~ ., data= TreinamentoDados,
                 method = "svmPoly",
                 na.action = na.omit,
                 preProcess = c("scale","center"),
                 trControl = trainControl(method="none"),
                 tuneGrid = data.frame(degree=1,scale=1,C=1)
)modeloC.cv <- train(Gênero ~ ., data= TreinamentoDados,
                     method = "svmPoly",
                     na.action = na.omit,
                     preProcess = c("scale","center"),
                     trControl = trainControl(method="cv", number=6),
                     tuneGrid = data.frame(degree=1,scale=1,C=1)
)
print(modeloC)
print(modeloC.cv)
#--------------------------------------------------------
#Testar o modelo "modeloC"
#--------------------------------------------------------
predicaoClassesC <- predict(modeloC, newdata = TesteDados)
str(predicaoClassesC)
confusionMatrix(predicaoClassesC, TesteDados$Gênero)t<-TesteDados[1,1:4]
head(t)
predicaoClassesC2 <- predict(modeloC, newdata = t)
#Dado Predito
predicaoClassesC2
#Dado Real
TesteDados[1,]$Gênero#--------------------------------------------------------
#Testar o modelo "modeloC.cv"
#--------------------------------------------------------
predicaoClassesCV <- predict(modeloC.cv, newdata = TesteDados)
str(predicaoClassesCV)
confusionMatrix(predicaoClassesCV, TesteDados$Gênero)t<-TesteDados[1,1:4]
head(t)
predicaoClassesCV2 <- predict(modeloC.cv, newdata = t)
#Dado Predito
predicaoClassesCV2
#Dado Real
TesteDados[1,]$Gênero#-----------------------------------------------------------------
# Método Floresta Randômica
#-----------------------------------------------------------------
#Usando randomForest
#https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/randomForest
modeloF <- randomForest(formula = Gênero ~ ., data = dados)
print(modeloF)#--------------------------------------------------------
#Testar o modelo "modeloF"
#--------------------------------------------------------
predicaoClassesF <- predict(modeloF, newdata = TesteDados)
str(predicaoClassesF)
confusionMatrix(predicaoClassesF, TesteDados$Gênero)t<-TesteDados[1,1:4]
head(t)
predicaoClassesF2 <- predict(modeloF, newdata = t)
#Dado Predito
predicaoClassesF2
#Dado Real
TesteDados[1,]$Gênero
#-----------------------------------------------------------------
# Método Máquina de Suporte Vetorial
#-----------------------------------------------------------------
#Usando e1071
#https://cran.r-project.org/web/packages/e1071/e1071.pdf
modeloSVM <- svm(formula = Gênero ~ ., data = dados)
print(modeloSVM)#--------------------------------------------------------
#Testar o modelo "modeloSVM"
#--------------------------------------------------------
predicaoClassesSVM <- predict(modeloSVM, newdata = TesteDados)
str(predicaoClassesSVM)
confusionMatrix(predicaoClassesSVM, TesteDados$Gênero)t<-TesteDados[1,1:4]
head(t)
predicaoClassesSVM2 <- predict(modeloSVM, newdata = t)
#Dado Predito
predicaoClassesSVM2
#Dado Real
TesteDados[1,]$Gênero
#-----------------------------------------------------------------
# Manipulação de Dados: Filtro
#-----------------------------------------------------------------
#Usando dplyr
dados %>%
  filter( Gênero == "M")
#-----------------------------------------------------------------
#-----------------------------------------------------------------
##NÃO USAR PARTIÇÃO NEM REDES NEURAIS
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# Método Regressão: Árvore Binária
#-----------------------------------------------------------------
#Usando rpart
#https://cran.r-project.org/web/packages/rpart/index.html
particao <- rpart(formula = Gênero~., data=dados)
plot(particao)
#--------------------------------------------------------
#Testar o modelo "particao"
#--------------------------------------------------------
predicaoClassesP <- predict(particao, newdata = TesteDados)
str(predicaoClassesP)t<-TesteDados[1,1:4]
head(t)
predicaoClassesP <- predict(predicaoClassesP, newdata = t)
#Dado Predito
predicaoClassesP2
#Dado Real
TesteDados[1,]$Gênero
#-----------------------------------------------------------------
# Método Redes Neurais
#-----------------------------------------------------------------
#Usando nnet
#https://www.rdocumentation.org/packages/nnet/versions/7.3-16/topics/nnet
modeloRN <- nnet(formula = Gênero ~ ., data = dados, size = 30)
print(modeloRN)#--------------------------------------------------------
#Testar o modelo "modeloRN"
#--------------------------------------------------------
predicaoClassesRN <- predict(modeloRN, newdata = TesteDados)
str(predicaoClassesRN)
table(TesteDados, predicaoClassesRN, type = "class")confusionMatrix(predicaoClassesRN, TesteDados$Gênero)t<-TesteDados[1,1:4]
head(t)
predicaoClassesF2 <- predict(modeloF, newdata = t)
#Dado Predito
predicaoClassesF2
#Dado Real
TesteDados[1,]$Gênero
CRAN - Package.caret

