#Caminho do arquivo no meu computador, deve ser atualizado em cada um
caminho = "//wsl$/Ubuntu/home/jhoonnyy1812/Git/Inteligencia_computacional_aplicada/HW1/data/abalone.csv"

#Armazenando os dados do abalone.csv em dados
dados = read.csv2(caminho)

install.packages("caTools")
install.packages("Metrics")
install.packages("caret")
install.packages("corrplot")
install.packages("e1071")
install.packages("glmnet")
install.packages("lmvar")
install.packages("pls")

library(caTools)
library(Metrics)
library(caret)
library(corrplot)
library(e1071)
library(glmnet)
library(lmvar)
library(pls)
#####Parte 0#####

#Calculando skewness para verificar se serÃ¡ necessÃ¡rio algum tipo de pre processamento
skewness(dados$LongestShell)
skewness(dados$Diameter)
skewness(dados$Height)
skewness(dados$WholeWeight)
skewness(dados$ShuckedWeight)
skewness(dados$VisceraWeight)
skewness(dados$ShellWeight)
skewness(dados$Rings)

#Plotando a matriz de correlaÃ§Ã£o (nÃºmerica e com circulos) para verificar as correlaÃ§Ãµes
corr <- cor(dados[, 3:10])
corrplot.mixed(corr, number.cex = .8, tl.col = "black", tl.cex = .8)

#Separando os dados em 2 conjuntos, um para treino (com 75% dos dados) e um para teste (com 25% dos dados)
set.seed(1812)
sample = sample.split(dados$Rings, SplitRatio = .67)
train = subset(dados, sample == TRUE)
test  = subset(dados, sample == FALSE)

#####Parte 1#####

##Criando modelos de regressão linear (simples)

#Modelo para apenas 1 variÃ¡vel usando a amostra de treino (Rings x Any)
linearMod <- lm(Rings ~ LongestShell, data=train)
print(linearMod)#Retorna os coeficientes achados pelo modelo acima
summary(linearMod)#Retorna mais informaÃ§Ãµes sobre o modelo linear acima
#Verificando erros no modelo preditivo
predictions <- predict(linearMod, data=test)
rmse(test$Rings, predictions)
R2(test$Rings, predictions)

#Verificando se os valores achados acima batem com os valores achados pela regressÃ£o linear da amostra de teste
linearMod <- lm(Rings ~ LongestShell, data=test)
print(linearMod)
summary(linearMod)

##Criando modelos de regressÃ£o linear (múltiplas)
#Modelo para várias variáveis usando a amostra de treino (Rings x Any*N)
linearMod <- lm(Rings ~ LongestShell + Height, data=train)
print(linearMod)#Retorna os coeficientes achados pelo modelo acima
summary(linearMod)#Retorna mais informaÃ§Ãµes sobre o modelo linear acima

#Verificando se os valores achados acima batem com os valores achados pela regressÃ£o linear da amostra de teste
linearMod <- lm(Rings ~ LongestShell + Height, data=test)
print(linearMod)
summary(linearMod)
#Para todos os valores
linearMod <- lm(Rings ~., data = train)

#Cálculo do RMSE e do R2
predictions <- predict(linearMod, test)
rmse(test$Rings, predictions)
R2(test$Rings, predictions)

#Usando métodos de k-fold validation
train.control <- trainControl(method = "cv", number = 10)
model <- train(Rings ~., data = train, method = "lm",
                trControl = train.control)

#Recálculo do RMSE e R2 após método k-fold validation
predictions <- predict(model, test)
rmse(test$Rings, predictions)
R2(test$Rings, predictions)

#####Parte 2#####

#Regressão usando o modelo L2 (também chamado de regressão de Ridge)
x_train <- as.matrix(train[,3:9])
y_train <- as.matrix(train[,10])

#Encontrando melhor valor de lambda
cv <- cv.glmnet(x_train, y_train, alpha = 0)
cv$lambda.min

#Cálculo da regressão de ridge
model <- glmnet(x_train, y_train, alpha = 0, lambda = cv$lambda.min)
coef(model)#Mostrando valores dos coeficientes
predictions <- predict(model, as.matrix(test[,3:9]))
data.frame(
  RMSE = RMSE(predictions, test$Rings),
  R2 = R2(predictions, test$Rings)
)

#Método com cross validation de 10
model <- train(Rings ~., data = train, method = "ridge",
               trControl = train.control)

#Cálculo do RMSE e R2 em relaÃ§Ã£o ao modelo preditor e o valor real de teste
predictions <- predict(model, test)
rmse(test$Rings, predictions)
R2(test$Rings, predictions)

#####Parte 3#####

#Retorna o PCR usando cross validation de 10, com gráficos mostrando o MSE (não o RMSE). Contém bastante informações
pcr.object <- pcr.cv(x_train,y_train,k=10,groups=NULL,scale=TRUE,eps=0.000001,
       plot.it=TRUE, compute.jackknife = TRUE)

pcr_fit = pcr(Rings~., data = train[,3:10], scale = TRUE, validation = "CV")#Faz o mesmo método acima, mas foi através dele que consegui colocar varios tipos de validacao e de testes de predição
summary(pcr_fit)
validationplot(pcr_fit, val.type = "RMSE")


