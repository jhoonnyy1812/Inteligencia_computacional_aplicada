#Carregando o enviroment básico entregue pela professora.
load("C:/Users/joaom/Desktop/HW3/grantData_hw3/grantData_HW3.RData")

#Salvando os nomes das variáveis para passar elas como um "paste" na função neuralnet
columns = as.vector(reducedSet)
pasting <- as.formula(paste('training$Class ~ ' ,paste(columns,collapse='+')))

#Carregando bibliotecas necessárias
library(caret)
library(neuralnet)
library(doParallel)
library(MASS)

#Criando outras 2 colunas para auxiliar o modelo na predição (cada coluna contém TRUE/FALSE)
training = cbind(training,training$Class=='successful')
training = cbind(training,training$Class=='unsuccessful')
names(training)[1883] <- 'successful'
names(training)[1884] <- 'unsuccessful'


#####Questão 1 - Linear Discriminant Analisys#####

#Cálculo da "linear discriminant analisys
modelo_lda = lda(pasting, data=training, prior=rep(1,2)/2)
#Predição dos resultados usando o dataset testing
prediction_lda = predict(modelo_lda, testing)
confusao_lda = table(prediction_lda$class, testing$Class)

#Plot da "confusion matrix"
confusao_lda


#####Questão 2 - Rede Neural#####


#Usando a biblioteca doParallel para usar CPU+GPU no processamento dos dados.
cl = makePSOCKcluster(30)
registerDoParallel(cl)

#Função time usada para medir o tempo necessário para o modelo treinar.
start.time = proc.time()

#Modelo treinado com todos os inputs que têm no training set (como alguns têm pouca relevância para a predição, foram retirados pela professora. Os dados "importântes" estão informados no reducedSet)
#modelo = neuralnet(training$Class ~ ., data=training, hidden=c(1024,512,256,128,64,32,16,8,4), threshold=0.7,
                   #linear.output = FALSE, stepmax=1e10)

modelo_nn = neuralnet(pasting, data=training[, 1:1881], hidden=c(10,10,10), threshold=0.1,
                   linear.output = FALSE, stepmax=1e15)

stop.time = proc.time()

run.time = stop.time - start.time
run.time
#plot(modelo_nn,rep="best",radius=0.07)
#Fechando computação em paralelo
stopCluster(cl)

#Plot da melhor rede neural
#plot(modelo_nn,rep="best",radius=0.07)

#Predição dos resultados usando o dataset de teste
prediction_nn = compute(modelo_nn, testing[, 1:1881])

#Salvando os dados em uma "confusion matrix" para calcular a precisão e também analisar as predições
resultado = as.data.frame(prediction_nn$net.result)
names(resultado)[1] <- 'successfull'
names(resultado)[2] <- 'unsuccessful'
resultado$class = colnames(resultado[,1:2])[max.col(resultado[,1:2], ties.method = 'first')]
confusao_nn = table(resultado$class,testing$Class)

#Plot da "confusion matrix"
confusao_nn