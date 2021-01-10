#Caminho do arquivo no meu computador, deve ser atualizado em cada um
caminho = "Homework 1/TI0077_HW1_assignment/data/abalone.csv"

#Armazenando os dados do abalone.csv em dados
dados = read.csv2(caminho)

#Instalando pacotes necessários
install.packages("e1071")
install.packages("plyr")
install.packages("gplot2")
install.packages("hrbrthemes")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
install.packages("factoextra")
install.packages("ggfortify")

#Carregando os pacotes acima no projeto
library(e1071)
library(plyr)
library(ggplot2)
library(hrbrthemes)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(factoextra)
library(ggfortify)

#####Parte 2#####

#Histogramas
hist(dados$LongestShell, col = "#00deed", border = "black", main = "Casca maior", xlab = "mm", ylab = "Frequência", breaks = 64)
hist(dados$Diameter, col = "#00deed", border = "black", main = "Diametro", xlab = "mm", ylab = "Frequência", breaks = 64)
hist(dados$Height, col = "#00deed", border = "black", main = "Altura", xlab = "mm", ylab = "Frequência", breaks = 64)
hist(dados$WholeWeight, col = "#00deed", border = "black", main = "Peso total", xlab = "gramas", ylab = "Frequência", breaks = 64)
hist(dados$ShuckedWeight, col = "#00deed", border = "black", main = "Casca maior", xlab = "gramas", ylab = "Frequência", breaks = 64)
hist(dados$VisceraWeight, col = "#00deed", border = "black", main = "Casca maior", xlab = "gramas", ylab = "Frequência", breaks = 64)
hist(dados$ShellWeight, col = "#00deed", border = "black", main = "Casca maior", xlab = "gramas", ylab = "Frequência", breaks = 64)
hist(dados$Rings, col = "#00deed", border = "black", main = "Casca maior", xlab = "Quantidade", ylab = "Frequência", breaks = 64)

#Médias
mean(dados$LongestShell)
mean(dados$Diameter)
mean(dados$Height)
mean(dados$WholeWeight)
mean(dados$ShuckedWeight)
mean(dados$VisceraWeight)
mean(dados$ShellWeight)
mean(dados$Rings)

#Desvios padrão
sd(dados$LongestShell)
sd(dados$Diameter)
sd(dados$Height)
sd(dados$WholeWeight)
sd(dados$ShuckedWeight)
sd(dados$VisceraWeight)
sd(dados$ShellWeight)
sd(dados$Rings)

#Skewness
skewness(dados$LongestShell)
skewness(dados$Diameter)
skewness(dados$Height)
skewness(dados$WholeWeight)
skewness(dados$ShuckedWeight)
skewness(dados$VisceraWeight)
skewness(dados$ShellWeight)
skewness(dados$Rings)

#####Parte 3####
#Funções para se conseguir a média/desvio padrão/skewness dos dados de forma mais direta e automatizada pelo R
aggregate(dados[, 3:10], list(dados$Type), mean)
aggregate(dados[, 3:10], list(dados$Type), sd)
aggregate(dados[, 3:10], list(dados$Type), skewness)

#Histogramas separados por classe e variável
#LongestShell
ggplot(dados, aes(x = LongestShell)) +
  geom_histogram(aes(color = Type, fill = Type), 
                 position = "identity", bins = 64, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#EE82EE")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#EE82EE"))
#Diameter
ggplot(dados, aes(x = Diameter)) +
  geom_histogram(aes(color = Type, fill = Type), 
                 position = "identity", bins = 64, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#EE82EE")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#EE82EE"))
#Height
ggplot(dados, aes(x = Height)) +
  geom_histogram(aes(color = Type, fill = Type), 
                 position = "identity", bins = 64, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#EE82EE")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#EE82EE"))
#WholeWeight
ggplot(dados, aes(x = WholeWeight)) +
  geom_histogram(aes(color = Type, fill = Type), 
                 position = "identity", bins = 64, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#EE82EE")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#EE82EE"))
#ShuckedWeight
ggplot(dados, aes(x = ShuckedWeight)) +
  geom_histogram(aes(color = Type, fill = Type), 
                 position = "identity", bins = 64, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#EE82EE")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#EE82EE"))
#VisceraWeight
ggplot(dados, aes(x = VisceraWeight)) +
  geom_histogram(aes(color = Type, fill = Type), 
                 position = "identity", bins = 64, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#EE82EE")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#EE82EE"))
#ShellWeight
ggplot(dados, aes(x = ShellWeight)) +
  geom_histogram(aes(color = Type, fill = Type), 
                 position = "identity", bins = 64, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#EE82EE")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#EE82EE"))
#Rings
ggplot(dados, aes(x = Rings)) +
  geom_histogram(aes(color = Type, fill = Type), 
                 position = "identity", bins = 64, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#EE82EE")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#EE82EE"))

#####Parte 4#####
##Abaixo têm alguns exemplos de gráficos (scatterplots) entre 2 variáveis e com indicativos de classes. Como são muitos gráficos
##decidi colocar apenas alguns exemplos e ir substituindo conforme a necessidade.
#Relação LongestShell x Rings
ggplot(dados, aes(x=LongestShell, y=Rings, color=Type)) + 
  geom_point(size=1.5) +
  ggtitle("LongestShell x Rings") +
  theme_ipsum()

#Relação LongestShell x WholeWeight
ggplot(dados, aes(x=LongestShell, y=WholeWeight, color=Type)) + 
  geom_point(size=1.5) +
  ggtitle("LongestShell x WholeWeight") +
  theme_ipsum()

#Relação WholeWeight x Rings
ggplot(dados, aes(x=Rings, y=WholeWeight, color=Type)) + 
  geom_point(size=1.5) +
  ggtitle("WholeWeight x Rings") +
  theme_ipsum()

#Relação WholeWeight x ShuckedWeight
ggplot(dados, aes(x=ShuckedWeight, y=WholeWeight, color=Type)) + 
  geom_point(size=1.5) +
  ggtitle("WholeWeight x ShuckedWeight") +
  theme_ipsum()


#Relação LongestShell x Diameter
ggplot(dados, aes(x=LongestShell, y=Diameter, color=Type)) + 
  geom_point(size=1.5) +
  ggtitle("LongestShell x Diameter") +
  theme_ipsum()


#Relação WholeWeight x Diameter
ggplot(dados, aes(y=WholeWeight, x=Diameter, color=Type)) + 
  geom_point(size=1.5) +
  ggtitle("WholeWeight x Diameter") +
  theme_ipsum()


#Relação Diameter x Height
ggplot(dados, aes(x=Diameter, y=Height, color=Type)) + 
  geom_point(size=1.5) +
  ggtitle("Diameter x Height") +
  theme_ipsum()


#Correlação entre as variáveis
corr <- cor(dados[, 3:10])
corrplot(corr, type = "lower", order = "hclust", 
         tl.col = "red", tl.srt = 45)
corrplot.mixed(res, number.cex = .8, tl.col = "black", tl.cex = .8)

title <- "Abalone p-value significance"
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, tl.col = "black", tl.srt = 30, method="color", col=col(200),  
         diag=FALSE, # tl.pos="d", 
         type="upper", order="hclust", 
         title=title, 
         addCoef.col = "black", # Add coefficient of correlation
         # Combine with significance
         # hide correlation coefficient on the principal diagonal
         mar=c(0,0,1,0) # http://stackoverflow.com/a/14754408/54964
)

#Correlação com mais dados mostrados (histograma, corrplot, relações + regressão)
chart.Correlation(dados[, 3:10], histogram=TRUE, pch=19)


#####Parte 5#####

#Cálculo dos PCA
res.pca <- prcomp(dados[, 3:10], scale = TRUE)

#Cálculo e demonstração dos eigenvalues para o PCA acima
eig.val <- get_eigenvalue(res.pca)
eig.val

#Plot dos PCA com base na importância
fviz_eig(res.pca)

#Plot de quanto cada variável influenciou cada PCA
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             xlab = "PC1",
             ylab = "PC2"
)

#Plot de cada individuo da abalone usando apenas PC1 e PC2 separados por classes e com 'elipses' para tentar agrupar as classes
autoplot(res.pca, data = dados, colour = 'Type', frame = TRUE, frame.type = 'norm')


