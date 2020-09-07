install.packages("ggplot2")
install.packages("nortest")
install.packages("car")
library(ggplot2)
library(nortest)
library(car)

# Arrays com o resultado dos alunos por turma #
turmaA <- c(21,23,13,19,13,19,20,21,16)
turmaB <- c(17,14,24,20,24,23,16,15,24)
turmaC <- c(28,30,29,24,27,30,28,28,23)
turmaD <- c(19,28,26,26,19,24,24,23,22)
turmaE <- c(21,14,13,19,15,15,10,18,20)

# Criação do data frame com as notas dos alunos e os tratamentos #
notasDosAlunos <-c(turmaA, turmaB, turmaC, turmaD, turmaE)
tratamentos <-c(rep("Controle",9),rep("Suplementares",9), rep("Elogiados",9), rep("Censurados", 9), rep("Ignorados",9))
tratamento <- factor(tratamentos)
dados <- data.frame(notasDosAlunos,tratamentos)

# Gráfico de caixas dos dados #
boxPlot <- ggplot(dados, aes(x=tratamentos, y=notasDosAlunos,fill=tratamentos)) +
      geom_boxplot(outlier.color = "red") +
      labs(
             title = "Boxplot do resultado final da avaliação por tratamento", 
             x="Tratamentos (Diferentes atitudes dos professores",
             y="Notas dos alunos"
             )
boxPlot

# Médias por tratamento
yBarraUmPonto <-mean(turmaA)
yBarraDoisPonto <- mean(turmaB)
yBarraTresPonto <- mean(turmaC)
yBarraQuatroPonto <- mean(turmaD)
yBarraCincoPonto <- mean(turmaE)
vetorDeMedias <- c(yBarraUmPonto, yBarraDoisPonto, yBarraTresPonto, yBarraQuatroPonto, yBarraCincoPonto)

# Média Global
yBarraPontoPonto <- mean(dados$notasDosAlunos)

# Soma de quadrados
sqTratamentos <- sum(9*(vetorDeMedias - yBarraPontoPonto)^2)
sqTotal <- sum(dados$notasDosAlunos^2) - length(dados$notasDosAlunos)*yBarraPontoPonto^2
sqRes <- sqTotal - sqTratamentos

# ANOVA
modelo<-aov(notasDosAlunos~tratamentos, data = dados)
summary(modelo)
#
#análise de diagnóstico
plot(modelo)
qqPlot(res, dist='norm', envelope=0.99, pch = 19, xlab = "Quantis da normal", ylab = "Resíduos")
#
res<-residuals(modelo)
pred<-fitted.values(modelo)
par(mfrow=c(1,2))
plot(pred,res,xlab='Valores preditos',ylab='Residuos')

# testes de normalidade 
lillie.test(res)
shapiro.test(res)
xb <- mean(res)
sx <- sd(res)
ks.test(res, "pnorm", xb, sx,alternative='two.sided')
ad.test(res)

#comparações múltiplas de Tukey
compar <- TukeyHSD(x=modelo, 'tratamentos', conf.level=0.95)
compar
