library(nortest)
library(car)
library(magrittr)
library(tidyverse)
library(patchwork)

#TRABALHO 1: PLANEJAMENTO E ANALISE DE EXPERIMENTOS
#ANOVA DOIS FATORES FIXOS

set.seed(15)

#Fixando os parametros, respeitando as restricoes.
tau1 <- runif(1, min = 0, max = 1)
tau1 <- round(tau1, 1)
tau2 <- -tau1


betas <- c(1,0,0)
while (sum(betas) != 0) {
  betas <- runif(3, min = -30, max = 30)
  betas <- round(betas, 1)
}

beta1 <- betas[1]
beta2 <- betas[2]
beta3 <- betas[3]


somaLinhas1 <- 1
somaLinhas2 <- 1
somaColunas1 <- 1
somaColunas2 <- 1
somaColunas3 <- 1
while (somaLinhas1 != 0 | somaLinhas2 != 0 | somaColunas1 != 0 | somaColunas2 != 0 | somaColunas3 != 0) {
  tau1beta1 <- runif(1, min = -1, max = 1)
  tau1beta1 <- round(tau1beta1, 1)
  
  tau2beta1 <- runif(1, min = -1, max = 1)
  tau2beta1 <- round(tau2beta1, 1)
  
  tau1beta2 <- runif(1, min = -1, max = 1)
  tau1beta2 <- round(tau1beta2, 1)
  
  tau2beta2 <- runif(1, min = -1, max = 1)
  tau2beta2 <- round(tau2beta2, 1)
  
  tau1beta3 <- runif(1, min = -1, max = 1)
  tau1beta3 <- round(tau1beta3, 1)
  tau2beta3 <- runif(1, min = -1, max = 1)
  tau2beta3 <- round(tau2beta3, 1)
  
  somaLinhas1 <- tau1beta1 + tau1beta2 + tau1beta3
  somaLinhas2 <- tau2beta1 + tau2beta2 + tau2beta3
  somaColunas1 <- tau1beta1 + tau2beta1
  somaColunas2 <- tau1beta2 + tau2beta2
  somaColunas3 <- tau1beta3 + tau2beta3
}

mu <- runif(1, min = 0, max = 100)
mu <- round(mu)

a=c(rep(1,48))
b=c(rep(1,24), rep(0,24))
c=c(rep(0,24), rep(1,24))
d=c(rep(c(rep(1,8),rep(0,16)),2)) 
e=c(rep(c(rep(0,8), rep(1,8), rep(0,8)),2))
f=c(rep(c(rep(0,16), rep(1,8)),2))
g=c(rep(1,8), rep(0,40))
h=c(rep(0,8), rep(1, 8), rep(0,32))
i=c(rep(0,16), rep(1,8), rep(0,24))
j=c(rep(0,24), rep(1,8), rep(0,16))
l=c(rep(0,32), rep(1,8), rep(0,8))
m=c(rep(0,40), rep(1,8))

X=cbind(a,b,c,d,e,f,g,h,i,j,l,m)
parametros=matrix(c(mu, tau1, tau2, beta1, beta2, beta3,tau1beta1,tau1beta2,
                    tau1beta3,tau2beta1, tau2beta2, tau2beta3),ncol=1)

#Gerando os fatores e os erros
A=as.factor(c(rep("N11",24),rep("N12",24)))
B=as.factor(rep(c(rep("N21",8),rep("N22",8),rep("N23",8)),2))
e_r= rnorm(48,0,7)
plot(density(e_r))

# Variavel resposta
Y= X%*%parametros + e_r
data =data.frame(cbind(A, B, Y))
data

# Analisando Y
ba=data %>% 
  ggplot(aes(x = as.factor(A), y = Y)) + 
  geom_boxplot(color = "red", fill = "pink", width = .5)+
  labs( y = ' ',
        x = ' ')+
  theme_minimal()+
  coord_flip()+
ba

bb=data %>% 
  ggplot(aes(x = as.factor(B), y = Y)) + 
  geom_boxplot(color = "red", fill = "pink")+
  labs( y = ' ',
        x = ' ')+
  theme_minimal()+
  coord_flip()
bb

interaction.plot(A, B, Y, fun = mean,xlab='A',ylab='Y',lty = 1:3)

#ANOVA
model = aov(Y ~ A*B)
anova(model)

#Suposicoes
res= residuals(model)
pred= fitted.values(model)
par(mfrow=c(1,2))
plot(pred,res,xlab='Valores preditos',ylab='Residuos',
     main = "Residuos x Preditos", col="red",  pch = 19)
qqnorm(res,xlab='Quantis teoricos', ylab='Quantis amostrais', col="red", pch = 19)
qqline(res)

# Testes para checar suposicoes
if(!require(nortest)) install.packages("nortest");require(nortest)
lillie.test(res)
shapiro.test(res)
xb= mean(res)
sx= sd(res)
ks.test(res, "pnorm", xb, sx,alternative='two.sided')
ad.test(res)
j= paste(A,B)
leveneTest(Y~j)

#Resultados da ANOVA
anova(model)

#Comparacoes multiplas de Tukey para Beta
compar=TukeyHSD(x=model, 'B', conf.level=0.95)
compar

compar2 = TukeyHSD(x=model, 'A', conf.level = 0.99)
compar2

#regioes criticas
x <- seq(0, 5, 0.01)
regiao <- seq(qf(0.95, 2, 42),10, 0.001)
cord.x <- c(min(regiao), regiao, max(regiao))
cord.y <- c(0, df(regiao, 2, 42), 0)
curve(df(x, 2, 42), xlim = c(0,5), ylim = c(0,0.8), ylab = "", xaxs = "i", yaxs = "i", col="black",lwd=2, xaxt='n')
polygon(cord.x,cord.y,col='orange2')
axis(side=1,at = c(qf(0.95, 2, 42)), 
     labels = c(round(qf(0.95, 2, 42), 4)))

x <- seq(0, 5, 0.01)
regiao <- seq(qf(0.95, 1, 42),10, 0.001)
cord.x <- c(min(regiao), regiao, max(regiao))
cord.y <- c(0, df(regiao, 1, 42), 0)
curve(df(x, 1, 42), xlim = c(0,5), ylim = c(0,2), ylab = "", xaxs = "i", yaxs = "i", col="black",lwd=2, xaxt='n')
polygon(cord.x,cord.y,col='orange2')
axis(side=1,at = c(qf(0.95, 1, 42)), 
     labels = c(round(qf(0.95, 1, 42), 4)))

#encontrando a média por nível de fator
somaFator1 <- 0
somaFator2 <- 0
somaFator3 <- 0
for(i in 1:length(data$B)){
  if(data$B[i] == 1){
    somaFator1 <- somaFator1 + data$V3[i]
  }
  if(data$B[i] == 2){
    somaFator2 <- somaFator2 + data$V3[i]
  }
  if(data$B[i] == 3){
    somaFator3 <- somaFator3 + data$V3[i]
  }
}
y.1 <- somaFator1/16
y.2 <- somaFator2/16
y.3 <- somaFator3/16

somaFator1A <- 0
somaFator2A <- 0
for(i in 1:length(data$A)){
  if(data$A[i] == 1){
    somaFator1A <- somaFator1A + data$V3[i]
  }
  if(data$A[i] == 2){
    somaFator2A <- somaFator2A + data$V3[i]
  }
}

x.1 <- somaFator1A/24
x.2 <- somaFator2A/24
