install.packages("magrittr")
install.packages("tidyverse")
install.packages()
library(magrittr)
library(tidyverse)
library(patchwork)
require(magrittr)
require(tidyverse)
require(patchwork)
require(nortest)
require(car)

#TRABALHO 1 - PLANEJAMENTO E ANÁLISE DE EXPERIMENTOS
set.seed(6)

#Fixando os parâmetros, respeitando as restrições.
tau1 <- runif(1, min = 0, max = 1)
tau1 <- round(tau1, 1)
tau2 <- -tau1
tau1 + tau2

betas <- c(1,0,0)
while (sum(betas) != 0) {
  betas <- runif(3, min = -30, max = 30)
  betas <- round(betas, 1)
}

beta1 <- betas[1]
beta2 <- betas[2]
beta3 <- betas[3]
beta1+beta2+beta3

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

a=c(rep(mu,48))
b=c(rep(tau1,24), rep(0,24))
c=c(rep(0,24), rep(tau2,24))
d=c(rep(c(rep(beta1,8),rep(0,16)),2)) 
e=c(rep(c(rep(0,8), rep(beta2,8), rep(0,8)),2))
f=c(rep(c(rep(0,16), rep(beta3,8)),2))
g=c(rep(tau1beta1,8), rep(0,40))
h=c(rep(0,8), rep(tau1beta2, 8), rep(0,32))
i=c(rep(0,16), rep(tau1beta3,8), rep(0,24))
j=c(rep(0,24), rep(tau2beta1,8), rep(0,16))
l=c(rep(0,32), rep(tau2beta2,8), rep(0,8))
m=c(rep(0,40), rep(tau2beta3,8))

x=cbind(a,b,c,d,e,f,g,h,i,j,l,m)
x

#Gerando os fatores e os erros
A=as.factor(c(rep("N11",24),rep("N12",24)))
B=as.factor(rep(c(rep("N21",8),rep("N22",8),rep("N23",8)),2))
e_r= rnorm(48,0,7)
plot(density(e_r))

# Variável resposta
Y= apply(x, 1, sum) + e_r
data =data.frame(cbind(A, B, Y))
data

# Analisando Y
ba=data %>% 
  ggplot(aes(x = as.factor(A), y = Y)) + 
  geom_boxplot(color = "red", fill = "pink", width = .5)+
  labs( y = 'Y',
        x = 'Fator A')+
  theme_minimal()


bb=data %>% 
  ggplot(aes(x = as.factor(B), y = Y)) + 
  geom_boxplot(color = "red", fill = "pink")+
  labs( y = 'Y',
        x = 'Fator B')+
  theme_minimal()

ba + bb

interaction.plot(A, B, Y, fun = mean,xlab='A',ylab='Y',lty = 1:3)

#ANOVA
model = aov(Y ~ A*B)
anova(model)

#Suposições
res= residuals(model)
pred= fitted.values(model)
par(mfrow=c(1,2))
plot(pred,res,xlab='Valores preditos',ylab='Residuos',
     main = "Residuos x Preditos", col="red")
qqnorm(res,xlab='Quantis teoricos', ylab='Quantis amostrais', col="red")
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
