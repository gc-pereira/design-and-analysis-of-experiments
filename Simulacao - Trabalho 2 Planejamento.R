library(ggplot2)
library(nortest)
library(PMCMRplus)
library(car)

set.seed(21) # Semente

mu = 70
sigma2 = 6
n = 5
sigma2_tau = 10

tau = rnorm(5,0,sqrt(sigma2_tau)) # Fator aluno
beta = rnorm(3,0,0) # Fator professor

taubeta = rbind(c(0,0,0), #Nao existem efeitos do fator professor e de interacao
                c(0,0,0),
                c(0,0,0),
                c(0,0,0),
                c(0,0,0))

erros = function(n,sigma2){
  epsilon = rnorm(n,0,sqrt(sigma2))
  return(epsilon)
}

data.sim = function(mu,n,tau,beta,taubeta,sigma2){
  y = numeric(0)
  x1 = numeric(0)
  x2 = numeric(0)
  for(i in 1:length(tau)){
    for(j in 1:length(beta)){
      e_i = erros(n,sigma2)
      y_i = mu + tau[i] + beta[j] + taubeta[i,j] + e_i
      y_i = round(y_i,digits = 2)
      y = c(y,y_i)
      xi = rep(i,n)
      xj = rep(j,n)
      x1 = c(x1,xi)
      x2 = c(x2,xj)
    }
  }
  data = data.frame(Aluno = factor(x1),
                     Professor = factor(x2),
                     Y = y)
  return(data)
}

dados = data.sim(mu,n,tau,beta,taubeta,sigma2)

modelo = aov(data = dados, Y~Aluno*Professor)
anova(modelo)

F_aluno = 286.416/4.722
pvalor_aluno = pf(F_aluno,4,8,lower.tail = F)

F_professor = 1.758/4.722
pvalor_professor = pf(F_professor,2,8,lower.tail = F)

sigma2_tau_chapeu = (53.505-6.726)/9

ggplot(modelo,aes(sample = .resid))+ 
  stat_qq(distribution = qnorm) + 
  stat_qq_line(distribution = qnorm,size = 1,col = "red") +
  xlab("Quantis teóricos") + ylab("Quantis amostrais") +
  ggtitle("Gráfico de probabilidade normal")

res=residuals(modelo)
mean.res = mean(res)
sd.res = sd(res)

shapiro.test(res)
ad.test(res)
lillie.test(res)
ks.test(res,"pnorm",mean.res,sd.res,alternative = "two.sided")

bartlett.test(data=dados,Y~interaction(Aluno,Professor))
hartleyTest(data=dados,Y~interaction(Aluno,Professor))
leveneTest(data=dados,Y~Aluno*Professor,center=mean)
leveneTest(data=dados,Y~Aluno*Professor,center=median)

ggplot(modelo,aes(x = .fitted ,y = .resid)) + geom_point() +
  xlab("Valores Preditos") + ylab("Resíduos") +
  ggtitle("Gráfico de valores preditos contra resíduos")


ggplot(dados,aes(x=Aluno,y=Y,fill=Aluno)) + geom_boxplot() + scale_fill_brewer(palette="Dark2")
ggplot(dados,aes(x=Professor,y=Y,fill=Professor)) + geom_boxplot() + scale_fill_brewer(palette="Dark2")

ggplot(dados,aes(y=Y,x=Professor,color=Aluno,group = Aluno)) +
  stat_summary(fun = mean,geom = "point",size = 2) +
  stat_summary(fun=mean,geom = "line",size=1) +
  ggtitle("Grafico de interação entre Professor e Aluno em relacao à nota")

