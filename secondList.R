# EXERCICIO 1
# LETRA A
'''As unidades experimentais representam a corrente (em microamperes), os fatores
sao o tipo de vidro e o tipo de fosforo no brilho os tratamentos são os nivels dos
fatores, ou seja o tipo de vidro 1 e 2 e os tipos de fosforo 1 ,2 e 3.'''
  
# LETRA B
'''Podemos adotar um modelo de efeitos fixos onde Y_ijk eh o brilho do k-esimo tubo de
televisão, mu eh o efeito comum independente do tipo de fosforo ou vidro, tau_i eh o efeito
do i-esimo tratamento do fator tipo de vidro e beta_j eh o efeito do j-esimo tratamento 
do fator tipo de fosforo e o taubeta_ij eh o efeito de iteracao entre os dois fatores ja citados
enquanto e_ij eh o erro aleatorio presente no modelo.'''
  
# LETRA C
'''O intuito eh testar se de fato os tipos se o efeito do tipo de vidro, efeito tipo de fósforo no 
brilho de um tubo de televisão e a interacao entre eles influenciam na corrente.'''
  
# LETRA D
y <- c(280,290,285,300,310,295,290,285,290,230,235,240,260,240,235,220,225,230)
tipo_de_fosforo <- factor(c(rep(1,3), rep(2,3),rep(3,3),rep(1,3), rep(2,3),rep(3,3)))
tipo_de_vidro <- factor(c(rep(1,9), rep(2,9)))
  
interaction.plot(tipo_de_fosforo, tipo_de_vidro, y)
'''O gráfico nos mostra que os fatores nao tem interacao entre si pois as linhas estao praticamente
paralelas e distantes uma da outra. '''
  
#LETRA E
modelo <-lm(y ~ tipo_de_fosforo+tipo_de_vidro)
anova(modelo)
plot(modelo)
#
#Exemplo das notas de aula
#
dif<-c(1.4,2.4,2.2,2.1,1.7,0.7,1.1,2.4,2.5,1.8,2.0,0.5,0.9,1.3)
genero<-c(rep("Masc",7),rep("Femin",7))
desen<-c(rep("Severo",3),rep("Moderado",2),rep("Leve",2),rep("Severo",1),rep("Moderado",3),rep("Leve",3))
#
interaction.plot(desen,genero,dif,fun=mean,xlab='Comprometimento do osso',ylab='Dif. na razao de crescimento',lty=1:2)
#
##Matriz de planejamento do modelo completo
X<-matrix(c(1,1,1,0,1,0,1,1,1,0,1,0,1,1,1,0,1,0,
            1,1,0,1,0,1,
            1,1,0,1,0,1,
            1,1,-1,-1,-1,-1,
            1,1,-1,-1,-1,-1,
            1,-1,1,0,-1,0,
            1,-1,0,1,0,-1,
            1,-1,0,1,0,-1,
            1,-1,0,1,0,-1,
            1,-1,-1,-1,1,1,
            1,-1,-1,-1,1,1,
            1,-1,-1,-1,1,1),nrow=14,ncol=6,byrow=TRUE)

difHat <- X%*%solve(t(X)%*%X)%*%t(X)%*%dif
#
modeloc<-lm(dif~X-1) #aqui eu tirei o intercepto porque já tenho a coluna de 1s na matriz X, se eu não tivesse basta retirar "-1" do comando
modeloc
anova(modeloc)
#
modelorint<-lm(dif~X[,-c(5,6)]-1) #aqui eu tirei o intercepto porque já tenho a coluna de 1s na matriz X, se eu não tivesse basta retirar "-1" do comando
modelorint
anova(modelorint)
#
modelorA<-lm(dif~X[,-c(2)]-1) #aqui eu tirei o intercepto porque já tenho a coluna de 1s na matriz X, se eu não tivesse basta retirar "-1" do comando
modelorA
anova(modelorA)
#
modelorB<-lm(dif~X[,-c(3,4)]-1) #aqui eu tirei o intercepto porque já tenho a coluna de 1s na matriz X, se eu não tivesse basta retirar "-1" do comando
modelorB
anova(modelorB)