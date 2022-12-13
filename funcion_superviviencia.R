library(foreign)
library(survival)
library(mclust)
library(LifeTables)
library(KMsurv)
library(nlme)
library(muhaz)
library(MASS)
library(TH.data)
library(ggplot2)
library(proto)
library(GGally)
library(lattice)
library(mice)
Datos<-read.csv("Churn_Modelling.csv")
Datos
summary(Datos)
str(Datos)
attach(Datos)
tabla1<-table(HasCrCard)
tabla1
b<-barplot(tabla1,
           col=rainbow(4),
           main="Tarjetahabientes",
           col.main="black", 
           xlab ="   Con Tarjeta                                            Sin tarjeta",
           col.lab="black",
           ylim = c(0,8000))

tabla2<-table(IsActiveMember)
tabla2

b<-barplot(tabla2,col=rainbow(5),
           main="Miembros Activos"
           ,col.main="black", 
           xlab = "Inactivos                   Activos",
           ylab = "Numero de personas",
           col.lab="black",
           ylim = c(0,8000))


tabla3<-table(NumOfProducts)
tabla3

b<-barplot(tabla3,col=rainbow(4),main="Productos contratados",
           col.main="black",
           xlab = "Produtos",
           ylab = "Numero de Clientes",
           col.lab="black",
           ylim = c(0,8000))


tabla4<-table(Exited)
tabla4

b<-barplot(tabla4,col=rainbow(5),
           main="Miembros Fuera",
           col.main="black", 
           xlab = "Estatus",
           col.lab="black",
           ylim = c(0,8000))


a<-ggplot(data=Datos, aes(x=CreditScore))+
  geom_histogram(breaks=seq(300, 800, by =50),aes(fill=..count..))+
  scale_fill_gradient("Clientes", low = "gray", high = "blue")
a+ labs(title="Calificación crediticia",
        x ="Malo                                      Regular                            Excelente", y = "Clientes") 

summary(Datos$Geography)
barplot(table(Datos$Geography),col=c("blue3","orange1","firebrick"), main="País", ylab ="Clientes")

summary(Datos$Gender)
barplot(table(Datos$Gender),col=c("darkgoldenrod1","forestgreen"), main="Género", ylab ="Clientes")

summary(Datos$Age)
c<-ggplot(data=Datos, aes(x=Age))+
  geom_histogram(breaks=seq(18, 92, by =2),aes(fill=..count..))+
  scale_fill_gradient("Clientes", low = "lightblue3", high = "lightpink")
c+ labs(title="Edad",
        x ="Años", y = "Clientes")

summary(Datos$Tenure)

d<-ggplot(data=Datos, aes(x=Tenure))+
  geom_histogram(breaks=seq(0, 20, by =1),aes(fill=..count..))+
  scale_fill_gradient("Clientes", low = "red", high = "lightcoral")
d+ labs(title="Tiempo de Permanencia",
        x ="Años de permanencia", y = "Clientes")

attach(Datos)

datos1.surv<-Surv(Tenure,Exited)

#Utilizando Kaplan-Meier
estimador<-survfit(datos1.surv~1,type="kaplan-meier", conf.type="log-log", conf.int=0.95, data=Datos)
summary(estimador)

plot(estimador, main="Funcion de supervivencia", xlab="Tiempo" , ylab="S(t)", col=2:4,lwd=1)
legend("bottomleft", c("Estimador K-M", "Lower", "Upper"), lty=c(1,1,1), col=2:4, lwd=1,xjust = 1,yjust = 1)

#Funcion de riesgo acumulado
plot(estimador, main="Funcion de riesgo",
     xlab="Tiempo", ylab="h(t)", 
     fun="cumhaz", conf.int=F, col="red")

#K-M para sexo
comp1 <- survfit(Surv(Tenure,Exited)~factor(Gender), type = "kaplan-meier", conf.type="plain",
                 data=Datos)

plot(comp1, conf.int=F,xlab="Tiempo", ylab="Supervivencia", lty=c(1,4), col=c("blue","pink1"),
     main = "Comparación S(t) variable Gender")
legend("bottomleft", c("Hombre", "Mujer"), lty=c(1,4), col=c("blue", "pink1"))

#Comparación de supervivencias

survdiff(Surv(Tenure,Exited)~factor(Gender),rho = 0)

survdiff(Surv(Tenure,Exited)~factor(Gender),rho = 1)

#K-M para IsActiveMember
comp2 <- survfit(Surv(Tenure,Exited)~factor(IsActiveMember), type = "kaplan-meier", conf.type="plain",
                 data=Datos)

plot(comp2, conf.int=F,xlab="Tiempo", ylab="S(t)", lty=c(1,4), col=c("blue","red"),
     main = "Comparación de S(t) para la variable IsActiveMember")
legend("bottomleft", c("Inactivos","Activos"), lty=c(1,1), 
       col=c("blue","red", xjust= 1, yjust = 1))

#Comparación de supervivencias

survdiff(Surv(Tenure,Exited)~factor(IsActiveMember),rho = 0)

survdiff(Surv(Tenure,Exited)~factor(IsActiveMember),rho = 1)

#K-M para HasCrCard
comp3 <- survfit(Surv(Tenure,Exited)~factor(HasCrCard), type = "kaplan-meier", conf.type="plain",
                 data=Datos)

plot(comp3, conf.int=F,xlab="Tiempo", ylab="S(t)", lty=c(1,4), 
     col=c("blue","red"),
     main = "Comparación de S(t) para la variable  HasCrCard")
legend("bottomleft", c("No Tiene", "Tiene"), 
       lty=c(1,4), col=c("blue","red"))

#Comparación de supervivencias

survdiff(Surv(Tenure,Exited)~factor(HasCrCard),rho = 0)

survdiff(Surv(Tenure,Exited)~factor(HasCrCard),rho = 1)

#K-M para NumOfProducts
comp4 <- survfit(Surv(Tenure,Exited)~factor(NumOfProducts), type = "kaplan-meier", conf.type="plain",
                 data=Datos)

plot(comp4, conf.int=F,xlab="Tiempo", ylab="S(t)", lty=c(1,4), 
     col=c("yellow","black","blue","red","green","violet","orange","magenta"),
     main = "Comparación de s(t) para la variable NumofProducts")
legend("bottomleft", c("2 productos","1 producto","3 productos", "4 productos"), lty=c(1,4), 
       col=c("yellow","black","blue","red"))

survdiff(Surv(Tenure,Exited)~factor(NumOfProducts),rho = 0)

survdiff(Surv(Tenure,Exited)~factor(NumOfProducts),rho = 1)

#Partiremos de la estimación KM

datos1.surv<-Surv(Tenure,Exited)
estimador<-survfit(datos1.surv~1,type="kaplan-meier", conf.type="log-log", conf.int=0.95, data=Datos)

## Modelo de Cox.

est1 <- coxph(datos1.surv~ Geography + Gender + Age  + Balance + NumOfProducts
              + HasCrCard + IsActiveMember + EstimatedSalary )
est1

exp(est1$coefficients)
modelo_Cox<-survfit(est1)
confint(est1, level = 0.95)

plot(modelo_Cox,main="S(t) estimada con\n el modelo de Cox\n para las variables",
     xlab="Tiempo", ylab="Proba. de sobrevivencia", 
     lwd=2, col="blue", cex.lab=0.7)

summary(est1)

variable_1 <- coxph(datos1.surv~Geography)
variable_1

variable_2 <- coxph(datos1.surv~Gender)
variable_2

variable_3 <- coxph(datos1.surv~Age)
variable_3

variable_4 <- coxph(datos1.surv~Balance)
variable_4