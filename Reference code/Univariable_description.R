# MÉTODOS DESCRIPTIVOS EN R

AFASIA <- c("Expresiva","Anómica","Anómica","Receptiva","Expresiva","Receptiva","Receptiva","Anómica","Receptiva","Anómica","Receptiva","Expresiva","Anómica","Expresiva","Anómica","Anómica","Anómica","Receptiva","Expresiva","Anómica","Receptiva","Anómica")
MILLAJE <- c(36.3,41.0,36.9,37.1,44.9,36.8,30.0,37.2,42.1,36.7,32.7,37.3,41.2,36.6,32.9,36.5,33.2,37.4,37.5,33.6,40.5,36.5,37.6,33.9,40.2,36.4,37.7,37.7,40.0,34.2,36.2,37.9,36.0,37.9,35.9,38.2,38.3,35.7,35.6,35.1,38.5,39.0,35.5,34.8,38.6,39.4,35.3,34.4,38.8,39.7,36.3,36.8,32.5,36.4,40.5,36.6,36.1,38.2,38.4,39.3,41.0,31.8,37.3,33.1,37.0,37.6,37.0,38.7,39.0,35.8,37.0,37.2,40.7,37.4,37.1,37.8,35.9,35.6,36.7,34.5,37.1,40.3,36.7,37.0,33.9,40.1,38.0,35.2,34.8,39.5,39.9,36.9,32.9,33.8,39.8,34.0,36.8,35.0,38.1,36.9)
DAÑOS <- c(2,1,2,4,0,1,3,2,0,5,3,3,1,3,2,4,7,0,2,3,0,4,2,1,3,1,1,3,4,1,2,3,2,2,8,4,5,1,3,1,5,0,2,3,2,1,0,6,4,2,1,6,0,3,3,3,6,1,2,3)

# DESCRIPCIÓN DE VARIABLES CUALITATIVAS
# Se construyen las distribuciones de frecuencias absolutas y algunos gráficos
tabas <- table(AFASIA)
pie(tabas)
pie(tabas,col=c(1,2,3),main="Distribución de los casos de afasia")
barplot(tabas)
barplot(tabas,col=c(1,2,3),legend=T,ylim=c(0,15),ylab="Cantidad",main="Distribución de los casos de afasia")

# Se construyen las distribuciones de frecuencias relativas
pafas <- prop.table(tabas)

# DESCRIPCIÓN DE VARIABLES DISCRETAS
# Se construye la distribución de frecuencias y el diagrama de barras
tbl <- table(DAÑOS)
barplot(tbl) 
barplot(tbl,xlab="Danos por lote",ylab="Frecuencia",main="Distribución de los danos"))

# Se calculan algunas medidas de resumen
xmin <- min(DAÑOS)
xmax <- max(DAÑOS)
xbar <- mean(DAÑOS)
xmed <- median(DAÑOS)
Q1 <- quantile(DAÑOS,0.25)
Q3 <- quantile(DAÑOS,0.75)
ran <- IQR(DAÑOS)
xvar <- var(DAÑOS)
xsd <- sd(DAÑOS)
summary(DAÑOS)

# Se construye el diagrama de caja y bigote
boxplot(DAÑOS)

# DESCRIPCIÓN DE VARIABLES CONTINUAS
mil <- table(MILLAJE)
hist(MILLAJE)
hist(MILLAJE,"Scott")


# ANÁLISIS DE DATOS ATÍPICOS
# Datos de la OCDE
# Después de ubicarse en el directorio de tragajo hacer:

datos <- read.csv("OCDE.csv",sep=";",dec=",",header=T)
datos
tasas <- datos$TIP
n <- length(tasas)
n
hist(tasas,breaks="Scott")
hist(tasas)

xbar <- mean(tasas)
xbar
des <-sqrt((n-1)*var(tasas)/n) 
des
inf <- xbar-3*des
inf
sup <- xbar+3*des
sup
quien.xbar <- tasas[tasas>sup]
quien.xbar
cual.xbar <- subset(datos,subset=datos$TIP>sup)
cual.xbar

med <- median(tasas)
med
MEDA <- median(abs(tasas-med))
MEDA
medinf <- med-4.5*MEDA
medinf
medsup <- med+4.5*MEDA
medsup
quien.med <- tasas[tasas>medsup]
quien.med
cual.med <- subset(datos,subset=datos$TIP>medsup)
cual.med

prop <- c(0.25,0.5,.75)
cuartiles <- quantile(tasas,prop)
cuartiles
q3 <- quantile(tasas,0.75)
q3
iqr <- IQR(tasas)
iqr

boxplot(tasas)
cual.box <- subset(datos,subset=datos$TIP>q3+1.5*iqr)
cual.box

boxplot(TIP~UE,data=datos,ylab="Tasas de incremento de precios,(%)")

X <- subset(datos,subset=datos$UE==0)
Y <- subset(datos,subset=datos$UE==1)

boxplot(X$TIP)
cuales.X <- subset(X,subset=X$TIP > quantile(X$TIP,0.75)+1.5*IQR(X$TIP))
cuales.X

boxplot(Y$TIP)
cuales.Y <- subset(Y,subset=Y$TIP > quantile(Y$TIP,0.75)+1.5*IQR(Y$TIP))
cuales.Y

# Transformaciones

log.tasas <- log(tasas)
hist(log.tasas)
hist(log.tasas,breaks="Scott")
boxplot(log.tasas)
cuales.log <- subset(datos,subset=log(datos$TIP) > quantile(log.tasas,0.75)+1.5*IQR(log.tasas))
cuales.log

# Tablas de contingencia

datos <- read.csv("Encuesta.csv",sep=";",dec=",",header=T)
datos
X <- datos$CLASE
Y <- datos$SATISFACCIÓN
tcon <- table(X,Y)
addmargins(tcon)
addmargins(prop.table(tcon))
prop.table(tcon,1)
prop.table(tcon,2)

barplot(table(Y,X),beside=T,legend=T,ylim=c(0,35))
barplot(table(Y,X),beside=F,legend=T,ylim=c(0,70),col=c(2,3,4))
barplot(table(Y,X),beside=F,legend=T,ylim=c(0,70),xlab="Clase de tiquete adquirido",ylab="Frecuencias de ocurrencia",main="Distribución del nivel de satisfacción \n deacuerdo con la clase de tiquete")

library(lattice)
barchart(table(X,Y),horizontal=FALSE,stack=FALSE,xlab="Clase de tiquete adquirido",ylab="Frecuencias de ocurrencia")
barchart(table(X,Y),horizontal=FALSE,stack=TRUE)

# Medidas de asociación

Cien <- c(10.85,10.44,10.50,10.89,10.62)
Salto <- c(7.84,7.96,7.81,7.47,7.74)
Atleta <- c("Roman Sebrle","Bryan Clay","Dmitriy Karpov","Dean Macey","Chiel Warners")
datos <- data.frame(Atleta,Cien,Salto)
X <- datos$Cien
Y <- datos$Salto
plot(X,Y)
abline(lm(Y~X))
cor(X,Y)

# Load the iris dataset.
data(iris)

iris1 <- iris[,-5]
sapply(iris1,mean)
cov(iris1)
cor(iris1) 

# Plot #1: Basic scatterplot matrix of the four measurements
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris)
boxplot(Sepal.Length~Species,data=iris,ylab="Longitud del sépalo (cm)", main="Mediciones en iris")



