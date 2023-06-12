#################################################
# Universidad nacional José Faustino Sánchez Carrión                                                                                                                                                 Mg. Víctor Guevara P.
# Mg. Victor Guevara P.
# Sesión: Valores ausentes y valores outliers
################################################ 
# ---- Verificar y configurar un directorio de trabajo ---- 

# Verificar el directorio de trabajo
getwd() 
# modificar el directorio de trabajo
#setwd("C:/micarpeta/analisis_multivariado/sesion3/")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#####################################################
# Caso: Suscripción bancaria
#####################################################
# Hoy en día las entidades bancarias constantemente ofrecen diversos productos 
# o servicios a sus clientes, entre estos se tiene a: lineas de crédito, tarjetas, 
# seguros, cuentas de ahorro, etc. El caso presentado corresponde a una campaña 
# realizado por una entidad local, para la suscripción de un servicio especial de crédito.
# La columna suscrito, representa al resultado de la campaña, 1. Si el cliente se suscribió al servicio y 
# 0. Si el cliente no se suscribió
# La entidad guarda información de cada uno de sus clientes.
# El detalle de las varibles es:
# X: Id registro
# duracion: Tiempo en meses de permanencia en la entidad
# monto: Monto de deuda del último crédito
# tasa: Tasa brindada al cliente
# Residencia: Residencia del cliente
# Edad: Edad del cliente
# tarjetas: Nro de tarjetas asignadas al cliente
# NroUsuarios: Nro de usuarios de las tarjetas asignadas
# Limite_Credito: Categorización del limite de crédito
# Nueva_cuenta: Nombre de la cuenta
# razonPrestam: Motivo del último prestamo asignado
# expiración: Expiración de la tarjeta
# EstadoCivil: Estado civil
# deuda: Garantía en préstamos 
# propiedades: Propiedades por parte del cliente
# creditos_otro: Si el cliente declaró créditos en otras entidades
# vivienda: Tipo de vivienda
# empleo:  Tipo de empleo
# telefono: Si el cliente tiene teléfono o no
# suscrito: Resultado de la última campaña realizada.

# Carga de datos
url<-"https://raw.githubusercontent.com/VictorGuevaraP/ME-Machine-Learning/master/banco%20mod.csv"
banco_df<-read.csv(url, sep = ",", stringsAsFactors = T, encoding = "latin1")
# Mostrar los primeros registros
head(banco_df)
View(banco_df)
# revisar la estructura de los datos
str(banco_df)
# Ver editor de datos
fix(banco_df)
# Resumen de los datos
summary(banco_df)

#----Verificación y tratamiento de valores missing----
which(colSums(is.na(banco_df))!=0)

# Porcentaje de valores perdidos
suma_perdidos<-colSums(is.na(banco_df[,c(2,10,18)]))
suma_perdidos
suma_perdidos/1219*100

# Verificar si hay patrones con librerias
library(VIM)
library(mice)
grafico_miss <- aggr(banco_df, numbers =T)
grafico_miss
summary(grafico_miss)
# Representación matricial
matrixplot(banco_df)

# otra representación
md.pattern(banco_df, rotate.names = T)

# Seguir visualizando, diferenciar por tipo de varaiable
library(visdat)
vis_dat(banco_df)
vis_miss(banco_df)

# Según los resultados el porcentaje de missing, respecto al total es 0.4% (Muy pequeño)
# El porcentaje de missing de acuerdo a cada columna esta entre 1-5% (manejable)
# En el análisis de observaciones, desde el punto de vista gráfico, no se observa
# Patrones de comportamiento, esto indicaría que se esta en un caso de Missin completamente al Azar (MCAR)
# Por lo tanto la solución sería aplica una técnica de imputación básica

# Tratamiento de missing

# Primer caso: Eliminar todos los missing
banco_sin_missing <- na.omit(banco_df)
dim(banco_df)
dim(banco_sin_missing)

# Aplicar técnicas de imputación de datos

# Caso básico o general
library(DMwR2)
# Reemplazo con la mediana y moda
banco_imp_mtc <- centralImputation(banco_df) # Mediana (numérico), moda (no numérico)
dim(banco_imp_mtc)
vis_miss(banco_imp_mtc)

# Reemplazo con la media, mediana y moda
library(VIM)
banco_imp_mtc2 <- initialise(banco_df, method="median") #Media (continuo), Mediana (discreto), moda (No numérico)
dim(banco_imp_mtc2)

# Métodos no paramétricos

# M. KNN (Vecinos más cercanos)
library(DMwR2)
banco_imp_knn <- knnImputation(banco_df, k=10)
dim(banco_imp_knn)

# Comparando
mean(banco_df$duracion,na.rm = T)
mean(banco_sin_missing$duracion)
mean(banco_imp_mtc$duracion)
mean(banco_imp_mtc2$duracion)
mean(banco_imp_knn$duracion)

# Comparando graficamente
par(mfrow=c(2,2))
hist(banco_df$duracion)
hist(banco_sin_missing$duracion)
hist(banco_imp_mtc$duracion)
hist(banco_imp_knn$duracion)
par(mfrow=c(1,1))

# Comparando con la variable Nueva_cuenta
par(mfrow=c(2,2))
plot(banco_df$Nueva_cuenta)
plot(banco_sin_missing$Nueva_cuenta)
plot(banco_imp_mtc$Nueva_cuenta)
plot(banco_imp_knn$Nueva_cuenta)
par(mfrow=c(1,1))

#Obtener las proporciones
prop.table(table(banco_df$Nueva_cuenta))
prop.table(table(banco_sin_missing$Nueva_cuenta))
prop.table(prop.table(table(banco_imp_mtc$Nueva_cuenta)))
prop.table(table(banco_imp_knn$Nueva_cuenta))

# Imputación missforest
library(missForest)
banco_imp_msf <- missForest(banco_df)
banco_imp_msf <- banco_imp_msf$ximp
summary(banco_imp_msf)

# Imputación por regresión (pendiente)
banco_imp_reg <- irmi(banco_df)
summary(banco_imp_reg)

# Tarea:
# Revisar el conjuntos de datos: 
# https://raw.githubusercontent.com/VictorGuevaraP/Multivariado_Analisis/master/CHURNM.CSV
# análizar todo lo correspondiente a valores missing


###########################################################
# ----Evaluación y tratamiento de valores outliers----
##########################################################
banco_c <- banco_imp_knn 
str(banco_c)

# Eliminar la columna X
banco_c <- banco_c[,2:20]
str(banco_c)

# outliers univariado
boxplot(banco_c[,1:7])

# transformación o estandariazación de datos
# (banco_c$duracion-mean(banco_c$duracion))/sd(banco_c$duracion)
# scale(banco_c$duracion)
head(banco_c)
# Estandarizar solo las variables cuantitativas y juntarlo con las cualitativas
banco_estandar<- cbind(scale(banco_c[,1:7]),banco_c[,8:19])
head(banco_estandar)

# Identificando valores outliers a partir de los estandarizados
rownames(banco_estandar[abs(banco_estandar[,1])>3,])

# Valores outliers a partir de percentiles
outlier<-boxplot(banco_c$duracion)$out
outlier

outtext<-as.character(outlier)
boxplot(banco_c$duracion)
for(i in 1:length(outlier))
{
  text(outlier[i],as.character(which(banco_c$duracion==outlier[i])), 
       cex = 0.6, pos=2)
}

banco_c[672:680,]

# outliers bivariado
plot(banco_c$duracion, banco_c$monto)
identify(banco_c$duracion, banco_c$monto)
banco_c[812:820,]

# Outliers multivariado (distancia de mahalanobis)
out<-mahalanobis(banco_c[,1:7],colMeans(banco_c[,1:7]),cov(banco_c[,1:7]))
out
barplot(out)
which.max(out)
boxplot(out, ylab = "Distancias de Mahalanobis")

# Mahalanobis tiene una distribución Chi Cuadrado con g.l. igual al número de variables analizadas
# Los datos atipicos, seran considerados aquellos que su probabilidad sean menores a ....
pchisq(out,7)

options(digits = 5)
# Combinamos los datos originales con la distancia de Mahalanobis y la probablidad chi-cuadrada
outliers_multi<-cbind(banco_c[,1:7],out, "p-valor"=round(pchisq(out,7),5))
head(outliers_multi)

# Mostrar solo atípicos
atipicos = subset(outliers_multi,  outliers_multi[9] < 0.01)
atipicos
atipicos[order(atipicos$out,decreasing = TRUE),]


# Paso 1: Importar la biblioteca "tidyverse"
library(tidyverse)

# Paso 2: Crear los vectores con los datos
A = c(62,60,63,59)
B = c(63,67,71,64,65)
C = c(68,66, 71,67,68)
D = c(56,62,60,61,63,64)

# Paso 3: Crear el DataFrame utilizando la función data.frame()
df <- data.frame(A = A, B = B, C = C, C=C)

d  = data.frame(A = c(62,60,63,59,NA ,NA),
                B = c(63,67,71,64,65,NA),
                C = c(68,66, 71,67,68,NA),
                D = c(56,62,60,61,63,64))
d

library(stats)
# Realizar el análisis de varianza
model = lm(res)
modelo <- lm(d ~A , data = d)
anova_resultados <- anova(modelo)
