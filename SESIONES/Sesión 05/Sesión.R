################################################
# Universidad nacional José Faustino Sánchez Carrión                                                                                                                                                 Mg. Víctor Guevara P.
# Mg. Victor Guevara P.
# Sesión: Análisis CLUSTERING
################################################
#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())
 
#---------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#################################
# Preliminares

# Distancias                                     

x =matrix(rnorm(20), nrow=5)
dist(x)
dist(x, method= "manhattan",diag = TRUE)
dist(x, method= "maximum",upper = TRUE)

library(cluster)
x =matrix(rnorm(20), nrow=5)
daisy(x)
x = cbind(rnorm(10),sample(1:3,10,replace=T))
x<-as.data.frame(x)
x[,2]<-as.factor(x[,2])
x
str(x)
summary(x)
daisy(x)

library(factoextra)
fviz_dist(daisy(x))

fviz_dist(daisy(x), 
          gradient = list(low = "blue", mid = "white", high = "red"))

###################################################
# Ingresamos los datos
edad<-c(23,24,	25,	26,	27,	22,	23)
peso<-c(62,	68,	60,	71,	60,	62,	65)
# Creamos un dataframe para los estudiantes
estudiantes<-data.frame(cbind(edad,peso))
estudiantes

dist(scale(estudiantes))
####################################################
# 1. ----Clustering de Particionamiento----

# Caso: Manejo y uso de aplicativo movil - mesa de ayuda
#       Puntuaciones de 0 a 100

# Facilidad: Facilidad de comunicaci?n con mesa de ayuda
# SelecciÖn: Facilidad en seleccionar idiomas
# Sencillez: Sencillez de menu y opciones
# Tiempo: Tiempo de espera al solicitar ayuda
# Amabilidad: Amabilidad en la atencion
# gestion: Interes en resolver sus dudas
# conocimiento: conocimiento del personal 
# informacion: Claridad en la informaci?n
# atencion: Tiempo de atencion
# solucion: Soluci?n brindada a su caso

# Cargamos las librer?as adecuadas
library(ggplot2)
library(cluster)
library(cowplot)
library(factoextra)
library(tidyr)
library(klaR)
library(clustMixType)

# Cargamos el conjunto de datos
caso<-read.csv("caso_aplicacion.csv", 
                  sep = ";",
                  stringsAsFactors = T, 
               encoding = "latin1")

#Mostramos los datos
head(caso)

#Verificamos todo el caso
View(caso)

#Verificamos la estructura de los datos
str(caso)

#----ANÁLISIS EXPLORATORIO---- 

# Resumen de los datos
summary(caso)

# No existe datos perdidos
# Verificacion de datos perdidos
library(DataExplorer)
plot_missing(caso)

# Para ver las variables con valores perdidos 
which(colSums(is.na(caso))!=0)

# ¿Se debe estandarizar los datos?

# Para estandarizar los datos (center y scale)
# caso_st <- as.data.frame(scale(caso))
# str(caso_st)

# Histograma para las variables num?ricas
plot_histogram(caso)

# Gr?fico de variables num?ricas
library(funModeling)
plot_num(caso)

# Reporte general de los datos
create_report(caso)

# Descripci?n de los datos
df_status(caso)

# Descripci?n de las variables num?ricas
profiling_num(caso)

# Revisamos valores outliers

# Graficando cada variable usando ggplot2
library(ggplot2)
ggplot(caso, aes(x=" ",y=Facilidad )) + 
  geom_boxplot(fill="red") +
  labs(title = "Facilidad de comunicaci?n con mesa de ayuda",x=" ")   -> g1; g1

ggplot(caso, aes(x=" ",y=Selección)) + 
  geom_boxplot(fill="green") +
  labs(title = "Selección en seleccionar idiomas",x=" ")        -> g2; g2

ggplot(caso, aes(x=" ",y=Sencillez)) + 
  geom_boxplot(fill="blue") +
  labs(title = "Sencillez de menu y opciones",x=" ")         -> g3; g3

ggplot(caso, aes(x=" ",y=Tiempo)) + 
  geom_boxplot(fill="yellow") +
  labs(title = "Tiempo de espera al solicitar ayuda",x=" ")           -> g4; g4

ggplot(caso, aes(x=" ",y=Amabilidad)) + 
  geom_boxplot(fill="aliceblue") +
  labs(title = "Amabilidad en la atencion",x=" ")                 -> g5; g5

ggplot(caso, aes(x=" ",y=gestion)) + 
  geom_boxplot(fill="dodgerblue") +
  labs(title = "Interes en resolver sus dudas",x=" ")       -> g6; g6

ggplot(caso, aes(x=" ",y=conocimiento)) + 
  geom_boxplot(fill="bisque2") +
  labs(title = "conocimiento del personal",x=" ")     -> g7; g7

ggplot(caso, aes(x=" ",y=informacion)) + 
  geom_boxplot(fill="brown1") +
  labs(title = "Claridad en la informaci?n",x=" ") -> g8; g8

ggplot(caso, aes(x=" ",y=atencion)) + 
  geom_boxplot(fill="burlywood") +
  labs(title = "Tiempo de atencion",x=" ")         -> g9; g9 

ggplot(caso, aes(x=" ",y=solucion)) + 
  geom_boxplot(fill="coral") +
  labs(title = "Soluci?n brindada a su caso",x=" ")          -> g10; g10

library(cowplot)
plot_grid(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10 ,ncol = 3)

# No existe valores outliers

#############################################
# ----CLUSTER DE PARTICIÓN ----
# k-means    
# K-means es una técnica para encontrar y clasificar K grupos de datos (clusters).

# Determinando el número de clusters

# Criterio de la Suma de Cuadrados (SSE)

# Usando la función kmeans()

set.seed(123) # Semilla
wss <- numeric()
for(k in 1:10){
  b <- kmeans(caso,k)
  wss[k]<-b$tot.withinss
}
wss
wss1 <- data.frame(cluster=c(1:10),wss)
wss1
# Graficamos
library(ggplot2)
ggplot(wss1,aes(x=cluster,y=wss)) + 
  geom_line(color="red") + 
  geom_point(color="red") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(title = "Método Elbow") + 
  scale_x_continuous(breaks=1:10) + 
  theme_classic()

# Usando la función fviz_nbclust() del paquete factoextra
library(factoextra)

set.seed(123) # Iniciamos una semilla
fviz_nbclust(caso, 
             kmeans, 
             method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Método Elbow")

# 
# Evaluaci?n de grupos con coeficiente de silueta

## Conociendo el uso del análisis cluster
## Investigar una conjunto de datos (real)
## Plataforma Kaggle, UCI Machine learning
## O caso de de su entorno
## Donde se aplique lo estudiante
## Cargar la descripción del dataset en 
## La carpeta del aula virtual.

# Usando la función k-means
# Método de silhouette

library(cluster)

set.seed(123)
distancias_tc <- daisy(caso)
par(mfrow=c(1,3))
for(h in 2:4){
  clu=kmeans(caso,h)
  plot(silhouette(clu$cluster,distancias_tc))
}

par(mfrow=c(1,1))

# Usando el paquete factoextra
library(factoextra)

set.seed(123)
fviz_nbclust(caso, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")


# Criterio de Calinski-Harabasz
library(fpc)
ch<-numeric()
for(k in 2:10){
  res<-kmeans((caso),k)
  ch[k-1]<-calinhara(caso,res$cluster)
}
plot(2:10,ch,type="b",xlab="k",
     ylab="Criterio de Calinski-Harabasz")

#Función de manera directa
kmeansruns(caso,criterion="ch")
?kmeansruns

#--------------------------------------------------------------
# Usando la funcion kmeans() con 2 clusters
set.seed(123)
agrupacion1_km <- kmeans(caso, 
                         centers=2,      # N?mero de Cluster
                         nstart = 25,    # N?mero de puntos iniciales
                         iter.max = 100) # N?mero de iteraciones m?xima

# ntart=25, significa que se probar?n 25 puntos iniciales 
# aleatorios y luego elegir? aquel donde la variaci?n 
# dentro de cluster sea m?nima. El valor por defecto es 1

# Mostrar resumen de los clusters
print(agrupacion1_km)

# Sumas de cuadrados
# Suma de cuadrados dentro de cada cluster
agrupacion1_km$withinss  
# Suma de cuadrados Total dentro de cada cluster
agrupacion1_km$tot.withinss 
# Suma de cuadrados total suma(cuadrado(x - media))
agrupacion1_km$totss  
# Suma de cuadrados entre cluster. 
agrupacion1_km$betweenss    

# Se obtiene por diferencia 

# Tamaño de cada cluster
agrupacion1_km$size

# Proporciones de cada grupo
prop.table(agrupacion1_km$size)

# Promedios de cada cluster 
agrupacion1_km$centers

# promedios de cada cluster - opci?n 2
aggregate(caso, by=list(cluster=agrupacion1_km$cluster), mean)

# Junta el archivo de datos con la columna de cluster
library(dplyr)
caso %>% mutate(grp=agrupacion1_km$cluster) -> caso_final
#Otra forma: caso_final <- cbind(caso,grp=km$cluster)
caso_final <- cbind(caso,grp=agrupacion1_km$cluster)

head(caso_final)
str(caso_final)
caso_final$grp <- factor(caso_final$grp)
# Guardar los datos en el directorio
write.csv(caso_final,"caso_final_grupos_km.csv")


# Validación de resultados
# índice de Validación de Davies-Bouldin (más bajo)
cluster<-as.integer(caso_final$grp)

library(clusterSim)
indice<-index.DB(caso, cluster, centrotypes = "centroids")
indice$DB

# Indice de Dunn (Más grande)
library(clValid)
dunn(Data=caso, clusters=cluster, distance = NULL)
####################################################
library(clv)
indices2<-cls.scatt.data(caso,cluster)
indices2

#Creando un for y comparando
set.seed(123)
db<-numeric()
dunn<-numeric()
for(k in 2:7){
  resul<-kmeans(caso,k)
  grupos<- resul$cluster
  indice_db<-index.DB(caso,grupos,centrotypes = "centroids")
  db[k]<-indice_db$DB
  indiceDunn<- dunn(Data=caso, clusters=grupos, distance = NULL)
  dunn[k]<- indiceDunn
}

db
dunn
indices_totales<-data.frame(cluster=c(2:7),
                            indice_db=db[2:7],
                            indiceDunn=dunn[2:7])

indices_totales

#####################################################
# Validación de Resultados del k-means
# Bootstrap Evaluation of Clusters
library(fpc)
kclusters <- clusterboot(caso,
                         B=100,
                         clustermethod=kmeansCBI,
                         k=2,
                         seed=123)

# Los promedios deben salir lo m?s cercano a 1 posible.
# Un valor > 0.75 - 0.85 es muy bueno.
# Un valor < 0.6 es malo

kclusters$bootmean

##############################################################
# Importancia de las variables con el paquete FeatureImpCluster

# devtools::install_github("o1iv3r/FeatureImpCluster")
library(FeatureImpCluster)
library(flexclust)

cl_km <- as.kcca(agrupacion1_km, caso) 

str(cl_km)

cl_km@xcent    # Promedios de cada variable
cl_km@centers  # Promedios de cada variable por cada cluster
barplot(cl_km)

caso_t <- as.data.table(caso)

Importancia_km <- FeatureImpCluster(cl_km,caso_t)
plot(Importancia_km)

agrupacion1_km$centers

# CARACTERIZACIÓN 
head(caso_final)
plot(caso_final$solucion, caso_final$conocimiento, col=caso_final$grp)

library(scatterplot3d)
scatterplot3d(caso_final[,1:3], pch = 16, color=caso_final$grp)
windows()
library(rgl)
plot3d(caso_final$solucion,
       caso_final$conocimiento,
       caso_final$gestion, col = caso_final$grp, size = 7, type="p")
?plot3d
str(caso_final)

# plot(caso, col=caso_final$grp)
#  Métodos de Partición: k-means++ 

# Usando la funcion kmeansapp() con 2 clusters
library(LICORS)
set.seed(123)
kmpp <- kmeanspp(caso, 
                 k=2, 
                 start="random",
                 nstart = 25,
                 iter.max=100)

# ntart=25, significa que R probar? 25 puntos iniciales 
# aleatorios y luego elegir? aquel donde la variaci?n 
# dentro de cluster sea m?nima. El valor por defecto es 1

# Mostrar resumen de los clusters
print(kmpp)

# Sumas de cuadrados
# Suma de cuadrados dentro de cada cluster
kmpp$withinss 
# suma de cuadrados Total dentro de cada cluster
kmpp$tot.withinss
# Suma de cuadrados total suma(cuadrado(x - media))
kmpp$totss  
# Suma de cuadrados entre cluster. 
kmpp$betweenss    
# Se obtiene por diferencia 

# Tamaño de cada cluster
kmpp$size
prop.table(kmpp$size)
# Promedios de cada cluster 
kmpp$centers

# Otra forma de obtener promedios de cada cluster
aggregate(caso, by=list(cluster=kmpp$cluster), mean)

# Junta el archivo de datos con la columna de cluster
caso.kpp <- cbind(caso,grp=kmpp$cluster)

head(caso.kpp)
str(caso.kpp)

#write.csv(caso.kpp,"caso_k-means++.csv")

#--------------------------------------------------------------
# Importancia de las variables
library(FeatureImpCluster)
library(flexclust)

cl_kmpp <- flexclust::as.kcca(kmpp, caso) 
barplot(cl_kmpp)
Importancia_kmpp <- FeatureImpCluster(cl_kmpp,caso_t)
plot(Importancia_kmpp)


############################################
####----CASO - USO TARJETA DE CREDITO---#########
############################################
# Este caso requiere desarrollar una segmentación de clientes para definir la estrategia de marketing. 
# El conjunto de datos de muestra resume el comportamiento de uso de 8950 titulares de tarjetas de cr?dito 
# activos durante los últimos 6 meses. El archivo est? a nivel de cliente con 18 variables de comportamiento.

# BALANCE : Saldo que queda en la cuenta del cliente para realizar compras
# BALANCE_FREQUENCY : frecuencia con la que se actualiza el saldo, puntuación entre 0 y 1
# PURCHASES : Importe de las compras realizadas desde la cuenta
# ONEOFF_PURCHASES : importe máximo de compra realizado de una sola vez
# INSTALLMENTS_PURCHASES : Importe de la compra realizada a plazos
# CASH_ADVANCE : Anticipo dado por el usuario
# PURCHASES_FREQUENCY : frecuencia con la que se realizan las compras, puntuación entre 0 y 1
# ONEOFFPURCHASESFREQUENCY : Con qué frecuencia se realizan compras de una sola vez
# PURCHASESINSTALLMENTSFREQUENCY : Frecuencia con la que se realizan compras a plazos.
# CASHADVANCEFREQUENCY : Con qué frecuencia se paga el anticipo en efectivo
# CASHADVANCETRX : Número de transacciones realizadas con "Cash in Advanced"
# PURCHASES_TRX : número de transacciones de compra realizadas
# CREDIT_LIMIT : límite de tarjeta de crédito para el usuario
# PAYMENTS : Monto del pago realizado por el usuario
# MINIMUM_PAYMENTS : cantidad mínima de pagos realizados por el usuario
# PRCFULLPAYMENT : porcentaje del pago total pagado por el usuario
# TENURE : Tenencia del servicio de tarjeta de crédito para el usuario

# Cargamos las librerías adecuadas
library(ggplot2)
library(cluster)
library(cowplot)
library(factoextra)
library(tidyr)
library(klaR)
library(clustMixType)


# Cargamos el conjunto de datos
credito<-read.csv("https://raw.githubusercontent.com/VictorGuevaraP/Multivariado_Analisis/master/credit_card.csv", 
                  sep = ";",
                  stringsAsFactors = T)
#Mostramos los datos
head(credito)
#write.csv(credito, file = "credito.csv")
#Verificamos todo el caso
View(credito)

#Verificamos la estructura de los datos
str(credito)

#Cambiamos a factor el ID cliente
#credito$CUST_ID <- as.factor(credito$CUST_ID)

# Seleccionamos solo las numericas
numericos <- sapply(credito, is.numeric)

# Me quedo solo con las numericas
creditonum<- credito[ ,numericos]
head(creditonum)
#----ANÁLISIS EXPLORATORIO---- 

# Resumen de los datos
summary(creditonum)

# verificamos que existen datos perdidos, por lo tanto se debe realizar
# una corrección de tales datos.

# Verificacion de datos perdidos
library(DataExplorer)
plot_missing(creditonum)

# Para ver las variables con valores perdidos 
which(colSums(is.na(creditonum))!=0)

# Graficar la cantidad de valores perdidos
library(VIM)
#windows()
graf_perdidos1 <- aggr(creditonum,prop = F, 
                       numbers = TRUE,
                       sortVars=T,
                       cex.axis=0.5)
# Resumen de datos perdidos
summary(graf_perdidos1)

#Imputacion con el paquete DMwR
library(DMwR2)

# Funcion centralImputation()
# Si la variable es numerica (numeric o integer) reemplaza los valores 
# faltantes con la mediana.
# Si la variable es categorica (factor) reemplaza los valores faltantes con 
# la moda. 
library(DataExplorer)
creditonum_im <- centralImputation(creditonum)
plot_missing(creditonum_im) 

# Estandarización de datos
# Para estandarizar los datos (center y scale)
credito_st <- as.data.frame(scale(creditonum_im))
str(credito_st)
head(credito_st)
# Histograma para las variables num?ricas
plot_histogram(credito_st)

# Gráfico de variables num?ricas
library(funModeling)
plot_num(credito_st)

# Reporte general de los datos
#create_report(credito_st)

library(funModeling)

# Descripción de los datos
df_status(credito_st)

# Descripción de las variables num?ricas
profiling_num(credito_st)


#--------------------------------------------------------------
# Graficando cada variable usando ggplot2
library(ggplot2)
ggplot(credito_st, aes(x=" ",y=BALANCE )) + 
  geom_boxplot(fill="red") +
  labs(title = "Monto del saldo que queda en su cuenta para realizar compras",x=" ")   -> g1; g1

ggplot(credito_st, aes(x=" ",y=BALANCE_FREQUENCY)) + 
  geom_boxplot(fill="green") +
  labs(title = "recuencia de actualizaci?n del saldo",x=" ")        -> g2; g2

ggplot(credito_st, aes(x=" ",y=PURCHASES)) + 
  geom_boxplot(fill="blue") +
  labs(title = "cantidad de compras realizadas desde la cuenta",x=" ")         -> g3; g3

ggplot(credito_st, aes(x=" ",y=ONEOFF_PURCHASES)) + 
  geom_boxplot(fill="yellow") +
  labs(title = "cantidad m?xima de compra realizada",x=" ")           -> g4; g4

ggplot(credito_st, aes(x=" ",y=INSTALLMENTS_PURCHASES)) + 
  geom_boxplot(fill="aliceblue") +
  labs(title = "cantidad de compra a plazos",x=" ")                 -> g5; g5

ggplot(credito_st, aes(x=" ",y=CASH_ADVANCE)) + 
  geom_boxplot(fill="dodgerblue") +
  labs(title = "pago por adelantado dado por el usuario",x=" ")       -> g6; g6

ggplot(credito_st, aes(x=" ",y=PURCHASES_FREQUENCY)) + 
  geom_boxplot(fill="bisque2") +
  labs(title = "Con qu? frecuencia se realizan las Compras",x=" ")     -> g7; g7

ggplot(credito_st, aes(x=" ",y=ONEOFF_PURCHASES_FREQUENCY)) + 
  geom_boxplot(fill="brown1") +
  labs(title = "Con qu? frecuencia se realizan Compras en one-go",x=" ") -> g8; g8

ggplot(credito_st, aes(x=" ",y=PURCHASES_INSTALLMENTS_FREQUENCY)) + 
  geom_boxplot(fill="burlywood") +
  labs(title = "Frecuencia con la que se realizan las compras a plazos",x=" ")         -> g9; g9 

ggplot(credito_st, aes(x=" ",y=CASH_ADVANCE_FREQUENCY)) + 
  geom_boxplot(fill="coral") +
  labs(title = "Frecuencia con la que se paga el anticipo de efectivo",x=" ")          -> g10; g10

ggplot(credito_st, aes(x=" ",y=CASH_ADVANCE_TRX )) + 
  geom_boxplot(fill="red") +
  labs(title = "N?mero de transacciones realizadas con Efectivo en anticipo",x=" ")   -> g11; g11

ggplot(credito_st, aes(x=" ",y=PURCHASES_TRX)) + 
  geom_boxplot(fill="green") +
  labs(title = "N?mero de transacciones de compra realizado",x=" ")        -> g12; g12

ggplot(credito_st, aes(x=" ",y=CREDIT_LIMIT)) + 
  geom_boxplot(fill="blue") +
  labs(title = "L?mite de tarjeta de cr?dito",x=" ")         -> g13; g13

ggplot(credito_st, aes(x=" ",y=PAYMENTS)) + 
  geom_boxplot(fill="yellow") +
  labs(title = "Monto del pago realizado por el usuario",x=" ")           -> g14; g14

ggplot(credito_st, aes(x=" ",y=MINIMUM_PAYMENTS)) + 
  geom_boxplot(fill="aliceblue") +
  labs(title = "Monto m?nimo de los pagos realizados por el usuario",x=" ")                 -> g15; g15

ggplot(credito_st, aes(x=" ",y=PRC_FULL_PAYMENT)) + 
  geom_boxplot(fill="dodgerblue") +
  labs(title = "Porcentaje del pago total pagado por el usuario",x=" ")       -> g16; g16

ggplot(credito_st, aes(x=" ",y=TENURE)) + 
  geom_boxplot(fill="bisque2") +
  labs(title = "Tenencia del servicio de tarjeta de cr?dito para el usuario",x=" ")     -> g17; g17

library(cowplot)
plot_grid(g1,g2,g3,g4,g5,g6,g7,g8,g9, ncol = 3)
plot_grid(g10, g11,g12,g13,g14,g15,g16,g17, ncol = 3)

#############################################
# ---CLUSTER DE PARTICIÓN----

# k-means    
# K-means es una técnica para encontrar y clasificar K grupos de datos (clusters).

# Determinando el número de clusters

# Criterio de la Suma de Cuadrados (SSE)

# Usando la función kmeans()
set.seed(123)
wss <- numeric()
for(k in 1:10){
  b<-kmeans(credito_st,k)
  wss[k]<-b$tot.withinss
}
wss
wss1 <- data.frame(cluster=c(1:10),wss)

library(ggplot2)
ggplot(wss1,aes(cluster,wss)) + geom_line(color="red") + 
  geom_point(color="red") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(title = "M?todo Elbow") + 
  scale_x_continuous(breaks=1:10) + 
  theme_classic()

# Usando la función fviz_nbclust() del paquete factoextra
library(factoextra)

set.seed(123) # Iniciamos una semilla
fviz_nbclust(credito_st, 
             kmeans, 
             method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "M?todo Elbow")

# Evaluación de grupos con coeficiente de silueta

# 2.2. Usando la función k-means
library(cluster)

set.seed(123)
distancias_tc <- daisy(credito_st)
par(mfrow=c(1,4))
for(h in 2:8){
  clu=kmeans(credito_st,h)
  plot(silhouette(clu$cluster,distancias_tc))
}

par(mfrow=c(1,1))

# Usando el paquete factoextra
library(factoextra)

set.seed(123)
fviz_nbclust(credito_st, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Criterio de C-H
library(fpc)
kmeansruns(credito_st,criterion="ch")

#--------------------------------------------------------------
# Usando la funcion kmeans() con 7 clusters
set.seed(123)
agrupacion2_km <- kmeans(credito_st, 
                          centers=7,      # Número de Cluster
                          nstart = 100,    # Número de puntos iniciales
                          iter.max = 1000) # Número de iteraciones máxima

# ntart=25, significa que se probarón 100 puntos iniciales 
# aleatorios y luego elegir? aquel donde la variación 
# dentro de cluster sea m?nima. El valor por defecto es 1

# Mostrar resumen de los clusters
print(agrupacion2_km)

# Sumas de cuadrados
agrupacion2_km$withinss     # Suma de cuadrados dentro de cada cluster
agrupacion2_km$tot.withinss # Suma de cuadrados Total dentro de cada cluster
agrupacion2_km$totss        # Suma de cuadrados total suma(cuadrado(x - media))
agrupacion2_km$betweenss    # Suma de cuadrados entre cluster. 

# Se obtiene por diferencia 

# Tama?o de cada cluster
agrupacion2_km$size

# Proporciones de cada grupo
prop.table(agrupacion2_km$size)*100

# Promedios de cada cluster 
agrupacion2_km$centers

# promedios de cada cluster - opción 2
aggregate(credito_st, by=list(cluster=agrupacion2_km$cluster), mean)

# Junta el archivo de datos con la columna de cluster
library(dplyr)
credito_st %>% mutate(grp=agrupacion2_km$cluster) -> credito_final
#Otra forma: credito_final <- cbind(credito_st,grp=km$cluster)

head(credito_final)
str(credito_final)
credito_final$grp <- factor(credito_final$grp)

#write.csv(credito_final,"credito_final_agrup_km.csv")

# Validación de resultados
# índice de Validaci?n de Davies-Bouldin (m?s peque?o)
cluster<-as.integer(credito_final$grp)

library(clusterSim)
indice<-index.DB(credito_st, cluster, centrotypes = "centroids")
indice$DB

# Indice de Dunn (Más grande)
library(clValid)
dunn(Data=credito_st, clusters=cluster, distance = NULL)

library(clv)
indices2<-cls.scatt.data(credito_st,cluster)
indices2

#Creando un for y comparando
set.seed(123)
db<-numeric()
dunn<-numeric()
for(k in 2:7){
  resul<-kmeans(credito_st,k)
  grupos<- resul$cluster
  indice_db<-index.DB(credito_st,grupos,centrotypes = "centroids")
  db[k]<-indice_db$DB
  indiceDunn<- dunn(Data=credito_st, clusters=grupos, distance = NULL)
  dunn[k]<- indiceDunn
}

indices_totales<-data.frame(cluster=c(2:7),
                            indice_db=db[2:7],
                            indiceDunn=dunn[2:7])

indices_totales

######################################################
# Visualización de los resultados usando ACP

# Si se tienen datos multidimensionales, una solución es 
# realizar un ACP y plotear los individuos de acuerdo a 
# los dos primeros componentes

library(factoextra)
fviz_cluster(agrupacion2_km, data = credito_st, ellipse.type = "convex") +
  theme_classic()

#####################################################
# Validación de Resultados del k-means
# Bootstrap Evaluation of Clusters
library(fpc)
kclusters <- clusterboot(credito_st,
                         B=100,
                         clustermethod=kmeansCBI,
                         k=7,
                         seed=123)

# Los promedios deben salir lo más cercano a 1 posible.
# Un valor > 0.75 - 0.85 es muy bueno.
# Un valor < 0.6 es malo

kclusters$bootmean
prop.table(table(credito_final$grp))*100
##############################################################
# Importancia de las variables con el paquete FeatureImpCluster

# devtools::install_github("o1iv3r/FeatureImpCluster")
library(FeatureImpCluster)
library(flexclust)

cl_km <- as.kcca(agrupacion2_km, credito_st) 

str(cl_km)

cl_km@xcent    # Promedios de cada variable
cl_km@centers  # Promedios de cada variable por cada cluster
barplot(cl_km)


data <- as.data.table(credito_st)

Importancia_km <- FeatureImpCluster(cl_km,data)
plot(Importancia_km)



#  Métodos de Partición: k-means++ 

# Usando la funcion kmeansapp() con 3 clusters
library(LICORS)
set.seed(123)
kmpp <- kmeanspp(credito_st, 
                 k=4, 
                 start="random",
                 nstart = 25,
                 iter.max=100)

# ntart=25, significa que R probar? 25 puntos iniciales 
# aleatorios y luego elegir? aquel donde la variaci?n 
# dentro de cluster sea m?nima. El valor por defecto es 1

# Mostrar resumen de los clusters
print(kmpp)

# Sumas de cuadrados
kmpp$withinss     # Suma de cuadrados dentro de cada cluster
kmpp$tot.withinss # suma de cuadrados Total dentro de cada cluster
kmpp$totss        # Suma de cuadrados total suma(cuadrado(x - media))
kmpp$betweenss    # Suma de cuadrados entre cluster. 
# Se obtiene por diferencia 

# Tamaño de cada cluster
kmpp$size

# Promedios de cada cluster 
kmpp$centers

# Otra forma de obtener promedios de cada cluster
aggregate(credito_st, by=list(cluster=kmpp$cluster), mean)

# Junta el archivo de datos con la columna de cluster
datos.kpp <- cbind(credito_st,grp=kmpp$cluster)

head(datos.kpp)
str(datos.kpp)

#write.csv(datos.kpp,"datos_means++.csv")

#--------------------------------------------------------------
# Importancia de las variables
library(FeatureImpCluster)
library(flexclust)

cl_kmpp <- flexclust::as.kcca(kmpp, credito_st) 
barplot(cl_kmpp)

data2<-as.data.table(credito_st)
Importancia_kmpp <- FeatureImpCluster(cl_kmpp,data2)
plot(Importancia_kmpp)

## Tarea
## Utilizar un conjunto de datos de la plataforma Kaggle, UCI ML
## github y aplicar todo el proceso para Kmeans
## Analizar los datos
## Determinar el algoritmo a utilizar
## Evaluar la cantidad óptima de grupos
## Generar los grupos
## Validar los grupos creados



#########################################################
# ----Métodos de Partición: PAM----   
head(credito_st)
head(credito)
# Calcular PAM
library(cluster)

# Estimar el número óptimo de clusters
fviz_nbclust(credito_st, method = "wss", pam)
# fviz_nbclust(credito_st, method = "silhouette", pam)


# Según los resultados se deben generar 5 grupos
set.seed(123)
pam.res <- pam(credito_st, k=5, stand=FALSE)

print(pam.res)

# Medoides de cada cluster
pam.res$medoids
View(credito_st)
# Número de observaciones de cada cluster
table(pam.res$clustering)
prop.table(table(pam.res$clustering))
# Promedios de cada cluster 
aggregate(credito_st, by=list(cluster=pam.res$clustering), mean)
aggregate(creditonum_im, by=list(cluster=pam.res$clustering), mean)
# Visualizaci?n de los resultados usando ACP
fviz_cluster(pam.res)


# ValidaciÓn de Resultados del PAM
# Bootstrap Evaluation of Clusters
library(fpc)
pamclusters <- clusterboot(credito_st,
                           B=10,
                           clustermethod=pamkCBI,
                           k=4,
                           seed=123)

# Los promedios deben salir lo m?s cercano a 1 posible.
# Un valor > 0.75 - 0.85 es muy bueno.
# Un valor < 0.6 es malo
pamclusters$bootmean

# Importancia de las variables
library(FeatureImpCluster)
library(flexclust)

cl_pam <- flexclust::as.kcca(pam.res, credito_st) 
barplot(cl_pam)
#credito_st<-as.data.frame(credito_st)

credito_st_t<-data.table(credito_st)
Importancia_pam <- FeatureImpCluster(cl_pam,credito_st_t)
plot(Importancia_pam)

#################################################
#  ----Métodos de Partición: CLARA----

library(cluster)


set.seed(123)
clara.res <- clara(credito_st,k=5, samples = 100, pamLike = TRUE)

print(clara.res)

# Medoides de cada  cluster
clara.res$medoids

# Número de cluster para cada observación 
table(clara.res$cluster)
prop.table(table(clara.res$cluster))
# Promedios de cada cluster 
aggregate(credito_st, by=list(cluster=clara.res$cluster), mean)
aggregate(creditonum_im, by=list(cluster=clara.res$cluster), mean)

# Visualizacion de los resultados con ACP
fviz_cluster(clara.res)

# Junta el archivo de datos con la columna de cluster
library(dplyr)
credito_st %>% mutate(grp=clara.res$cluster) -> credito_final
#Otra forma: credito_final <- cbind(credito_st,grp=clara.res$cluster)

head(credito_final)
str(credito_final)
credito_final$grp <- factor(credito_final$grp)

write.csv(credito_final,"credito_final_clara.csv")


# Validación de Resultados del CLARA
# Bootstrap Evaluation of Clusters
library(fpc)
claraclusters <- clusterboot(credito_st,
                             B=10,
                             clustermethod=claraCBI,
                             k=5,
                             seed=123)

# Los promedios deben salir lo más cercano a 1 posible.
# Un valor > 0.75 - 0.85 es muy bueno.
# Un valor < 0.6 es malo

claraclusters$bootmean

#--------------------------------------------------------------
# Importancia de las variables
library(FeatureImpCluster)
library(flexclust)

cl_clara <- flexclust::as.kcca(clara.res, credito_st) 
barplot(cl_clara)

credito_st2 <- as.data.table(credito_st)
Importancia_clara <- FeatureImpCluster(cl_clara,credito_st2)
plot(Importancia_clara)


#####################################
# CARACTERIZANDO A LOS CLUSTERS 
# Caracterizando al cluster 

str(credito_final)
head(credito_final)
library(compareGroups)
grupos <- compareGroups(grp~.,data=credito_final)
grupos
clustab <- createTable(grupos, digits=3, show.p.overall=FALSE)
clustab

#--------------------------------------------------------------
# Diagrama de Cajas de cada variable segun Cluster
# usando el paquete ggplot2

library(ggplot2)

ggplot(credito_final, aes(x=grp,y=BALANCE,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")  -> g1

ggplot(credito_final, aes(x=grp,y=BALANCE_FREQUENCY,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")       -> g2

ggplot(credito_final, aes(x=grp,y=PURCHASES,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")        -> g3

ggplot(credito_final, aes(x=grp,y=ONEOFF_PURCHASES,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")          -> g4

ggplot(credito_final, aes(x=grp,y=INSTALLMENTS_PURCHASES,fill=grp)) + 
  geom_boxplot() +
  labs(title = "..",x="Cluster")                -> g5

ggplot(credito_final, aes(x=grp,y=CASH_ADVANCE,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")      -> g6

ggplot(credito_final, aes(x=grp,y=PURCHASES_FREQUENCY,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")    -> g7

ggplot(credito_final, aes(x=grp,y=ONEOFF_PURCHASES_FREQUENCY,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster") -> g8

ggplot(credito_final, aes(x=grp,y=PURCHASES_INSTALLMENTS_FREQUENCY,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")         -> g9

ggplot(credito_final, aes(x=grp,y=CASH_ADVANCE_FREQUENCY,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")          -> g10

library(patchwork)
(g1+g2+g3)/(g4+g5+g6)/(g7+g8+g9)/(g10)
(g1+g2+g3)/(g4+g5+g6)/(g7+g8+g9)/(g10 + plot_spacer()+plot_spacer())


# Diagrama de lineas de promedios por cluster
library(dplyr)

credito_final %>% 
  group_by(grp) %>% 
  summarise_all(list(mean)) -> medias
medias

credito_final %>%  summarise_if(is.numeric,mean) -> general
general
general <- cbind(grp="general",general)
general

medias  <- as.data.frame(rbind(medias,general))
medias

# Convirtiendo la data formato tidy
library(tidyr)

credito_final_k <- pivot_longer(data  = medias, 
                                 -grp,
                                 names_to = "variable",
                                 values_to = "valor")

credito_final_k

ggplot(credito_final_k,aes(x=variable,y=valor,color=grp)) + 
  geom_point() + 
  geom_line(aes(group = grp)) +
  scale_y_continuous(breaks = 1:17) +
  theme_bw() +
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 45)) +
  labs(title="Diagrama de líneas de cluster por variable",
       x="Variable") +
  scale_colour_discrete("Cluster") 


#####################################
# CARACTERIZANDO A LOS CLUSTERS Ejemplo 1
# Caracterizando al cluster k-means

str(caso_final)

library(compareGroups)
grupos <- compareGroups(grp~.,data=caso_final)
clustab <- createTable(grupos, digits=3, show.p.overall=FALSE)
clustab

#--------------------------------------------------------------
# Diagrama de Cajas de cada variable segun Cluster
# usando el paquete ggplot2

library(ggplot2)

ggplot(caso_final, aes(x=grp,y=Facilidad,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")  -> g1

ggplot(caso_final, aes(x=grp,y=Selecci?n,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")       -> g2

ggplot(caso_final, aes(x=grp,y=Sencillez,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")        -> g3

ggplot(caso_final, aes(x=grp,y=Tiempo,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")          -> g4

ggplot(caso_final, aes(x=grp,y=Amabilidad,fill=grp)) + 
  geom_boxplot() +
  labs(title = "..",x="Cluster")                -> g5

ggplot(caso_final, aes(x=grp,y=gestion,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")      -> g6

ggplot(caso_final, aes(x=grp,y=conocimiento,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")    -> g7

ggplot(caso_final, aes(x=grp,y=informacion,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster") -> g8

ggplot(caso_final, aes(x=grp,y=atencion,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")         -> g9

ggplot(caso_final, aes(x=grp,y=solucion,fill=grp)) + 
  geom_boxplot() +
  labs(title = "...",x="Cluster")          -> g10

library(patchwork)
(g1+g2+g3)/(g4+g5+g6)/(g7+g8+g9)/(g10)
(g1+g2+g3)/(g4+g5+g6)/(g7+g8+g9)/(g10 + plot_spacer()+plot_spacer())


# Diagrama de lineas de promedios por cluster
library(dplyr)

caso_final %>% 
  group_by(grp) %>% 
  summarise_all(list(mean)) -> medias
medias

caso_final %>%  summarise_if(is.numeric,mean) -> general
general
general <- cbind(grp="general",general)
general

medias  <- as.data.frame(rbind(medias,general))
medias

