# Sesión: Valores ausentes y valores outliers

# limpiamos la memoria
rm(list = ls())
ls()

#Obsevamos el directorio de trabajo
getwd()

#Cargamos la data
url = "C:/Users/lenovo/Documents/GitHub/Analisis-Multivariado/SESIONES/Sesión 03/CHURNM.CSV"
data = read.csv(url, sep = ";")

# vemoos la 6 primeras filas de la dataset
head(data)

# revisamos la estructura de los datos
str(data)

# Resumen descriptivo de las variables numéricas
summary(data)

# ==============================================================================
# Verificación y tratamiento de valores missing

# El número de valores ausentes por cada variable es:
colSums(is.na(data))

# Determinar cuantos valores NA hay en cada columna.
sapply(data, function(x) sum(is.na(x)))

# Resumen de datos de dataset
summary(data)


#Visualización
library(visdat) # visualización de datos
#Visualización gráfica de proporción de datos perdidos y donde se producen.
vis_dat(data)

#Determinación del porcentaje de datos perdidos.
vis_miss(data)

# cluster agrupa los datos faltantes
# sort_miss = TRUE: es opcional
# con sort_miss = TRUE ordena las columnas desde la que posea mayor 
# pérdida de datos (cantidad de datos faltantes) a la menor.
# argumento opcional: cluster = TRUE, que permite agrupar los datos faltantes.
vis_miss(data ,sort_miss = TRUE, cluster = TRUE)

