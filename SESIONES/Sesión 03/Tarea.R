# TAREA: Revisar el conjuntos de datos: 
# https://raw.githubusercontent.com/VictorGuevaraP/Multivariado_Analisis/master/CHURNM.CSV
# análizar todo lo correspondiente a valores missing

#Limpiamos la memoria
rm(list = ls())
ls()

# Verificar el directorio de trabajo
getwd()

# Generamos el dataframe con el contenido del CSV
library(readr)
#data = read_delim("GitHub/Analisis-Multivariado/SESIONES/Sesión 03/CHURNM.CSV", delim = ";")
library(readr)
data =  read_delim("CHURNM.CSV", delim = ";", escape_double = FALSE, 
                   locale = locale(encoding = "WINDOWS-1252"),trim_ws = TRUE)
View(data)
#visualizamos la data
head(data)

# revisar la estructura de los datos
str(data)

# Resumen de los datos
summary(data)

# Visualización de distribuciones para la variable MetodoPago
library(ggplot2)
ggplot(data = data) +
  geom_bar(mapping = aes(x = MetodoPago))



# ==============================================================================
# VERIFICACIÓN DE VALORES MISSING
# ==============================================================================
# Devuelve un vector lógico
head(is.na(data))

# Devuelve un único valor lógico, cierto o falso, si existe algún valor ausente
any(is.na(data))

# Devuelve el número de NAs que presenta la tabla
sum(is.na(data))

# Devuelve el % de valores perdidos
mean(is.na(data)) 


# Detección del número de valores perdidos en cada una de las columnas que 
# presenta la tabla
colSums(is.na(data))

# Detección del % de valores perdidos en cada una de las columnas que presenta 
# la tabla
colMeans(is.na(data), round(2))


# determinación del porcentaje de valores perdidos respecto del total de datos
# para cada columna
porcentajeMiss = function(x) {sum(is.na(x)) / length(x)*100}
# por columna
apply(data, 2, porcentajeMiss)

#Determinar los valores existentes en cada columna del set de datos.
unique(data$MetodoPago)

# Determinar cuantos valores NA hay en cada columna.
apply(data, MARGIN = -1, function(x) sum(is.na(x))) 


#=========================================================================
#Visualización
# Visdat y Naniar. Estas librerías permiten visualizar en forma más expedita la 
# proporción de datos perdidos dentro de un conjunto de datos.
library(visdat) # visualización de datos
library(naniar)

#Visualización gráfica de proporción de datos perdidos y donde se producen.
vis_dat(data)

# Para determinar el patrón de datos hacemos uso de la librería mice.
library(mice)
md.pattern(data,rotate.names = T)

#Determinación del porcentaje de datos perdidos.
vis_miss(data ,sort_miss = TRUE) 

# Verificar si hay patrones con librerias
library(VIM)
library(mice)
grafico_miss <- aggr(data, numbers =T)
grafico_miss
summary(grafico_miss)
# Representación matricial
matrixplot(data)

# otra representación
md.pattern(data, rotate.names = T)

library(simputation)
library(tidyverse)
library(mice)
md.pattern(data,rotate.names = T)

colSums(is.na(data))

porcentajeMiss = function(x) {sum(is.na(x)) / length(x)*100}
# por columna
apply(data, 2, porcentajeMiss)

# Determinar cuantos valores NA hay en cada columna.
sapply(data, function(x) sum(is.na(x)))
vis_miss(data)


# cluster agrupa los datos faltantes
vis_miss(data ,sort_miss = TRUE, cluster = TRUE)


#==============================================================================
#---------------------------TRATAMIENTO DE DATOS AUSENTES----------------------

##--------------------------------- Primer caso: Eliminar todos los missing

data_sin_missing = na.omit(data)
dim(data_sin_missing)
##--------------------------------- Reemplazo con la mediana y moda

library(DMwR2)
data_imp_mtc = centralImputation(data) # Mediana (numérico), moda (no numérico)
dim(data_imp_mtc)
vis_miss(data_imp_mtc)

##--------------------------------- Reemplazo con la media, mediana y moda

library(VIM)
data_imp_mtc2 = initialise(data, method="median") #Media (continuo), Mediana (discreto), moda (No numérico)
dim(data_imp_mtc2)


#==============================================================================
#--------------DETECCIÓN Y TRATAMIENTO DE VALORES ATÍPICOS (OUTLIERS)-----------

# Gráfico de caja (boxplot) de una variable ImporteTotal respecto al método de pago

library(plotly)
plot_ly (data =  data_imp_mtc, x = ~ImporteTotal, y = ~ MetodoPago,type = "box", color = ~ MetodoPago)
# Estandarizar de las variables del DataFrame 
data_estandar = cbind(scale(data[,12:14]),data[,1:11], data[,15:16])
data_estandar = cbind(scale(data[,12:14]),data[,1:11], data[,15:16])
head(data_estandar)
data_imp_mtc[,12:14]

# Identificando valores outliers a partir de los estandarizados
# Calcular el límite para considerar valores atípicos (por ejemplo, si definimos que un valor es atípico si su puntaje Z es mayor a 2 o menor a -2)
limite = 3 
# Identificar los valores atípicos
outliers= data_estandar [apply(data_estandar[,1:3] ) > limite, 1, any), ]
outliers <- df[apply(abs(df_std) > limite, 1, any), ]
# Mostrar los valores atípicos
outliers

data_std = as.data.frame(data_estandar[,1:3])

# Identificar los valores atípicos
outliers = data_std[apply(abs(df_std) > limite, 1, any), ]
outliers


# Valores outliers a partir de percentiles
outlier = boxplot(data$ImporteTotal)$out
outlier
# outliers bivariado
# Crear un gráfico de caja y bigote bivariado
boxplot(data_std, outline = TRUE, col = "lightgray" )
# Identificar los outliers bivariados
outliers = boxplot.stats(data_std)
outliers$x$out
outliers$y$out

#===================================================================

library(dplyr)
library(outliers)
data_imp_mtc[, 12:14]
df_scaled = as.data.frame(scale(data_imp_mtc[, 12:14]))
df_scaled

outliers_ = df_scaled %>%
  mutate_if(is.numeric, funs(outlier = ifelse(abs(.) > 1.5 * IQR(.), "Yes", "No")))

outliers_

# boxplot con las variables estandarizadas
boxplot(data_estandar[,1:3])

## Eliminamos los Outliers
### Usando el rango intercuartil (IQR) - Cargo mensual 
q1C = quantile(df_scaled$CargoMensual, 0.25)
q3C = quantile(df_scaled$CargoMensual, 0.75)
iqrC = q3C - q1C
lim_infC = q1C - 1.5 * iqrC
lim_supC = q3C + 1.5 * iqrC
datos_sin_outliers_Cargo = df_scaled$CargoMensual[df_scaled$CargoMensual >= lim_infC & df_scaled$ImporteTotal <= lim_supC]
print(datos_sin_outliers_Cargo) # Imprimir el resultado
boxplot(datos_sin_outliers_Cargo, main = "Boxplot de la variable Importe Cargo Mensual ") #Grafica

### Usando el rango intercuartil (IQR) - ImporteTotal 
q1I = quantile(df_scaled$ImporteTotal, 0.25)
q3I = quantile(df_scaled$ImporteTotal, 0.75)
iqrI = q3I - q1I
lim_infI = q1I - 1.5 * iqrI
lim_supI = q3I + 1.5 * iqrI
datos_sin_outliers_Importe = df_scaled$ImporteTotal[df_scaled$ImporteTotal >= lim_infI & df_scaled$ImporteTotal <= lim_supI]
print(datos_sin_outliers_Importe) # Imprimir el resultado
boxplot(datos_sin_outliers_Importe, main = "Boxplot de la variable Importe Total") #Grafica

### Usando el rango intercuartil (IQR) - Permanencia 
q1P = quantile(df_scaled$pemanencia, 0.25)
q3P = quantile(df_scaled$pemanencia, 0.75)
iqrP = q3P - q1P
lim_infP = q1P - 1.5 * iqrP
lim_supP = q3P + 1.5 * iqrP
datos_sin_outliers_Permanencia = df_scaled$pemanencia[df_scaled$pemanencia >= lim_infP & df_scaled$pemanencia <= lim_supP]
print(datos_sin_outliers_Permanencia) # Imprimir el resultado
boxplot(datos_sin_outliers_Permanencia, main = "Boxplot de la variable Permanencia") #Grafica
