#################################################
# Universidad nacional José Faustino Sánchez Carrión                                                                                                                                                 Mg. Víctor Guevara P.
# Mg. Victor Guevara P.
# Sesión: Exploración de datos
################################################ 

rm(list = ls())
ls()
# ---- Verificar y configurar un directorio de trabajo ---- 
getwd("C:/Users/lenovo/Documents/GitHub/Analisis-Multivariado/SESIONES/Sesión 02/Exploracion")

# Verificar el directorio de trabajo
getwd() 


# Caso 1: Nivel de Obesidad
# Conjunto de datos para la estimación de niveles de obesidad basados en hábitos
# alimentarios y condición física en individuos de Colombia, Perú y México.
# Objetivo:
#  El informe presenta datos para la estimación de los niveles de obesidad en
#individuos de los países de México, Perú y Colombia, con base en sus hábitos
#alimentarios y condición física.
#Los datos contienen 17 atributos y 2111 registros, los registros están etiquetados
#con la variable de clase NObesidad (Nivel de Obesidad), que permite clasificar los
#datos utilizando los valores de Peso Insuficiente, Peso Normal, Sobrepeso Nivel I,
#Sobrepeso Nivel II, Obesidad Tipo I , Obesidad Tipo II y Obesidad Tipo III.
#Revisar: https://www.sciencedirect.com/science/article/pii/S2352340919306985

# Cargar el conjunto de datos
url <- "https://raw.githubusercontent.com/sombragris1/SMedicas/main/Obesidad.csv"
obesidad_df <- read.csv(url, sep=";",encoding = "latin1", stringsAsFactors = TRUE)

# Mostrar los 6 primeros registros 
head(obesidad_df)

#Observar todo el conjunt de datos
View(obesidad_df)

# Observar y manipular (cambiar) en DF
fix(obesidad_df)

# Estructura de los datos
str(obesidad_df)


#---- Exploración de datos----
# Resumen básico de datos
summary(obesidad_df)

# Rivisar si hay valores missing (Ausentes)
which(colSums(is.na(obesidad_df))!=0)

# Tenemos dos variables ordinales, hay que especificar al programa
# obesidad_df$N_Obesidad

# Verificar los niveles de la variable
levels(obesidad_df$N_Obesidad)

# Ordenar adecuadamente los niveles y especificar el orden
obesidad_df$N_Obesidad <- factor(obesidad_df$N_Obesidad, levels = c("Peso bajo",
                                                                    "Peso normal",
                                                                    "Nivel de sobrepeso I",
                                                                    "Nivel de sobrepeso II",
                                                                    "Obesidad tipo I",
                                                                    "Obesidad tipo II",
                                                                    "Obesidad tipo III"),ordered = T)
str(obesidad_df)

# Ordenar adecuadamente los niveles de las variables "CAEC" "CALC"
levels(obesidad_df$CAEC)
obesidad_df$CAEC <- factor(obesidad_df$CAEC, levels = c("Nunca",
                                                        "Algunas veces",
                                                        "Frecuentemente",
                                                        "Siempre"), ordered = T)
levels(obesidad_df$CALC)
obesidad_df$CALC <- factor(obesidad_df$CALC, levels = c("Nunca",
                                                        "Algunas veces",
                                                        "Frecuentemente",
                                                        "Siempre"), ordered = T)
str(obesidad_df)

######################################################
# Para seleccionar un grupos de columnas (variables) o filas (observaciones) se puede
# realizar de la siguiente manera

# Forma 1
obesidad_df$Genero
# Forma 2 : [filas,columnas], se puede agregar :  (inicio:fin)
obesidad_df[ ,1]
obesidad_df[ ,1:4]
obesidad_df[1, ]
obesidad_df[1:5,]
obesidad_df[1:5,1:3]
# Forma 3 : C()
obesidad_df[ ,c(2,6,8)]

# La opción de attach, permite que el programa reconozca las columnas como objetos
attach(obesidad_df)
# Edad
# Genero

# Generar tablas de frecuencia y gráficos
library(summarytools)
freq(obesidad_df$Genero)
freq(obesidad_df$Genero, report.nas = FALSE)
# summarytools::freq(obesidad_df$Genero)
?freq
freq(obesidad_df$N_Obesidad, report.nas = FALSE)

library(epiDisplay)
tab1(obesidad_df$Genero)
tab1(obesidad_df$N_Obesidad)
tab1(obesidad_df$N_Obesidad, sort.group = "decreasing")

library(DataExplorer)
# Genere un resumen de los datos
create_report(obesidad_df)

library(funModeling)
df_status(obesidad_df)
# Gráficos para variables cualitativas
freq(obesidad_df)
freq(obesidad_df, path_out = "graficos")

# Gráficos para variables cuantitativas
plot_num(obesidad_df)
plot_density(obesidad_df)

# Medidas estadísticas
profiling_num(obesidad_df)

############################################################
# Tablas de frecuencia de las variables cuantitativas
library(agricolae)
tabla1 <- table.freq(hist(obesidad_df$Edad, breaks = "sturges"))
tabla1

# Gráficos
hist(obesidad_df$Estatura)

# Para particionar el espacio gráfico pordemos realizar:
par(mfrow = c(2,2))
hist(obesidad_df$Edad)
hist(obesidad_df$Estatura)
hist(obesidad_df$Peso)
hist(obesidad_df$FCVC)
par(mfrow = c(1,1))

# Gráfico de densidad
plot(density(obesidad_df$Edad))

# Gráfico de caja y bigotes
boxplot(obesidad_df$Edad)

# Del mundo GGPLOT2
library(ggplot2)

ggplot(data=obesidad_df, aes(y=Edad, fill=N_Obesidad))+
  geom_boxplot()+
  theme_classic()

ggplot(data=obesidad_df, aes(y=Peso, fill=N_Obesidad))+
  geom_boxplot()

ggplot(data=obesidad_df, aes(x=Peso, color="red"))+
  geom_histogram(bins = 11)+
  geom_freqpoly(bins = 11)+
  labs(title = "Distribución de encuestados según su peso")

# Análisis bivariado
# Diagramas de dispersión
plot(obesidad_df$Edad, obesidad_df$Estatura)
plot(obesidad_df[,2:4])

library(rgl)
plot3d(obesidad_df[,2:4], type="s", radius = 4, 
       col = as.integer(N_Obesidad))


