
7+9 #esto es una suma
# Multiplicación
78*9
# Resta
45-8
# Div
8/6
# Potencia
8^2
##################################
# Asignación (<-)
a <- 64
# Imprimir
print(a)
a

b<-45
# Operaciones con objetos
a+b
# Reasignar
a<-80
a

# Vectores
edades <- c(23,23,22,22,22,29)
edades
edades[3:5]

# Operacioes con vectores

edades/100

# Matrices
1:8
matrix(1:8)
matrix(1:8,nrow = 4)

matrix(edades, ncol = 2)

# Data frame

# Cargar datos desde internet (Github)
marketing <- read.csv("https://raw.githubusercontent.com/VictorGuevaraP/Multivariado_Analisis/master/Marketing.csv",sep = ";", stringsAsFactors = TRUE)
# Mostrar los primeros registros
head(marketing)
# Mostrar los últimos registros
tail(marketing)
# Mostrar todos los datos
View(marketing)

# Verificar la estructura de los datos
str(marketing)

# Resumen del conjunto de datos
summary(marketing)

summary(marketing$Edad)
summary(marketing$Salario)

# Para mejorar resultados R proporciona una serie de librerias
# por ejemplo
# Si quisiera mejorar u obtener algunos resumenes más profundos puedo utilizar
# La libreria ggplot2 para gráficos

# Instalar un libreria (una sola vez)

#install.packages("......")
library(PerformanceAnalytics)
chart.Correlation((marketing[9:10]))



