#Limpiamos el directprio de trabajo
rm(list = ls())
ls()

#cargamos la data
telefonia = read_excel("~/GitHub/Analisis-Multivariado/Caso_telefonia.xlsx")

#vemos la data
head(telefonia)

#vemos el nombre de las columnas
names(telefonia)

# vemos la columna Género
telefonia[,c("Género", "Estado_civil")]

