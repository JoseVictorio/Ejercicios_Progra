rm(list = ls())
library(tidyverse)
library(magrittr)
#PARTE 1---------------------
#1----------------
comb <- factorial(20) / (factorial(7) * (factorial(20-7)))
pot <- (0.4^7)*(0.6^13)

c <- comb * pot
round(c, 2)

#2-----------------
num_cons <- function(n) {
  a <- n*(n+1)
  sum <- a/2
  return(sum)
}
num_cons(1000)

sum_geo <- function(a,n) {
  x <- 1 - a^(n+1)
  y <- 1-a
  s <- x/y
  return(s)
}
sum_geo(2,10)

#3------------------

print(grupo)
length(grupo)
which(grupo == "A")

#4------------------
# El vector nota representa la nota de un examen de los alumnos que están en los grupos del vector grupo.
print(nota)

# ¿Cuanto suman todas las notas?
suma <- sum(nota)
suma

# ¿Cual es la media aritmética de todas las notas?
m_arit <- mean(nota)
m_arit

# ¿En qué posiciones están las notas mayores de 7.0?
which(nota > 7)

# Visualiza las notas ordenadas de mayor a menor
sort(nota, decreasing = TRUE)

#¿En qué posición está la nota máxima?

which(nota == max(nota))


#5---------------------------
#A partir de los vectores grupo y nota definidos.
notas <- data.frame(grupo, nota)
head(notas)
str(notas)
#Suma las notas de los 10 primeros alumnos del vector
sum(nota[1:10])
#¿Cuántos alumnos hay del grupo C?
cstud <- filter(notas, grupo == "C") %>% 
  nrow()
cstud
#  ¿Cuántos alumnos han aprobado?
approved <- filter(notas, nota >= 5.5)
nrow(approved)
#  ¿Cuántos alumnos del grupo B han aprobado?
grupb <- filter(notas, grupo == "B", nota >=5.5) %>% 
  nrow()

grupb


#  ¿Qué porcentaje de alumnos del grupo C han aprobado?
capprov <- filter(notas, grupo =="C", nota >=5.5) %>%
  nrow()

capprov

perc_c <- capprov/cstud
perc_c*100

#  ¿De qué grupos son la máxima y mínima notas de toda la muestra?
min_max <- which.max(notas$nota)
min_max
notas[120,]
  
min_max1 <- which.min(notas$nota)
min_max1  
notas[142,]

min_max <- function(x){
  max <- which.max(x)
  df <- x[max,]
  return(df)
}

min_max(notas$nota)
# Nota media de los alumnos de grupo A y B, juntos, considerando sólo a los que han aprobado.

abgroup <- filter(notas, grupo == "A" | grupo == "B", nota >= 5.5)
  summary()
abapp

#otra forma
nota[grupo %in% c("A", "B") & nota >= 5.5] %>% 
  mean()

#6-----------------------------
#Calcula el percentil 66 de las notas de todos los alumnos, y también de los alumnos del grupo C.

per66 <- notas$nota %>% 
  quantile(notas$nota, probs = .66, na.rm = TRUE) %>% 
  print()

cstudn <- filter(notas, nota >= 5.5) %>% 
  nrow()
cstudn
quantile(cstudn, probs = .66)

#otra forma
quantile(nota, 0.66)
quantile(nota[grupo == "C"], 0.66)

#7-----------------------------
#Un alumno tiene una nota de 4.9. 
#¿Qué porcentaje, del total de alumnos, tiene una nota menor o igual que la suya? 
#¿Y qué porcentaje tiene una nota mayor o igual que la suya?

#8-----------------------------
#Realiza el gráfico de diagramas de caja de las notas de cada grupo, 
#para poder comparar el nivel de cada uno de ellos.

boxplot(nota ~ grupo, data = notas)

boxplot(nota[grupo == "A"]) %>% 
  boxplot(nota[grupo == "B"]) %>% 
  boxplot(nota[grupo == "C"]) %>% 
  boxplot(nota[grupo == "D"]) %>% 
  boxplot(nota[grupo == "E"]) %>% 
  layout.show(n = 5)

#9----------------------------
#Si la variable conc recoge la concentración de plomo (en ppm) en el aire 
#de cierta zona durante un día complet
conc
# ¿Cuál ha sido la concentración máxima?
max(conc)
#   ¿En cuántos de los muestreos se ha superado la concentración de 40.0 ppm?
which(conc > 40)
sum(conc >= 40)
#   ¿Cuál ha sido la concentración media del día?
mean(conc)
#   ¿Cuáles fueron las 10 mediciones más bajas del día?
sort(conc) %>% 
  head(10)
#   Si la primera medida fue a las 00:00. 
#¿A qué hora del día se alcanzó la concentración máxima?
as_tibble(conc) %>% 
  mutate(
    hour = seq(
    as.POSIXct("2020-01-01 00:00"),
    as.POSIXct("2020-01-01 23:59"),
    by = "5 min"
  ) ) %>% 
  print()
  


conc
length(conc)

max(conc)

ind_max <- which(conc == max(conc))

hora_Data <- (24)/length(conc)

hour <- ind_max*hora_Data
min <- round((hour - floor(hour))*60, 0)
min
hour %<>% floor()
paste(hour, ":", min, "hrs")
#PARTE 2---------------------
#PARTE 3---------------------

#Corre el siguiente código para cargar los vectores year y co2 en memoria

data(co2)
means = aggregate(co2, FUN=mean)
means
year = as.vector(time(means))
class(year)
co2 = as.vector(means)
class(co2)

#El vector co2 contiene medidas de  CO2 en la atmósfera, 
#en unidades de ppm, durante el periodo 1959-1997. 
#El vector year contiene sus años correspondientes.

#Calcular un vector de diferencias de  CO2 entre años consecutivos, que sería:
#CO2 en 1960 menos CO2 en 1959
#CO2 en 1961 menos CO2 en 1960 y así sucesivamente…

dif_vector <- diff(co2)
dif_vector
class(dif_vector)
#Crear un plot con lineas y puntos mostrando las diferencias consecutivas de  CO2
# en función del tiempo (1960, 1961, etc…), en negrita
# La diferencia de concentración de  CO2 entre 2020 y 2019 fue igual a 2.64. 
#Agregar un punto rojo representando esa diferencia al plot ya creado
newnames <- c("year", "diff_co2")

dfyear <- data.frame(year[2:39]) 
dfdif <- data.frame(dif_vector)

df <- cbind(dfyear, dfdif) %>% 
  as_tibble()

names(df)
rename(df, year = year.2.39. , diff_co2 = dif_vector)

names(df)

plot(df, 
     type = "b",
     pch = 16,
     xlab("Year"),
     ylab("CO2 difference"),
     main = "Consecutive differences in CO2 concentrarion (ppm)",
     xlim = c(1960, 2020),
     ylim = c(0.2, 2.7)
     )
points(x = 2020,
       y = 2.64,      # Coordenadas
       pch = 4,      # Símbolo
       cex = 2,       # Tamaño del símbolo
       bg = "red",   # Color de fondo del símbolo
       col = "red",  # Color del borde del símbolo
       lwd = 2# Ancho del borde del símbolo
       )       

#(usar una forma diferente, como pch=4)

#14---------------------------------
#Lee el archivo rainfall.csv como un data.frame
#Calcula e imprime un vector con los nombres de las estaciones 
#donde al menos uno de los meses tiene una precipitación superior a 180mm.

rainf <- read.csv("rainfall.csv") %>% 
  as_data_frame() %>% 
  print

rainf2 <- select(rainf, sep:name) 
rainf2

pp <- gather(data = rainf2, key = "mes", value = "pp", 1:9)
  
pp180 <- filter(pp180, pp >= 180)
table(pp180$name)


# 15--------------------
# Manipule los dataframe según se solicite
# Se tiene el conjuntos de datos de precipitación diaria (período 1980 - 2013)
#de ciertas estaciones meteorológicas (raingaugeDataset.csv), 
#donde cada una de estas están asociadas a un código único (p.e. qc00000208). 
#Asimismo, se tiene una lista con los nombres, códigos, coordenadas y elevación
#de cada una de las estaciones (listRaingauge.csv).
#Grupo 07: LAMBAYEQUE

MetEst <- read_csv("listRaingauge.csv") %>% 
  filter(DEPARTAMENTO == "LAMBAYEQUE", NOM_EST == "LAMBAYEQUE") %>% 
  print()

dataL <- read_csv("raingaugeDataset.csv") %>%
  select(date, "qc00000301") %>% 
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
  rename(pp = qc00000301) %>%
  arrange(date) %>% 
  as.tibble()

tail(dataL)
seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by = "day") %>% 
  length()
# De lo descrito anteriormente, se solicita:
# Determine la cantidad de missing values de la serie de tiempo a paso diario. 

NAVALUES <-
  dataL %>%
  mutate( #indicara el nro de missing values de cada mes
    misVal = is.na(pp)#retorna la suma del conteo de na de la columna pp
  ) %>%
  summarise(
    naVal = sum(misVal)
  ) %>%
  print()

# Calcule la serie de tiempo de precipitación acumulada mensual 
#(si el # de días con missing values, en un mes, 
#supera el 10%, la precipitación acumulada mensual será considerado como un NA).

NAVALUES <-
  dataL %>%
  mutate( #indicara el nro de missing values de cada mes
    misVal = is.na(pp)#retorna la suma del conteo de na de la columna pp
  ) %>% 
  print()

ppMonth <- 
  NAVALUES %>% 
  group_by(date = str_sub(date, 1,7)) %>% 
  mutate( #indicara el nro de missing values de cada mes
    misVal = sum(is.na(pp))*100/n() #retorna la suma del conteo de na de la columna pp y lo convierte a %
  ) %>% 
  summarise( #creara una tabla resumen con el promedio de pp y su % de NA
    pp = sum(pp, na.rm = T ),#suma de la pp por mes
    misVal = unique(misVal)
  ) %>% 
  mutate(
    pp = ifelse(misVal >= 10, NA, pp),#si misVal es igual o mayor a 10, se convierte en NA, de lo contrario, conserva su valor
    date = as.Date(sprintf("%1$s-01", date)),
    month = str_sub(date, 6,7)
  ) %>% 
  print()
# Determine la cantidad de missing values de la serie de tiempo a paso mensual.
ppMonth
naCant <- 
  ppMonth%>% 
  summarise(isNa = sum(is.na(pp))) %>% 
  print()

# Cree una función que calcule, a partir de los datos de preicpitación mensual, 
#la climatología (Ene-Dic) para el período 1980-2010.  

pp8010 <-
  ppMonth %>% 
  filter(date >= "1980-01-01" & date <="2010-12-31") %>% 
  print()
  group_by(month) %>%
  summarise(
    ppmean = mean(pp, na.rm = T)
  ) %>%
  print()

  
# clima <- function(x, y, z){
#   x <- pp8010
#   y <- pp8010$month
#   z <- pp8010$pp
#   
#   group_by(x, y) %>%
#     summarise(
#       ppmean = mean(z, na.rm = T)
#     ) %>% 
#     print()
#   return(clima)
# }

# Plotear (boxplot) la variabilidad de los valores mensuales (Ene-Dic) 
  #para el período 1980-2013.
  pp8010 <-
    ppMonth %>% 
    filter(date >= "1980-01-01" & date <="2010-12-31") %>% 
    print()
  
  ggplot(pp8010, aes(month, pp)) + #en el eje x van los meses
    geom_boxplot()+ #definimos la geometria de boxplot
    theme_bw()+ #se le agrega un tema
    scale_x_discrete( #le damos la etiqueta a las meses 
      labels = month.abb #.abb primeras tres letras ene, feb, mar, etc
    )
  
                                                               