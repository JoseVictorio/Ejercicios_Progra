---
title: "Ejercicios"
author: 
- "Valeria Llactayo Peña           17160046"
- "Joaquin Romualdo Peña           17150052"
- "Javier Sullcaray Barrientos     17160201"
- "Jose Victorio Gonzales          17160051"
- "Daniel Yauri Leiva              17160208"
date: "10/07/2021"
output: html_document
header-includes:
  - \renewcommand{\and}{\\}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Este documento R Markdown presenta la resolución de ejercicios del Curso de Programación II. Se adjunta el repositorio con los archivos utilizados en el siguiente [enlace](https://github.com/JoseVictorio/Ejercicios_Progra).

```{r }
load("datos.Rdata")
```

## 1ra Parte

Cargar las variables almacenadas en el siguiente archivo [RData](https://goo.gl/uDzU8v).

**1. Calcula los valores numéricos aproximados de:**

 a) $\frac{0.3\cdot0.15}{0.3\cdot0.15+0.2\cdot0.8+0.5\cdot0.12}$
 
 ```{r a}
a <- (0.3*0.15)/((0.3*0.15) + (0.2*0.8) + (0.5*0.12))
round(a, 2)
```

 b) $\frac{5^6}{6!}e^{-5}$
 
 ```{r b}
 b <- (5^6 / factorial(6)) * (exp(1))^(-5)
round(b, 2)
 ```
 
 c) $\begin{equation}
{20 \choose 7}
\end{equation}\: 0.4^7\: 6^{13}$

```{r c}
comb <- factorial(20) / (factorial(7) * (factorial(20-7)))
pot <- (0.4^7)*(0.6^13)

c <- comb * pot
round(c, 2)
```

**2. Realizar la siguiente suma:**

 a) $1+2+3+\ldots + 1000$
 
 ```{r}
Sn<-function(n){
  n*(n+1)/2
}
Sn(1000)
 ```
 
 b) $1+2+4+8+16+\ldots + 1024$
 
  ```{r}
Sx<-function(a1, r, n){
  a1*(r**n-1)/(r-1)
}
Sx(1, 2, 11)
 ```

**3. El vector `grupo` representa el grupo al que pertenece una serie de alumnos:**

 a) ¿Cuántos elementos tiene?
 
   ```{r}
grupo

table(grupo)
length(grupo)
 ```
 
 b) ¿En qué posiciones del vector está la letra “A”?
 
   ```{r}
which(grupo == "A")
 ```

**4. El vector `nota` representa la nota de un examen de los alumnos que están en los grupos del vector `grupo`.**

 a) ¿Cuanto suman todas las notas?
 ```{r}
 sum(nota)
 ```
 b) ¿Cual es la media aritmética de todas las notas?
 ```{r}
 mean(nota)
 ```
  c) ¿En qué posiciones están las notas mayores de 7.0?
 ```{r}
 which(nota > 7.0)
 ```
 d) Visualiza las notas ordenadas de mayor a menor
 ```{r}
 sort(decreasing = T, nota)
 ```
 e) ¿En qué posición está la nota máxima?
 ```{r}
which.max(nota)
```

**5. A partir de los vectores `grupo` y `nota` definidos.**
```{r warning = FALSE, include =FALSE}
library(tidyverse)
```
 a) Suma las notas de los 10 primeros alumnos del vector
 ```{r}
 sum(nota[1:10])
 ```
 b) ¿Cuántos alumnos hay del grupo C?
 ```{r}
 grupo[grupo == "C"] %>% length()
 ```
 
 c) ¿Cuántos alumnos han aprobado?
 ```{r}
 nota[nota>=5.5] %>% length()
 ```
 d) ¿Cuántos alumnos del grupo B han aprobado?
 ```{r}
 grupo[(grupo == "B" & nota >=5.5)] %>% length()
 ``` 
 e) ¿Qué porcentaje de alumnos del grupo C han aprobado?
 ```{r}
 x<- grupo[grupo == "C" & nota >=5.5] %>% length()
 y<- grupo[grupo == "C"] %>% length
 (x/y*100)
 ```
 f) ¿De qué grupos son la máxima y mínima notas de toda la muestra?
 ```{r}
 names(nota) <- grupo
 which(nota == min(nota))
 which(nota == max(nota))
```

 g) Nota media de los alumnos de grupo A y B, juntos, considerando sólo a los que han aprobado.
 ```{r}
 mean(nota[grupo %in% c("A", "B") & nota>=5.5])
 ```
**6. Calcula el percentil 66 de las notas de todos los alumnos, y también de los alumnos del grupo C.**
```{r}
quantile(nota, 0.66)
quantile(nota[grupo == "C"], 0.66) 
```

**7. Un alumno tiene una nota de 4.9. ¿Qué porcentaje, del total de alumnos, tiene una nota menor o igual que la suya? ¿Y qué porcentaje tiene una nota mayor o igual que la suya?**
```{r}
length(nota[nota <= 4.9])/length(nota)*100
length(nota[nota >= 4.9])/length(nota)*100
```

**8. Realiza el gráfico de diagramas de caja de las notas de cada grupo, para poder comparar el nivel de cada uno de ellos.**
```{r}
boxplot(nota[grupo == "A"])
boxplot(nota[grupo == "B"])
boxplot(nota[grupo == "C"])
boxplot(nota[grupo == "D"])
boxplot(nota[grupo == "E"])
```

**9. Si la variable `conc` recoge la concentración de plomo (en ppm) en el aire de cierta zona durante un día completo**

 a) ¿Cuál ha sido la concentración máxima?
 ```{r}
 max(conc)
 ```
 b) ¿En cuántos de los muestreos se ha superado la concentración de 40.0 ppm?
 ```{r}
 conc[conc>40] %>% length()
 ```
 c) ¿Cuál ha sido la concentración media del día?
 ```{r}
 mean(conc)
 ```
 d) ¿Cuáles fueron las 10 mediciones más bajas del día?
 ```{r}
 sort(conc)[1:10]
 ```
 e) Si la primera medida fue a las 00:00. ¿A qué hora del día se alcanzó la concentración máxima?
 ```{r}
#Hallando el intervalo para la toma de muestras
(24*60)/(length(conc)) 

as_tibble(conc) %>% 
  mutate(hour = seq(
    as.POSIXct("2020-01-01 00:00"),
    as.POSIXct("2020-01-01 23:59"),
    by = "5 min"
  ))%>% 
  print() %>% 
  filter(value == max(value)) %>% 
  print()

```

## Parte 2

**1. 1.Graficar los puntos $(1,1),(2,4),(3,6),(4,8),(5,25),(6,36),(7,49),(8,61),(9,81),(10,100)$ en un plano utilizando RStudio.**
```{r}
a <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
b <- c(1, 4, 6, 8, 25, 36, 49, 61, 81, 100)

plot(a, b)
```

**2. Ingresar la matriz A en RStudio**
$A = \begin{pmatrix} 1 & 2 & 3 \\ 2 & 4 & 6 \\ 3 & 6 & 9 \\ 4 & 8 & 12  \end{pmatrix}$
```{r}
A<-cbind(seq(1, 4), 
         seq(2, 8, by = 2), 
         seq(3, 12, by = 3))
A
```

**3. Ingresar la matriz identidad de tamaño 3**
$I = \begin{pmatrix} 1 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 1  \end{pmatrix}$
```{r}
diag(1, 3, 3)
```

**4. Crea una función que cree una matriz nula ingresando las dimensiones**
```{r}
MatrizN <- function(k) {
  nula <- diag(k);
  for (i in 1:k) {
    nula[i, i] = 0
  };
  return(nula)
}
MatrizN(4)
```

**5. Modificar la matriz `diag(4)`, para que se parezca a la matriz B**
$B = \begin{pmatrix} 0 & 0 & 0 & 0 \\ 0 & 2 & 0 & 0 \\ 0 & 0 & 3 & 0 \\ 0 & 0 & 0 & 4  \end{pmatrix}$
```{r}
B<-diag(c(0, 2, 3, 4))
B
```

**6. Obtener la matriz transpuesta de A (ejercicio 2)**
```{r}
t(A)
```

**7. Realizar las siguientes operaciones $A + B$, $A - B$, $3B$ y $AB$**
```{r}
3*B
```

**8. Crea una función para calcular $P^6$**
$P = \begin{pmatrix} 1 & 2 & 3 \\ -2 & 4 & -2 \\ 1 & 0 & 1  \end{pmatrix}$
```{r}
P<-matrix(c(1, -2, 1, 2, 4, 0, 3, -2, 1), nrow = 3, ncol =3)
P
Potencia <- function(n) {
  P**n
}
Potencia(6)
```
```{r}
library(matlib)
```
**9. Resolver el sistema de ecuaciones**
$3x - y + z  = -1\\9x - 2y + z = -9\\3x + y - 2z = -9$

```{r}
matriz01 <- matrix(c(3,9,3,-1,-2,1,1,1,-2), nrow = 3, ncol =3 )
matriz01
matriz02<- matrix(c(-1, -9, -9), nrow = 3, ncol = 1)
matriz02
Solve(matriz01, matriz02)
```

**10. Utilizando la ayuda de R, investigue para qué sirven las funciones `eigen()` y `det()`**
det() = nos calcula el determinante de una matriz cuadrada
eigen() = Calcula autovalores y autovectores de matrices numéricas (dobles, enteras, lógicas) o complejas.
**11. Considerando las matrices**

$B= \begin{pmatrix} 
1 & 2 & 3 & 4 & 5 \\
2 & 4 & 6 & 8 & 10 \\
3 & 6 & 9 & 12 & 15 \\
4 & 8 & 12 & 16 & 20 \\
5 & 10 & 15 & 20 & 25 \\
6 & 12 & 18 & 24 & 30 \\
7 & 14 & 21 & 28 & 35 \\
8 & 16 & 24 & 32 & 40 \\
9 & 18 & 27 & 36 & 45 \\
10 & 20 & 30 & 40 & 50
\end{pmatrix}$

$A = \begin{pmatrix}
  0 & 1 & 0 & 1 & 0 \\
  1 & 0 & 1 & 0 & 1 \\
  0 & 1 & 0 & 1 & 0 \\
  0 & 1 & 0 & 0 & 1 \\
  1 & 0 & 1 & 1 & 0 \\
\end{pmatrix}$

Calcular $A \cdot B - A B^t$

```{r}
MA <- cbind(c(1:10), 
           seq(2, 20, by = 2), 
           seq(3, 30, by = 3), 
           seq(4, 40, by = 4),
           seq(5, 50, by = 5))

MB <- cbind(c(0, 1, 0, 0, 1), 
           c(1, 0, 1, 1, 0),
           c(0, 1, 0, 0, 1),
           c(1, 0, 1, 0, 1),
           c(0, 1, 0, 1, 0))

(MA %*% MB) - (MA %*% (t(MB)))
#B <- matrix(B, nrow = 5, ncol = 5, byrow = T)
```

**12. Considere**
$\hat\beta = (X^t \cdot X)^{-1} \cdot X^t \cdot Y$

Determine la matriz $\hat\beta$

$x=\begin{pmatrix}1 & 1\\ 1 & -1\\ 1 & 0\\ 1 & 1\\ 1 & 2\\ \end{pmatrix}$

$y = \begin{pmatrix}0\\0\\1\\1\\3\\\end{pmatrix}$

```{r }
x <- cbind(c(1:5), c(1, -1, 0, 1, 2))
y <- cbind(c(0, 0, 1, 1, 3))
 
(solve(t(x) %*% x)) %*% t(x) %*% y
```

**13. Corre el siguiente código para cargar los vectores `year` y `co2`** en memoria

```{r }
data(co2)
means = aggregate(co2, FUN=mean)
year = as.vector(time(means))
co2 = as.vector(means)
```

- El vector `co2` contiene medidas de $CO_2$ en la atmósfera, en unidades de *ppm*, durante el periodo 1959-1997. El vector `year` contiene sus años correspondientes.

- Calcular un vector de diferencias de $CO_2$ entre años consecutivos, que sería:
  - $CO_2$ en 1960 menos $CO_2$  en 1959
  - $CO_2$ en 1961 menos $CO_2$  en 1960
  - y así sucesivamente...
  
 ```{r}
co2

#corremos un valor a la derecha
lag(co2)

dif <- co2 - lag(co2)
dif
 ```
  
- Crear un **plot** con lineas y puntos mostrando las diferencias consecutivas de $CO_2$ en función del tiempo (1960, 1961, etc...), en **negrita**

```{r }

plot(x = year, y = dif, 
     type = "b",
     pch = 16,
     xlab = "Year",
     ylab = "CO2 increasing",
     main = "Consecutive differences in CO2 concentration (ppm)",
     xlim = c(1960, 2020),
     ylim = c(0.2, 2.7)
     )
```


- La diferencia de concentración de $CO_2$ entre 2020 y 2019 fue igual a 2.64. Agregar un punto rojo representando esa diferencia al plot ya creado (usar una forma diferente, como `pch=4`)

```{r}
dif_2020_2019 <- 2.64
year_2020 <- 2020

plot(x = year, y = dif, 
     type = "b",
     pch = 16,
     xlab = "Year", 
     ylab = "CO2 increasing",
     main = "Consecutive differences in CO2 concentration (ppm)",
     xlim = c(1960, 2020),
     ylim = c(0.2, 2.7))
points(x = year_2020, y = dif_2020_2019, pch = 4, col = "red")
```

**14.**

- Lee el archivo `rainfall.csv` como un `data.frame`
- Calcula e imprime un vector con los nombres de las estaciones donde al menos uno de los meses tiene una precipitación superior a 180mm.

``` {r}

rainf <- read_csv("rainfall.csv") %>% 
  select(sep:name) %>% #seleccionamos las columnas con las que trabajaremos
  gather(key = "mes", value = "pp", 1:9) %>% #acomodamos los datos en dos columnas
  filter(pp >= 180) %>% #aplicamos el filtro a la columna "pp"
  print() %>% 
  collect %>% .[[1]] %>% #visualizamos el resultado de las estaciones en un vector
  print()

```

## Parte 3

**15. Manipule los dataframe según se solicite**

Utilizando el csv **(raingaugeDataset.csv)** de datos de precipitación diaria (período 1980 - 2013) de la estación "LAMBAYEQUE" con código único ***"qc00000301"***, para obtenerlo, realizamos lo siguiente:

``` {r}

#filtrando los datos de nuestra estación en el archivo "listRaingauge.csv"
MetEst <- read_csv("listRaingauge.csv") %>% 
  filter(DEPARTAMENTO == "LAMBAYEQUE", NOM_EST == "LAMBAYEQUE")

MetEst
```

``` {r}
#trabajando con los datos de precipitación
dataL <- read_csv("raingaugeDataset.csv") %>%
  select(date, "qc00000301") %>% #seleccionando la columna de nuestra estación
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% #dandole un formato de fecha a la columna date"
  rename(pp = qc00000301) %>% #renombramos la columna 
  arrange(date) #ordenamos los datos 
 
dataL
```

``` {r}
#comprobando que los datos estén completos

tail(dataL) #visualizando la fecha del último dato
```

``` {r}
#obteniendo el numero de datos del periodo 1980-2013
seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by = "day") %>% 
  length()
```

a) Determine la cantidad de missing values de la serie de tiempo a paso diario.

``` {r}
NAVALUES <-
  dataL %>%
  mutate( #creando variable que indicará el número de missing values de cada mes
    misVal = is.na(pp) #retorna la suma del conteo de NA de la columna "pp"
  )
NAVALUES
```
``` {r}
#obteniendo la suma de NA 
summarise(NAVALUES, 
          naVal = sum(misVal)) %>%
  print()

```

b) Calcule la serie de tiempo de precipitación **acumulada mensual** (si el # de días con missing values, en un mes, supera el 10 %, la precipitación acumulada mensual será considerado como un **`NA`**).

```{r}

ppMonth <- 
  NAVALUES %>% 
  group_by(date = str_sub(date, 1,7)) %>% #extraemos solo el año y el més de la columna "date"
  mutate( #indicara el nro de missing values de cada mes
    misVal = sum(is.na(pp))*100/n() #retorna la suma del conteo de NA de la columna "pp" y lo convierte a %
  ) %>% 
  summarise( #creara una tabla resumen con la precipitación acumulada y su % de NA
    pp = sum(pp, na.rm = T ),#suma de la pp por mes
    misVal = unique(misVal) #para obtener los valores únicos de porcentaje
  ) %>% 
  mutate(
    pp = ifelse(misVal >= 10, NA, pp),#si misVal es igual o mayor a 10, se convierte en NA, de lo contrario, conserva su valor
    month = str_sub(date, 6,7) #agregamos la columna con los el mes que corresponde a cada dato
  ) %>% 
  print()

```
c) Determine la cantidad de **missing values** de la serie de tiempo a paso mensual.

```{r}

naCant <- 
  ppMonth%>% 
  summarise(isNa = sum(is.na(pp))) %>% 
  print()

```


d) Cree una función que calcule, a partir de los datos de preicpitación mensual, la **climatología (Ene-Dic)** para el **período 1980-2010**.

```{r}

pp8010 <-
  ppMonth %>% 
  filter(date >= "1980-01-01" & date <="2010-12-31") %>% 
  print() %>% 
  group_by(month) %>%
  summarise(
    ppmean = mean(pp, na.rm = T)
  ) %>%
  print()

```
e) Poltear (boxplot) la variabilidad de los valores mensuales (Ene-Dic) para el período 1980-2013.

```{r}
  ggplot(ppMonth, aes(month, pp)) + #en el eje x van los meses
    geom_boxplot()+ #definimos la geometria de boxplot
    theme_bw()+ #se le agrega un tema
    scale_x_discrete( #le damos la etiqueta a los meses 
      labels = month.abb #.abb primeras tres letras ene, feb, mar, etc
    )
```
