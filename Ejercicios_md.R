#1 ----
#1.a

a <- (0.3*0.15)/((0.3*0.15) + (0.2*0.8) + (0.5*0.12))
round(a, 2)

#1.b

b <- (5^6 / factorial(6)) * (exp(1))^(-5)

round(b, 2)

#1.c

comb <- factorial(20) / (factorial(7) * (factorial(20-7)))
pot <- (0.4^7)*(0.6^13)

c <- comb * pot
round(c, 2)

#2 ----
#2.a Suma de primeros n numeros

(1000*(1000+1))/2 #(n*(n+1))/2

sum(1:1000)

#2.b Suma de serie de potencias

(1-(2^(10+1)))/(1-2) #(1-(r^(n+1)))/(1-r)

#3 ----
grupo
#3.a ¿Cuántos elementos tiene?

table(grupo)
length(grupo)

#3.b ¿En qué posiciones del vector está la letra “A”?

which(grupo == "A")

#4 ----
nota
#4.a

sum(nota)

#4.b

mean(nota)

#4.c

which(nota >= 7.0)

#4.d

sort(nota, decreasing = T)

#4.e

max(nota)
which(nota == 7.7)

#5 ----

#a

sum(nota[1:10])

#b

sum(grupo == "C")
table(grupo) #Comprobacion

#c
#Aprobado >=6

sum(nota >= 6)

#d

#Con tibble me complique.
library(tidyverse)
df_notas <- tibble(grupo, nota)
df_notas

c_aprob <- dplyr::filter(df_notas, grupo == "B" & nota >= 6)
dim(c_aprob)[1] #Cantidad de aprobados
table(c_aprob)

#Con vectores
nota[ grupo == "B" & nota >= 6]
length(nota[ grupo == "B" & nota >= 6])

#e

x <- length(nota[ grupo == "C" & nota >= 6])

y <- length(nota[grupo == "C"])

(x/y)*100

#f

which(nota == min(nota))
which(nota == max(nota))

#g

mean(nota[grupo %in% c("A", "B") & nota >= 6])

#6 ----
quantile(nota, 0.66) #Percentil 66 de toda la muestra

quantile(nota[grupo == "C"], 0.66)
#El 66% de la muestra tiene una nota de ""

#7 ----

(sum(nota <= 4.9)/length(nota))*100
(sum(nota >= 4.9)/length(nota))*100

#8 ----

boxplot(nota[grupo == "A"])
boxplot(nota[grupo == "B"])
boxplot(nota[grupo == "C"])
boxplot(nota[grupo == "D"])
boxplot(nota[grupo == "E"])

#9 ----

conc

#a 
max(conc)

#b
sum(conc >= 40.0)

#c
mean(conc)

#d

sort(conc)[1:10]

#e >>> Falta comprobar

library(magrittr)

length(conc)

max(conc)
which(conc == max(conc))

(24)/length(conc)

hour <- which(conc == max(conc))*((24)/length(conc))
min <- round((hour - floor(hour))*60, 0)
min
hour %<>% floor()
hour
paste(hour, ":", min)
