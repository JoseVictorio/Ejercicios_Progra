#1RA PARTE ----
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
load()
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
names(nota) <- grupo
nota
nota[ grupo == "B" & nota >= 6]
length(nota[ grupo == "B" & nota >= 6])

#e

x <- length(nota[ grupo == "C" & nota >= 6])

y <- length(nota[grupo == "C"])

(x/y)*100

#f
names(nota) <- NULL #Si borramos el nombre del vector no sale
names(nota) <- grupo
which(nota == min(nota))
which(nota == max(nota))

#g

mean(nota[grupo %in% c("A", "B") & nota >= 6])

subset()

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

conc
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



  #2DA PARTE ----

#1

x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9 , 10)
y <- c(1, 4, 6, 8, 25, 36, 49, 61, 81, 100)

plot(x, y, type = "o")

#2

A <- cbind(c(1:4), seq(2, 8, by = 2), seq(3, 12, by = 3))
A

#3

i <- diag(1, 3, 3)
i

#4
#Con matriz
m_nula <- matrix(rep(0, 16), nrow = 4, ncol = 4)
m_nula

#Con function
matrix_nula <- function(k) {
  nula <- diag(k);
  for (i in 1:k) {
  nula[i, i] = 0
  };
  return(nula)
}
matrix_nula(4)  

#5
diag(c(0, 2, 3, 4))

B <- diag(4)
B[1, 1] = 0
B[2, 2] = 2 
B[3, 3] = 3
B[4, 4] = 4
B

#6
A
A_T <- matrix(matrix_a, nrow = 3, ncol = 4, byrow = T)
A_T

t(A) #Funcion transpuesta de A

#7

#8

P <- matrix(c(1, -2, 1, 2, 4, 0, 3, -2, 1), nrow = 3, ncol = 3)
P %*% P 

pot_m <- function(m, n) {
  pot = m;
  for (i in 2:n) { 
    pot <- pot %*% m
    };
  return(pot)
}
pot_m(P, 2)

#9

solve()

#13

data(co2)
means = aggregate(co2, FUN=mean)
year = as.vector(time(means))
co2 = as.vector(means)

co2
lag(co2)

dif <- co2 - lag(co2)
dif

dif_2020_2019 <- 2.64
year_2020 <- 2020

plot(x = year, y = dif, 
     type = "b", 
     xlab = "Año", 
     ylab = "CO2 aumento por año",
     xlim = c(1960, 2020),
     ylim = c(0.2, 2.7))
points(x = year_2020, y = dif_2020_2019, pch = 4, col = "red")

#14
rainfall <- read_csv("data/rainfall.csv")
rainfall

result <- rainfall %>% 
                  dplyr::filter(sep >= 180 | oct >= 180 |
                                  nov >= 180 | dec >= 180 |
                                  jan >= 180 | feb >= 180 |
                                  mar >= 180 | apr >= 180 |
                                  may >= 180) %>%
          select(name)

result2 <- as.vector(result[[1]])


rain2 <- read.csv("data/rainfall.csv") %>% 
  as_data_frame() %>% 
    print
