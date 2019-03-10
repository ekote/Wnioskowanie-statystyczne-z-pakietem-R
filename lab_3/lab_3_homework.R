# 3.6
sd <- 1.5
# (1 - ALFA) = 0.99
afla <- 1 - 0.99
d <- 0.5 # blad_oszacowania_sredniego 
#(qt(1-alfa/2,length(WeightInitial)-1)*sd(WeightInitial)/0.5)^2
#(qnorm(alfa))^2
# przez analogie
wzor <- (qnorm(1 -(afla/2) * sd/d))^2
wzor
# n >= [1] 4.709292
    

# 3.8
d <- 0.025
alfa <- 1 - 0.95
# Cecha X (czyta vs nie czyta) ma rozklad dwupunktowy 
# P(X=1)=p
# P(X=0)=q = 1 - p , p-nieznane
# Model IV wzor 2
# ? Pytanie o kwadrat w innym miejscu
wzor1 <- (qnorm(1 -(alfa/2) * 1/4*d^2))^2
wzor2 <- (qnorm(1 -(alfa/2)))^2 * 1/4*d^2
wzor1 # [1] 19.98321
wzor2 # [1] 0.0006002279 - to na pewno nie. Nie ma sensu.

# 3.9
d <- 0.01
alfa <- 1-0.9
p_0 <- 0.16 # 16 % nie zapina pas�w
q_0 <- 1 - p_0 # q_0 - zapinajacy pasy
q_0
# Model IV wzor 1
parametr_do_qnorma <- (1-(alfa/2)* p_0 * q_0 / d^2)
parametr_do_qnorma
wzor1 <- (qnorm(1-(alfa/2)* p_0 * q_0 / d^2))^2
wzor2 <- qnorm(1-(alfa/2))^2 *p_0*q_0/d^2
wzor1
wzor2 # [1] 3636.25



# 3.4
# A)
library(MASS)
data(geyser)
attach(geyser)

View(geyser)
# test shapiro 
shapiro.test(duration) # W = 0.84352, p-value < 2.2e-16
# wynik to prawie zero

# p-value <= alfa => odrzucamy hipoteze o
# p-value > alfa => nie ma podstaw do odrzucenia hipotezy
# JAKI POZIOM ISTOSTOSCI?
# przyjmujemy jak w zadaniu z kozami
# L = 0.05 a u nas p.value bliskie 0
# Odrzucamy hipoteze => to nie jest rozklad normalny 

qqnorm(duration) # prosta
qqline(duration) # wykres

# dane nie leza na kresce
plot(duration) # totalnie nie wyglada na rozklad normalny
hist(duration) 
# B)

przedzial_srednia <- function(dane, poziom_ufnosci){
  n <- length(dane)
  alfa <- 1-poziom_ufnosci
  s <- sqrt(var(dane))
  t_prawy <- n - s/sqrt(n) 
  t_lewy <- 1-alfa/2
  t <- qt(t_lewy, t_prawy)
  x <- mean(dane)
  
  return(c(x-t, x+t))
}

przedzial_srednia(duration,0.95)
# [1] 1.492882 5.428745
t.test(x =duration ,conf.level=0.95)$conf.int
# [1] 3.330171 3.591457
 