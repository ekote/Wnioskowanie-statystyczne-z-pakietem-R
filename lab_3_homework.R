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
p_0 <- 0.16 # 16 % nie zapina pasów
q_0 <- 1 - p_0 # q_0 - zapinajacy pasy
q_0
# Model IV wzor 1
parametr_do_qnorma <- (1-(alfa/2)* p_0 * q_0 / d^2)
parametr_do_qnorma
wzor1 <- (qnorm(1-(alfa/2)* p_0 * q_0 / d^2))^2
wzor2 <- qnorm(1-(alfa/2))^2 *p_0*q_0/d^2
wzor1
wzor2 # [1] 3636.25

