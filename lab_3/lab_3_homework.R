# 3.6
# Ustalić, jak liczna powinna być próba, aby na jej podstawie można było oszacować
# wzrost noworodków, jeżeli wiadomo, że ma on rozk lad normalny o odchyleniu standardowym 1,5 cm.
# Przyja̧ć, że maksymalny błąd oszacowania średniego wzrostu na poziomie ufności 0,99 ma wynosić
# 0,5 cm.
sd <- 1.5
# (1 - ALFA) = 0.99
alfa <- 1 - 0.99
d <- 0.5 # blad_oszacowania_sredniego 
#(qt(1-alfa/2,length(WeightInitial)-1)*sd(WeightInitial)/0.5)^2
#(qnorm(alfa))^2
# przez analogie
wzor <- (qnorm(1 -(alfa/2) * sd/d))^2
wzor
# n >= [1] 4.709292
    

# 3.8
# Jak duża̧ próbę należy pobrać, aby z maksymalnym błędem 2,5% oszacować na poziomie
# ufności 0,95 procent doros lych Polaków czytająych rocznie przynajmniej jedną książkę?
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

# te na gorze sa zle - dlatego tutaj dobre wzory
wzor3 <- (qnorm(1 -(alfa/2)))^2 * (1/(4*d^2))
# co jest równoważne:
wzor4 <- ((qnorm(1 -(alfa/2))) * (1/(2*d)))^2



# 3.9
# Jak dużą próbę należy pobrać, aby z maksymalnym błędem 1% oszacować na poziomie
# ufności 0,9 procent kierowców nie zapinaja̧cych pasów bezpieczeństwa? Uwzględnić rezultaty wstępnych
# badań, z których wynika, że interesującą nas wielkość jest rzędu 16%.
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




# 3.4
# A) Napisać funkcję, która dla dużej próby losowej (n ≥ 100) z dowolnego rozkładu,
# zwraca przedział ufności dla średniej na zadanym poziomie ufności. Zadbać by funkcja zwracała bła̧d
# w przypadku jej użycia do próby o liczności mniejszej niż 100.
# (b) W pakiecie MASS znajduje się zbiór danych geyser zawierająy kolumnę duration z czasami trwania
# (w min) wybuchów gejzeru Old Faithful w Parku Narodowym Yellowstone w USA.
# (b1) Sprawdzić czy można uznać, że rozkład czasu trwania wybuchu tego gejzeru jest normalny.
# (b2) Na poziomie ufności 0,95 wyznaczyć przedział ufności dla średniego czasu trwania wybuchu
# tego gejzeru.

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

# Wiadomosc od prowadzacej
# Wniosek, że dane nie pochodzą z rozkładu normalnego jest poprawny (proszę
# zwrócić uwagę na fakt, że wniosek ten będzie taki sam dla każdego
# rozsądnie przyjętego poziomu istotności - za poziom istotności przyjmuje
# się liczby rzędu 0.01 lub 0.1, najczęściej używane to 0.05).

# Zatem formalnie nie mają Państwo podstaw do korzystania z MODELU II.
# Trzeba wybrać inny model!
# Ale nawet gdyby model II tu pasował, to źle go Państwo zaimplementowali.
# Proszę popatrzeć uważnie na wzór. Kwantyl t ma dwa indeksy: 1-(alfa/2)
# oraz n-1. Następnie kwantyl ten jest mnożony przez s/sqrt(n).

# Ciekawostką jest że używając modelu II i tego, który tu rzeczywiście
# pasuje, otrzymają Państwo zbliżone wyniki. Na zajęciach zastanowimy się
# dlaczego tak się dzieje.


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
 