# zadania do domu: Zad 2.2 
# a estymator najwiekszej wiarygodnosci
# b wartosci oczekiwanej X1
# Zad 2.3 a 
# b




#2.3
?rgamma
set.seed(1)
gamma100<-rgamma(n=100,shape=2,scale=1)
gamma100res<-fitdistr(gamma100,"gamma",lower=c(0,0))
gamma100res

gamma1000<-rgamma(n=1000,shape=2,scale=1)
gamma1000res<-fitdistr(gamma1000,"gamma", start = list(shape = 2, scale = 1), lower=c(0,0))
gamma1000res


#ZADANIE 2.3
#Generuje liczby z rozkladu Gamma(2,1)
set.seed(1337) #ustawienie ziarna = powtarzalnosc
Dane100 <- rgamma(n=100,shape=2,scale=1)
Dane1000 <- rgamma(n=1000,shape=2,scale=1)

params100 <-fitdistr(Dane100,"gamma"
                     ,lower=c(0,0))
params1000 <-fitdistr(Dane1000,
                      "gamma",lower=c(0,0))

print(params100)
print(params1000)




kozy <- read.table(file.choose(), header = T)
attach(kozy)
# test shapiro wike
shapiro.test(WeightInitial)
# p-value <= alfa => odrzucamy hipoteze o
# p-value > alfa => nie ma podstaw do odrzucenia hipotezy
# L = 0.05 a u nas p.value 0.1 
# odrzucamy H0 -> H1 przyjmujemy -> nie jest normalny rozklad

# inna metoda sprawdzenia normalnosci na "na oko"
# wykres kwanylowy (normalnosci, kwatnyl-kwantyl)
qqnorm(WeightInitial) # prosta
qqline(WeightInitial) # wykres
# punkty w miare dobrze sie ukladaja wzdluz prostej
#co moze sugeroewac ze dane pochodza z rozkladu normalnego

# 3.2
# to co wiemy
# - wiemy ze waga koz ma rozklad normalny
# n - minimalna licznosc proby
# n >= (t_ 1- ALFA/2, n_0-1 * S_0/d)^2
# d to maksymalny blad
# 1 - ALFA to poziom ufnosci
# n_0 pobranej probki wstepnej 
# S_0 to odchylenie standardowe popranej probki wstepnej


d = 0.5
# S_0 
so <- sd(WeightInitial)

# n_0 to lciznosc probki wstepnej
no <- length(WeightInitial)

# 1 - ALFA poziom ufnosci. My chcemy miec 0.9 (szacujemy na takim poziomie)
# 1 - alfa = 0.9
alfa <- 0.1
1-alfa/2 # 0.95 

# kwantyl rozkladu t-Studenta
(qt(1-alfa/2, no - 1) * so/d)^2  # [1] 138.3298

# potrzebujemy 139 obserwacji
# Ile trzeba dolosowac?
139 - no # 99 - tyle trzeba dolosowac


# 3.3
# PRZEDZIAL UFNOSCI DLA WARIANCJI
# (n-1)*S^2 / 
# n - licznosc
# S^2 - wariancja
# poziom ufnosci
# HI/FI - qchisq()

przedzial.var <- function(dane, poziom.ufnosci) {
  # musimy lewa i prawa strone policzyc wiec
  # start: liczymy licznik
  n.zerio.minus1 <- length(dane) - 1
  licznik <- (n.zerio.minus1^2)*var(dane)
  alfa <- 1-poziom.ufnosci
  lewa <- licznik/qchisq(1-alfa/2, n.zerio.minus1)
  prawa <- licznik/qchisq(alfa/2, n.zerio.minus1)
  return(c(lewa,prawa))
}
#b1 przedzial ufnosci dla wariancji wagi kóz:
przedzial.var(WeightInitial, 0.99)

# b2 - przedzial ufnosci dla odchylenia standardowego wagi koz
sqrt(przedzial.var(WeightInitial, 0.99))


# 3.7 - badana cecha ma rozklad dwupunktowy
# znalezc przedzial ufnosci dla frakcji (to znaczy prawdopodobienstwo)
# nie mozemy stosowac przyblizenia rozkladem normalnym - czyli nie 
# mozemy uzywac binom.test bo 
# nq^(z daszkiem) - liczba porazek = 4 ( 25 - 21 ) - jest mniejsze niz 5 - nie mozemy uzywac prop testa
# pozostaje nam uzyc 
binom.test(x = 21, n = 25 ,conf.level = 0.9)$conf.int #[1] 0.6703917 0.9434374




# 
plot(x,y, col="back", main="NAGLOWEK", xlab="", ylab="", type="p")

p - punkty
l - linia
b - bolad cos innego

lines - to samo co plot ale ma domyslnie ustawione type=line <- 
  ona nie czysci okna graficznego

text(x,y, labels="tekst", col="black")