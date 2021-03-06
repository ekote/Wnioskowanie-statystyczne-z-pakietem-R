########### Zaj�cia 3 #############################
###################################################

#### ZADANIE 2.3 #################################

probka1 <- rgamma(n = 100, shape = 2, scale = 1)

fitdistr(x = probka1, densfun = "gamma")

?fitdistr
fitdistr(x = probka1, densfun = "gamma", 
         start = list(shape = 2, scale = 1), lower = c(0,0))
# Oszacowane wartosci parametrow: shape = 1.86, scale = 1.13
# Pierwotne wartosci: shape = 2, scale = 1

####### zadanie 3.1 ##########################
kozy <- read.table(file.choose(),header=T)
attach(kozy)
#H0 - pr�bka pochodzi z rozkladu normlaneho
#H1 - probka nie pochodzi z rozkladu normalnego
shapiro.test(WeightInitial)
#p-value = 0.1034 >alfa=0,05 - nie ma podstaw do 
#odrzucenia H0

# inna metoda sprawdzenia normalno�ci "na oko"
# wykres kwantylowy (normalno�ci, kwantyl-kwantyl)
qqnorm(WeightInitial) #wykres
qqline(WeightInitial) #prosta
# punkty w miar� dobrze uk�adaj� si� wzdlu� prostej,
# co mo�e sugerowa�, �e dane pochodz� z rozk�. N

############## ZADANIE 3.2 ###################################
(24.07982-22.22018)/2   # 0.92982
# Przedzia� z poprzedniego zadania jst za dlugi.
# Z zadania 3.1, wiemy, �e waga k�z ma rozk�ad normalny.
# d=0.5
# S0=odchylenie standardowe pr�bki wst�pnej
sd(WeightInitial)
# n0=licznosc pr�bki wst�pnej
length(WeightInitial)
# 1-alfa=0.9 => 
alfa <- 0.1 
1-alfa/2  # 0.95

(qt(1-alfa/2,length(WeightInitial)-1)*sd(WeightInitial)/0.5)^2
# 138.3298 => potrzebujemy 139 obserwacji
# trzeba dolosowa� 99 obserwacji:
139-length(WeightInitial)

######### ZADANIE 3.3 ###############################
# a)
przedzial.var <- function(dane,poziom.ufnosci){
  n.zero.minus1 <- length(dane)-1
  licznik <- n.zero.minus1*var(dane)
  alfa <- 1-poziom.ufnosci
  lewa <- licznik/qchisq(1-alfa/2,n.zero.minus1)
  prawa <- licznik/qchisq(alfa/2,n.zero.minus1)
  return(c(lewa,prawa))
}
# b1) przedzia� ufnosci dla wariancji wagi k�z
przedzial.var(WeightInitial,0.99)
# b2) przedzia� ufnosci dla odchylenia standardowego wagi k�z
sqrt(przedzial.var(WeightInitial,0.99))

### Zadanie 3.7 #############
# Badana cecha na rozk�ad dwupunktowy.
# Nie mo�emy stosowa� przybli�enia rozk�adrm normalnym czyli
# nie mozemy uzywa� prop.test, bo
# nq.z.daszkiem=liczba pora�ek=25-21=4 jest mniejsze niz 5
binom.test(x=21,n=25,conf.level=0.9)$conf.int
# (0.6703917 ; 0.9434374)

# ZAD DOMOWE:
# 3.4, 3.5, 3.6, 3.8, 3.8

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

